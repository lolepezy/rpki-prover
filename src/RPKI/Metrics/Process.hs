{-# LANGUAGE OverloadedStrings #-}

module RPKI.Metrics.Process (
    DiskIO(..),
    ProcessStats(..),
    processStat,
    getProcessPeakRss,
    getProcessDiskIO,
    countIncomingTraffic,
    getIncomingTraffic,
    mbToSize,
    sizeMb
) where

import           Control.Exception             (try, SomeException)
import           Control.Monad.IO.Class

import           Data.Int
import           Data.IORef
import           Data.Monoid.Generic

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Read                as Text

import           GHC.Generics
import           GHC.Stats

import           System.IO.Unsafe              (unsafePerformIO)

import           RPKI.AppTypes
import           RPKI.Time
import           RPKI.Store.Base.Serialisation


{- | Bytes the process moved between itself and the files it works with.

/proc/self/io offers two views of that and neither of them alone is enough:

 * `rchar`/`wchar` count what went through read()/write() and friends. They see 
   everything the process does through syscalls, cached or not, but they are blind 
   to mmap-ed files.

 * `read_bytes`/`write_bytes` are counted at the block layer instead. They see 
   page-ins of mmap-ed files and write-back of dirty pages, but they stay at zero 
   for anything that never reaches the device, i.e. page cache hits and everything 
   on tmpfs.

So each direction here is the larger of the two, which counts no byte twice and 
still notices both the mmap-ed storage and the temporary files on tmpfs.

The kernel adds the counters of reaped children to the ones of the parent, so what 
an rsync client process did is included as well. That also means that rsync reading 
its socket lands in `rchar`, i.e. for the rsync fetcher 'diskRead' includes what 
rsync pulled off the network.
-}
data DiskIO = DiskIO {
        diskRead  :: Size,
        diskWrite :: Size
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup DiskIO
    deriving Monoid    via GenericMonoid DiskIO


-- | What a process can say about its own resource use.
data ProcessStats = ProcessStats {
        statCpuTime         :: CPUTime,
        statMaxRtsHeap      :: MaxMemory,
        statProcessRss      :: MaxMemory,
        statDiskIO          :: DiskIO,
        statIncomingTraffic :: Size
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

processStat :: MonadIO m => m ProcessStats
processStat = do 
    statCpuTime <- getCpuTime
    RTSStats {..} <- liftIO getRTSStats
    let statMaxRtsHeap = MaxMemory $ fromIntegral max_mem_in_use_bytes
    statProcessRss <- MaxMemory . fromIntegral . unSize <$> getProcessPeakRss
    statDiskIO <- getProcessDiskIO
    statIncomingTraffic <- getIncomingTraffic
    pure ProcessStats {..}


-- | Peak RSS the process has ever reached (VmHWM).
-- Works only on Linux with procfs, returns 0 otherwise
getProcessPeakRss :: MonadIO m => m Size
getProcessPeakRss = liftIO $ do
    procStatus <- readProcNumbers "/proc/self/status"
    pure $ Size $ 1024 * Map.findWithDefault 0 "VmHWM" procStatus

-- | Total amount of data the process has read from and written to files.
-- Works only on Linux with procfs, returns zeros otherwise
getProcessDiskIO :: MonadIO m => m DiskIO
getProcessDiskIO = liftIO $ do
    procIo <- readProcNumbers "/proc/self/io"
    let field key = Size $ Map.findWithDefault 0 key procIo
    pure $ DiskIO
        (max (field "rchar") (field "read_bytes"))
        (max (field "wchar") (field "write_bytes"))


-- | Parse the "key: <number> [unit]" lines that procfs files are made of.
readProcNumbers :: FilePath -> IO (Map.Map Text.Text Int64)
readProcNumbers file = do
    content <- try @SomeException $ BS.readFile file
    pure $ case content of
        Left _      -> Map.empty
        Right bytes -> Map.fromList $ concatMap parseLine $ Text.lines $ Text.decodeUtf8 bytes
  where
    parseLine line =
        case Text.breakOn ":" line of
            (key, rest)
                | Text.null rest -> []
                | otherwise ->
                    case Text.decimal $ Text.stripStart $ Text.drop 1 rest of
                        Right (value, _) -> [(key, value)]
                        Left _           -> []


{- | Bytes this process has pulled in over the network so far.

It is a process-global counter rather than something in the AppState because the 
places that download things (`downloadConduit`) have no AppContext at hand, and 
because the worker's self-monitoring thread only has its `WorkerInput`.

It only counts HTTP response bodies, i.e. it doesn't include headers, TLS overhead 
or whatever an external rsync client process transfers on its own.
-}
incomingTrafficCounter :: IORef Size
incomingTrafficCounter = unsafePerformIO $ newIORef 0
{-# NOINLINE incomingTrafficCounter #-}

countIncomingTraffic :: MonadIO m => Int -> m ()
countIncomingTraffic bytes = liftIO $
    atomicModifyIORef' incomingTrafficCounter $ \s -> (s <> Size (fromIntegral bytes), ())

getIncomingTraffic :: MonadIO m => m Size
getIncomingTraffic = liftIO $ readIORef incomingTrafficCounter


mbToSize :: Int -> Size
mbToSize mb = Size $ fromIntegral mb * 1024 * 1024

sizeMb :: Size -> Int64
sizeMb (Size s) = s `div` (1024 * 1024)

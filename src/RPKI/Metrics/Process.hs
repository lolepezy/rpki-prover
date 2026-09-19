{-# LANGUAGE OverloadedStrings #-}

module RPKI.Metrics.Process (
    DiskIO(..),
    ProcessStats(..),
    processStat,
    getProcessPeakRss,
    getProcessDiskIO,
    countIncomingTraffic,
    getIncomingTraffic,
    foldProcNumbers,
    mbToSize,
    sizeMb
) where

import           Control.Exception             (try, SomeException)
import           Control.Monad.IO.Class

import           Data.Char                     (isSpace)
import           Data.Int
import           Data.IORef
import           Data.Monoid.Generic

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8

import           GHC.Generics
import           GHC.Stats

import           System.IO.Unsafe              (unsafePerformIO)

import           RPKI.AppTypes
import           RPKI.Time
import           RPKI.Store.Base.Serialisation


{- | Bytes the process moved between itself and the files it works with.

The kernel offers two views of that and neither of them alone is enough:

 * `rchar`/`wchar` count what went through read()/write() and friends. They see 
   everything the process does through syscalls, cached or not, but they are blind 
   to mmap-ed files.

 * `read_bytes`/`write_bytes` are counted at the block layer instead. They see 
   page-ins of mmap-ed files and write-back of dirty pages, but they stay at zero 
   for anything that never reaches the device, i.e. page cache hits and everything 
   on tmpfs.

So each direction here is the larger of the two, which counts no byte twice and 
still notices both the mmap-ed storage and the temporary files on tmpfs.

The kernel adds the counters of reaped children to the ones of the parent, and a 
child's own reaped children were already in its counters, so this covers a whole 
tree of processes, as long as they have exited and been waited for. That is on 
purpose: what an rsync client process did counts as the rsync fetcher's, since that
is where the disk IO of an rsync fetch actually happens. It also means that rsync
reading its socket lands in `rchar`, i.e. for the rsync fetcher 'diskRead' includes 
what rsync pulled off the network. What it doesn't see is a child that is still 
running: its IO only shows up once it has exited.

This is different from the CPU time and the peak RSS, which are about this process
alone.
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
getProcessPeakRss = liftIO $
    -- /proc/self/status is some 60 lines and exactly one of them is wanted.
    Size . (* 1024) <$> foldProcNumbers "/proc/self/status" 0 (\case
        "VmHWM" -> Just const
        _       -> Nothing)

-- | Total amount of data the process, and the children it has reaped, have 
-- read from and written to files.
-- Works only on Linux with procfs, returns zeros otherwise
getProcessDiskIO :: MonadIO m => m DiskIO
getProcessDiskIO = liftIO $ do
    IoCounters {..} <- foldProcNumbers "/proc/self/io" (IoCounters 0 0 0 0) $ \case
        -- Whole keys, not prefixes: the file also has a `cancelled_write_bytes`.
        "rchar"       -> Just $ \v c -> c { rchar      = Size v }
        "wchar"       -> Just $ \v c -> c { wchar      = Size v }
        "read_bytes"  -> Just $ \v c -> c { readBytes  = Size v }
        "write_bytes" -> Just $ \v c -> c { writeBytes = Size v }
        _             -> Nothing
    pure $ DiskIO (max rchar readBytes) (max wchar writeBytes)

-- | The counters of /proc/self/io that 'getProcessDiskIO' is made of.
data IoCounters = IoCounters {
        rchar      :: Size,
        wchar      :: Size,
        readBytes  :: Size,
        writeBytes :: Size
    }


{- | Scan the "key: <number> [unit]" lines that procfs files are made of, 
     keeping only the keys the caller asks for.

     `wanted` is given each key as it comes up and answers with what to do with 
     its value, or 'Nothing' to skip the line. Skipped lines are never turned 
     into a number, and no line is ever decoded or collected into a map, which 
     matters because a worker reads its IO counters once a second for every 
     thread it has.
-}
foldProcNumbers :: FilePath 
                -> a 
                -> (BS.ByteString -> Maybe (Int64 -> a -> a)) 
                -> IO a
foldProcNumbers file initial wanted = 
    either (const initial) (foldl' takeLine initial . C8.lines) 
        <$> try @SomeException (BS.readFile file)
  where
    takeLine !acc line =
        case C8.break (== ':') line of
            (key, rest)
                | BS.null rest -> acc
                | otherwise    ->
                    case wanted key of
                        Nothing  -> acc
                        Just set ->
                            -- Whitespace, not just spaces: /proc/self/io separates
                            -- the two with a space but /proc/self/status uses a tab.
                            case C8.readInt $ C8.dropWhile isSpace $ BS.drop 1 rest of
                                Just (value, _) -> set (fromIntegral value) acc
                                Nothing         -> acc


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

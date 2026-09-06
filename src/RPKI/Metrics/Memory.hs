{-# LANGUAGE StrictData        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A snapshot of where the process's memory actually is.
--
-- 'RPKI.Metrics.System.ResourceUsage' tracks the Haskell heap, which the RTS
-- reports directly. That is only part of the picture: SQLite allocates through
-- the C allocator, so the gap between the RTS numbers and the process's RSS is
-- invisible to GHC.Stats. This module samples all three levels at once
--
--   * the whole process, from \/proc\/self\/status
--   * the Haskell heap, from GHC.Stats
--   * the C allocator, from glibc's mallinfo2, plus SQLite's own accounting
--
-- so a sample can be attributed rather than guessed at: 'mallocInUse' against
-- 'mallocFromOs' separates memory that is genuinely held from memory the
-- allocator kept after it was freed, and 'sqliteInUse' says how much of the
-- former is SQLite's.
module RPKI.Metrics.Memory where

import           Control.Exception             (try, SomeException)
import           Control.Monad.IO.Class

import           Data.Aeson                    ((.=))
import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Int                      (Int64)
import qualified Data.Map.Strict               as Map
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Read                as Text

import           Foreign.C.Types               (CInt(..))
import           Foreign.Marshal.Array         (allocaArray, peekArray)
import           Foreign.Ptr                   (Ptr)

import           GHC.Generics
import           GHC.Stats

import           RPKI.AppTypes
import           RPKI.Store.Base.Serialisation


-- | One memory sample. Every field is in bytes; a field this platform cannot
-- provide is 0 (see 'mallocStatsAvailable').
data MemoryStats = MemoryStats {
        -- | Resident set size of the whole process.
        processRss           :: Size,
        -- | Peak RSS since start (VmHWM); unlike 'processRss' it never drops.
        processRssPeak       :: Size,
        -- | Resident anonymous memory: the Haskell heap plus everything the C
        -- allocator holds. This is the number to watch.
        processRssAnon       :: Size,
        -- | Resident file-backed memory. For LMDB this is the mapped database;
        -- for SQLite it is close to nothing, since its cache lives in the
        -- kernel page cache, outside the process.
        processRssFile       :: Size,
        processVmSize        :: Size,

        -- | What the RTS currently has mapped from the OS for the Haskell
        -- heap, as of the last GC. This is the figure to subtract from
        -- 'processRssAnon' to get the C side's share.
        rtsInUse             :: Size,
        -- | High-water mark of the above since start.
        rtsPeakInUse         :: Size,
        -- | Live Haskell data as of the last GC.
        rtsLive              :: Size,
        -- | Bytes allocated by Haskell code since start (monotonic). Only
        -- refreshed at a GC, so it reads 0 until the first one has run.
        rtsAllocated         :: Size,
        rtsGcs               :: Int64,
        rtsMajorGcs          :: Int64,
        -- | CPU spent in GC and in the program proper, since start. The ratio
        -- is what a tighter heap actually costs: shrinking the heap trades
        -- memory for GC time, and this says how much.
        rtsGcCpuMs           :: Int64,
        rtsMutatorCpuMs      :: Int64,

        -- | Total the C allocator has taken from the OS (arena + mmapped).
        mallocFromOs         :: Size,
        -- | Of that, what is handed out and actually in use.
        mallocInUse          :: Size,
        -- | Of that, what was freed but is still held by the allocator. Large
        -- here against a small 'mallocInUse' means fragmentation rather than
        -- genuine demand.
        mallocFree           :: Size,
        -- | What malloc_trim could plausibly hand back to the OS.
        mallocReleasable     :: Size,
        -- | False when the platform has no mallinfo2 (musl), so that the four
        -- fields above read 0 because they are unknown, not because they are
        -- genuinely zero.
        mallocStatsAvailable :: Bool,

        -- | SQLite's own accounting of what it currently holds.
        sqliteInUse          :: Size,
        -- | SQLite's high-water mark since start.
        sqlitePeak           :: Size,
        -- | Prepared statements cached across all connections. Each costs
        -- SQLite heap in proportion to its parameter count, so this is the
        -- first thing to check when 'sqliteInUse' looks too big.
        preparedStatements   :: Int
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

emptyMemoryStats :: MemoryStats
emptyMemoryStats = MemoryStats {
        processRss = 0, processRssPeak = 0, processRssAnon = 0,
        processRssFile = 0, processVmSize = 0,
        rtsInUse = 0, rtsPeakInUse = 0, rtsLive = 0, rtsAllocated = 0,
        rtsGcs = 0, rtsMajorGcs = 0, rtsGcCpuMs = 0, rtsMutatorCpuMs = 0,
        mallocFromOs = 0, mallocInUse = 0, mallocFree = 0, mallocReleasable = 0,
        mallocStatsAvailable = False,
        sqliteInUse = 0, sqlitePeak = 0, preparedStatements = 0
    }

-- | Resident anonymous memory that is not the Haskell heap, i.e. what the C
-- side costs the process. Derived, so it cannot drift from its inputs.
nonHaskellMemory :: MemoryStats -> Size
nonHaskellMemory MemoryStats {..} =
    Size $ max 0 $ unSize processRssAnon - unSize rtsInUse

-- | Share of CPU spent in GC rather than doing work. Rises when the heap is
-- squeezed, so it is the counterweight to any heap-shrinking RTS tuning.
gcCpuFraction :: MemoryStats -> Double
gcCpuFraction MemoryStats {..}
    | total <= 0 = 0
    | otherwise  = fromIntegral rtsGcCpuMs / fromIntegral total
  where
    total = rtsGcCpuMs + rtsMutatorCpuMs

-- | Freed-but-retained as a fraction of what the allocator holds. Close to 1
-- means almost everything the C allocator is holding is fragmentation.
mallocFreeRatio :: MemoryStats -> Double
mallocFreeRatio MemoryStats {..}
    | unSize mallocFromOs <= 0 = 0
    | otherwise = fromIntegral (unSize mallocFree) / fromIntegral (unSize mallocFromOs)


-- ---------------------------------------------------------------------------
-- Gathering
-- ---------------------------------------------------------------------------

-- | Take a sample. The prepared statement count is passed in so that this
-- module doesn't have to depend on the storage layer.
getMemoryStats :: MonadIO m => Int -> m MemoryStats
getMemoryStats preparedStatementCount = liftIO $ do
    procStatus <- readProcStatus
    let procField key = Size $ 1024 * Map.findWithDefault 0 key procStatus

    (mallocAvailable, mallocValues) <- readMallocStats
    let mallocField i = Size $ case drop i mallocValues of
                                (v : _) -> v
                                []      -> 0

    RtsSample {..} <- readRtsStats

    sqliteUsed <- Size <$> c_sqlite3_memory_used
    sqliteHigh <- Size <$> c_sqlite3_memory_highwater 0

    pure MemoryStats {
            processRss     = procField "VmRSS",
            processRssPeak = procField "VmHWM",
            processRssAnon = procField "RssAnon",
            processRssFile = procField "RssFile",
            processVmSize  = procField "VmSize",

            rtsInUse     = rtsSampleInUse,
            rtsPeakInUse = rtsSamplePeakInUse,
            rtsLive      = rtsSampleLive,
            rtsAllocated = rtsSampleAllocated,
            rtsGcs          = rtsSampleGcs,
            rtsMajorGcs     = rtsSampleMajorGcs,
            rtsGcCpuMs      = rtsSampleGcCpuMs,
            rtsMutatorCpuMs = rtsSampleMutatorCpuMs,

            -- arena + hblkhd: everything the allocator took from the OS
            mallocFromOs         = mallocField 0 + mallocField 1,
            mallocInUse          = mallocField 2,
            mallocFree           = mallocField 3,
            mallocReleasable     = mallocField 4,
            mallocStatsAvailable = mallocAvailable,

            sqliteInUse        = sqliteUsed,
            sqlitePeak         = sqliteHigh,
            preparedStatements = preparedStatementCount
        }

-- | Just the process RSS, for the paths where a full sample would be overkill.
getProcessRss :: MonadIO m => m Size
getProcessRss = liftIO $ do
    procStatus <- readProcStatus
    pure $ Size $ 1024 * Map.findWithDefault 0 "VmRSS" procStatus

data RtsSample = RtsSample {
        rtsSampleInUse     :: Size,
        rtsSamplePeakInUse :: Size,
        rtsSampleLive      :: Size,
        rtsSampleAllocated :: Size,
        rtsSampleGcs       :: Int64,
        rtsSampleMajorGcs  :: Int64,
        rtsSampleGcCpuMs   :: Int64,
        rtsSampleMutatorCpuMs :: Int64
    }

-- | Zeros when the program was built or run without @-T@. The counters only
-- refresh at a GC, so they also read 0 until the first one has happened.
readRtsStats :: IO RtsSample
readRtsStats = do
    enabled <- getRTSStatsEnabled
    if not enabled
        then pure $ RtsSample 0 0 0 0 0 0 0 0
        else do
            RTSStats {..} <- getRTSStats
            pure RtsSample {
                    -- current, as of the last GC -- not max_mem_in_use_bytes,
                    -- which is a high-water mark and would make the C side's
                    -- share come out negative
                    rtsSampleInUse     = Size $ fromIntegral $ gcdetails_mem_in_use_bytes gc,
                    rtsSamplePeakInUse = Size $ fromIntegral max_mem_in_use_bytes,
                    rtsSampleLive      = Size $ fromIntegral $ gcdetails_live_bytes gc,
                    rtsSampleAllocated = Size $ fromIntegral allocated_bytes,
                    rtsSampleGcs       = fromIntegral gcs,
                    rtsSampleMajorGcs  = fromIntegral major_gcs,
                    rtsSampleGcCpuMs   = fromIntegral gc_cpu_ns `div` 1_000_000,
                    rtsSampleMutatorCpuMs = fromIntegral mutator_cpu_ns `div` 1_000_000
                }

-- | \/proc\/self\/status as a map of its "VmRSS"-style keys to kB values.
-- Empty on platforms without procfs.
readProcStatus :: IO (Map.Map Text Int64)
readProcStatus = do
    content <- try @SomeException $ BS.readFile "/proc/self/status"
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

-- | glibc's mallinfo2 as (available, [arena, hblkhd, uordblks, fordblks, keepcost]).
readMallocStats :: IO (Bool, [Int64])
readMallocStats =
    allocaArray mallocStatsFields $ \ptr -> do
        available <- c_malloc_stats ptr
        values    <- peekArray mallocStatsFields ptr
        pure (available == 1, values)

mallocStatsFields :: Int
mallocStatsFields = 5


-- | Render a sample as one line of JSON, for a log line that later gets
-- grepped out and parsed back. Keys are written out by hand (rather than
-- derived) so that the log format stays stable if the record changes.
memoryStatsJson :: MemoryStats -> Text
memoryStatsJson ms@MemoryStats {..} =
    Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ Aeson.object [
            "processRss"           .= unSize processRss,
            "processRssPeak"       .= unSize processRssPeak,
            "processRssAnon"       .= unSize processRssAnon,
            "processRssFile"       .= unSize processRssFile,
            "processVmSize"        .= unSize processVmSize,

            "rtsInUse"             .= unSize rtsInUse,
            "rtsPeakInUse"         .= unSize rtsPeakInUse,
            "rtsLive"              .= unSize rtsLive,
            "rtsAllocated"         .= unSize rtsAllocated,
            "rtsGcs"               .= rtsGcs,
            "rtsMajorGcs"          .= rtsMajorGcs,
            "rtsGcCpuMs"           .= rtsGcCpuMs,
            "rtsMutatorCpuMs"      .= rtsMutatorCpuMs,
            "rtsGcCpuFraction"     .= gcCpuFraction ms,

            "mallocFromOs"         .= unSize mallocFromOs,
            "mallocInUse"          .= unSize mallocInUse,
            "mallocFree"           .= unSize mallocFree,
            "mallocReleasable"     .= unSize mallocReleasable,
            "mallocStatsAvailable" .= mallocStatsAvailable,

            "sqliteInUse"          .= unSize sqliteInUse,
            "sqlitePeak"           .= unSize sqlitePeak,
            "preparedStatements"   .= preparedStatements,

            -- derived, so that a reader of the log doesn't have to recompute them
            "nonHaskellMemory"     .= unSize (nonHaskellMemory ms),
            "mallocFreeRatio"      .= mallocFreeRatio ms
        ]


-- | Ask the C allocator to hand back whatever it is holding but not using.
-- Returns True if anything was actually released.
--
-- Worth doing because the allocator never returns memory on its own: it grows
-- to the high-water mark of transient allocation (reading payload BLOBs out
-- of SQLite, mostly) and keeps it on its free lists indefinitely. 'mallocFree'
-- against 'mallocInUse' says how much is up for grabs.
trimMalloc :: MonadIO m => m Bool
trimMalloc = liftIO $ (== 1) <$> c_malloc_trim

foreign import ccall unsafe "rpki_prover_malloc_stats"
    c_malloc_stats :: Ptr Int64 -> IO CInt

foreign import ccall safe "rpki_prover_malloc_trim"
    c_malloc_trim :: IO CInt

foreign import ccall unsafe "sqlite3_memory_used"
    c_sqlite3_memory_used :: IO Int64

foreign import ccall unsafe "sqlite3_memory_highwater"
    c_sqlite3_memory_highwater :: CInt -> IO Int64

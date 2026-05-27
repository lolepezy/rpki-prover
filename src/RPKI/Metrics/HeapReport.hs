{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

-- | Structured heap report derived from GHC's RTSStats.
--
-- Intended use: emit one report at the end of every worker execution so that
-- the text can be pasted into an AI conversation for memory-optimisation
-- analysis.  The format is intentionally line-oriented:
--
--   <left-padded key>  = <value>  <unit / note>
--
-- so it is easy to grep, diff, or feed to a script.
--
-- To enable the full *per-type* heap profile (info-table profiling) the
-- binary must be compiled with -finfo-table-map (already added to the build
-- config) and the worker must be started with:
--
--   +RTS -hi -l-aug -po<output-base> -RTS
--
-- The resulting <output-base>.eventlog can be converted to HTML+JSON with:
--
--   eventlog2html <output-base>.eventlog
--
-- Pass the JSON section of that output to an AI for per-constructor analysis.

module RPKI.Metrics.HeapReport (
    HeapReport,
    sampleHeapReport,
    formatHeapReport,
    logHeapReport
) where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GHC.Stats
import           Numeric                (showFFloat)

import           RPKI.Logging


-- | Opaque wrapper so callers don't depend on RTSStats directly.
newtype HeapReport = HeapReport RTSStats


-- | Sample the current RTS statistics.  Requires the program to be run with
-- @+RTS -T@ (already the default in this project).
sampleHeapReport :: MonadIO m => m HeapReport
sampleHeapReport = liftIO $ HeapReport <$> getRTSStats


-- | Log a heap report at INFO level.  The report is emitted to stderr in
-- worker processes, which the parent process captures and forwards to its own
-- logger.
logHeapReport :: (Logger l, MonadIO m) => l -> Text -> m ()
logHeapReport logger label = do
    report <- sampleHeapReport
    logInfo logger $ formatHeapReport label report


-- | Render an RTSStats snapshot as a structured, AI-consumable text block.
--
-- Reading guide for an AI:
--
--  * peak_live_bytes       – the most important number; maximum retained heap
--                           size across the run.  Optimisations aim to reduce
--                           this.
--  * peak_mem_in_use_bytes – OS-level RSS peak; always >= peak_live_bytes due
--                           to GC overhead and fragmentation.
--  * total_allocated       – cumulative bytes allocated; high values with a
--                           low peak_live_bytes indicate short-lived garbage.
--  * allocation_rate       – MB/s of allocation during mutator time; a proxy
--                           for GC pressure.
--  * gc_cpu_overhead       – percentage of CPU spent in GC.  > 30% is a red
--                           flag; > 50% is a crisis.
--  * copied_during_gc      – bytes copied between GC generations; reducing
--                           long-lived allocation reduces this.
--  * most-recent GC        – live_bytes here is the *current* retained heap
--                           right after the last collection.

formatHeapReport :: Text -> HeapReport -> Text
formatHeapReport label (HeapReport s) = Text.unlines $
    [ rule
    , "=== Heap Report: " <> label <> " ==="
    , rule
    , ""
    , "-- Live heap"
    , kv "peak_live_bytes          " (mb $ max_live_bytes s)           "MB"
    , kv "peak_mem_in_use_bytes    " (mb $ max_mem_in_use_bytes s)     "MB  (RSS-level)"
    , kv "peak_large_objects_bytes " (mb $ max_large_objects_bytes s)  "MB  (ByteString, Array, etc.)"
    , kv "peak_compact_bytes       " (mb $ max_compact_bytes s)        "MB"
    , kv "peak_slop_bytes          " (mb $ max_slop_bytes s)           "MB"
    , ""
    , "-- Allocation"
    , kv "total_allocated          " (mb $ allocated_bytes s)          "MB"
    , kv "allocation_rate          " allocRate                         "MB/s  (mutator wall time)"
    , kv "cumulative_live          " (mb $ cumulative_live_bytes s)    "MB  (sum of live at each GC sample)"
    , kv "copied_during_gc         " (mb $ copied_bytes s)            "MB  (moved between generations)"
    , ""
    , "-- GC"
    , kv "gcs_total                " (show $ gcs s)                   ""
    , kv "gcs_major                " (show $ major_gcs s)             ""
    , kv "gc_cpu_ms                " (ns2ms $ gc_cpu_ns s)            "ms"
    , kv "gc_elapsed_ms            " (ns2ms $ gc_elapsed_ns s)        "ms"
    , kv "mutator_cpu_ms           " (ns2ms $ mutator_cpu_ns s)       "ms"
    , kv "mutator_elapsed_ms       " (ns2ms $ mutator_elapsed_ns s)   "ms"
    , kv "gc_cpu_overhead          " gcCpuPct                         "% of total CPU"
    , kv "gc_elapsed_overhead      " gcElapsedPct                     "% of elapsed wall time"
    , ""
    , "-- Most-recent GC  (generation " <> Text.pack (show $ gcdetails_gen (gc s)) <> ")"
    , kv "  live_bytes             " (mb $ gcdetails_live_bytes          (gc s)) "MB  (current retained heap)"
    , kv "  large_objects_bytes    " (mb $ gcdetails_large_objects_bytes (gc s)) "MB"
    , kv "  slop_bytes             " (mb $ gcdetails_slop_bytes          (gc s)) "MB"
    , kv "  mem_in_use_bytes       " (mb $ gcdetails_mem_in_use_bytes    (gc s)) "MB"
    , kv "  copied_bytes           " (mb $ gcdetails_copied_bytes        (gc s)) "MB"
    , kv "  cpu_ms                 " (ns2ms $ gcdetails_cpu_ns     (gc s))       "ms"
    , kv "  elapsed_ms             " (ns2ms $ gcdetails_elapsed_ns (gc s))       "ms"
    , rule
    ]
  where
    rule = Text.replicate 70 "-"

    mb :: (Integral a) => a -> String
    mb n = showFFloat (Just 2) (fromIntegral n / 1_048_576.0 :: Double) ""

    ns2ms :: (Integral a, Show a) => a -> String
    ns2ms n = show (n `div` 1_000_000)

    kv :: Text -> String -> Text -> Text
    kv key val unit = key <> "= " <> Text.pack val <> "  " <> unit

    pct :: (Integral a) => a -> a -> String
    pct _ 0 = "0.0"
    pct n d = showFFloat (Just 1) (100.0 * fromIntegral n / fromIntegral d :: Double) ""

    allocRate
        | mutator_elapsed_ns s == 0 = "0.0"
        | otherwise =
            let elapsedS = fromIntegral (mutator_elapsed_ns s) / 1.0e9 :: Double
                allocMB  = fromIntegral (allocated_bytes s) / 1_048_576.0 :: Double
            in showFFloat (Just 1) (allocMB / elapsedS) ""

    gcCpuPct     = pct (gc_cpu_ns s) (cpu_ns s)
    gcElapsedPct = pct (gc_elapsed_ns s) (elapsed_ns s)

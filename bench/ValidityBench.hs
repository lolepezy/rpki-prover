{-# LANGUAGE ScopedTypeVariables #-}
-- | Benchmark measuring the live-heap footprint of 'PrefixIndex'.
--
-- Run on each branch and compare the "Live heap after GC" line:
--
--   stack bench rpki-prover:bench:validity-bench
--
-- The '-with-rtsopts=-T' flag is baked into the executable so that
-- GHC.Stats is always available.  You can add extra RTS flags at
-- run-time, e.g.:
--
--   stack bench rpki-prover:bench:validity-bench \
--     --benchmark-arguments '+RTS -s -RTS'
--
module Main where

import Control.DeepSeq      (force)
import Control.Exception    (evaluate)
import Data.IORef           (newIORef, readIORef)
import Data.List            (foldl')
import GHC.Stats
import System.CPUTime       (getCPUTime)
import System.Mem           (performMajorGC)
import Text.Printf          (printf)

import RPKI.Resources.Types     (ASN (..), IpPrefix (..), PrefixLength (..))
import RPKI.Resources.Validity  (PrefixIndex, ValidityResult (..), createPrefixIndex, prefixValidity)
import BenchCommon              (genIPv4Vrps, genIPv6Vrps, mkIPv4Probes, mkIPv6Probes)

-- ---------------------------------------------------------------------------
-- Probe generators for prefixValidity throughput
-- ---------------------------------------------------------------------------

-- | 70k IPv4 probes — offsets from VRP seed so some will match, some won't.
genIPv4Probes :: [(ASN, IpPrefix)]
genIPv4Probes = mkIPv4Probes [0 .. 69999]

-- | 30k IPv6 probes — same global-unicast range, slightly offset.
genIPv6Probes :: [(ASN, IpPrefix)]
genIPv6Probes = mkIPv6Probes [0 .. 29999]

-- ---------------------------------------------------------------------------
-- Index construction
-- ---------------------------------------------------------------------------

-- Build the index in a separate function so the VRP list is not kept alive
-- after construction (allowing the GC to reclaim it before we measure).
buildIndex :: IO PrefixIndex
buildIndex =
    evaluate $ force $ createPrefixIndex (genIPv4Vrps ++ genIPv6Vrps)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Building PrefixIndex (~700k IPv4  +  ~300k IPv6 VRPs)..."

    statsEnabled <- getRTSStatsEnabled

    t0  <- getCPUTime
    idx <- buildIndex
    t1  <- getCPUTime

    let buildSecs = fromIntegral (t1 - t0) / (1e12 :: Double)
    printf "Build time                : %.2f s\n" buildSecs

    -- Pin idx in an IORef so the GC cannot collect it before we measure.
    ref <- newIORef idx

    performMajorGC

    if statsEnabled
        then do
            s <- getRTSStats
            let live   = gcdetails_live_bytes (gc s)
                liveMB = fromIntegral live / (1024 * 1024) :: Double
                alloc  = allocated_bytes s
                allocMB = fromIntegral alloc / (1024 * 1024) :: Double
            printf "Live heap after major GC  : %.1f MB  (%d bytes)\n" liveMB live
            printf "Total allocated           : %.1f MB\n" allocMB
            printf "Major GCs                 : %d\n" (major_gcs s)
        else
            putStrLn "(Recompile/rerun with +RTS -T to enable heap statistics)"

    -- Throughput measurement for prefixValidity
    idx' <- readIORef ref
    let probes = genIPv4Probes ++ genIPv6Probes
        nProbes = length probes
    putStrLn $ "\nRunning prefixValidity on " ++ show nProbes ++ " probes..."
    t2 <- getCPUTime
    -- Force each result to WHNF by inspecting the constructor tag.
    let !total = foldl' (\(!n) (asn, pfx) ->
                    case prefixValidity asn pfx idx' of
                        Unknown          -> n
                        InvalidOverall _ -> n + 1
                        ValidOverall _ _ -> n + 2
                ) (0 :: Int) probes
    t3 <- getCPUTime
    let lookupSecs = fromIntegral (t3 - t2) / (1e12 :: Double)
        throughput = fromIntegral nProbes / lookupSecs :: Double
    printf "Lookup time (total)       : %.3f s (%d probes, %d non-unknown)\n"
        lookupSecs nProbes total
    printf "Throughput                : %.0f lookups/sec\n" throughput

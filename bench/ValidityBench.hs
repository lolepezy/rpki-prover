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
import Data.Word            (Word32, Word8)
import GHC.Stats
import System.CPUTime       (getCPUTime)
import System.Mem           (performMajorGC)
import Text.Printf          (printf)

import RPKI.Domain              (Vrp (..))
import RPKI.Resources.Resources (mkIpv4Block, mkIpv6Block)
import RPKI.Resources.Types     (ASN (..), IpPrefix (..), PrefixLength (..))
import RPKI.Resources.Validity  (PrefixIndex, ValidityResult (..), createPrefixIndex, prefixValidity)

-- ---------------------------------------------------------------------------
-- VRP generators
-- ---------------------------------------------------------------------------

-- | ~700k IPv4 VRPs spread across the full address space.
-- Prefix lengths range from /8 to /24, mirroring a realistic BGP table.
genIPv4Vrps :: [Vrp]
genIPv4Vrps =
    [ Vrp (ASN asn) (Ipv4P (mkIpv4Block addr len)) (PrefixLength len)
    | i <- [0 .. 699999 :: Int]
    , let addr = fromIntegral (i * 57321)      :: Word32   -- wraps, spreading across space
    , let len  = fromIntegral (8 + i `mod` 17) :: Word8
    , let asn  = fromIntegral (i `mod` 200000) :: Word32
    ]

-- | ~300k IPv6 VRPs spread across global-unicast space.
-- Prefix lengths range from /32 to /48, mirroring typical ISP allocations.
genIPv6Vrps :: [Vrp]
genIPv6Vrps =
    [ Vrp (ASN asn) (Ipv6P (mkIpv6Block (w0, w1, w2, w3) len)) (PrefixLength len)
    | i <- [0 .. 299999 :: Int]
    , let w0  = 0x20010db8 + fromIntegral (i `div` 65536)  :: Word32
    , let w1  = fromIntegral (i * 13 `mod` 65536)           :: Word32
    , let w2  = fromIntegral (i * 7)                         :: Word32
    , let w3  = 0                                            :: Word32
    , let len = fromIntegral (32 + i `mod` 17)              :: Word8
    , let asn = fromIntegral (i `mod` 200000)               :: Word32
    ]

-- ---------------------------------------------------------------------------
-- Probe generators for prefixValidity throughput
-- ---------------------------------------------------------------------------

-- | 70k IPv4 probes — offsets from VRP seed so some will match, some won't.
genIPv4Probes :: [(ASN, IpPrefix)]
genIPv4Probes =
    [ (ASN asn, Ipv4P (mkIpv4Block addr len))
    | i <- [0 .. 69999 :: Int]
    , let addr = fromIntegral (i * 57321 + 12345) :: Word32
    , let len  = fromIntegral (16 + i `mod` 9)    :: Word8
    , let asn  = fromIntegral (i `mod` 200000)     :: Word32
    ]

-- | 30k IPv6 probes — same global-unicast range, slightly offset.
genIPv6Probes :: [(ASN, IpPrefix)]
genIPv6Probes =
    [ (ASN asn, Ipv6P (mkIpv6Block (w0, w1, w2, w3) len))
    | i <- [0 .. 29999 :: Int]
    , let w0  = 0x20010db8 + fromIntegral (i `div` 65536) :: Word32
    , let w1  = fromIntegral (i * 13 `mod` 65536)          :: Word32
    , let w2  = fromIntegral (i * 7 + 100)                  :: Word32
    , let w3  = 0                                            :: Word32
    , let len = fromIntegral (40 + i `mod` 9)              :: Word8
    , let asn = fromIntegral (i `mod` 200000)               :: Word32
    ]

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

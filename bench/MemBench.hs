{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores  #-}

-- | Memory footprint benchmark comparing:
--
--   • Current representation: fixed-size fields stored as BSS.ShortByteString
--     (Hash = 32 bytes SHA-256, KI = 20 bytes SHA-1)
--
--   • Proposed representation: fields stored as unpacked Word64/Word32,
--     bypassing the two-level indirection  SBS constructor -> ByteArray# object.
--
-- Run with:
--   stack exec mem-bench -- +RTS -T
--
-- Expected closure sizes on 64-bit GHC (1 machine word = 8 bytes):
--
--   HashOld  SBS     : 16 B  (header 8 + ByteArray# ptr 8)
--            ByteArray# for 32 B: 16 B header + 32 B data = 48 B
--            Total per Hash = 64 B
--   HashNew  4 x UNPACK Word64: 8 B header + 4x8 B = 40 B  -> saves 24 B (37.5%)
--
--   KIOld    SBS     : 16 B
--            ByteArray# for 20 B: 16 B header + 24 B data (pad to word) = 40 B
--            Total per KI = 56 B
--   KINew    2 x UNPACK Word64 + UNPACK Word32:
--            8 B header + 8 + 8 + 8(*) = 32 B -> saves 24 B (42.9%)
--            (*) GHC pads each UNPACK'd field to one machine word in the closure
--
-- Metric: `allocated_bytes` from GHC.Stats - a monotonically-increasing
-- counter unaffected by GC scheduling or liveness analysis.  For live-heap
-- comparison we pin each result in an IORef before calling performMajorGC.

module Main where

import           Control.DeepSeq              (NFData (..), force)
import           Control.Exception            (evaluate)
import qualified Data.ByteString.Short        as BSS
import qualified Data.Map.Strict              as Map
import           Data.Bits
import           Data.IORef
import           Data.List                    (foldl')
import           Data.Word
import           GHC.Stats
import           System.Mem                   (performMajorGC)
import           Text.Printf                  (printf)

-------------------------------------------------------------------------------
-- Current representations (mirrors Domain.hs)
-------------------------------------------------------------------------------

newtype HashOld = HashOld BSS.ShortByteString
    deriving (Eq, Ord)

instance NFData HashOld where
    rnf (HashOld !_) = ()

newtype KIOld = KIOld BSS.ShortByteString
    deriving (Eq, Ord)

instance NFData KIOld where
    rnf (KIOld !_) = ()

-------------------------------------------------------------------------------
-- Proposed: unpack into primitive words
-------------------------------------------------------------------------------

data HashNew = HashNew
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    deriving (Eq, Ord)

instance NFData HashNew where
    rnf (HashNew !_ !_ !_ !_) = ()

-- 2 x Word64 covers bytes 0-15, Word32 covers bytes 16-19.
-- GHC aligns each UNPACK'd field to a machine word, so effective size:
--   8 (header) + 8 + 8 + 8 = 32 bytes.
data KINew = KINew
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word64
    {-# UNPACK #-} !Word32
    deriving (Eq, Ord)

instance NFData KINew where
    rnf (KINew !_ !_ !_) = ()

-------------------------------------------------------------------------------
-- MftPair-equivalent structures  (manifest filename index + hash)
-------------------------------------------------------------------------------

data MftPairOld = MftPairOld {-# UNPACK #-} !Int !HashOld
instance NFData MftPairOld where
    rnf (MftPairOld !_ h) = rnf h

data MftPairNew = MftPairNew {-# UNPACK #-} !Int !HashNew
instance NFData MftPairNew where
    rnf (MftPairNew !_ h) = rnf h

-------------------------------------------------------------------------------
-- Construction helpers (NOINLINE prevents CSE / constant-folding)
-------------------------------------------------------------------------------

lcg :: Int -> Int -> Int
lcg seed i = (seed * 6364136223846793005 + i) `mod` 256
{-# NOINLINE lcg #-}

mkHashOld :: Int -> HashOld
mkHashOld seed = HashOld $ BSS.pack $ map (fromIntegral . lcg seed) [0..31]
{-# NOINLINE mkHashOld #-}

mkHashNew :: Int -> HashNew
mkHashNew seed = HashNew (w 0) (w 8) (w 16) (w 24)
  where
    byte64 i = fromIntegral (lcg seed i) :: Word64
    w off    = foldl' (\acc i -> (acc `shiftL` 8) .|. byte64 (off + i)) 0 [0..7 :: Int]
{-# NOINLINE mkHashNew #-}

mkKIOld :: Int -> KIOld
mkKIOld seed = KIOld $ BSS.pack $ map (fromIntegral . lcg seed) [0..19]
{-# NOINLINE mkKIOld #-}

mkKINew :: Int -> KINew
mkKINew seed = KINew (w 0) (w 8) w16
  where
    byte64 i = fromIntegral (lcg seed i) :: Word64
    w off    = foldl' (\acc i -> (acc `shiftL` 8) .|. byte64 (off + i)) 0 [0..7 :: Int]
    w16 :: Word32
    w16      = foldl' (\acc i -> (acc `shiftL` 8) .|. fromIntegral (lcg seed (16 + i))) 0 [0..3 :: Int]
{-# NOINLINE mkKINew #-}

-------------------------------------------------------------------------------
-- Measurement primitives
-------------------------------------------------------------------------------

getStats :: IO RTSStats
getStats = getRTSStats
{-# NOINLINE getStats #-}

-- | Total bytes allocated by this action (monotonically increasing counter).
measureAlloc :: NFData a => IO a -> IO (Word64, a)
measureAlloc action = do
    s0     <- getStats
    let before = allocated_bytes s0
    result <- evaluate . force =<< action
    s1     <- getStats
    let after = allocated_bytes s1
    return (after - before, result)

-- | Live heap bytes consumed by the action's result.
--   Pins the result in an IORef so it survives the major GC.
measureLive :: NFData a => IO a -> IO (Int, a)
measureLive action = do
    performMajorGC
    s0     <- getStats
    let before = fromIntegral $ gcdetails_live_bytes $ gc s0
    result <- evaluate . force =<< action
    ref    <- newIORef result
    performMajorGC
    s1     <- getStats
    let after = fromIntegral $ gcdetails_live_bytes $ gc s1
    result' <- readIORef ref
    return (after - before, result')

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

nMap :: Int
nMap = 200_000

nList :: Int
nList = 500_000

data BenchResult = BenchResult
    { brAllocBytes :: Word64
    , brLiveBytes  :: Int
    }

benchBoth :: NFData a => String -> IO a -> IO BenchResult
benchBoth label action = do
    (alloc, _) <- measureAlloc action
    (live,  _) <- measureLive  action
    printf "    %-42s alloc=%10d B   live=%10d B\n" label alloc live
    return BenchResult { brAllocBytes = alloc, brLiveBytes = live }

benchHashMap :: IO ()
benchHashMap = do
    putStrLn $ "\n--- Map Hash->Int  (" ++ show nMap ++ " entries) ---"
    old <- benchBoth "HashOld  (BSS.ShortByteString)" $
               return $! Map.fromList [(mkHashOld i, i) | i <- [1..nMap]]
    new <- benchBoth "HashNew  (4 x UNPACK Word64)  " $
               return $! Map.fromList [(mkHashNew i, i) | i <- [1..nMap]]
    reportDelta (brAllocBytes old) (brAllocBytes new)
                (brLiveBytes  old) (brLiveBytes  new)
                nMap

benchKIMap :: IO ()
benchKIMap = do
    putStrLn $ "\n--- Map KI->Int  (" ++ show nMap ++ " entries) ---"
    old <- benchBoth "KIOld  (BSS.ShortByteString)   " $
               return $! Map.fromList [(mkKIOld i, i) | i <- [1..nMap]]
    new <- benchBoth "KINew  (2xWord64 + Word32)     " $
               return $! Map.fromList [(mkKINew i, i) | i <- [1..nMap]]
    reportDelta (brAllocBytes old) (brAllocBytes new)
                (brLiveBytes  old) (brLiveBytes  new)
                nMap

-- Simulate the list of MftPair kept in memory during manifest validation.
-- At internet scale: ~15,000 CAs x ~400 entries = 6M entries peak.
benchMftPairList :: IO ()
benchMftPairList = do
    putStrLn $ "\n--- [MftPair] list  (" ++ show nList ++ " entries) ---"
    old <- benchBoth "MftPairOld  (BSS hash)        " $
               return $! [MftPairOld i (mkHashOld i) | i <- [1..nList]]
    new <- benchBoth "MftPairNew  (UNPACK Word64)   " $
               return $! [MftPairNew i (mkHashNew i) | i <- [1..nList]]
    reportDelta (brAllocBytes old) (brAllocBytes new)
                (brLiveBytes  old) (brLiveBytes  new)
                nList

reportDelta :: Word64 -> Word64 -> Int -> Int -> Int -> IO ()
reportDelta oldAlloc newAlloc oldLive newLive n = do
    let allocDelta = (fromIntegral oldAlloc :: Int) - fromIntegral newAlloc
        liveDelta  = oldLive - newLive
        pct x tot  = if tot == 0 then 0 else 100.0 * fromIntegral x / fromIntegral tot :: Double
    printf "    Alloc savings: %10d B total  (%6.1f B/entry, %4.1f%% reduction)\n"
        allocDelta (fromIntegral allocDelta / fromIntegral n :: Double)
        (pct allocDelta (fromIntegral oldAlloc :: Int))
    printf "    Live  savings: %10d B total  (%6.1f B/entry, %4.1f%% reduction)\n"
        liveDelta (fromIntegral liveDelta / fromIntegral n :: Double)
        (pct liveDelta oldLive)

projections :: IO ()
projections = do
    putStrLn "\n--- Projected savings at RPKI internet scale ---"
    putStrLn "    Assumption: 500K objects, 15K CAs x 400 MFT entries/CA"
    printf "    Hash in hashToKey map  (500K keys):   ~%3d MB\n"
        (500_000 * 24 `div` 1_000_000 :: Int)
    printf "    Hash in peak MftPairs  (6M entries):  ~%3d MB\n"
        (6_000_000 * 24 `div` 1_000_000 :: Int)
    printf "    KI in SKI/AKI maps     (30K keys):    ~%3d KB\n"
        (30_000 * 24 `div` 1_000 :: Int)
    printf "    Estimated total heap reduction:       ~%3d MB\n"
        ((500_000 * 24 + 6_000_000 * 24) `div` 1_000_000 :: Int)

main :: IO ()
main = do
    enabled <- getRTSStatsEnabled
    if not enabled
        then putStrLn "ERROR: run with  stack exec mem-bench -- +RTS -T"
        else do
            putStrLn "======================================================================="
            putStrLn " BSS.ShortByteString  vs  UNPACK'd Word64 fields"
            putStrLn " alloc = total bytes allocated (includes intermediates)"
            putStrLn " live  = live heap after major GC (final data structure only)"
            putStrLn "======================================================================="
            benchHashMap
            benchKIMap
            benchMftPairList
            projections
            putStrLn "\nDone."

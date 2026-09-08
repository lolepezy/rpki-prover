{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How long it takes to count distinct VRPs, per TA and over all TAs.
--
-- Both numbers go into the metrics of every validation run, and the validation
-- worker computes them while holding the write transaction, so the cost is on
-- the critical path.
module Main where

import           Control.DeepSeq            (force)
import           Control.Exception          (evaluate)
import           Control.Monad              (forM, forM_)

import qualified Data.List                  as List
import qualified Data.Text                  as Text
import qualified Data.Map.Monoidal.Strict   as MonoidalMap
import qualified Data.Vector.Unboxed        as VU

import           Data.Function              (on)
import           Data.Bits                  (shiftR, xor)
import           Data.Word                  (Word64)
import           GHC.Clock                  (getMonotonicTimeNSec)
import           Text.Printf                (printf)

import           RPKI.Domain
import           RPKI.Domain.Packed


taCount, vrpsPerTa :: Int
taCount   = 5
vrpsPerTa = 200000

main :: IO ()
main = do
    input <- evaluate . force =<< pure mkInput

    printf "Dataset: %d TAs x %d VRPs = %d total (80%% IPv4, 20%% IPv6, ~5%% duplicates)\n"
        taCount vrpsPerTa (taCount * vrpsPerTa)
    putStrLn "Each implementation is measured 5 times after one warmup run."
    putStrLn ""

    -- Both must agree, otherwise the timings are meaningless.
    let expected = twoSorts input
        actual   = uniqueVrpCounts input
    forM_ [("per-TA", show (fst expected) == show (fst actual)),
           ("total", snd expected == snd actual)] $ \(what, ok) ->
        printf "%-8s agree: %s\n" (what :: String) (show ok)
    printf "total distinct = %d, per TA = %s\n\n"
        (snd actual) (show (MonoidalMap.elems (fst actual)))

    results <- forM implementations $ \impl -> benchmark 5 input impl
    mapM_ printResult results
    let fastest = List.minimumBy (compare `on` avgMs) results
    printf "\nFastest: %s (avg %.1f ms, min %.1f ms)\n"
        (implName fastest) (avgMs fastest) (minMs fastest)


implementations :: [(String, PerTA Vrps -> (MonoidalMap.MonoidalMap TaName Int, Int))]
implementations =
    [ ("two-sorts (previous)", twoSorts)
    , ("sort-once + merge",    uniqueVrpCounts)
    ]

-- | What this used to do: sort and deduplicate every TA, then sort and
-- deduplicate the concatenation of all of them a second time.
twoSorts :: PerTA Vrps -> (MonoidalMap.MonoidalMap TaName Int, Int)
twoSorts vrps = (MonoidalMap.map countVrps (unPerTA vrps), countVrps (allTAs vrps))
  where
    countVrps (Vrps v4 v6) =
        VU.length (sortDedup compare v4) + VU.length (sortDedup compare v6)


-- Deterministic pseudo-random VRPs, drawn from a key space shared by all TAs
-- and small enough that VRPs repeat both within a TA and across TAs, which is
-- what the counting has to cope with.
mkInput :: PerTA Vrps
mkInput = toPerTA [ (TaName (Text.pack ("ta-" <> show taIx)), mkVrps taIx)
                  | taIx <- [0 .. taCount - 1] ]
  where
    -- Expected duplicates are ~n^2/2m, so a space of ten times the draw count
    -- puts them around 5%.
    keySpace = fromIntegral (taCount * vrpsPerTa * 10) :: Word64

    mkVrps taIx = Vrps (VU.fromList (map mk4 v4keys)) (VU.fromList (map mk6 v6keys))
      where
        keys = take vrpsPerTa
             $ map (`mod` keySpace)
             $ iterate mix (0x9e3779b97f4a7c15 * fromIntegral (taIx + 1))
        (v6keys, v4keys) = List.splitAt (vrpsPerTa `div` 5) keys

    -- splitmix64 finaliser: cheap, and spreads the low bits properly
    mix :: Word64 -> Word64
    mix x0 = let a = x0 + 0x9e3779b97f4a7c15
                 b = (a `xor` (a `shiftR` 30)) * 0xbf58476d1ce4e5b9
                 c = (b `xor` (b `shiftR` 27)) * 0x94d049bb133111eb
             in c `xor` (c `shiftR` 31)

    -- Distinct keys must give distinct VRPs, or the key space above would not
    -- describe the duplicate rate.
    mk4 k = let (hi, lo) = k `divMod` 20000
            in PackedVrp4 (fromIntegral lo) (fromIntegral (hi * 256)) 24 24
    mk6 k = let (hi, lo) = k `divMod` 20000
            in PackedVrp6 (fromIntegral lo) (hi * 4294967296) 0 48 48


data ImplResult = ImplResult {
    implName :: String,
    avgMs    :: Double,
    minMs    :: Double,
    maxMs    :: Double
}

benchmark :: Int
          -> PerTA Vrps
          -> (String, PerTA Vrps -> (MonoidalMap.MonoidalMap TaName Int, Int))
          -> IO ImplResult
benchmark repetitions input (name, implementation) = do
    _ <- run   -- warmup
    times <- forM [1 .. repetitions] $ \_ -> do
        start <- getMonotonicTimeNSec
        _ <- run
        end <- getMonotonicTimeNSec
        pure $ fromIntegral (end - start) / 1e6
    pure ImplResult {
        implName = name,
        avgMs    = sum times / fromIntegral (length times),
        minMs    = minimum times,
        maxMs    = maximum times
    }
  where
    run = evaluate $ force $ let (m, t) = implementation input
                             in (MonoidalMap.elems m, t)

printResult :: ImplResult -> IO ()
printResult r = printf "  %-22s avg %8.1f ms   min %8.1f ms   max %8.1f ms\n"
    (implName r) (avgMs r) (minMs r) (maxMs r)

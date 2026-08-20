{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.DeepSeq (force)
import           Control.Exception (evaluate)
import           Control.Monad (forM, forM_, unless)

import qualified Data.HashSet as HashSet
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Set  as Set
import qualified Data.Text as Text
import qualified Data.Vector as V

import           Data.Bits ((.&.), shiftR)
import           Data.Function (on)
import           Data.Hashable (Hashable, hashWithSalt)
import           Data.Int (Int64)
import           Data.Word (Word8, Word32, Word64)
import           Numeric (showHex)
import           GHC.Clock (getMonotonicTimeNSec)
import           Text.Printf (printf)

import           RPKI.Domain
import           RPKI.Resources.Resources (prefixV4ToBytes, prefixV6ToBytes, readIp4, readIp6)
import           RPKI.Resources.Types (ASN(..), IpPrefix(..), PrefixLength(..))
import           RPKI.RTR.Types


main :: IO ()
main = do
    input <- force <$> mkBenchmarkInput
    let roasInput = force $ mkBenchmarkRoas input

    putStrLn "Dataset: PerTA Vrps with 5 entries x 200000 VRPs = 1000000 total (80% IPv4, 20% IPv6)"
    putStrLn "Each implementation is measured 5 times after one warmup run."
    putStrLn ""

    putStrLn "uniqueVrpCount benchmarks"
    results <- forM implementations $ \impl -> benchmarkImpl 5 input impl
    mapM_ printResult results
    let fastest = List.minimumBy (compare `on` avgMs) results
    putStrLn ""
    printf "Fastest implementation: %s (avg %.2f ms, min %.2f ms)\n"
        (implName fastest) (avgMs fastest) (minMs fastest)

    let uniqInput = allTAs input
    verifyUniqImplementations uniqInput

    putStrLn ""
    putStrLn "uniqVrps benchmarks"
    uniqResults <- forM uniqImplementations $ \impl -> benchmarkUniqImpl 5 uniqInput impl
    mapM_ printResult uniqResults
    let uniqFastest = List.minimumBy (compare `on` avgMs) uniqResults
    putStrLn ""
    printf "Fastest uniqVrps implementation: %s (avg %.2f ms, min %.2f ms)\n"
        (implName uniqFastest) (avgMs uniqFastest) (minMs uniqFastest)

    putStrLn ""
    putStrLn "estimateVrpCountRoas benchmarks"
    roaCountResults <- forM roaCountImplementations $ \impl -> benchmarkRoaCountImpl 5 roasInput impl
    mapM_ printResult roaCountResults
    let roaFastest = List.minimumBy (compare `on` avgMs) roaCountResults
    putStrLn ""
    printf "Fastest estimateVrpCountRoas implementation: %s (avg %.2f ms, min %.2f ms)\n"
        (implName roaFastest) (avgMs roaFastest) (minMs roaFastest)


-- 1,000,000 total VRPs: 5 TAs x 200,000 each, with 80/20 IPv4/IPv6 split.
mkBenchmarkInput :: IO (PerTA Vrps)
mkBenchmarkInput = pure $ toPerTA
    [ mkTaEntry taIx | taIx <- [0 .. taCount - 1] ]
  where
    taCount = 5 :: Int


mkBenchmarkRoas :: PerTA Vrps -> Roas
mkBenchmarkRoas perTaVrps =
        Roas $ MonoidalMap.fromList $ Prelude.zipWith mkEntry [1 :: Int64 ..] allVrps
    where
        allVrps = concatMap (V.toList . unVrps . snd) $ perTA perTaVrps

        mkEntry :: Int64 -> Vrp -> (ObjectKey, VrpsPerAs)
        mkEntry ix vrp = (ObjectKey $ asKey ix, vrpToPayload vrp)


vrpToPayload :: Vrp -> VrpsPerAs
vrpToPayload (Vrp asn prefix maxLen) =
    case prefix of
        Ipv4P p -> VrpsPerAs asn [Vrp4 p maxLen] []
        Ipv6P p -> VrpsPerAs asn [] [Vrp6 p maxLen]


mkTaEntry :: Int -> (TaName, Vrps)
mkTaEntry taIx =
    ( TaName $ Text.pack $ "ta-" <> show taIx
    , createVrps $ mkTaVrps taIx
    )


mkTaVrps :: Int -> [Vrp]
mkTaVrps taIx =
    [ mkIpv4Vrp (globalIx taIx localIx) | localIx <- [0 .. v4PerTa - 1] ] <>
    [ mkIpv6Vrp (globalIx taIx localIx) | localIx <- [v4PerTa .. perTaCount - 1] ]
  where
    perTaCount = 200000 :: Int
    v4PerTa    = 160000 :: Int


globalIx :: Int -> Int -> Int
globalIx taIx localIx = taIx * 200000 + localIx


mkIpv4Vrp :: Int -> Vrp
mkIpv4Vrp ix =
    Vrp
        (ASN $ fromIntegral $ 64512 + (ix `mod` 40000))
        (Ipv4P $ readIp4 $ ipv4Prefix24 ix)
        (PrefixLength 24)


mkIpv6Vrp :: Int -> Vrp
mkIpv6Vrp ix =
    Vrp
        (ASN $ fromIntegral $ 64512 + (ix `mod` 40000))
        (Ipv6P $ readIp6 $ ipv6Prefix64 ix)
        (PrefixLength 64)


ipv4Prefix24 :: Int -> String
ipv4Prefix24 n =
    show o1 <> "." <> show o2 <> "." <> show o3 <> ".0/24"
  where
    o1 = (n `shiftR` 16) .&. 255
    o2 = (n `shiftR` 8) .&. 255
    o3 = n .&. 255


ipv6Prefix64 :: Int -> String
ipv6Prefix64 n =
    "2001:" <> showHex g2 "" <> ":" <> showHex g3 "" <> "::/64"
  where
    g2 = (n `shiftR` 16) .&. 65535
    g3 = n .&. 65535


uniqueVrpCountFoldInsert :: PerTA Vrps -> Int
uniqueVrpCountFoldInsert = Set.size . foldlPerTA Set.empty
  where
    foldlPerTA acc =
        foldl step acc . perTA

    step acc (_, Vrps vrps) =
        V.foldl' (flip Set.insert) acc vrps


uniqueVrpCountSetUnionPerTa :: PerTA Vrps -> Int
uniqueVrpCountSetUnionPerTa = Set.size . foldlPerTA Set.empty
  where
    foldlPerTA acc =
        foldl step acc . perTA

    step acc (_, Vrps vrps) =
        acc `Set.union` Set.fromList (V.toList vrps)


uniqueVrpCountSortGroup :: PerTA Vrps -> Int
uniqueVrpCountSortGroup =
    length
        . List.group
        . List.sort
        . concatMap (V.toList . unVrps . snd)
        . perTA


uniqueVrpCountSortVecScan :: PerTA Vrps -> Int
uniqueVrpCountSortVecScan perTaVrps =
    case V.length sorted of
        0 -> 0
        n -> 1 + countTransitions 1 n
  where
    allVrps :: V.Vector Vrp
    allVrps =
        V.concat
            . map (unVrps . snd)
            $ perTA perTaVrps

    sorted :: V.Vector Vrp
    sorted =
        V.fromList
            . List.sort
            . V.toList
            $ allVrps

    countTransitions ix n
        | ix >= n = 0
        | otherwise =
            let prev = sorted V.! (ix - 1)
                cur  = sorted V.! ix
            in (if cur /= prev then 1 else 0) + countTransitions (ix + 1) n


uniqVrpsSetFoldInsert :: Vrps -> V.Vector Vrp
uniqVrpsSetFoldInsert vrps =
    let s = V.foldl' (flip Set.insert) Set.empty (unVrps vrps)
        sorted = List.sortBy cmpVrps $ Set.toList s
    in V.fromListN (length sorted) sorted


uniqVrpsSortGroup :: Vrps -> V.Vector Vrp
uniqVrpsSortGroup vrps =
    V.fromListN (length uniqueSorted) uniqueSorted
  where
    sorted = List.sortBy cmpVrps $ V.toList $ unVrps vrps
    uniqueSorted = dedupSortedList sorted


uniqVrpsSortVecScan :: Vrps -> V.Vector Vrp
uniqVrpsSortVecScan vrps =
    case V.length sorted of
        0 -> V.empty
        n ->
            let uniqueCount = 1 + countTransitions 1 n
                uniqueVrps = collectUnique 1 n [sorted V.! 0]
            in V.fromListN uniqueCount $ Prelude.reverse uniqueVrps
  where
    allVrps :: V.Vector Vrp
    allVrps = unVrps vrps

    sorted :: V.Vector Vrp
    sorted =
        V.fromList
            . List.sortBy cmpVrps
            . V.toList
            $ allVrps

    countTransitions ix n
        | ix >= n = 0
        | otherwise =
            let prev = sorted V.! (ix - 1)
                cur  = sorted V.! ix
            in (if cur /= prev then 1 else 0) + countTransitions (ix + 1) n

    collectUnique ix n acc
        | ix >= n = acc
        | otherwise =
            let prev = sorted V.! (ix - 1)
                cur  = sorted V.! ix
            in if cur /= prev
                then collectUnique (ix + 1) n (cur : acc)
                else collectUnique (ix + 1) n acc


dedupSortedList :: Eq a => [a] -> [a]
dedupSortedList = \case
    [] -> []
    x : xs -> x : go x xs
  where
    go _ [] = []
    go prev (y : ys)
        | y == prev = go prev ys
        | otherwise = y : go y ys


uniqImplementations :: [(String, Vrps -> V.Vector Vrp)]
uniqImplementations =
    [ ("domain/uniqVrpsBy-cmpVrps", uniqVrpsBy cmpVrps)
    , ("set-fold-insert", uniqVrpsSetFoldInsert)
    , ("sort-group", uniqVrpsSortGroup)
    , ("sort-vec-scan", uniqVrpsSortVecScan)
    ]


roaCountImplementations :: [(String, Roas -> Int)]
roaCountImplementations =
    [ ("domain/estimateVrpCountRoas", estimateVrpCountRoas)
    , ("foldl-payload-count", estimateVrpCountRoasFoldl)
    , ("flatten-list-length", estimateVrpCountRoasFlatten)
    , ("toVrps-length", estimateVrpCountRoasViaToVrps)
    ]


estimateVrpCountRoasFoldl :: Roas -> Int
estimateVrpCountRoasFoldl (Roas roas) =
    List.foldl' (\acc payload -> acc + payloadVrpCount payload) 0 $ MonoidalMap.elems roas


estimateVrpCountRoasFlatten :: Roas -> Int
estimateVrpCountRoasFlatten (Roas roas) =
    sum $ Prelude.map (length . roaPayloadToVrps) $ MonoidalMap.elems roas


estimateVrpCountRoasViaToVrps :: Roas -> Int
estimateVrpCountRoasViaToVrps = V.length . unVrps . toVrps


payloadVrpCount :: VrpsPerAs -> Int
payloadVrpCount (VrpsPerAs _ v4 v6) = length v4 + length v6


data ImplResult = ImplResult {
    implName :: String,
    implCount :: Int,
    avgMs :: Double,
    minMs :: Double,
    maxMs :: Double
}


implementations :: [(String, PerTA Vrps -> Int)]
implementations =
    [ 
      ("sort-group", uniqueVrpCountSortGroup)        
                , ("sort-vec-scan", uniqueVrpCountSortVecScan)
        , ("sort-group-split", uniqueVrpCountSortGroupSplit)
        , ("hashset-packed", uniqueVrpCountHashSetPacked)
    , ("intset-buckets", uniqueVrpCountIntSetBuckets)
    , ("domain/uniqueVrpCount", uniqueVrpCount)
    , ("set-fold-insert", uniqueVrpCountFoldInsert)
    , ("set-union-per-ta", uniqueVrpCountSetUnionPerTa)

    ]


uniqueVrpCountIntSetBuckets :: PerTA Vrps -> Int
uniqueVrpCountIntSetBuckets = uniqueCount . foldl foldTa emptyState . perTA
  where
    foldTa state (_, Vrps vrps) = V.foldl' insertVrp state vrps


uniqueVrpCountSortGroupSplit :: PerTA Vrps -> Int
uniqueVrpCountSortGroupSplit perTaVrps =
    uniqueLen v4Keys + uniqueLen v6Keys
  where
    (v4Keys, v6Keys) = foldl foldTa ([], []) (perTA perTaVrps)

    foldTa acc (_, Vrps vrps) = V.foldl' addVrp acc vrps

    addVrp (v4s, v6s) (Vrp (ASN asn) prefix (PrefixLength maxLen)) =
        case prefix of
            Ipv4P p ->
                let (a, b, c, d) = prefixV4ToBytes p
                in ((asn, maxLen, a, b, c, d) : v4s, v6s)
            Ipv6P p ->
                let (a, b, c, d) = prefixV6ToBytes p
                in (v4s, (asn, maxLen, a, b, c, d) : v6s)

    uniqueLen = length . List.group . List.sort


uniqueVrpCountHashSetPacked :: PerTA Vrps -> Int
uniqueVrpCountHashSetPacked = HashSet.size . foldl foldTa HashSet.empty . perTA
  where
    foldTa acc (_, Vrps vrps) =
        V.foldl' (\seen vrp -> HashSet.insert (toPackedKey vrp) seen) acc vrps


data PackedVrpKey =
      PackedV4 !Word32 !Word8 !Word8 !Word8 !Word8 !Word8
    | PackedV6 !Word32 !Word8 !Word32 !Word32 !Word32 !Word32
    deriving (Eq, Ord)


instance Hashable PackedVrpKey where
    hashWithSalt salt key =
        case key of
            PackedV4 asn maxLen a b c d ->
                hashWithSalt salt (0 :: Int, asn, maxLen, a, b, c, d)
            PackedV6 asn maxLen a b c d ->
                hashWithSalt salt (1 :: Int, asn, maxLen, a, b, c, d)


toPackedKey :: Vrp -> PackedVrpKey
toPackedKey (Vrp (ASN asn) prefix (PrefixLength maxLen)) =
    case prefix of
        Ipv4P p ->
            let (a, b, c, d) = prefixV4ToBytes p
            in PackedV4 asn maxLen a b c d
        Ipv6P p ->
            let (a, b, c, d) = prefixV6ToBytes p
            in PackedV6 asn maxLen a b c d


data IntSetBucketState = IntSetBucketState {
    seenHashes :: !IntSet.IntSet,
    buckets :: !(IntMap.IntMap [Vrp]),
    uniqueCount :: !Int
}


emptyState :: IntSetBucketState
emptyState = IntSetBucketState IntSet.empty IntMap.empty 0


insertVrp :: IntSetBucketState -> Vrp -> IntSetBucketState
insertVrp state vrp =
    let h = hashVrp vrp
    in if IntSet.member h (seenHashes state)
        then case IntMap.lookup h (buckets state) of
            Just existing
                | vrp `elem` existing -> state
                | otherwise ->
                    IntSetBucketState
                        (seenHashes state)
                        (IntMap.insert h (vrp : existing) (buckets state))
                        (uniqueCount state + 1)
            Nothing ->
                IntSetBucketState
                    (seenHashes state)
                    (IntMap.insert h [vrp] (buckets state))
                    (uniqueCount state + 1)
        else
            IntSetBucketState
                (IntSet.insert h (seenHashes state))
                (IntMap.insert h [vrp] (buckets state))
                (uniqueCount state + 1)


hashVrp :: Vrp -> Int
hashVrp (Vrp (ASN asn) prefix (PrefixLength maxLen)) =
    case prefix of
        Ipv4P p ->
            let (a, b, c, d) = prefixV4ToBytes p
            in hashWithSalt 0 (0 :: Int, asn, maxLen, a, b, c, d)
        Ipv6P p ->
            let (a, b, c, d) = prefixV6ToBytes p
            in hashWithSalt 0 (1 :: Int, asn, maxLen, a, b, c, d)


benchmarkImpl :: Int -> PerTA Vrps -> (String, PerTA Vrps -> Int) -> IO ImplResult
benchmarkImpl repetitions input (name, implementation) = do
    -- Warm up for fairer timings.
    warmupCount <- evaluate $ force $ runWithSalt 0 implementation input
    samples <- forM [1 .. repetitions] $ \sampleIx -> do
        started <- getMonotonicTimeNSec
        count <- evaluate $ force $ runWithSalt sampleIx implementation input
        ended <- getMonotonicTimeNSec
        pure (count, nanosToMillis $ ended - started)

    let counts = map fst samples
    let timesMs = map snd samples
    let total = sum timesMs
    let avg = total / fromIntegral repetitions
    let mn = minimum timesMs
    let mx = maximum timesMs
    if all (== warmupCount) counts
        then pure $ ImplResult name warmupCount avg mn mx
        else error $ "Inconsistent unique counts for implementation: " <> name


benchmarkUniqImpl :: Int -> Vrps -> (String, Vrps -> V.Vector Vrp) -> IO ImplResult
benchmarkUniqImpl repetitions input (name, implementation) = do
    warmupCount <- evaluate $ force $ runUniqWithSalt 0 implementation input
    samples <- forM [1 .. repetitions] $ \sampleIx -> do
        started <- getMonotonicTimeNSec
        count <- evaluate $ force $ runUniqWithSalt sampleIx implementation input
        ended <- getMonotonicTimeNSec
        pure (count, nanosToMillis $ ended - started)

    let counts = map fst samples
    let timesMs = map snd samples
    let total = sum timesMs
    let avg = total / fromIntegral repetitions
    let mn = minimum timesMs
    let mx = maximum timesMs
    if all (== warmupCount) counts
        then pure $ ImplResult name warmupCount avg mn mx
        else error $ "Inconsistent uniqVrps counts for implementation: " <> name


benchmarkRoaCountImpl :: Int -> Roas -> (String, Roas -> Int) -> IO ImplResult
benchmarkRoaCountImpl repetitions input (name, implementation) = do
    warmupCount <- evaluate $ force $ runRoaCountWithSalt 0 implementation input
    samples <- forM [1 .. repetitions] $ \sampleIx -> do
        started <- getMonotonicTimeNSec
        count <- evaluate $ force $ runRoaCountWithSalt sampleIx implementation input
        ended <- getMonotonicTimeNSec
        pure (count, nanosToMillis $ ended - started)

    let counts = map fst samples
    let timesMs = map snd samples
    let total = sum timesMs
    let avg = total / fromIntegral repetitions
    let mn = minimum timesMs
    let mx = maximum timesMs
    if all (== warmupCount) counts
        then pure $ ImplResult name warmupCount avg mn mx
        else error $ "Inconsistent estimateVrpCountRoas counts for implementation: " <> name


verifyUniqImplementations :: Vrps -> IO ()
verifyUniqImplementations input = do
    let expected = uniqVrpsBy cmpVrps input
    forM_ uniqImplementations $ \(name, implementation) -> do
        let actual = implementation input
        unless (actual == expected) $ do
            error $ "uniqVrps implementation output mismatch: " <> name


printResult :: ImplResult -> IO ()
printResult result =
    printf "%-20s count=%d avg=%.2f ms min=%.2f ms max=%.2f ms\n"
    (implName result)
    (implCount result)
    (avgMs result)
    (minMs result)
    (maxMs result)


nanosToMillis :: Word64 -> Double
nanosToMillis ns = fromIntegral ns / 1000000.0


runWithSalt :: Int -> (PerTA Vrps -> Int) -> PerTA Vrps -> Int
runWithSalt salt implementation input =
    let count = implementation input
    in count `seq` (count + salt - salt)
{-# NOINLINE runWithSalt #-}


runUniqWithSalt :: Int -> (Vrps -> V.Vector Vrp) -> Vrps -> Int
runUniqWithSalt salt implementation input =
    let output = implementation input
        count = V.length output
    in output `seq` (count + salt - salt)
{-# NOINLINE runUniqWithSalt #-}


runRoaCountWithSalt :: Int -> (Roas -> Int) -> Roas -> Int
runRoaCountWithSalt salt implementation input =
    let count = implementation input
    in count `seq` (count + salt - salt)
{-# NOINLINE runRoaCountWithSalt #-}

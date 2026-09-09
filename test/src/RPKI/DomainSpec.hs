{-# LANGUAGE OverloadedStrings #-}

module RPKI.DomainSpec where

import           Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

import qualified Data.List       as List
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Set        as Set
import qualified Data.Vector     as V
import qualified Data.Vector.Unboxed as VU

import           RPKI.Domain
import           RPKI.Resources.Resources (readIp4)
import           RPKI.Resources.Types (ASN(..), IpPrefix(..), PrefixLength(..))


domainCountersGroup :: TestTree
domainCountersGroup =
    testGroup "Domain counters"
        [ HU.testCase "estimateVrpCount counts duplicates" testEstimateVrpCount
        , HU.testCase "estimateVrpCountRoas counts duplicates" testEstimateVrpCountRoas
        , HU.testCase "uniqueVrpCount deduplicates flattened VRPs" testUniqueVrpCount
        , HU.testCase "uniqueVrpCounts reports per-TA and overall counts" testUniqueVrpCounts
        , HU.testCase "countSortedDistinct handles edge cases" testCountSortedDistinct
        , HU.testCase "countDistinctUnion handles edge cases" testCountDistinctUnion
        , QC.testProperty "countDistinctUnion agrees with Set.union" propCountDistinctUnion
        ]


testEstimateVrpCount :: HU.Assertion
testEstimateVrpCount =
    HU.assertEqual "estimateVrpCount must include duplicate VRPs" 5
        $ estimateVrpCount perTaVrps


testEstimateVrpCountRoas :: HU.Assertion
testEstimateVrpCountRoas =
    HU.assertEqual "estimateVrpCountRoas must include duplicate ROA payload entries" 5
        $ estimateVrpCountRoas roasFixture


testUniqueVrpCount :: HU.Assertion
testUniqueVrpCount =
    HU.assertEqual "uniqueVrpCount must deduplicate identical VRPs" 2
        $ uniqueVrpCount perTaVrps


-- Both TAs hold `duplicateVrp`, so the overall count is not the sum of the
-- per-TA counts and the merge across TAs has to deduplicate too.
testUniqueVrpCounts :: HU.Assertion
testUniqueVrpCounts = do
    let (perTaCounts, total) = uniqueVrpCounts perTaVrps
    HU.assertEqual "per-TA counts must be deduplicated within each TA"
        [(TaName "ta-1", 2), (TaName "ta-2", 2)]
        (MonoidalMap.toList perTaCounts)
    HU.assertEqual "overall count must deduplicate across TAs" 2 total


testCountSortedDistinct :: HU.Assertion
testCountSortedDistinct = do
    HU.assertEqual "empty" 0 (countSortedDistinct (VU.empty :: VU.Vector Int))
    HU.assertEqual "singleton" 1 (countSortedDistinct (VU.fromList [7 :: Int]))
    HU.assertEqual "all equal" 1 (countSortedDistinct (VU.fromList [7, 7, 7 :: Int]))
    HU.assertEqual "all distinct" 3 (countSortedDistinct (VU.fromList [1, 2, 3 :: Int]))
    HU.assertEqual "runs" 3 (countSortedDistinct (VU.fromList [1, 1, 2, 3, 3, 3 :: Int]))


testCountDistinctUnion :: HU.Assertion
testCountDistinctUnion = do
    HU.assertEqual "no vectors" 0 (countDistinctUnion ([] :: [VU.Vector Int]))
    HU.assertEqual "only empty vectors" 0 (countDistinctUnion [VU.empty, VU.empty :: VU.Vector Int])
    HU.assertEqual "single vector" 2 (countDistinctUnion [VU.fromList [1, 1, 2 :: Int]])
    HU.assertEqual "empties are skipped" 2
        (countDistinctUnion [VU.empty, VU.fromList [1, 2 :: Int], VU.empty])
    HU.assertEqual "identical vectors" 3
        (countDistinctUnion [VU.fromList [1, 2, 3], VU.fromList [1, 2, 3 :: Int]])
    HU.assertEqual "disjoint vectors" 6
        (countDistinctUnion [VU.fromList [1, 2, 3], VU.fromList [4, 5, 6 :: Int]])
    -- More sources than two, with a value shared by all of them
    HU.assertEqual "five overlapping sources" 6
        (countDistinctUnion [ VU.fromList [0, 1], VU.fromList [0, 2], VU.fromList [0, 3]
                            , VU.fromList [0, 4], VU.fromList [0, 5 :: Int] ])


-- The merge has to give the same answer as building the union outright, for
-- any number of sources of any length.
propCountDistinctUnion :: [[Int]] -> Bool
propCountDistinctUnion xss =
    countDistinctUnion sorted == Set.size (Set.unions (map Set.fromList xss))
  where
    -- the merge's contract is that every source is already sorted
    sorted = map (VU.fromList . List.sort) xss


perTaVrps :: PerTA Vrps
perTaVrps =
    toPerTA
        [ (TaName "ta-1", createVrps [duplicateVrp, duplicateVrp, uniqueVrp])
        , (TaName "ta-2", createVrps [duplicateVrp, uniqueVrp])
        ]


roasFixture :: Roas
roasFixture = Roas $ MonoidalMap.fromList
    [ (ObjectKey $ asKey 1, VrpsPerAs (ASN 64500)
        [ Vrp4 (readIp4 "10.0.0.0/24") (PrefixLength 24)
        , Vrp4 (readIp4 "10.0.0.0/24") (PrefixLength 24)
        ] [])
    , (ObjectKey $ asKey 2, VrpsPerAs (ASN 64500)
        [Vrp4 (readIp4 "10.0.0.0/24") (PrefixLength 24)] [])
    , (ObjectKey $ asKey 3, VrpsPerAs (ASN 64501)
        [ Vrp4 (readIp4 "10.0.1.0/24") (PrefixLength 24)
        , Vrp4 (readIp4 "10.0.1.0/24") (PrefixLength 24)
        ] [])
    ]


duplicateVrp :: Vrp
duplicateVrp =
    Vrp (ASN 64500) (Ipv4P $ readIp4 "10.0.0.0/24") (PrefixLength 24)


uniqueVrp :: Vrp
uniqueVrp =
    Vrp (ASN 64501) (Ipv4P $ readIp4 "10.0.1.0/24") (PrefixLength 24)

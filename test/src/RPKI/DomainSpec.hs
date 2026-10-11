module RPKI.DomainSpec where

import           Test.Tasty
import qualified Test.Tasty.HUnit as HU
import qualified Test.Tasty.QuickCheck as QC

import           Control.Lens    ((&), (.~), (%~))
import           Control.Monad   (forM_)
import qualified Data.ByteString as BS
import qualified Data.List       as List
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Set        as Set
import qualified Data.Vector.Unboxed as VU

import           RPKI.AppMonad (runValidatorIO)
import           RPKI.Domain
import           RPKI.Parse.Internal.MFT (parseMft)
import           RPKI.Reporting (newScopes)
import           RPKI.Time (Instant(..))
import           RPKI.Util (hashed)
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


manifestDigestGroup :: TestTree
manifestDigestGroup =
    testGroup "Manifest content digest"
        [ HU.testCase "The order of entries doesn't matter" testDigestIgnoresOrder
        , HU.testCase "The CRL, the number and the times don't matter" testDigestIgnoresCrlNumberAndTimes
        , HU.testCase "Any other entry matters" testDigestFollowsEntries
        ]


testDigestIgnoresOrder :: HU.Assertion
testDigestIgnoresOrder = do
    mft <- fixtureManifest "test/data/afrinic_mft2.mft"
    HU.assertBool "The fixture must have a few entries" (length mft.mftEntries > 2)
    HU.assertEqual "Reversed entries must give the same digest"
        (manifestContentDigest mft)
        (manifestContentDigest $ mft & #mftEntries %~ reverse)


testDigestIgnoresCrlNumberAndTimes :: HU.Assertion
testDigestIgnoresCrlNumberAndTimes = do
    mft <- fixtureManifest "test/data/afrinic_mft1.mft"
    HU.assertEqual "The fixture must have one CRL" 1 (length $ filter isCrlEntry mft.mftEntries)
    let otherCrl (MftPair name hash)
            | isCrlEntry (MftPair name hash) = MftPair ("other-" <> name) (Hash "another hash")
            | otherwise                      = MftPair name hash
    let reissued = mft & #mftEntries %~ map otherCrl
                       & #mftNumber  .~ Serial 123456
                       & #thisTime   .~ Instant 1
                       & #nextTime   .~ Instant 2
    HU.assertEqual "A re-issued manifest with the same objects must have the same digest"
        (manifestContentDigest mft) (manifestContentDigest reissued)

    let withoutCrl = mft & #mftEntries %~ filter (not . isCrlEntry)
    HU.assertEqual "The CRL entry must not count"
        (manifestContentDigest mft) (manifestContentDigest withoutCrl)


testDigestFollowsEntries :: HU.Assertion
testDigestFollowsEntries = do
    mft <- fixtureManifest "test/data/afrinic_mft2.mft"
    let digest = manifestContentDigest mft
    let (crls, entries) = List.partition isCrlEntry mft.mftEntries
    MftPair name hash <- case entries of
                e : _ -> pure e
                []    -> HU.assertFailure "The fixture must have entries other than its CRL"
    let changed f = manifestContentDigest $ mft & #mftEntries .~ crls <> f entries
    let replaceFirst e = (e :) . drop 1
    let changes :: [(String, Hash)]
        changes = [
            ("added",   changed (MftPair "new.roa" (Hash "some hash") :)),
            ("removed", changed (drop 1)),
            ("renamed", changed (replaceFirst $ MftPair "renamed.roa" hash)),
            ("changed", changed (replaceFirst $ MftPair name (Hash "other hash")))
          ]
    forM_ changes $ \(what, digest') ->
        HU.assertBool ("A manifest with an entry " <> what <> " must have another digest")
            (digest /= digest')

    -- The same bytes, with the boundary between the name and the hash moved
    HU.assertBool "The name and the hash must be kept apart"
        (changed (replaceFirst $ MftPair "a.roa" (Hash "xyz")) /=
         changed (replaceFirst $ MftPair "a.roax" (Hash "yz")))


fixtureManifest :: FilePath -> IO Manifest
fixtureManifest path = do
    bytes <- BS.readFile path
    (r, _) <- runValidatorIO (newScopes "fixture") $ parseMft (hashed bytes)
    case r of
        Left e    -> HU.assertFailure $ "Couldn't parse " <> path <> ": " <> show e
        Right mft -> pure $ getCMSContent mft.cmsPayload


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

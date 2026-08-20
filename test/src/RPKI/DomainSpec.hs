{-# LANGUAGE OverloadedStrings #-}

module RPKI.DomainSpec where

import           Test.Tasty
import qualified Test.Tasty.HUnit as HU

import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Vector     as V

import           RPKI.Domain
import           RPKI.Resources.Resources (readIp4)
import           RPKI.Resources.Types (ASN(..), IpPrefix(..), PrefixLength(..))


domainCountersGroup :: TestTree
domainCountersGroup =
    testGroup "Domain counters"
        [ HU.testCase "estimateVrpCount counts duplicates" testEstimateVrpCount
        , HU.testCase "estimateVrpCountRoas counts duplicates" testEstimateVrpCountRoas
        , HU.testCase "uniqueVrpCount deduplicates flattened VRPs" testUniqueVrpCount
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


perTaVrps :: PerTA Vrps
perTaVrps =
    toPerTA
        [ (TaName "ta-1", Vrps $ V.fromList [duplicateVrp, duplicateVrp, uniqueVrp])
        , (TaName "ta-2", Vrps $ V.fromList [duplicateVrp, uniqueVrp])
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

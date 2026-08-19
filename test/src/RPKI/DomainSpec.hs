{-# LANGUAGE OverloadedStrings #-}

module RPKI.DomainSpec where

import           Test.Tasty
import qualified Test.Tasty.HUnit as HU

import qualified Data.Map.Strict as Map
import qualified Data.Vector     as V

import           RPKI.Domain
import           RPKI.Resources.Resources (readIp4)
import           RPKI.Resources.Types (ASN(..), PrefixLength(..))


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
        [ (TaName "ta-1", Vrps $ V.fromList [duplicatePayload, duplicatePayload])
        , (TaName "ta-2", Vrps $ V.singleton uniquePayload)
        ]


roasFixture :: Roas
roasFixture = Roas $ Map.fromList
    [ (ObjectKey $ asKey 1, duplicatePayload)
    , (ObjectKey $ asKey 2, duplicatePayload)
    , (ObjectKey $ asKey 3, uniquePayload)
    ]


duplicatePayload :: VrpsPerAs
duplicatePayload =
    VrpsPerAs
        (ASN 64500)
        [ Vrp4 (readIp4 "10.0.0.0/24") (PrefixLength 24)
        , Vrp4 (readIp4 "10.0.0.0/24") (PrefixLength 24)
        ]
        []


uniquePayload :: VrpsPerAs
uniquePayload =
    VrpsPerAs
        (ASN 64501)
        [ Vrp4 (readIp4 "10.0.1.0/24") (PrefixLength 24) ]
        []

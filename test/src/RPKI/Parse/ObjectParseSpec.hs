{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels           #-}

module RPKI.Parse.ObjectParseSpec where

import Control.Lens
import qualified Data.ByteString        as BS
import           Data.Maybe (isJust)
import qualified Data.Set               as Set

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Reporting
import           RPKI.Parse.Parse
import           RPKI.Parse.Internal.Aspa
import           RPKI.Parse.Internal.SPL

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU
import RPKI.Parse.Internal.Erik
import RPKI.Util (hashHex)


-- TODO Implement a bunch of good tests here
-- There should be a test suite with real objects, which is way too long and tedious, 
-- so far all the testing is happening on the level of comparing VRP lists.

objectParseSpec :: TestTree
objectParseSpec = testGroup "Unit tests for object parsing" [
    shoudlParseBGPSec,
    shouldParseAspa1,
    shouldParseAspa2,
    shouldParseSpl,
    shouldParseErikIndex,
    shouldParseErikPartition
  ]


shoudlParseBGPSec :: TestTree
shoudlParseBGPSec = HU.testCase "Should parse a BGPSec certificate" $ do        
    bs <- BS.readFile "test/data/bgp_router_cert.cer"
    let (Right (rc, ct, ski, aki, hash), _) = 
            runPureValidator (newScopes "parse") $ parseResourceCertificate bs
    
    HU.assertEqual "It is a BGPSec certificate" ct  BGPCert               
    HU.assertEqual "SPKI is right" 
        (getSubjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature rc)  
        (SPKI $ EncodedBase64 "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAET10FMBxP6P3r6aG/ICpfsktp7X6ylJIY8Kye6zkQhNOt0y+cRzYngH8MGzY3cXNvZ64z4CpZ22gf4teybGq8ow==")
    HU.assertBool "It has AKI" (isJust aki)   


shouldParseAspa1 :: TestTree
shouldParseAspa1 = HU.testCase "Should parse an ASPA object" $ do        
    bs <- BS.readFile "test/data/AS204325.asa"
    let (Right aspaObject, _) = runPureValidator (newScopes "parse") $ parseAspa bs

    let Aspa {..} = getCMSContent $ cmsPayload aspaObject
    HU.assertEqual "Wrong customer" customer (ASN 204325)
    HU.assertEqual "Wrong providers" providers (Set.fromList [ASN 65000, ASN 65002, ASN 65003])    

shouldParseAspa2 :: TestTree
shouldParseAspa2 = HU.testCase "Should not parse an ASPA object" $ do        
    bs <- BS.readFile "test/data/aspa-no-explicit-version.asa"
    let (x, _) = runPureValidator (newScopes "parse") $ parseAspa bs
    case x of
        Left (ParseE (ParseError s)) -> 
             HU.assertEqual "Wrong outcome" s "Couldn't parse embedded ASN1 stream: Wrong provider AS (Start Sequence)"
        _ -> HU.assertFailure $ "Expected a parse error, but got something else" <> show x

shouldParseSpl = HU.testCase "Should parse an SPL object" $ do        
    bs <- BS.readFile "test/data/9X0AhXWTJDl8lJhfOwvnac-42CA.spl"
    let (Right splObject, _) = runPureValidator (newScopes "parse") $ parseSpl bs

    let SplPayload asn prefixes = getCMSContent $ cmsPayload splObject
    HU.assertEqual "Wrong ASN" asn (ASN 15562)
    HU.assertEqual "Wrong prefix list length" (length prefixes) 23    

shouldParseErikIndex :: TestTree
shouldParseErikIndex = HU.testCase "Should parse an Erik index" $ do        
    bs <- BS.readFile "test/data/erik/ca.rg.net"
    let (Right ErikIndex {..}, _) = 
            runPureValidator (newScopes "parse") $ parseErikIndex bs
    
    HU.assertEqual "Wrong index" indexScope "ca.rg.net"
    HU.assertEqual "Wrong number of partitions" (length partitionList) 1
    HU.assertEqual "Wrong hash" 
        "c504bbac23736e418393a18491c08183a2c82f0f1d020badb175dc142d61f7df"
        (hashHex $ head partitionList ^. #hash)

shouldParseErikPartition :: TestTree
shouldParseErikPartition = HU.testCase "Should parse an Erik partition" $ do
    bs <- BS.readFile "test/data/erik/7I-e8mmhb4Hx5EcgYvZhRMHBkqsp-fHo1hUvReXcKe4"
    let (Right p, _) = 
            runPureValidator (newScopes "parse") $ parseMft bs
    
    HU.assertBool "Wrong index" True

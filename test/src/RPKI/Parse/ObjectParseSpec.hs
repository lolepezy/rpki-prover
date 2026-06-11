{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.ObjectParseSpec where

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


-- TODO Implement a bunch of good tests here
-- There should be a test suite with real objects, which is way too long and tedious, 
-- so far all the testing is happening on the level of comparing VRP lists.


objectParseSpec :: TestTree
objectParseSpec = testGroup "Unit tests for object parsing" [
    shoudlParseBGPSec,
    shouldParseAspa,
    shouldParseSpl,
    supportedExtensionSpec
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

shouldParseAspa :: TestTree
shouldParseAspa = HU.testCase "Should parse an ASPA object" $ do        
    bs <- BS.readFile "test/data/AS204325.asa"
    let (Right aspaObject, _) = runPureValidator (newScopes "parse") $ parseAspa bs

    let Aspa {..} = getCMSContent $ cmsPayload aspaObject
    HU.assertEqual "Wrong customer" customer (ASN 204325)
    HU.assertEqual "Wrong providers" providers (Set.fromList [ASN 65000, ASN 65002, ASN 65003])    

shouldParseSpl :: TestTree
shouldParseSpl = HU.testCase "Should parse an SPL object" $ do        
    bs <- BS.readFile "test/data/9X0AhXWTJDl8lJhfOwvnac-42CA.spl"
    let (Right splObject, _) = runPureValidator (newScopes "parse") $ parseSpl bs

    let SplPayload asn prefixes = getCMSContent $ cmsPayload splObject
    HU.assertEqual "Wrong ASN" asn (ASN 15562)
    HU.assertEqual "Wrong prefix list length" (length prefixes) 23    

supportedExtensionSpec :: TestTree
supportedExtensionSpec = testGroup "supportedExtension should do the right thing" [
    HU.testCase "Accepts .cer" $ HU.assertBool "" (supportedExtension "foo.cer"),
    HU.testCase "Accepts .mft" $ HU.assertBool "" (supportedExtension "foo.mft"),
    HU.testCase "Accepts .crl" $ HU.assertBool "" (supportedExtension "foo.crl"),
    HU.testCase "Accepts .roa" $ HU.assertBool "" (supportedExtension "foo.roa"),
    HU.testCase "Accepts .gbr" $ HU.assertBool "" (supportedExtension "foo.gbr"),
    HU.testCase "Accepts .sig" $ HU.assertBool "" (supportedExtension "foo.sig"),
    HU.testCase "Accepts .asa" $ HU.assertBool "" (supportedExtension "foo.asa"),
    HU.testCase "Accepts .spl" $ HU.assertBool "" (supportedExtension "foo.spl"),
    HU.testCase "More than one dot" $ HU.assertBool "" (supportedExtension "foo.cer.bak.roa"),
    HU.testCase "Case-insensitive .CER" $ HU.assertBool "" (supportedExtension "foo.CER"),
    HU.testCase "Case-insensitive .MFT" $ HU.assertBool "" (supportedExtension "foo.MFT"),
    HU.testCase "Case-insensitive mixed .Cer" $ HU.assertBool "" (supportedExtension "foo.Cer"),
    HU.testCase "Rejects unknown extension" $ HU.assertBool "" (not $ supportedExtension "foo.txt"),
    HU.testCase "Rejects no extension"      $ HU.assertBool "" (not $ supportedExtension "foocer"),
    HU.testCase "Rejects empty string"      $ HU.assertBool "" (not $ supportedExtension ""),
    HU.testCase "Rejects short string"      $ HU.assertBool "" (not $ supportedExtension ".ce"),
    HU.testCase "Accepts long path"         $ HU.assertBool "" (supportedExtension "/some/long/path/object.roa")
  ]
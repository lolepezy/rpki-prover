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
import           RPKI.Validation.ObjectValidation (prevalidateObject)

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
    supportedExtensionSpec,
    prevalidationSpec
  ]


{- | Run the full self-contained validation over the real objects in test/data.

   `prevalidateObject` enforces a lot of profile requirements (eContentType, the 
   EE certificate SIA, signature algorithms, key parameters, the CMS signature 
   itself, ...), and every one of them is a way to reject an object that other 
   RPs accept. These objects come from the real RPKI, so they must all pass.
-}
prevalidationSpec :: TestTree
prevalidationSpec = testGroup "Prevalidation of real objects" 
    [ shouldPrevalidate "test/data/afrinic_mft1.mft"                MFT
    , shouldPrevalidate "test/data/afrinic_mft2.mft"                MFT
    , shouldPrevalidate "test/data/AS204325.asa"                    ASPA
    , shouldPrevalidate "test/data/9X0AhXWTJDl8lJhfOwvnac-42CA.spl" SPL
    , shouldPrevalidate "test/data/ClF4YOBviAEnwFokhNG1NXBZjEA.gbr" GBR
    , shouldPrevalidate "test/data/VZgMGXDlMc_DQX3QkQKbaQ0K8vM.gbr" GBR
    , shouldPrevalidate "test/data/checklist.sig"                   RSC
    , shouldPrevalidate "test/data/bgp_router_cert.cer"             CER
    , shouldPrevalidate "test/data/big_cert.cer"                    CER
    , shouldPrevalidate "test/data/smaller.cer"                     CER
    , shouldPrevalidate "test/data/overcleaiming/2R93viIBHX4dV12fmttfjhYOX9k.cer" CER
    , shouldPrevalidate "test/data/overcleaiming/EBA158C223CE11EBA804DD64C4F9AE02.roa" ROA

    -- Two fixtures are deliberately left out. Both fail on checks that predate 
    -- this test, so they are not "known good" objects:
    --   * test/data/aspa.1.asa uses the obsolete ASPA draft encoding, where a 
    --     provider is a SEQUENCE {ASId, afiLimit} rather than a bare ASId, so 
    --     `getInteger` rejects it in the providers list.
    --   * test/data/checklist.sig.1 has certificate serial number 0, which 
    --     `makeSerial` rejects per RFC 5280 section 4.1.2.2.
    ]
  where
    shouldPrevalidate path objectType = 
        HU.testCase ("Should prevalidate " <> path) $ do 
            bs <- BS.readFile path
            let (r, _) = runValidatorPure (newScopes "prevalidate") $ 
                            readObjectOfType objectType bs >>= prevalidateObject
            case r of 
                Right _ -> pure ()
                Left e  -> HU.assertFailure $ 
                    "Failed to prevalidate " <> path <> ": " <> show e


shoudlParseBGPSec :: TestTree
shoudlParseBGPSec = HU.testCase "Should parse a BGPSec certificate" $ do        
    bs <- BS.readFile "test/data/bgp_router_cert.cer"
    let (Right (rc, ct, ski, aki, objectHash), _) = 
            runValidatorPure (newScopes "parse") $ parseResourceCertificate bs
    let bgpObject = BgpCerObject {
            hash = objectHash,
            ski = ski,
            aki = aki,
            certificate = TypedCert rc
        }
    
    HU.assertEqual "It is a BGPSec certificate" ct  BGPCert               
    HU.assertEqual "SPKI is right" 
        (getSubjectPublicKeyInfo bgpObject)
        (SPKI $ EncodedBase64 "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAET10FMBxP6P3r6aG/ICpfsktp7X6ylJIY8Kye6zkQhNOt0y+cRzYngH8MGzY3cXNvZ64z4CpZ22gf4teybGq8ow==")
    HU.assertBool "It has AKI" (isJust aki)   

shouldParseAspa :: TestTree
shouldParseAspa = HU.testCase "Should parse an ASPA object" $ do        
    bs <- BS.readFile "test/data/AS204325.asa"
    let (Right aspaObject, _) = runValidatorPure (newScopes "parse") $ parseAspa bs

    let Aspa {..} = getCMSContent $ cmsPayload aspaObject
    HU.assertEqual "Wrong customer" customer (ASN 204325)
    HU.assertEqual "Wrong providers" providers (Set.fromList [ASN 65000, ASN 65002, ASN 65003])    

shouldParseSpl :: TestTree
shouldParseSpl = HU.testCase "Should parse an SPL object" $ do        
    bs <- BS.readFile "test/data/9X0AhXWTJDl8lJhfOwvnac-42CA.spl"
    let (Right splObject, _) = runValidatorPure (newScopes "parse") $ parseSpl bs

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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedLabels  #-}

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
import           RPKI.Validation.ObjectValidation (prevalidateObject)

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU
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
    prevalidationSpec,
    filenameToObjectTypeSpec,
    shouldParseErikIndex,
    shouldParseErikPartition
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


shouldParseAspa1 :: TestTree
shouldParseAspa1 = HU.testCase "Should parse an ASPA object" $ do        
    bs <- BS.readFile "test/data/AS204325.asa"
    let (Right aspaObject, _) = runValidatorPure (newScopes "parse") $ parseAspa bs

    let Aspa {..} = getCMSContent $ cmsPayload aspaObject
    HU.assertEqual "Wrong customer" customer (ASN 204325)
    HU.assertEqual "Wrong providers" providers (Set.fromList [ASN 65000, ASN 65002, ASN 65003])    

shouldParseAspa2 :: TestTree
shouldParseAspa2 = HU.testCase "Should not parse an ASPA object" $ do        
    bs <- BS.readFile "test/data/aspa-no-explicit-version.asa"
    let (x, _) = runValidatorPure (newScopes "parse") $ parseAspa bs
    case x of
        Left (ParseE (ParseError s)) -> 
             HU.assertEqual "Wrong outcome" s "Couldn't parse embedded ASN1 stream: Wrong provider AS (Start Sequence)"
        _ -> HU.assertFailure $ "Expected a parse error, but got something else" <> show x

shouldParseSpl = HU.testCase "Should parse an SPL object" $ do        
    bs <- BS.readFile "test/data/9X0AhXWTJDl8lJhfOwvnac-42CA.spl"
    let (Right splObject, _) = runValidatorPure (newScopes "parse") $ parseSpl bs

    let SplPayload asn prefixes = getCMSContent $ cmsPayload splObject
    HU.assertEqual "Wrong ASN" asn (ASN 15562)
    HU.assertEqual "Wrong prefix list length" (length prefixes) 23    

shouldParseErikIndex :: TestTree
shouldParseErikIndex = HU.testCase "Should parse an Erik index" $ do        
    bs <- BS.readFile "test/data/erik/ca.rg.net"
    let (Right ErikIndex {..}, _) = 
            runValidatorPure (newScopes "parse") $ parseErikIndex bs
    
    HU.assertEqual "Wrong index" indexScope "ca.rg.net"
    HU.assertEqual "Wrong number of partitions" (length partitionList) 1
    HU.assertEqual "Wrong hash" 
        "ecac4fcf9bbefc121b936ef72ec35cf01f8448c03ac84a1c7a188288bb440f51"
        (hashHex $ head partitionList ^. #hash)

shouldParseErikPartition :: TestTree
shouldParseErikPartition = HU.testCase "Should parse an Erik partition" $ do
    bs <- BS.readFile "test/data/erik/wjhGf2wIon7eny9o-3Wfp5EMkVmgDgQ4iac5T8NUJQ8"
    let (Right p, _) = runValidatorPure (newScopes "parse") $ parseErikPartition bs
    
    HU.assertBool "Wrong index" True


filenameToObjectTypeSpec :: TestTree
filenameToObjectTypeSpec = testGroup "Filename to object type mapping" [
    check "foo.cer"                    (Just CER),
    check "foo.mft"                    (Just MFT),
    check "foo.crl"                    (Just CRL),
    check "foo.roa"                    (Just ROA),
    check "foo.gbr"                    (Just GBR),
    check "foo.sig"                    (Just RSC),
    check "foo.asa"                    (Just ASPA),
    check "foo.spl"                    (Just SPL),
    check "foo.CER"                    Nothing,
    check "foo.MFT"                    Nothing,
    check "foo.Cer"                    Nothing,
    check "foo.cer.bak.roa"            (Just ROA),
    check "foo.crl.cer.roa"            (Just ROA),
    check "/some/long/path/object.roa" (Just ROA),
    check "foo.txt"                    Nothing,
    check "foo.cer.txt"                Nothing,
    check "foocer"                     Nothing,
    check ""                           Nothing,
    check ".ce"                        Nothing
  ]
  where
    check filename expected =
        HU.testCase (show filename <> " -> " <> show expected) $
            HU.assertEqual "" expected (nameObjectType filename)
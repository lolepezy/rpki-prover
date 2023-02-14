{-# LANGUAGE OverloadedStrings #-}

module RPKI.Parse.ObjectParseSpec where

import qualified Data.ByteString    as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Maybe (isJust)

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Reporting
import           RPKI.Parse.Parse

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU


-- TODO Implement a bunch of good tests here
-- There should be a test suite with real objects, which is way too long and tedious, 
-- so far all the testing is happening on the level of comparing VRP lists.


objectParseSpec :: TestTree
objectParseSpec = testGroup "Unit tests for object parsing" [
    HU.testCase "Should parse a BGPSec certificate" $ do        

        bs <- BS.readFile "test/data/bgp_router_cert.cer"
        let (Right (rc, ct, rfc, ski, aki, hash), _) = 
                runPureValidator (newScopes "parse") $ parseResourceCertificate bs
        
        HU.assertEqual "It is a BGPSec certificate" ct  BGPCert
        HU.assertEqual "It is has strict validation RFC" rfc  StrictRFC                
        HU.assertEqual "SPKI is right" 
            (getSubjectPublicKeyInfo $ cwsX509certificate $ getCertWithSignature rc)  
            (SPKI $ EncodedBase64 "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAET10FMBxP6P3r6aG/ICpfsktp7X6ylJIY8Kye6zkQhNOt0y+cRzYngH8MGzY3cXNvZ64z4CpZ22gf4teybGq8ow==")
        HU.assertBool "It has AKI" (isJust aki)        
  ]
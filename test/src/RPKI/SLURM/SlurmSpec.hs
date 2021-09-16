{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes                #-}

module RPKI.SLURM.SlurmSpec where

import qualified Data.ByteString.Lazy as LBS

import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit      as HU

import           Data.These

import           Data.String
import           Data.Aeson as Json

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.String.Interpolate.IsString

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Orphans
import           RPKI.SLURM.Types
import           RPKI.Resources.Types
import           RPKI.Resources.Resources


slurmGroup :: TestTree
slurmGroup = testGroup "Slurm" [
        HU.testCase "Test empty" test_empty,
        HU.testCase "Test broken 1" test_broken_1,
        HU.testCase "Test Full" test_full
    ]


test_empty :: HU.Assertion
test_empty = 
    assertParsed
        (Slurm { 
                slurmVersion = 1,
                validationOutputFilters = ValidationOutputFilters [] [],
                locallyAddedAssertions = LocallyAddedAssertions [] []
            })
        ([i|{
            "slurmVersion": 1,
            "validationOutputFilters": {
                "prefixFilters": [],
                "bgpsecFilters": []
            },
            "locallyAddedAssertions": {
                "prefixAssertions": [],
                "bgpsecAssertions": []
            }
            }|])

test_broken_1 :: HU.Assertion
test_broken_1 = 
    assertNotParsed
        [i|Error in $.validationOutputFilters: parsing RPKI.SLURM.Types.ValidationOutputFilters(ValidationOutputFilters) failed, key "prefixFilters" not found|]
        ([i|{
            "slurmVersion": 1,
            "validationOutputFilters": {                
                "bgpsecFilters": []
            },
            "locallyAddedAssertions": {
                "prefixAssertions": [],
                "bgpsecAssertions": []
            }
            }|])


test_full :: HU.Assertion
test_full = 
    assertParsed
        (Slurm {
            slurmVersion = 1, 
            validationOutputFilters = ValidationOutputFilters {
                prefixFilters = [
                    PrefixFilter {
                        asnAndPrefix = That (Ipv4P $ readIp4 "192.0.2.0/24"), 
                        comment = Just "All VRPs encompassed by prefix"
                    },
                    PrefixFilter {
                        asnAndPrefix = This (ASN 64496), 
                        comment = Just "All VRPs matching ASN"
                    },
                    PrefixFilter {
                        asnAndPrefix = These (ASN 64497) (Ipv4P $ readIp4 "198.51.100.0/24"), 
                        comment = Just "All VRPs encompassed by prefix, matching ASN"
                    }
                ], 
                bgpsecFilters = [
                    BgpsecFilter {
                        asnAndSKI = This (ASN 64496), 
                        comment = Just "All keys for ASN"
                    },
                    BgpsecFilter {
                        asnAndSKI = That (DecodedBase64 "foo"), 
                        comment = Just "Key matching Router SKI"
                    },
                    BgpsecFilter {
                        asnAndSKI = These (ASN 64497) (DecodedBase64 "bar"), 
                        comment = Just "Key for ASN 64497 matching Router SKI"
                    }
                ]
            }, 
            locallyAddedAssertions = LocallyAddedAssertions {
                prefixAssertions = [
                    PrefixAssertion {
                        asn = ASN 64496, 
                        prefix = Ipv4P (readIp4 "198.51.100.0/24"), 
                        maxPrefixLength = Nothing, 
                        comment = Just "My other important route"
                    },
                    PrefixAssertion {
                        asn = ASN 64496, 
                        prefix = Ipv6P (readIp6 "2001:db8::/32"), 
                        maxPrefixLength = Just (PrefixLength 48), 
                        comment = Just "My other important de-aggregated routes"
                    }
                ], 
                bgpsecAssertions = [
                    BgpsecAssertion {
                        asn = ASN 64496, 
                        ski = DecodedBase64 "<some base64 SKI>", 
                        routerPublicKey = DecodedBase64 "<some base64 public key>", 
                        comment = Just "My known key for my important ASN"
                    }
                ]
            }
        })
        fullJson


-- fullJson :: Either String Slurm
fullJson = [i|
    {
        "slurmVersion": 1,
        "validationOutputFilters": {
        "prefixFilters": [
            {
            "prefix": "192.0.2.0/24",
            "comment": "All VRPs encompassed by prefix"
            },
            {
            "asn": 64496,
            "comment": "All VRPs matching ASN"
            },
            {
            "prefix": "198.51.100.0/24",
            "asn": 64497,
            "comment": "All VRPs encompassed by prefix, matching ASN"
            }
        ],
        "bgpsecFilters": [
            {
            "asn": 64496,
            "comment": "All keys for ASN"
            },
            {
            "SKI": "Zm9v",
            "comment": "Key matching Router SKI"
            },
            {
            "asn": 64497,
            "SKI": "YmFy",
            "comment": "Key for ASN 64497 matching Router SKI"
            }
        ]
        },
        "locallyAddedAssertions": {
        "prefixAssertions": [
            {
            "asn": 64496,
            "prefix": "198.51.100.0/24",
            "comment": "My other important route"
            },
            {
            "asn": 64496,
            "prefix": "2001:DB8::/32",
            "maxPrefixLength": 48,
            "comment": "My other important de-aggregated routes"
            }
        ],
        "bgpsecAssertions": [
            {
            "asn": 64496,
            "comment" : "My known key for my important ASN",
            "SKI": "PHNvbWUgYmFzZTY0IFNLST4=",
            "routerPublicKey": "PHNvbWUgYmFzZTY0IHB1YmxpYyBrZXk+"
            }
        ]
        }
        }
        |]            


assertParsed slurm t = let
        decoded = Json.eitherDecode t
    in HU.assertEqual ("Not the same: decoded " <> show decoded) (Right slurm) decoded

assertNotParsed :: String -> LBS.ByteString -> HU.Assertion
assertNotParsed errorMessage t = let
        decoded :: Either String Slurm = Json.eitherDecode t
    in HU.assertEqual 
        ("Didn't complain with (" <> show errorMessage <> ")") 
        (Left errorMessage) 
        decoded


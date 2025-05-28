{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.SLURM.SlurmSpec where

import           Control.Lens

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           Test.Tasty
import qualified Test.Tasty.HUnit      as HU

import           Data.These

import           Data.String
import           Data.Aeson as Json

import qualified Data.Set                 as Set
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import           Data.String.Interpolate.IsString

import           RPKI.Domain
import           RPKI.AppMonad
import           RPKI.Reporting
import           RPKI.Orphans
import           RPKI.SLURM.Types
import           RPKI.SLURM.SlurmProcessing
import           RPKI.Resources.Types
import           RPKI.Resources.Resources
import           RPKI.AppState
import           RPKI.Util 

slurmGroup :: TestTree
slurmGroup = testGroup "Slurm" [
        HU.testCase "Test empty" test_empty,
        HU.testCase "Test broken 1" test_broken_1,
        HU.testCase "Test Full" test_full,
        HU.testCase "Test reject duplicate SLURM files" test_reject_full_duplicates,
        HU.testCase "Test reject partial prefix duplicate SLURM files" test_reject_partial_prefix_duplicates,
        HU.testCase "Test reject partial ASN duplicate SLURM files" test_reject_partial_asn_duplicates,
        HU.testCase "Test SLURM filtering is applied to RTR payload" test_apply_slurm
    ]


test_empty :: HU.Assertion
test_empty = 
    assertParsed
        (Slurm { 
                slurmVersion = SlurmVersion 1, 
                validationOutputFilters = ValidationOutputFilters [] [],
                locallyAddedAssertions = LocallyAddedAssertions [] []
            })
        [i|{
            "slurmVersion": 1,
            "validationOutputFilters": {
                "prefixFilters": [],
                "bgpsecFilters": []
            },
            "locallyAddedAssertions": {
                "prefixAssertions": [],
                "bgpsecAssertions": []
            }
        }|]

test_broken_1 :: HU.Assertion
test_broken_1 = 
    assertNotParsed
        [i|Error in $.validationOutputFilters: parsing RPKI.SLURM.Types.ValidationOutputFilters(ValidationOutputFilters) failed, key "prefixFilters" not found|]
        [i|{
            "slurmVersion": 1,
            "validationOutputFilters": {                
                "bgpsecFilters": []
            },
            "locallyAddedAssertions": {
                "prefixAssertions": [],
                "bgpsecAssertions": []
            }
        }|]


test_full :: HU.Assertion
test_full = 
    assertParsed
        bigTestSlurm
        fullJson


test_reject_full_duplicates :: HU.Assertion
test_reject_full_duplicates = do
    -- validate overlapping with itself    
    (z, _) <- runValidatorT (newScopes "test") 
        $ vHoist $ validateNoOverlaps [("Foo", bigTestSlurm), ("Bar", bigTestSlurm)]
    HU.assertEqual 
        ("Wrong validation message " <> show z) 
        (Left (SlurmE (SlurmValidationError 
            $ "File Foo has prefix overlaps with file Bar: " <> 
              "[(Ipv4P (Ipv4Prefix 198.51.100.0/24),Ipv4P (Ipv4Prefix 198.51.100.0/24))," <> 
              "(Ipv6P (Ipv6Prefix 2001:db8::/32),Ipv6P (Ipv6Prefix 2001:db8::/32))," <> 
              "(Ipv4P (Ipv4Prefix 192.0.2.0/24),Ipv4P (Ipv4Prefix 192.0.2.0/24))]"))) 
        z    

test_reject_partial_prefix_duplicates :: HU.Assertion
test_reject_partial_prefix_duplicates = do
    let slurm1 = (mempty :: Slurm) {
        validationOutputFilters = mempty {
            prefixFilters = [
                PrefixFilter {
                    asnAndPrefix = That (Ipv4P $ readIp4 "192.0.2.0/24"), 
                    comment = Just "All VRPs encompassed by prefix"
                }            
            ]        
        }
    }
    let slurm2 = (mempty :: Slurm) {
        locallyAddedAssertions = mempty {
            prefixAssertions = [
                PrefixAssertion {
                    asn = NumericASN 64496, 
                    prefix = Ipv4P (readIp4 "192.0.0.0/16"), 
                    maxPrefixLength = Nothing, 
                    comment = Just "My other important route"
                }
            ]
        }      
    }
    (z, _) <- runValidatorT (newScopes "test") 
        $ vHoist $ validateNoOverlaps [("Foo", slurm1), ("Bar", slurm2)]
    HU.assertEqual 
        ("Wrong validation message " <> show z) 
        (Left (SlurmE (SlurmValidationError $
            "File Foo has prefix overlaps with file Bar: " <> 
            "[(Ipv4P (Ipv4Prefix 192.0.0.0/16),Ipv4P (Ipv4Prefix 192.0.2.0/24))]"))) 
        z    


test_reject_partial_asn_duplicates :: HU.Assertion
test_reject_partial_asn_duplicates = do
    let slurm1 = (mempty :: Slurm) {
        validationOutputFilters = mempty {
            bgpsecFilters = [
                BgpsecFilter {
                    asnAndSKI = This (NumericASN 64496), 
                    comment = Just "All keys for ASN"
                }
            ]     
        }
    }
    let slurm2 = (mempty :: Slurm) {
        locallyAddedAssertions = mempty {
            bgpsecAssertions = [
                BgpsecAssertion {
                    asn = NumericASN 64496, 
                    ski = DecodedBase64 "<some base64 SKI>", 
                    routerPublicKey = DecodedBase64 "<some base64 public key>", 
                    comment = Just "My known key for my important ASN"
                }
            ]
        }      
    }
    (z, _) <- runValidatorT (newScopes "test") 
        $ vHoist $ validateNoOverlaps [("Foo", slurm1), ("Bar", slurm2)]
    HU.assertEqual 
        ("Wrong validation message " <> show z) 
        (Left (SlurmE (SlurmValidationError
            "File Foo has ASN overlaps with file Bar: [ASN 64496]"))) 
        z    


test_apply_slurm :: HU.Assertion
test_apply_slurm = do    
    let rtrPayloads = 
            mkRtrPayloads 
                (asPerTA $ createVrps [
                    mkVrp4 123 "192.168.0.0/16" 16,
                    mkVrp4 124 "192.0.2.0/24" 24,
                    mkVrp4 64496 "10.1.1.0/24" 24,
                    mkVrp4 64497 "198.51.100.0/24" 24,
                    mkVrp4 64497 "198.51.101.0/24" 24
                ]) 
                (Set.fromList [
                    mkBgpSec "aabb" [ASN 64496] "aabbcc",
                    mkBgpSec "foo" [ASN 123] "112233",                
                    mkBgpSec "bar" [ASN 64497] "112233",
                    mkBgpSec "bar" [ASN 64497, ASN 111] "445566",
                    mkBgpSec "1122" [ASN 234] "112233"
                ])

    let filtered_ = filterWithSLURM rtrPayloads bigTestSlurm

    let expected = 
            mkRtrPayloads 
                (toPerTA [
                    (TaName "default", createVrps [
                        mkVrp4 123 "192.168.0.0/16" 16,
                        mkVrp4 64497 "198.51.101.0/24" 24                    
                    ]),
                    (TaName "slurm", createVrps [
                        mkVrp4 64496 "198.51.100.0/24" 24,
                        mkVrp6 64496 "2001:db8::/32" 48
                    ])
                ]) 
                (Set.fromList [                                        
                    mkBgpSec "1122" [ASN 234] "112233",
                    mkBgpSec "bar" [ASN 111] "445566",
                    mkBgpSec "<some base64 SKI>"
                        [ASN 64496] "PHNvbWUgYmFzZTY0IHB1YmxpYyBrZXk+"
                ])
    HU.assertEqual "Wrong BGPSecs:" (expected ^. #bgpSec) (filtered_ ^. #bgpSec)
    HU.assertEqual "Wrong VRPs:" (expected ^. #vrps) (filtered_ ^. #vrps)
    
{- 
PerTA {unPerTA = MonoidalMap {getMonoidalMap = fromList [
    ("default",
        Vrps {unVrps = [
            Vrp (ASN 123) (Ipv4P (Ipv4Prefix 192.168.0.0/16)) (PrefixLength 16),
            Vrp (ASN 64497) (Ipv4P (Ipv4Prefix 198.51.101.0/24)) (PrefixLength 24)
        ]}),
    ("slurm",
        Vrps {unVrps = [
            Vrp (ASN 64496) (Ipv4P (Ipv4Prefix 198.51.100.0/24)) (PrefixLength 24),
            Vrp (ASN 64496) (Ipv6P (Ipv6Prefix 2001:db8::/32)) (PrefixLength 48)]})
        ]}}
PerTA {unPerTA = MonoidalMap {getMonoidalMap = fromList [
    ("default",
        Vrps {unVrps = [
            Vrp (ASN 123) (Ipv4P (Ipv4Prefix 192.168.0.0/16)) (PrefixLength 16),
            Vrp (ASN 64497) (Ipv4P (Ipv4Prefix 198.51.101.0/24)) (PrefixLength 24),
            Vrp (ASN 64496) (Ipv4P (Ipv4Prefix 198.51.100.0/24)) (PrefixLength 24),
            Vrp (ASN 64496) (Ipv6P (Ipv6Prefix 2001:db8::/32)) (PrefixLength 48)]})]}}

-}

  where
    mkVrp4 asn prefix length_ = 
        Vrp (ASN asn) (Ipv4P $ readIp4 prefix) (PrefixLength length_)
    mkVrp6 asn prefix length_ = 
        Vrp (ASN asn) (Ipv6P $ readIp6 prefix) (PrefixLength length_)

    mkBgpSec ski asns spki = let 
            bgpSecSki  = SKI $ mkKI ski
            bgpSecAsns = asns
            bgpSecSpki = SPKI $ EncodedBase64 spki 
        in BGPSecPayload {..}

bigTestSlurm :: Slurm
bigTestSlurm = Slurm {
    slurmVersion = SlurmVersion 1, 
    validationOutputFilters = ValidationOutputFilters {
        prefixFilters = [
            PrefixFilter {
                asnAndPrefix = That (Ipv4P $ readIp4 "192.0.2.0/24"), 
                comment = Just "All VRPs encompassed by prefix"
            },
            PrefixFilter {
                asnAndPrefix = This (NumericASN 64496), 
                comment = Just "All VRPs matching ASN"
            },
            PrefixFilter {
                asnAndPrefix = These (NumericASN 64497) (Ipv4P $ readIp4 "198.51.100.0/24"), 
                comment = Just "All VRPs encompassed by prefix, matching ASN"
            }
        ], 
        bgpsecFilters = [
            BgpsecFilter {
                asnAndSKI = This (NumericASN 64496), 
                comment = Just "All keys for ASN"
            },
            BgpsecFilter {
                asnAndSKI = That (DecodedBase64 "foo"), 
                comment = Just "Key matching Router SKI"
            },
            BgpsecFilter {
                asnAndSKI = These (NumericASN 64497) (DecodedBase64 "bar"), 
                comment = Just "Key for ASN 64497 matching Router SKI"
            }
        ]
    }, 
    locallyAddedAssertions = LocallyAddedAssertions {
        prefixAssertions = [
            PrefixAssertion {
                asn = NumericASN 64496, 
                prefix = Ipv4P (readIp4 "198.51.100.0/24"), 
                maxPrefixLength = Nothing, 
                comment = Just "My other important route"
            },
            PrefixAssertion {
                asn = NumericASN 64496, 
                prefix = Ipv6P (readIp6 "2001:db8::/32"), 
                maxPrefixLength = Just (PrefixLength 48), 
                comment = Just "My other important de-aggregated routes"
            }
        ], 
        bgpsecAssertions = [
            BgpsecAssertion {
                asn = NumericASN 64496, 
                ski = DecodedBase64 "<some base64 SKI>", 
                routerPublicKey = DecodedBase64 "<some base64 public key>", 
                comment = Just "My known key for my important ASN"
            }
        ]
    }
}

fullJson :: IsString a => a
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
            "routerPublicKey": "#{mkBase64 "<some base64 public key>"}"
            }
        ]
        }
        }
    |]            

mkBase64 :: BS.ByteString -> BS.ByteString
mkBase64 bs = let EncodedBase64 z = encodeBase64 $ DecodedBase64 bs in z

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


asPerTA :: Vrps -> PerTA Vrps
asPerTA vrps = PerTA $ MonoidalMap.singleton (TaName "default") vrps

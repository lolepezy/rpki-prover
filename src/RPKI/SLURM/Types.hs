{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module RPKI.SLURM.Types where

import Data.Text
import Data.These

import           GHC.Generics
import           Codec.Serialise

import           Data.Aeson as JSON

import           Data.String.Interpolate.IsString

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Orphans.Json

-- NOTE: All the data here is strict

data Slurm = Slurm {
        slurmVersion :: Int,
        validationOutputFilters :: ValidationOutputFilters,
        locallyAddedAssertions :: LocallyAddedAssertions
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data ValidationOutputFilters = ValidationOutputFilters {
        prefixFilters :: [PrefixFilter],
        bgpsecFilters :: [BgpsecFilter]
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data LocallyAddedAssertions = LocallyAddedAssertions {
       prefixAssertions :: [PrefixAssertion],
       bgpsecAssertions :: [BgpsecAssertion]
    }    
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise


data PrefixFilter = PrefixFilter {
        asnAndPrefix :: These ASN IpPrefix,                
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    

data BgpsecFilter = BgpsecFilter {
        asnAndSKI :: These ASN EncodedBase64,  
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    

data PrefixAssertion = PrefixAssertion {
        asn    :: ASN,
        prefix :: IpPrefix,
        maxPrefixLength :: Maybe PrefixLength,
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise 

data BgpsecAssertion = BgpsecAssertion {
        asn    :: ASN,
        prefix :: IpPrefix,
        routerPublicKey :: EncodedBase64,
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise       


instance FromJSON Slurm
instance FromJSON ValidationOutputFilters
instance FromJSON LocallyAddedAssertions


instance FromJSON BgpsecFilter
instance FromJSON PrefixFilter
instance FromJSON BgpsecAssertion
instance FromJSON PrefixAssertion


testJson = JSON.decode [i|
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
            "SKI": "<some base64 SKI>",
            "routerPublicKey": "<some base64 public key>"
            }
        ]
        }
        }
        |]


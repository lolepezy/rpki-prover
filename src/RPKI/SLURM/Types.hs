{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module RPKI.SLURM.Types where

import Data.Text (Text)
import qualified Data.Text as Text

import Data.These

import           GHC.Generics
import           Codec.Serialise

import           Data.Aeson as Json
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

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
        comment      :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    

data BgpsecFilter = BgpsecFilter {
        asnAndSKI :: These ASN EncodedBase64,  
        comment   :: Maybe Text      
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
        asn :: ASN,
        ski :: EncodedBase64,
        routerPublicKey :: EncodedBase64,
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise       


instance FromJSON Slurm
instance FromJSON ValidationOutputFilters
instance FromJSON LocallyAddedAssertions

instance FromJSON BgpsecFilter where
    parseJSON = withObject "BgpsecFilter" $ \o -> do
        asnAndSKI <- parseOneOrBoth o "asn" "SKI"
        comment   <- o .: "comment"
        pure $ BgpsecFilter {..}

instance FromJSON PrefixFilter where
    parseJSON = withObject "PrefixFilter" $ \o -> do
        asnAndPrefix <- parseOneOrBoth o "asn" "prefix"
        comment      <- o .: "comment"
        pure $ PrefixFilter {..}

instance FromJSON BgpsecAssertion where
    parseJSON = genericParseJSON opts
        where
            opts = defaultOptions { 
                    fieldLabelModifier = \case
                        "ski" -> "SKI"
                        f     -> f
                }

instance FromJSON PrefixAssertion

parseOneOrBoth :: (FromJSON a, FromJSON b) => 
                  Object -> Text -> Text -> Parser (These a b)
parseOneOrBoth o t1 t2 = do 
    let asn = HM.lookup t1 o
    let prefix = HM.lookup t2 o
    case (asn, prefix) of
        (Nothing, Nothing)      -> fail [i|At least one of "#{t1}" or "#{t2}" must be there|]
        (Just asn, Nothing)     -> This <$> parseJSON asn 
        (Nothing, Just prefix)  -> That <$> parseJSON prefix
        (Just asn, Just prefix) -> These <$> parseJSON asn <*> parseJSON prefix


testJson :: Either String Slurm
testJson = Json.eitherDecode [i|
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


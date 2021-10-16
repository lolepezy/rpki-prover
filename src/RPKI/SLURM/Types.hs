{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.SLURM.Types where


import           Codec.Serialise

import Data.Text (Text)

import Data.These

import           Data.Aeson as Json
import           Data.Aeson.Types

import           Data.Set (Set)

import qualified Data.HashMap.Strict as HM
import           Data.String.Interpolate.IsString

import           Data.Semigroup
import           Data.Monoid.Generic


import           GHC.Generics

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Orphans.Json


-- NOTE: All the data here is strict

newtype SlurmVersion = SlurmVersion Int
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Bounded)
    deriving Semigroup via Max SlurmVersion
    deriving Monoid via Max SlurmVersion


data Slurm = Slurm {
        slurmVersion :: SlurmVersion,
        validationOutputFilters :: ValidationOutputFilters,
        locallyAddedAssertions :: LocallyAddedAssertions
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup Slurm
    deriving Monoid    via GenericMonoid Slurm

data ValidationOutputFilters = ValidationOutputFilters {
        prefixFilters :: [PrefixFilter],
        bgpsecFilters :: [BgpsecFilter]
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup ValidationOutputFilters
    deriving Monoid    via GenericMonoid ValidationOutputFilters    

data LocallyAddedAssertions = LocallyAddedAssertions {
       prefixAssertions :: [PrefixAssertion],
       bgpsecAssertions :: [BgpsecAssertion]
    }    
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup LocallyAddedAssertions
    deriving Monoid    via GenericMonoid LocallyAddedAssertions    

data PrefixFilter = PrefixFilter {
        asnAndPrefix :: These ASN IpPrefix,                
        comment      :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    

data BgpsecFilter = BgpsecFilter {
        asnAndSKI :: These ASN DecodedBase64,  
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
        ski :: DecodedBase64,
        routerPublicKey :: DecodedBase64,
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise       

instance FromJSON Slurm
instance FromJSON SlurmVersion
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
parseOneOrBoth o t1 t2 =     
    case (HM.lookup t1 o, HM.lookup t2 o) of        
        (Just a, Nothing)  -> This  <$> parseJSON a
        (Nothing, Just b)  -> That  <$> parseJSON b
        (Just a, Just b)   -> These <$> parseJSON a <*> parseJSON b
        (Nothing, Nothing) -> fail [i|At least one of "#{t1}" or "#{t2}" must be there|]


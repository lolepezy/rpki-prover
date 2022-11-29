{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.SLURM.Types where

import           Data.Text (Text)
import           Data.These
import           Data.Aeson as Json
import           Data.Aeson.Types
import           Data.Swagger

import qualified Data.HashMap.Strict as HM
import           Data.String.Interpolate.IsString

import           Data.Semigroup
import           Data.Monoid.Generic

import           Data.Word (Word32)
import           GHC.Generics

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Orphans.Json
import           RPKI.Orphans.Swagger
import           RPKI.Store.Base.Serialisation


-- NOTE: All the data here is strict

newtype SlurmVersion = SlurmVersion Int
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving Semigroup via Max SlurmVersion    

instance Monoid SlurmVersion where
    mempty = SlurmVersion 1

newtype NumericASN = NumericASN Word32
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data Slurm = Slurm {
        slurmVersion :: SlurmVersion,
        validationOutputFilters :: ValidationOutputFilters,
        locallyAddedAssertions :: LocallyAddedAssertions
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving Semigroup via GenericSemigroup Slurm
    deriving Monoid    via GenericMonoid Slurm

data ValidationOutputFilters = ValidationOutputFilters {
        prefixFilters :: [PrefixFilter],
        bgpsecFilters :: [BgpsecFilter]
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving Semigroup via GenericSemigroup ValidationOutputFilters
    deriving Monoid    via GenericMonoid ValidationOutputFilters    

data LocallyAddedAssertions = LocallyAddedAssertions {
       prefixAssertions :: [PrefixAssertion],
       bgpsecAssertions :: [BgpsecAssertion]
    }    
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary
    deriving Semigroup via GenericSemigroup LocallyAddedAssertions
    deriving Monoid    via GenericMonoid LocallyAddedAssertions    

data PrefixFilter = PrefixFilter {
        asnAndPrefix :: These NumericASN IpPrefix,                
        comment      :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary    

data BgpsecFilter = BgpsecFilter {
        asnAndSKI :: These NumericASN DecodedBase64,  
        comment   :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary    

data PrefixAssertion = PrefixAssertion {
        asn    :: NumericASN,
        prefix :: IpPrefix,
        maxPrefixLength :: Maybe PrefixLength,
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary 

data BgpsecAssertion = BgpsecAssertion {
        asn :: NumericASN,
        ski :: DecodedBase64,
        routerPublicKey :: DecodedBase64,
        comment :: Maybe Text      
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary       

instance FromJSON Slurm
instance FromJSON SlurmVersion
instance FromJSON NumericASN
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
                    Json.fieldLabelModifier = \case
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


instance ToJSON Slurm
instance ToJSON SlurmVersion
instance ToJSON NumericASN
instance ToJSON ValidationOutputFilters
instance ToJSON LocallyAddedAssertions

instance ToSchema Slurm
instance ToSchema SlurmVersion
instance ToSchema NumericASN
instance ToSchema ValidationOutputFilters
instance ToSchema LocallyAddedAssertions
instance ToSchema PrefixAssertion
instance ToSchema BgpsecAssertion
instance ToSchema BgpsecFilter
instance ToSchema PrefixFilter


instance ToJSON PrefixAssertion where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance ToJSON BgpsecAssertion where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance ToJSON BgpsecFilter where
    toJSON BgpsecFilter {..} = object $ 
           oneOrBothToJSON asnAndSKI "asn" "SKI" 
        <> jsonComment comment

instance ToJSON PrefixFilter where
    toJSON PrefixFilter {..} = object $ 
           oneOrBothToJSON asnAndPrefix "asn" "prefix" 
        <> jsonComment comment        

oneOrBothToJSON :: (ToJSON a, ToJSON b) => 
                  These a b -> Text -> Text -> [Pair]
oneOrBothToJSON these' t1 t2 =     
    case these' of 
        This a    -> [ t1 .= toJSON a ]
        That b    -> [ t2 .= toJSON b ]
        These a b -> [ t1 .= toJSON a, t2 .= toJSON b ]

jsonComment :: (KeyValue a1, ToJSON a2) => Maybe a2 -> [a1]
jsonComment Nothing = []
jsonComment (Just c) = [ "comment" .= toJSON c ]
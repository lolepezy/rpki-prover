{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleInstances          #-}

module RPKI.Http.Api where

import           Data.Int
import           Data.Proxy
import           Data.Text            (Text)
import qualified Data.ByteString.Short   as BSS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           GHC.Generics         (Generic)

import Data.Aeson as Json
import Data.Csv  (ToRecord, ToField(..), DefaultOrdered, ToNamedRecord)
import qualified Data.Csv as Csv

import Data.Hourglass.Types.Orphans

import           Data.Hex
import           Servant.API
import           Servant.API.Generic
import           Servant.CSV.Cassava

import           RPKI.Domain as Domain
import           RPKI.Errors
import           RPKI.Resources.Types
import           RPKI.Resources.IntervalSet
import           RPKI.Util (convert)
import           RPKI.Time


data CSVOptions = CSVOptions

instance EncodeOpts CSVOptions where
    encodeOpts _ = Csv.defaultEncodeOptions { Csv.encUseCrLf = False } 

type CSVType = CSV' 'HasHeader CSVOptions

type API =     
           "vrps.csv"  :> Get '[CSVType] [VRP]
      :<|> "vrps.json" :> Get '[JSON] [VRP]
      :<|>  "validation-results" :> Get '[JSON] [ValidationResult]


api :: Proxy API
api = Proxy


data ValidationResult = ValidationResult {
    problems :: ![VProblem],
    context  :: ![Text]
} deriving (Generic)

data VRP = VRP {
    asn :: !ASN,
    prefix :: !IpPrefix,
    maxLength :: !Int16
} deriving (Eq, Show, Generic)

-- CSV
instance ToRecord VRP
instance DefaultOrdered VRP
instance ToNamedRecord VRP

instance ToField ASN where
    toField (ASN as) = ("AS" :: Csv.Field) <> toField as

instance ToField IpPrefix where
    toField (Ipv4P (Ipv4Prefix p)) = convert $ show p
    toField (Ipv6P (Ipv6Prefix p)) = convert $ show p


-- JSON
instance ToJSON VRP 
instance ToJSON ASN where
    toJSON (ASN as) = toJSON $ "AS" <> show as

instance ToJSON IpPrefix where
    toJSON (Ipv4P (Ipv4Prefix p)) = toJSON $ show p
    toJSON (Ipv6P (Ipv6Prefix p)) = toJSON $ show p

instance MimeRender CSV VRP where
    mimeRender _ vrp = Csv.encode [vrp]

instance ToJSON ValidationResult
instance ToJSON VProblem
instance ToJSON VWarning
instance ToJSON AppError
instance ToJSON InitError
instance ToJSON  a => ToJSON (ParseError a)
instance ToJSON ValidationError
instance ToJSON StorageError
instance ToJSON RsyncError
instance ToJSON RrdpError
instance ToJSON TALError
instance ToJSON PrefixesAndAsns

instance ToJSON Instant where
    toJSON = toJSON . show

instance ToJSON RpkiURL where
    toJSON = toJSON . getURL

instance ToJSON SKI where
    toJSON (SKI ki) = toJSON ki

instance ToJSON AKI where
    toJSON (AKI ki) = toJSON ki

instance ToJSON Hash where
    toJSON (Hash h) = shortBsJson h

instance ToJSON KI where
    toJSON (KI bs) = shortBsJson bs
      
instance ToJSON Domain.URI where
    toJSON (Domain.URI u) = toJSON u

instance ToJSON Domain.Serial where
    toJSON (Serial s) = toJSON s

instance ToJSON Domain.EncodedBase64 where
    toJSON (EncodedBase64 bs) = toJSON bs

instance ToJSON BS.ByteString where
    toJSON = toJSON . showHex

instance ToJSON BSL.ByteString where
    toJSON = toJSON . showHex

instance ToJSON a => ToJSON (IntervalSet a) where
    toJSON = toJSON . toList
   
instance ToJSON Ipv4Prefix where
    toJSON = toJSON . show

instance ToJSON Ipv6Prefix where
    toJSON = toJSON . show

instance ToJSON AsResource where
    toJSON = toJSON . show


shortBsJson :: BSS.ShortByteString -> Json.Value
shortBsJson = toJSON . showHex . BSS.fromShort

showHex :: (Show a, Hex a) => a -> String
showHex = show . hex

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

import           GHC.Generics         (Generic)

import           Data.Aeson           (ToJSON(..))
import Data.Aeson
import Data.Csv  (ToRecord, ToField(..), DefaultOrdered, ToNamedRecord)
import qualified Data.Csv as Csv
import           Servant.API
import           Servant.API.Generic
import           Servant.CSV.Cassava

import           RPKI.Domain as Domain
import           RPKI.Errors
import           RPKI.Resources.Types
import           RPKI.Util (convert)



type API =     
           "vrps.csv"  :> Get '[CSV] [VRP]
      :<|> "vrps.json" :> Get '[JSON] [VRP]
    --   :<|>  "validation-results" :> StreamGet NewlineFraming JSON (SourceIO ValidationResult)


api :: Proxy API
api = Proxy


data ValidationResult = ValidationResult {
    context  :: ![Text],
    problems :: ![VProblem]
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
    toJSON (ASN as) = String $ "AS" <> convert (show as)

instance ToJSON IpPrefix where
    toJSON (Ipv4P (Ipv4Prefix p)) = String $ convert $ show p
    toJSON (Ipv6P (Ipv6Prefix p)) = String $ convert $ show p

instance MimeRender CSV VRP where
    mimeRender _ vrp = Csv.encode [vrp]

-- instance ToJSON ValidationResult
-- instance ToJSON VProblem
-- instance ToJSON VWarning
-- instance ToJSON AppError
-- instance ToJSON  a => ToJSON (ParseError a)
-- instance ToJSON ValidationError
-- instance ToJSON StorageError
-- instance ToJSON RsyncError
-- instance ToJSON RrdpError
-- instance ToJSON TALError
-- instance ToJSON PrefixesAndAsns

-- instance ToJSON SKI where
--     toJSON (SKI ki) = toJSON ki

-- instance ToJSON AKI where
--     toJSON (AKI ki) = toJSON ki

-- instance ToJSON Hash where
--     toJSON (Hash h) = shortBsJson h

-- instance ToJSON KI where
--     toJSON (KI bs) = shortBsJson bs
      
-- instance ToJSON Domain.URI


-- shortBsJson :: BSS.ShortByteString -> Json.Value
-- shortBsJson = String . convert . hex . BSS.fromShort

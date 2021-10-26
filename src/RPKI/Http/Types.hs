{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeOperators         #-}

module RPKI.Http.Types where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Short       as BSS
import           Data.Bifunctor

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.ByteArray              (convert)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           Data.Aeson                  hiding ((.=))
import qualified Data.Aeson                  as Json
import           Data.Csv                    (DefaultOrdered, ToField (..), ToNamedRecord, ToRecord, (.=))
import qualified Data.Csv                    as Csv
import           Data.Tuple.Strict
import           GHC.Generics                (Generic)

import qualified Crypto.PubKey.Curve25519    as C25519
import qualified Crypto.PubKey.Curve448      as C448
import           Crypto.PubKey.DSA           (Params (..), PublicKey (..))
import           Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.Ed25519       as E25519
import qualified Crypto.PubKey.Ed448         as E448
import           Crypto.PubKey.RSA.Types     (PublicKey (..))
import           Data.ASN1.BitArray
import           Data.ASN1.Types
import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexLazy
import           Data.Hourglass
import           Data.X509                   as X509

import           Servant.API
import           Servant.CSV.Cassava

import           RPKI.Domain                 as Domain
import           RPKI.RRDP.Types             (RrdpSerial)
import           RPKI.Config
import           RPKI.Orphans.Json
import           RPKI.Reporting

import           RPKI.Resources.IntervalSet
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable

import           RPKI.Store.Database
import           RPKI.Time
import qualified RPKI.Util                   as U


data ValidationResult = ValidationResult {
    problems :: [VProblem],
    context  :: [Text]
} deriving stock (Generic)

data VrpDto = VrpDto {
    asn       :: ASN,
    prefix    :: IpPrefix,
    maxLength :: PrefixLength
} deriving stock (Eq, Show, Generic)

newtype RObject = RObject (Located RpkiObject)
    deriving stock (Eq, Show, Generic)


instance ToJSON ValidationResult
instance ToJSON RObject

-- CSV
instance ToRecord VrpDto where
    toRecord VrpDto {
        asn = ASN as, 
        maxLength = PrefixLength ml,
        ..
    } = Csv.record [ "AS" <> toField as, toField prefix, toField ml ]
    
instance DefaultOrdered VrpDto

instance ToNamedRecord VrpDto where
    toNamedRecord VrpDto {
        asn = ASN as, 
        maxLength = PrefixLength ml,
        ..
    } = Csv.namedRecord [ 
            "asn"       .= ("AS" <> toField as), 
            "prefix"    .= toField prefix, 
            "maxLength" .= toField ml 
        ]
    
instance ToField IpPrefix where
    toField (Ipv4P (Ipv4Prefix p)) = toField $ show p
    toField (Ipv6P (Ipv6Prefix p)) = toField $ show p


-- JSON
instance ToJSON VrpDto 

instance MimeRender CSV VrpDto where
    mimeRender _ vrp = Csv.encode [vrp]

-- Parsing
instance FromHttpApiData Hash where    
  parseUrlPiece = parseHash

parseHash :: Text -> Either Text Hash
parseHash hashText = bimap 
    (Text.pack . ("Broken hex: " <>) . show)
    U.mkHash
    $ Hex.decode $ encodeUtf8 hashText
        

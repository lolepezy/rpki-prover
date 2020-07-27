{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module RPKI.Http.Api where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Short      as BSS

import           Data.Int
import           Data.Proxy
import           Data.Text                  (Text)

import           Data.ByteArray             (convert)
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)

import           GHC.Generics               (Generic)

import           Data.Aeson                 as Json
import           Data.Csv                   (DefaultOrdered, ToField (..), ToNamedRecord, ToRecord)
import qualified Data.Csv                   as Csv

import qualified Crypto.PubKey.Curve25519   as C25519
import qualified Crypto.PubKey.Curve448     as C448
import           Crypto.PubKey.DSA          (Params (..), PublicKey (..))
import           Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.Ed25519      as E25519
import qualified Crypto.PubKey.Ed448        as E448
import           Crypto.PubKey.RSA.Types    (PublicKey (..))
import           Data.ASN1.BitArray
import           Data.ASN1.Types
import           Data.Hex
import           Data.Hourglass
import           Data.X509                  as X509

import           Servant.API
import           Servant.CSV.Cassava

import           RPKI.Domain                as Domain
import           RPKI.Errors
import           RPKI.Resources.IntervalSet
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable

import           RPKI.Store.Database
import           RPKI.Time
import qualified RPKI.Util                  as U



data CSVOptions = CSVOptions

instance EncodeOpts CSVOptions where
    encodeOpts _ = Csv.defaultEncodeOptions { Csv.encUseCrLf = False } 

type CSVType = CSV' 'HasHeader CSVOptions

type API = "api" :> (     
                "vrps.csv"  :> Get '[CSVType] [VRP]
            :<|> "vrps.json" :> Get '[JSON] [VRP]
            :<|> "validation-results" :> Get '[JSON] [ValidationResult]
            :<|> "lmdb-stats" :> Get '[JSON] DBStats
            :<|> "object" :> Capture "hash" Hash :> Get '[JSON] (Maybe RObject)
        )


api :: Proxy API
api = Proxy


data ValidationResult = ValidationResult {
    problems :: ![VProblem],
    context  :: ![Text]
} deriving stock (Generic)

data VRP = VRP {
    asn :: !ASN,
    prefix :: !IpPrefix,
    maxLength :: !Int16
} deriving stock (Eq, Show, Generic)

newtype RObject = RObject RpkiObject
    deriving stock (Eq, Show, Generic)


-- CSV
instance ToRecord VRP
instance DefaultOrdered VRP
instance ToNamedRecord VRP

instance ToField ASN where
    toField (ASN as) = ("AS" :: Csv.Field) <> toField as

instance ToField IpPrefix where
    toField (Ipv4P (Ipv4Prefix p)) = U.convert $ show p
    toField (Ipv6P (Ipv6Prefix p)) = U.convert $ show p


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

-- TODO Maybe it's not the best thing to do 
instance ToJSON DateTime where
    toJSON = toJSON . show . Instant

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
    toJSON = toJSON . showHexL

instance ToJSON a => ToJSON (IntervalSet a) where
    toJSON = toJSON . toList
    
instance ToJSON a => ToJSON (RSet a)
   
instance ToJSON Ipv4Prefix where
    toJSON = toJSON . show

instance ToJSON Ipv6Prefix where
    toJSON = toJSON . show

instance ToJSON AsResource where
    toJSON = toJSON . show


instance ToJSON SStats
instance ToJSON RpkiObjectStats
instance ToJSON VResultStats
instance ToJSON RepositoryStats
instance ToJSON DBStats


-- RPKI Object
instance ToJSON RObject
instance ToJSON RpkiObject
instance (ToJSON a, ToJSON b) => ToJSON (With a b)
instance ToJSON a => ToJSON (CMS a)
instance ToJSON a => ToJSON (SignedObject a)
instance ToJSON a => ToJSON (SignedData a)
instance ToJSON a => ToJSON (EncapsulatedContentInfo a)

instance ToJSON Gbr
instance ToJSON Roa
instance ToJSON Manifest
instance ToJSON CertificateWithSignature
instance ToJSON ResourceCertificate
instance ToJSON (ResourceCert 'Strict_)     
instance ToJSON (ResourceCert 'Reconsidered_)
instance ToJSON (WithRFC 'Strict_ ResourceCert)
instance ToJSON (WithRFC 'Reconsidered_ ResourceCert)
instance (ToJSON s, ToJSON r) => ToJSON (WithRFC_ s r)

instance ToJSON AsResources
instance ToJSON IpResources
instance ToJSON AllResources
instance ToJSON IpResourceSet

instance ToJSON IdentityMeta
instance ToJSON SignCRL
instance ToJSON ContentType
instance ToJSON SignerInfos
instance ToJSON SignerIdentifier
instance ToJSON SignatureValue
instance ToJSON SignatureAlgorithmIdentifier
instance ToJSON SignedAttributes
instance ToJSON Attribute
instance ToJSON DigestAlgorithmIdentifiers
instance ToJSON CMSVersion

instance ToJSON X509.Certificate
instance ToJSON X509.CRL
instance ToJSON X509.RevokedCertificate
instance ToJSON a => ToJSON (X509.SignedExact a)    
instance ToJSON a => ToJSON (X509.Signed a) 
    
instance ToJSON SignatureALG

instance ToJSON Date
instance ToJSON TimeOfDay
instance ToJSON Month
instance ToJSON Hours
instance ToJSON Minutes
instance ToJSON Seconds
instance ToJSON NanoSeconds
instance ToJSON TimezoneOffset

instance ToJSON ASN1
instance ToJSON DistinguishedName
instance ToJSON PubKey
instance ToJSON PubKeyEC
instance ToJSON PubKeyALG
instance ToJSON Extensions
instance ToJSON ExtensionRaw
instance ToJSON HashALG

instance ToJSON Crypto.PubKey.RSA.Types.PublicKey
instance ToJSON Crypto.PubKey.DSA.PublicKey
instance ToJSON Crypto.PubKey.DSA.Params

instance ToJSON C25519.PublicKey where    
    toJSON = toJSON . showHex . convert

instance ToJSON E25519.PublicKey where
    toJSON = toJSON . showHex . convert

instance ToJSON C448.PublicKey where
    toJSON = toJSON . showHex . convert

instance ToJSON E448.PublicKey where
    toJSON = toJSON . showHex . convert

instance ToJSON BitArray
instance ToJSON ASN1CharacterString
instance ToJSON ASN1TimeType
instance ToJSON ASN1StringEncoding
instance ToJSON ASN1Class
instance ToJSON ASN1ConstructionType
instance ToJSON SerializedPoint
instance ToJSON Crypto.PubKey.ECC.Types.CurveName

-- Parsing
instance FromHttpApiData Hash where    
  parseUrlPiece = parseHash

parseHash :: Text -> Either Text Hash
parseHash hashText = do 
    h <- unhex $ encodeUtf8 hashText
    pure $ U.mkHash h


-- Some utilities
shortBsJson :: BSS.ShortByteString -> Json.Value
shortBsJson = toJSON . showHex . BSS.fromShort

showHex :: BS.ByteString -> Text
showHex = decodeUtf8 . hex

showHexL :: BSL.ByteString -> Text
showHexL = decodeUtf8 . BSL.toStrict . hex

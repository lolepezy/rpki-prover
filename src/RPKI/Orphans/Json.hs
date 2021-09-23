{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module RPKI.Orphans.Json where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Short       as BSS

import           Data.Text                   (Text)
import qualified Data.Text                   as Text

import           Data.ByteArray              (convert)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)

import           Data.Aeson                  hiding ((.=))
import qualified Data.Aeson                  as Json
import           Data.Csv                    (DefaultOrdered, ToField (..), ToNamedRecord, ToRecord, (.=))
import qualified Data.Csv                    as Csv
import           Data.Tuple.Strict

import           Data.String.Interpolate.IsString

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


import HaskellWorks.Data.Network.Ip.Ip as Ips

import Data.ASN1.Types

import Data.These

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson as Json

import           RPKI.Orphans.Generics

import           RPKI.Domain                 as Domain
import           RPKI.RRDP.Types             (RrdpSerial)
import           RPKI.Config

import           RPKI.Reporting
import           RPKI.Resources.IntervalSet
import           RPKI.Resources.Types
import           RPKI.Store.Types
import           RPKI.Store.Base.Storable

import           RPKI.Store.Types
import           RPKI.Time
import qualified RPKI.Util                   as U


instance ToJSON ASN where
    toJSON (ASN as) = toJSON $ "AS" <> show as

instance ToJSON IpPrefix where
    toJSON (Ipv4P (Ipv4Prefix p)) = toJSON $ show p
    toJSON (Ipv6P (Ipv6Prefix p)) = toJSON $ show p

instance ToJSON VProblem
instance ToJSON VWarning
instance ToJSON AppError
instance ToJSON InitError
instance ToJSON  a => ToJSON (ParseError a)
instance ToJSON ValidationError
instance ToJSON Locations
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

instance ToJSON BSS.ShortByteString where
    toJSON = toJSON . showHex . BSS.fromShort

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

instance ToJSON Size where 
    toJSON (Size s) = toJSON s
    
instance ToJSON SStats
instance ToJSON RpkiObjectStats
instance ToJSON VResultStats
instance ToJSON RepositoryStats
instance ToJSON DBStats
instance ToJSON TotalDBStats
instance ToJSON AppMetric
instance ToJSON a => ToJSON (MetricMap a)
instance ToJSON ValidationMetric
instance ToJSON RsyncMetric
instance ToJSON RrdpMetric
instance ToJSON PathKind
instance ToJSON FetchFreshness
instance ToJSON HttpStatus where
    toJSON (HttpStatus s) = toJSON s
    
instance ToJSON RrdpSource
instance ToJSONKey (Path 'Metric)
instance ToJSON (Path 'Metric)
instance ToJSON TimeMs where 
    toJSON (TimeMs s) = toJSON $ show s <> "ms"

instance ToJSON Count where
    toJSON (Count s) = toJSON s

-- RPKI Object
instance ToJSON EECerObject
instance ToJSON CerObject
instance ToJSON CrlObject
instance ToJSON RpkiObject
instance ToJSON a => ToJSON (Located a)
instance ToJSON a => ToJSON (CMSBasedObject a)
instance ToJSON a => ToJSON (CMS a)
instance ToJSON a => ToJSON (SignedObject a)
instance ToJSON a => ToJSON (SignedData a)
instance ToJSON a => ToJSON (EncapsulatedContentInfo a)

instance ToJSON PrefixLength
instance ToJSON Gbr where
    toJSON (Gbr s) = toJSON $ show s

instance ToJSON Vrp
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

instance ToJSON SessionId where
    toJSON (SessionId s) = shortBsJson s

instance ToJSON RrdpSerial

instance (ToJSON a, ToJSON b) => ToJSON (T2 a b) where
    toJSON (T2 a b) = toJSON (a, b)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (T3 a b c) where
    toJSON (T3 a b c) = toJSON (a, b, c)    


-- Some utilities
shortBsJson :: BSS.ShortByteString -> Json.Value
shortBsJson = toJSON . showHex . BSS.fromShort

showHex :: BS.ByteString -> Text
showHex = decodeUtf8 . Hex.encode

showHexL :: BSL.ByteString -> Text
showHexL = decodeUtf8 . BSL.toStrict . HexLazy.encode


-- FromJSON
instance FromJSON ASN
instance FromJSON PrefixLength

instance FromJSON IpPrefix where
    parseJSON = withText "IpPrefix" $ \s ->           
        case Ips.canonicalise (read (U.convert s)) of
            Nothing -> fail [i|Prefix #{s} is not canonical|]
            Just cb -> pure $ case cb of 
                        IpBlockV4 b -> Ipv4P $ Ipv4Prefix b
                        IpBlockV6 b -> Ipv6P $ Ipv6Prefix b                    


instance FromJSON EncodedBase64 where
    parseJSON = withText "EncodedBase64" $ pure . EncodedBase64 . U.convert

instance FromJSON DecodedBase64 where
    parseJSON = withText "DecodedBase64" $ \s -> do 
        let encoded = EncodedBase64 $ U.convert s
        case U.decodeBase64 encoded s of 
            Left e        -> fail [i|Broken base64: #{e}|]
            Right decoded -> pure decoded        


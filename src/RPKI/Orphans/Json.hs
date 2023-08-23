{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module RPKI.Orphans.Json where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as BSS

import           Data.Text                   (Text)
import qualified Data.List                   as List
import           Data.ByteArray              (convert)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Foldable               (toList)

import           Data.Aeson                  
import qualified Data.Aeson                  as Json
import           Data.Aeson.Types            (toJSONKeyText)
import           Data.Aeson.TH
import           Data.Tuple.Strict
import           Deque.Strict                as Deq

import           Data.String.Interpolate.IsString

import qualified Crypto.PubKey.Curve25519    as C25519
import qualified Crypto.PubKey.Curve448      as C448
import           Crypto.PubKey.DSA           
import           Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.Ed25519       as E25519
import qualified Crypto.PubKey.Ed448         as E448
import           Crypto.PubKey.RSA.Types     
import           Data.ASN1.BitArray
import           Data.ASN1.Types
import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexLazy
import           Data.Hourglass
import           Data.X509                   as X509

import HaskellWorks.Data.Network.Ip.Ip as Ips

import           RPKI.AppTypes
import           RPKI.Domain                 as Domain
import           RPKI.RRDP.Types             (RrdpSerial)
import           RPKI.Config

import           RPKI.Logging
import           RPKI.Reporting
import           RPKI.Metrics.Metrics
import           RPKI.Metrics.System
import qualified RPKI.Resources.IntervalSet as IS
import           RPKI.Resources.Types
import           RPKI.RTR.Types
import           RPKI.RTR.Protocol
import           RPKI.Store.Base.Storable

import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import qualified RPKI.Util                   as U

instance ToJSON ASN where
    toJSON (ASN as) = toJSON $ "AS" <> show as

instance ToJSON IpPrefix where
    toJSON (Ipv4P (Ipv4Prefix p)) = toJSON $ show p
    toJSON (Ipv6P (Ipv6Prefix p)) = toJSON $ show p

instance ToJSON WorldVersion where
    toJSON (WorldVersion v) = toJSON $ show $ toInteger v

instance (ToJSON a, ToJSON b) => ToJSON (T2 a b) where
    toJSON (T2 a b) = toJSON (a, b)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (T3 a b c) where
    toJSON (T3 a b c) = toJSON (a, b, c)    

-- Some utilities
showHex :: BS.ByteString -> Text
showHex = decodeUtf8 . Hex.encode

showHexL :: LBS.ByteString -> Text
showHexL = decodeUtf8 . LBS.toStrict . HexLazy.encode

shortBsJson :: BSS.ShortByteString -> Json.Value
shortBsJson = toJSON . showHex . BSS.fromShort

oid2text :: OID -> String
oid2text oid = List.intercalate "." $ map show oid

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

instance ToJSON Domain.RsyncURL where
    toJSON = toJSON . Domain.getURL

instance ToJSON Domain.Serial where
    toJSON (Serial s) = toJSON $ show s

instance ToJSON Domain.EncodedBase64 where
    toJSON (EncodedBase64 bs) = toJSON (U.convert bs :: Text)

instance ToJSON BS.ByteString where
    toJSON = toJSON . showHex

instance ToJSON BSS.ShortByteString where
    toJSON = toJSON . showHex . BSS.fromShort

instance ToJSON LBS.ByteString where
    toJSON = toJSON . showHexL

instance ToJSON a => ToJSON (IntervalSet a) where
    toJSON = toJSON . IS.toList
    
instance ToJSON a => ToJSON (RSet a)
   
instance ToJSON Ipv4Prefix where
    toJSON (Ipv4Prefix p) = toJSON $ show p

instance ToJSON Ipv6Prefix where
    toJSON (Ipv6Prefix p) = toJSON $ show p

instance ToJSON AsResource where
    toJSON = \case 
        AS a        -> toJSON a 
        ASRange a b -> toJSON $ show a <> "-" <> show b

instance ToJSON AddrFamily where
    toJSON = \case 
        Ipv4F -> toJSON ("ipv4" :: Text)
        Ipv6F -> toJSON ("ipv6" :: Text)

instance ToJSON PrefixesAndAsns

instance ToJSON Size where 
    toJSON (Size s) = toJSON s

instance ToJSON TaName where 
    toJSON (TaName t) = toJSON t

instance ToJSON Count where
    toJSON (Count s) = toJSON s

instance ToJSON Focus
instance ToJSONKey (Scope 'Metric)
instance ToJSON (Scope 'Metric)
instance ToJSON a => ToJSON (MetricMap a)

instance ToJSON TimeMs where 
    toJSON (TimeMs s) = toJSON s
    
instance ToJSON CPUTime where 
    toJSON (CPUTime s) = toJSON s

instance ToJSON MaxMemory where 
    toJSON (MaxMemory s) = toJSON s

instance ToJSON HttpStatus where
    toJSON (HttpStatus s) = toJSON s
    
$(deriveToJSON defaultOptions ''RrdpSource)

instance ToJSONKey TaName where
    toJSONKey = toJSONKeyText unTaName

instance ToJSONKey RpkiURL where
    toJSONKey = toJSONKeyText $ unURI . getURL

$(deriveToJSON defaultOptions ''ValidationMetric)

instance ToJSON a => ToJSON (GroupedValidationMetric a)

$(deriveToJSON defaultOptions ''FetchFreshness)
$(deriveToJSON defaultOptions ''RsyncMetric)
$(deriveToJSON defaultOptions ''RrdpMetric)
$(deriveToJSON defaultOptions ''ResourceUsage)
$(deriveToJSON defaultOptions ''SystemMetrics)
$(deriveToJSON defaultOptions ''ScopeKind)

$(deriveToJSON defaultOptions ''SStats)
$(deriveToJSON defaultOptions ''RpkiObjectStats)
$(deriveToJSON defaultOptions ''VResultStats)
$(deriveToJSON defaultOptions ''RepositoryStats)
$(deriveToJSON defaultOptions ''DBStats)
$(deriveToJSON defaultOptions ''DBFileStats)
$(deriveToJSON defaultOptions ''TotalDBStats)
$(deriveToJSON defaultOptions ''VrpCounts)
$(deriveToJSON defaultOptions ''RawMetric)
    

$(deriveToJSON defaultOptions ''PrefixLength)

instance ToJSON Gbr where
    toJSON (Gbr s) = toJSON $ show s

instance ToJSON r => ToJSON (PolyRFC r rfc)
instance ToJSON r => ToJSON (SomeRFC r)

$(deriveToJSON defaultOptions ''IpResourceSet)
$(deriveToJSON defaultOptions ''AsResources)
$(deriveToJSON defaultOptions ''IpResources)
$(deriveToJSON defaultOptions ''AllResources)

$(deriveToJSON defaultOptions ''Month)
$(deriveToJSON defaultOptions ''Hours)
$(deriveToJSON defaultOptions ''Minutes)
$(deriveToJSON defaultOptions ''Seconds)
$(deriveToJSON defaultOptions ''NanoSeconds)
$(deriveToJSON defaultOptions ''TimezoneOffset)
$(deriveToJSON defaultOptions ''TimeOfDay)
$(deriveToJSON defaultOptions ''Date)

$(deriveToJSON defaultOptions ''BitArray)
$(deriveToJSON defaultOptions ''ASN1StringEncoding)
$(deriveToJSON defaultOptions ''ASN1CharacterString)
$(deriveToJSON defaultOptions ''ASN1TimeType)
$(deriveToJSON defaultOptions ''ASN1Class)
$(deriveToJSON defaultOptions ''ASN1ConstructionType)
$(deriveToJSON defaultOptions ''SerializedPoint)
$(deriveToJSON defaultOptions ''Crypto.PubKey.ECC.Types.CurveName)
$(deriveToJSON defaultOptions ''ASN1)
$(deriveToJSON defaultOptions ''DistinguishedName)

$(deriveToJSON defaultOptions ''Crypto.PubKey.DSA.Params)
$(deriveToJSON defaultOptions ''Crypto.PubKey.RSA.Types.PublicKey)
$(deriveToJSON defaultOptions ''Crypto.PubKey.DSA.PublicKey)

instance ToJSON C25519.PublicKey where    
    toJSON = toJSON . showHex . convert

instance ToJSON E25519.PublicKey where
    toJSON = toJSON . showHex . convert

instance ToJSON C448.PublicKey where
    toJSON = toJSON . showHex . convert

instance ToJSON E448.PublicKey where
    toJSON = toJSON . showHex . convert

instance ToJSON ContentType where
    toJSON (ContentType oid) = toJSON $ oid2text oid

$(deriveToJSON defaultOptions ''PubKeyEC)
$(deriveToJSON defaultOptions ''PubKeyALG)
$(deriveToJSON defaultOptions ''ExtensionRaw)
$(deriveToJSON defaultOptions ''Extensions)
$(deriveToJSON defaultOptions ''HashALG)
$(deriveToJSON defaultOptions ''SignatureALG)

instance ToJSON SignatureAlgorithmIdentifier where
    toJSON (SignatureAlgorithmIdentifier sai) = 
        case sai of 
            X509.SignatureALG_Unknown oid -> toJSON $ oid2text oid
            _ -> toJSON $ show sai

$(deriveToJSON defaultOptions ''PubKey)
$(deriveToJSON defaultOptions ''Version)
$(deriveToJSON defaultOptions ''CMSVersion)
$(deriveToJSON defaultOptions ''SignerIdentifier)
$(deriveToJSON defaultOptions ''SignatureValue)

instance ToJSON Attribute where
    toJSON = \case
        ContentTypeAttr x -> Json.object [ 
                "type" .= ("ContentType" :: Text),
                "value" .= x 
            ]
        MessageDigest b -> Json.object [
                "type" .= ("MessageDigest" :: Text),
                "value" .= b
            ]            
        SigningTime dt tz -> Json.object [
                "type" .= ("SigningTime" :: Text),
                "value" .= (instantDateFormat $ Instant dt)                
            ]
        BinarySigningTime bst -> Json.object [
                "type" .= ("BinarySigningTime" :: Text),
                "value" .= bst 
            ]
        UnknownAttribute oid vals -> Json.object [
                "type" .= ("UnknownAttribute" :: Text),
                "oid" .= oid,
                "asn1" .= vals
            ]

instance ToJSON SignedAttributes where
    toJSON (SignedAttributes attrs _) = toJSON $ map toJSON attrs

instance ToJSON DigestAlgorithmIdentifier where
    toJSON (DigestAlgorithmIdentifier oid) = toJSON $ oid2text oid

instance ToJSON DigestAlgorithmIdentifiers where
    toJSON (DigestAlgorithmIdentifiers oids) = toJSON $ map oid2text oids

$(deriveToJSON defaultOptions ''SignerInfos)
$(deriveToJSON defaultOptions ''SignCRL)

$(deriveToJSON defaultOptions ''X509.Certificate)
$(deriveToJSON defaultOptions ''X509.RevokedCertificate)
$(deriveToJSON defaultOptions ''X509.CRL)

$(deriveToJSON defaultOptions ''Aspa)
$(deriveToJSON defaultOptions ''RSC)
$(deriveToJSON defaultOptions ''Vrp)
$(deriveToJSON defaultOptions ''Manifest)
$(deriveToJSON defaultOptions ''CertificateWithSignature)
$(deriveToJSON defaultOptions ''RawResourceCertificate)
$(deriveToJSON defaultOptions ''ResourceCertificate)

-- RPKI Object
instance ToJSON a => ToJSON (TypedCert a t)

$(deriveToJSON defaultOptions ''EECerObject)

-- $(deriveToJSON defaultOptions ''CaCerObject)
-- $(deriveToJSON defaultOptions ''BgpCerObject)
-- $(deriveToJSON defaultOptions ''CrlObject)
-- $(deriveToJSON defaultOptions ''RpkiObject)

instance ToJSON a => ToJSON (X509.SignedExact a)    
instance ToJSON a => ToJSON (X509.Signed a) 

instance ToJSON Locations where
    toJSON = toJSON . locationsToList
instance ToJSON a => ToJSON (Located a)
instance ToJSON a => ToJSON (CMSBasedObject a)
instance ToJSON a => ToJSON (CMS a)
instance ToJSON a => ToJSON (SignedObject a)
instance ToJSON a => ToJSON (SignedData a)
instance ToJSON a => ToJSON (EncapsulatedContentInfo a)

instance ToJSON SPKI

instance ToJSON SessionId where
    toJSON (SessionId s) = toJSON s

instance ToJSON RrdpSerial
instance ToJSON RtrState
instance ToJSON BGPSecPayload
instance ToJSON SerialNumber
instance ToJSON RtrSessionId

instance ToJSON AscOrderedVrp where
    toJSON (AscOrderedVrp v) = toJSON v
instance ToJSON a => ToJSON (Deq.Deque a) where
    toJSON = toJSON . toList

instance (ToJSON a, ToJSON b) => ToJSON (GenDiffs a b)
instance ToJSON a => ToJSON (Diff a)


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

instance ToJSON DecodedBase64 where
    toJSON = toJSON . U.encodeBase64

instance ToJSON LogLevel
instance ToJSON Parallelism
instance ToJSON ManifestProcessing
instance ToJSON TAL
-- $(deriveToJSON defaultOptions ''TAL)
instance ToJSON HttpApiConfig
instance ToJSON ValidationConfig
instance ToJSON AsyncFetchConfig
instance ToJSON RtrConfig
instance ToJSON SystemConfig
instance ToJSON RrdpConf
instance ToJSON RsyncConf    
instance ToJSON Config

instance ToJSON VersionKind
instance ToJSON VIssue
instance ToJSON VWarning
instance ToJSON AppError
instance ToJSON InitError
instance ToJSON InternalError
instance ToJSON SlurmError
instance ToJSON a => ToJSON (ParseError a)
instance ToJSON ValidationError
instance ToJSON StorageError
instance ToJSON RsyncError
instance ToJSON RrdpError
instance ToJSON TALError
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}


module RPKI.Orphans.Swagger where

import           Data.Text                   (Text)
import           Data.These

import           Data.Aeson                  hiding ((.=))

import           Data.Proxy
import           Data.Swagger hiding (ValidationError)

import           Data.Map.Strict (Map)
import           Data.Map.Monoidal.Strict (MonoidalMap)
import           Data.Tuple.Strict
import           Deque.Strict                as Deq
 
import           Data.ASN1.Types
import           Data.Hourglass
import           Data.X509                   as X509

import           RPKI.AppTypes
import           RPKI.Domain                 as Domain
import           RPKI.RRDP.Types             (RrdpSerial)
import           RPKI.Config
import           RPKI.Logging

import           RPKI.Reporting
import           RPKI.Metrics.Metrics
import           RPKI.Metrics.System
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable

import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Orphans.Json
import           RPKI.Orphans.Generics

import           RPKI.RTR.Types
import           RPKI.RTR.Protocol

-- ToSchema insrances for Swagger doc generation
instance ToSchema ArtificialKey
instance ToSchema ObjectKey
instance ToSchema Focus
instance ToSchema RpkiURL
instance ToSchema RsyncURL where
     declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema RrdpURL
instance ToSchema URI where
     declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema WorldVersion     
instance ToSchema VersionKind
instance ToSchema Instant where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema DateTime     
instance ToSchema Date     
instance ToSchema TimeOfDay     
instance ToSchema Month     
instance ToSchema Hours     
instance ToSchema Minutes     
instance ToSchema Seconds     
instance ToSchema NanoSeconds    
instance ToSchema TimezoneOffset

instance ToSchema ASN
instance ToSchema IpPrefix
instance ToSchema PrefixLength
instance ToSchema AddrFamily 
instance ToSchema Ipv4Prefix where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema Ipv6Prefix where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema DecodedBase64 where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema SPKI where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance (ToSchema a, ToSchema b) => ToSchema (These a b)
instance (ToSchema a, ToJSONKey a, ToSchema b) => ToSchema (MonoidalMap a b) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (Map a b))

instance ToSchema a => ToSchema (GroupedValidationMetric a)

instance ToSchema SessionId
instance ToSchema Serial
instance ToSchema RrdpSerial
instance ToSchema TaName where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema a => ToSchema (MetricMap a)
instance ToSchema ValidationMetric
instance ToSchema RsyncMetric
instance ToSchema RrdpMetric
instance ToSchema ResourceUsage
instance ToSchema SystemMetrics
instance ToSchema ScopeKind
instance ToSchema FetchFreshness
instance ToSchema HttpStatus
instance ToSchema RrdpSource

instance ToSchema (Scope 'Metric)
instance ToSchema Count where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Integer)
instance ToSchema TimeMs where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Integer)
instance ToSchema CPUTime where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Integer)
instance ToSchema MaxMemory where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Integer)
instance ToSchema Size where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Integer)

instance ToSchema DBFileStats
instance ToSchema StorageStats
instance ToSchema TotalDBStats
instance ToSchema SStats

instance ToSchema Config
instance ToSchema Parallelism
instance ToSchema RsyncConf
instance ToSchema RrdpConf
instance ToSchema ValidationConfig
instance ToSchema AsyncFetchConfig
instance ToSchema SystemConfig
instance ToSchema HttpApiConfig
instance ToSchema RtrConfig
instance ToSchema LogLevel
instance ToSchema ManifestProcessing
instance ToSchema ValidationAlgorithm
instance ToSchema FetchTimingCalculation

instance ToSchema a => ToSchema (Located a)

instance ToSchema Locations where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)


instance ToSchema RpkiObject
instance ToSchema CaCerObject
instance ToSchema BgpCerObject
instance ToSchema CrlObject
instance ToSchema EECerObject

instance ToSchema a => ToSchema (CMSBasedObject a)
instance ToSchema a => ToSchema (CMS a)
instance ToSchema a => ToSchema (SignedObject a)
instance ToSchema a => ToSchema (SignedData a)
instance ToSchema a => ToSchema (EncapsulatedContentInfo a)

instance ToSchema Vrp
instance ToSchema SplPayload
instance ToSchema Gbr where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema Rsc
instance ToSchema Aspa
instance ToSchema MftPair
instance ToSchema Manifest
instance ToSchema CertificateWithSignature  where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema ResourceCertificate
instance ToSchema RawResourceCertificate
instance ToSchema r => ToSchema (PolyRFC r rfc)
instance ToSchema r => ToSchema (SomeRFC r)
instance ToSchema r => ToSchema (TypedCert r t)

instance ToSchema Hash where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema SKI
instance ToSchema AKI
instance ToSchema KI where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema VIssue
instance ToSchema VWarning
instance ToSchema AppError
instance ToSchema InitError
instance ToSchema InternalError
instance ToSchema SlurmError
instance ToSchema a => ToSchema (ParseError a)
instance ToSchema ValidationError  where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema StorageError
instance ToSchema RsyncError
instance ToSchema RrdpError where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema TALError
instance ToSchema PrefixesAndAsns


instance ToSchema AsResources
instance ToSchema IpResources
instance ToSchema AllResources
instance ToSchema IpResourceSet

instance ToSchema SignCRL where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema ContentType
instance ToSchema SignerInfos
instance ToSchema SignerIdentifier where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema SignatureValue where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema SignatureAlgorithmIdentifier
instance ToSchema SignedAttributes where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema Attribute where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema DigestAlgorithmIdentifier
instance ToSchema DigestAlgorithmIdentifiers
instance ToSchema Version
instance ToSchema CMSVersion

instance ToSchema X509.Certificate
instance ToSchema X509.CRL
instance ToSchema X509.RevokedCertificate
instance ToSchema a => ToSchema (X509.SignedExact a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema a => ToSchema (X509.Signed a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
    
instance ToSchema SignatureALG

instance ToSchema ASN1 where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema DistinguishedName where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema PubKey where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema PubKeyEC where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema PubKeyALG where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema Extensions where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema ExtensionRaw where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema HashALG

instance ToSchema AsResource

instance (ToSchema a, ToSchema b) => ToSchema (T2 a b)
instance (ToSchema a, ToSchema b, ToSchema c) => ToSchema (T3 a b c)

instance ToSchema a => ToSchema (IntervalSet a)
instance ToSchema a => ToSchema (RSet a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)


instance ToSchema RtrState
instance ToSchema BGPSecPayload
instance ToSchema SerialNumber
instance ToSchema RtrSessionId
instance ToSchema AscOrderedVrp where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Vrp)
instance ToSchema a => ToSchema (Deq.Deque a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance (ToSchema a, ToSchema b) => ToSchema (GenDiffs a b)
instance ToSchema a => ToSchema (Diff a)    
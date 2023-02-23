{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}

module RPKI.Http.Types where

import           Control.Lens hiding ((.=))

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Base16      as Hex
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (encodeUtf8)

import           Data.Aeson.Types
import           Data.Proxy

import           GHC.Generics                (Generic)
import qualified Data.Vector                 as V
import qualified Data.List.NonEmpty          as NonEmpty
import qualified Data.Map.Strict             as Map

import           Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap

import           Servant.API
import           Data.Swagger hiding (url)
import           Network.HTTP.Media ((//))

import           Data.ASN1.OID

import           RPKI.Config
import           RPKI.AppTypes
import           RPKI.Repository
import           RPKI.Domain
import           RPKI.Metrics.Metrics
import           RPKI.Orphans.Json
import           RPKI.Orphans.Swagger
import           RPKI.Reporting

import           RPKI.Resources.Types
import           RPKI.RTR.Types
import           RPKI.Time
import           RPKI.Util (mkHash)


data ValidationsDto a = ValidationsDto {
        worldVersion :: WorldVersion,
        timestamp    :: Instant,
        validations  :: [a]
    } 
    deriving stock (Eq, Show, Generic)

data IssueDto = ErrorDto Text | WarningDto Text
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, ToSchema)

data FullVDto = FullVDto {
        issues  :: [IssueDto],
        path    :: [Focus],
        url     :: Focus
    } 
    deriving stock (Eq, Show, Generic)

newtype MinimalVDto = MinimalVDto FullVDto
    deriving stock (Eq, Show, Generic)

data VrpDto = VrpDto {
        asn       :: ASN,
        prefix    :: IpPrefix,
        maxLength :: PrefixLength,
        ta        :: Text
    } 
    deriving stock (Eq, Show, Generic)

data VrpMinimalDto = VrpMinimalDto {
        asn       :: ASN,
        prefix    :: IpPrefix,
        maxLength :: PrefixLength        
    } 
    deriving stock (Eq, Show, Generic)

data ProviderAsn = ProviderAsn {
        asn      :: ASN, 
        afiLimit :: Maybe AddrFamily
    }
    deriving stock (Eq, Show, Generic)

data AspaDto = AspaDto {
        customerAsn  :: ASN,
        providerAsns :: [ProviderAsn]        
    } 
    deriving stock (Eq, Show, Generic)


data BgpCertDto = BgpCertDto {
        ski  :: SKI,
        asns :: [ASN],
        subjectPublicKeyInfo :: SPKI
    } 
    deriving stock (Eq, Show, Generic)

newtype RObject = RObject (Located ObjectDto)
    deriving stock (Eq, Show, Generic)


data ObjectDto = CertificateD (ObjectContentDto CertificateDto)
                | ManifestD (ObjectContentDto (CMSObjectDto ManifestDto))
                | CRLD (ObjectContentDto CrlDto)
                | BGPSecD (ObjectContentDto BgpCertDto)                
                | ROAD (ObjectContentDto (CMSObjectDto RoaDto))
                | ASPAD (ObjectContentDto (CMSObjectDto AspaDto))
                | GBRD (ObjectContentDto (CMSObjectDto GrbDto))
                | RSCD (ObjectContentDto (CMSObjectDto RscDto))
    deriving stock (Eq, Show, Generic)

data ObjectContentDto payload = ObjectContentDto {
        hash :: Hash,
        ski  :: Maybe SKI,
        aki  :: Maybe AKI,
        eeCertificate :: Maybe CertificateDto,          
        payload       :: payload
    }
    deriving stock (Eq, Show, Generic)


data CMSObjectDto cmsPayload = CMSObjectDto {
        cmsVersion         :: CMSVersion,
        signedInfoVersion  :: CMSVersion,
        contentType        :: ContentType,        
        encapsulatedContentType :: ContentType,        
        digestAlgorithms   :: DigestAlgorithmIdentifiers,
        signatureAlgorithm :: SignatureAlgorithmIdentifier,
        signerIdentifier   :: SignerIdentifier,        
        signature          :: SignatureValue,
        signedAttributes   :: SignedAttributes,
        cmsPayload         :: cmsPayload
    }
    deriving stock (Eq, Show, Generic)

data ManifestDto = ManifestDto {
        mftNumber   :: Serial, 
        fileHashAlg :: Text, 
        thisTime    :: Instant, 
        nextTime    :: Instant, 
        entries     :: [(Text, Hash)]
    } 
    deriving stock (Show, Eq, Generic)


data CertificateDto = CertificateDto {
        certVersion      :: Version,
        certSerial       :: Serial,
        certSignatureAlg :: Text,
        certIssuerDN     :: Text,
        certSubjectDN    :: Text,
        notValidBefore   :: Instant,
        notValidAfter    :: Instant,        
        pubKey           :: Either Text PubKeyDto,
        ipv4             :: IntervalSet Ipv4Prefix,        
        ipv6             :: IntervalSet Ipv6Prefix,        
        asn              :: IntervalSet AsResource,
        extensions       :: ExtensionsDto
    }
    deriving stock (Eq, Show, Generic)

data PubKeyDto = PubKeyDto {
        pubKeySize :: Int,
        pubKeyPQ   :: Integer,
        pubKeyExp  :: Integer
    }
    deriving stock (Eq, Show, Generic)

newtype OIDDto = OIDDto OID 
    deriving stock (Eq, Show, Generic)

data ExtensionDto = ExtensionDto {
        oid      :: OIDDto,
        bytes    :: BS.ByteString,
        critical :: Bool,
        value    :: Text
    }
    deriving stock (Eq, Show, Generic)

newtype ExtensionsDto = ExtensionsDto [ExtensionDto]
    deriving stock (Eq, Show, Generic)

data CrlDto = CrlDto {
        thisUpdateTime     :: Instant,
        nextUpdateTime     :: Maybe Instant,
        signatureAlgorithm :: SignatureAlgorithmIdentifier,
        signatureValue     :: SignatureValue,
        crlNumber          :: Serial,
        revokedSerials     :: [Serial]        
    }
    deriving stock (Eq, Show, Generic)


data RoaDto = RoaDto {
        asn      :: ASN,
        prefixes :: [RoaPrefixDto]
    }  
    deriving stock (Eq, Show, Generic)

data RoaPrefixDto = RoaPrefixDto {
        prefix    :: IpPrefix,
        maxLength :: PrefixLength        
    }
    deriving stock (Eq, Show, Generic)


data GrbDto = GrbDto {

    }  
    deriving stock (Eq, Show, Generic)

data RscDto = RscDto {

    }  
    deriving stock (Eq, Show, Generic)



data MetricsDto = MetricsDto {
        groupedValidations :: GroupedValidationMetric ValidationMetric,      
        rsync              :: MonoidalMap (DtoScope 'Metric) RsyncMetric,
        rrdp               :: MonoidalMap (DtoScope 'Metric) RrdpMetric
    } 
    deriving stock (Eq, Show, Generic)

data PublicationPointDto = PublicationPointDto {
        rrdp :: [(RrdpURL, RrdpRepository)]
        -- TODO Add rsync tree here
    } 
    deriving stock (Eq, Show, Generic)

newtype JobsDto = JobsDto {
        jobs :: [(Text, Instant)]
    } 
    deriving stock (Eq, Show, Generic)

data ResourcesDto = ResourcesDto {
        tag                 :: Text,
        aggregatedCpuTime   :: CPUTime,
        maxMemory           :: MaxMemory,        
        avgCpuTimeMsPerSecond :: Double
    }
    deriving stock (Eq, Show, Generic)

data SystemDto = SystemDto {
        proverVersion :: Text,
        config        :: Config,
        resources     :: [ResourcesDto]
    }
    deriving stock (Eq, Show, Generic)

newtype RtrDto = RtrDto {
        rtrState :: RtrState
    }
    deriving stock (Eq, Show, Generic)

data ManualCVS = ManualCVS

newtype RawCSV = RawCSV { unRawCSV :: LBS.ByteString }
    deriving stock (Eq, Show, Generic)

instance Accept ManualCVS where
    contentType _ = "text" // "csv"

instance MimeRender ManualCVS RawCSV where
    mimeRender _ = unRawCSV    

instance ToSchema RawCSV where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToJSON RObject
instance ToJSON ObjectDto where
    toJSON = \case
        CertificateD v -> object ["type" .= ("certificate" :: Text), "value" .= toJSON v]
        ManifestD v    -> object ["type" .= ("manifest" :: Text), "value" .= toJSON v]
        CRLD v         -> object ["type" .= ("CRL" :: Text), "value" .= toJSON v]
        BGPSecD v      -> object ["type" .= ("BGPSec" :: Text), "value" .= toJSON v]
        ROAD v  -> object ["type" .= ("ROA" :: Text), "value" .= toJSON v]
        ASPAD v -> object ["type" .= ("ASPA" :: Text), "value" .= toJSON v]
        GBRD v  -> object ["type" .= ("GBR" :: Text), "value" .= toJSON v]
        RSCD v  -> object ["type" .= ("RSC" :: Text), "value" .= toJSON v]


instance ToJSON a => ToJSON (ObjectContentDto a) where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance ToJSON a => ToJSON (CMSObjectDto a)
instance ToJSON CertificateDto
instance ToJSON PubKeyDto
instance ToJSON ExtensionDto where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }
instance ToJSON ExtensionsDto
instance ToJSON OIDDto where
    toJSON (OIDDto oid) = toJSON $ oid2text oid

instance ToJSON ManifestDto
instance ToJSON CrlDto
instance ToJSON RoaDto
instance ToJSON RoaPrefixDto
instance ToJSON GrbDto
instance ToJSON RscDto
instance ToJSON VrpDto     
instance ToJSON VrpMinimalDto     
instance ToJSON AspaDto
instance ToJSON BgpCertDto
instance ToJSON RtrDto

instance ToSchema RObject
instance ToSchema VrpDto where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema VrpMinimalDto where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema ObjectDto
instance ToSchema a => ToSchema (ObjectContentDto a)
instance ToSchema a => ToSchema (CMSObjectDto a)
instance ToSchema CertificateDto
instance ToSchema PubKeyDto
instance ToSchema ExtensionDto where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema ExtensionsDto
instance ToSchema OIDDto
instance ToSchema ManifestDto
instance ToSchema CrlDto
instance ToSchema RoaDto
instance ToSchema RoaPrefixDto
instance ToSchema GrbDto
instance ToSchema RscDto
instance ToSchema AspaDto
instance ToSchema BgpCertDto
instance ToSchema RtrDto
instance ToSchema ProviderAsn

instance ToJSON ProviderAsn where
    toJSON = genericToJSON $ defaultOptions { omitNothingFields = True } 

instance ToJSON JobsDto     
instance ToSchema JobsDto   
instance ToJSON SystemDto  
instance ToJSON ResourcesDto
instance ToSchema SystemDto     
instance ToSchema ResourcesDto     

instance ToJSON a => ToJSON (ValidationsDto a)
instance ToSchema a => ToSchema (ValidationsDto a)

instance ToSchema FullVDto
instance ToJSON FullVDto where
    toJSON FullVDto {..} = object [         
            "url"       .= url,
            "full-path" .= path,
            "issues"    .= Array (V.fromList $ issuesJson issues)
        ]      

instance ToSchema MinimalVDto
instance ToJSON MinimalVDto where
    toJSON (MinimalVDto FullVDto {..}) = object [         
            "url"       .= url,
            "issues"    .= Array (V.fromList $ issuesJson issues)
        ]      

issuesJson :: [IssueDto] -> [Value]
issuesJson issues = flip map issues $ \case
    ErrorDto e   -> object [ "error"   .= e ]
    WarningDto w -> object [ "warning" .= w ]


newtype DtoScope (s :: ScopeKind) = DtoScope (Scope s)
    deriving stock (Show, Eq, Ord, Generic)

instance ToJSON MetricsDto
instance ToJSON RrdpURL
instance ToJSON FetchStatus
instance ToJSON RrdpRepository
instance ToJSON PublicationPointDto

instance ToSchema MetricsDto
instance ToSchema FetchStatus where     
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema RrdpRepository
instance ToSchema PublicationPointDto

instance ToJSONKey (DtoScope s) where 
    toJSONKey = toJSONKeyText $ \(DtoScope (Scope s)) -> focusToText $ NonEmpty.head s    

instance ToJSON (DtoScope s) where
    toJSON (DtoScope (Scope s)) = Array $ V.fromList $ map toJSON $ NonEmpty.toList s

instance ToSchema (DtoScope 'Metric)

toMinimalValidations :: ValidationsDto FullVDto -> ValidationsDto MinimalVDto
toMinimalValidations = (& #validations %~ map MinimalVDto)

toMetricsDto :: RawMetric -> MetricsDto
toMetricsDto rawMetrics = MetricsDto {
        groupedValidations = groupedValidationMetric rawMetrics,
        rsync    = MonoidalMap.mapKeys DtoScope $ unMetricMap $ rawMetrics ^. #rsyncMetrics,
        rrdp     = MonoidalMap.mapKeys DtoScope $ unMetricMap $ rawMetrics ^. #rrdpMetrics
    }

toPublicationPointDto :: PublicationPoints -> PublicationPointDto
toPublicationPointDto PublicationPoints {..} = PublicationPointDto {
        rrdp = Map.toList $ unRrdpMap rrdps
    }

parseHash :: Text -> Either Text Hash
parseHash hashText = bimap 
    (Text.pack . ("Broken hex: " <>) . show)
    mkHash
    $ Hex.decode $ encodeUtf8 hashText
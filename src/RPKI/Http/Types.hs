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
import           Data.Coerce

import           GHC.Generics                (Generic)
import qualified Data.Vector                 as V
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Map.Strict             (Map)
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
import           RPKI.TAL
import           RPKI.Metrics.Metrics
import           RPKI.Orphans.Json
import           RPKI.Orphans.Swagger
import           RPKI.Reporting

import           RPKI.RRDP.Types
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

data FocusResolvedDto = TextDto Text 
                    | ObjectLink Text
                    | DirectLink Text
                    | TA_UI Text
    deriving stock (Show, Eq, Ord)      

data FullVDto focus = FullVDto {
        issues  :: [IssueDto],
        path    :: [focus],
        url     :: Focus
    } 
    deriving stock (Eq, Show, Generic)

newtype OriginalVDto = OriginalVDto (FullVDto Focus)
    deriving stock (Eq, Show, Generic)

newtype ResolvedVDto = ResolvedVDto  { 
        unResolvedVDto :: FullVDto FocusResolvedDto
    }
    deriving stock (Eq, Show, Generic)

newtype MinimalVDto focus = MinimalVDto (FullVDto focus)
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
        asn :: ASN, 
        afi :: Maybe AddrFamily
    }
    deriving stock (Eq, Show, Generic)

data AspaDto = AspaDto {
        customer  :: ASN,
        providers :: [ProviderAsn]        
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
                | GBRD (ObjectContentDto (CMSObjectDto GbrDto))
                | RSCD (ObjectContentDto (CMSObjectDto RscDto))
    deriving stock (Eq, Show, Generic)

data ObjectContentDto payload = ObjectContentDto {
        hash :: Hash,
        ski  :: Maybe SKI,
        aki  :: Maybe AKI,
        eeCertificate :: Maybe CertificateDto,          
        objectPayload :: payload
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


data GbrDto = GbrDto {
        vcard :: Map Text Text
    }  
    deriving stock (Eq, Show, Generic)

data RscDto = RscDto {
        rscResources    :: PrefixesAndAsns,        
        checkList       :: [CheckListDto],
        digestAlgorithm :: DigestAlgorithmIdentifier
    }  
    deriving stock (Eq, Show, Generic)

data CheckListDto = CheckListDto {
        fileName :: Maybe Text,
        hash     :: Hash
    }  
    deriving stock (Eq, Show, Generic)


data MetricsDto = MetricsDto {
        groupedValidations :: GroupedValidationMetric ValidationMetric,      
        rsync              :: MonoidalMap (DtoScope 'Metric) RsyncMetric,
        rrdp               :: MonoidalMap (DtoScope 'Metric) RrdpMetric
    } 
    deriving stock (Eq, Show, Generic)

data PublicationPointDto = PublicationPointDto {
        rrdp  :: [(RrdpURL, RrdpRepository)],
        rsync :: [(RsyncURL, RsyncNodeInfo)]        
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
        startUpTime   :: Instant,
        resources     :: [ResourcesDto]
    }
    deriving stock (Eq, Show, Generic)

newtype RtrDto = RtrDto {
        rtrState :: RtrState
    }
    deriving stock (Eq, Show, Generic)

data TalDto = TalDto {
        tal :: TAL,
        repositories :: [Text]
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
instance ToJSON GbrDto
instance ToJSON RscDto
instance ToJSON CheckListDto
instance ToJSON VrpDto     
instance ToJSON VrpMinimalDto     
instance ToJSON AspaDto
instance ToJSON BgpCertDto
instance ToJSON RtrDto
instance ToJSON TalDto

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
instance ToSchema GbrDto
instance ToSchema RscDto
instance ToSchema CheckListDto
instance ToSchema AspaDto
instance ToSchema BgpCertDto
instance ToSchema RtrDto
instance ToSchema TalDto
instance ToSchema TAL
instance ToSchema EncodedBase64 where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
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

instance ToSchema OriginalVDto
instance ToSchema t => ToSchema (FullVDto t)
instance ToJSON f => ToJSON (FullVDto f) where
    toJSON FullVDto {..} = object [         
            "url"       .= url,
            "full-path" .= path,
            "issues"    .= Array (V.fromList $ issuesJson issues)
        ]      

instance ToSchema FocusResolvedDto where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema ResolvedVDto
instance ToSchema f => ToSchema (MinimalVDto f)
instance ToJSON (MinimalVDto t) where
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
instance ToJSON ETag
instance ToJSON Speed
instance ToJSON RrdpRepository
instance ToJSON RsyncNodeInfo
instance ToJSON PublicationPointDto

instance ToSchema MetricsDto
instance ToSchema FetchStatus where     
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema ETag where     
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema Speed where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema RrdpRepository
instance ToSchema RsyncNodeInfo
instance ToSchema PublicationPointDto

instance ToJSONKey (DtoScope s) where 
    toJSONKey = toJSONKeyText $ \(DtoScope (Scope s)) -> focusToText $ NonEmpty.head s    

instance ToJSON (DtoScope s) where
    toJSON (DtoScope (Scope s)) = Array $ V.fromList $ map toJSON $ NonEmpty.toList s

instance ToSchema (DtoScope 'Metric)

toMinimalValidations :: Coercible dto (FullVDto f) => 
                        ValidationsDto dto 
                     -> ValidationsDto (MinimalVDto f)
toMinimalValidations = (& #validations %~ coerce)

toMetricsDto :: RawMetric -> MetricsDto
toMetricsDto rawMetrics = MetricsDto {
        groupedValidations = groupedValidationMetric rawMetrics,
        rsync    = MonoidalMap.mapKeys DtoScope $ unMetricMap $ rawMetrics ^. #rsyncMetrics,
        rrdp     = MonoidalMap.mapKeys DtoScope $ unMetricMap $ rawMetrics ^. #rrdpMetrics
    }

toPublicationPointDto :: PublicationPoints -> PublicationPointDto
toPublicationPointDto PublicationPoints {..} = PublicationPointDto {
        rrdp  = Map.toList $ unRrdpMap rrdps,
        rsync = flattenRsyncTree rsyncs
    }

parseHash :: Text -> Either Text Hash
parseHash hashText = bimap 
    (Text.pack . ("Broken hex: " <>) . show)
    mkHash
    $ Hex.decode $ encodeUtf8 hashText


resolvedFocusToText :: FocusResolvedDto -> Text
resolvedFocusToText = \case  
    TextDto t    -> t 
    ObjectLink t -> t
    DirectLink t -> t
    TA_UI t      -> t
 
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

import           Servant.API
import           Data.Swagger hiding (url)
import           Network.HTTP.Media ((//))

import           Data.ASN1.OID

import           RPKI.Config
import           RPKI.AppTypes
import           RPKI.Validation.Types
import           RPKI.Repository
import           RPKI.Domain
import           RPKI.TAL
import           RPKI.Metrics.Metrics
import           RPKI.Orphans.Json
import           RPKI.Reporting

import           RPKI.RRDP.Types
import           RPKI.Resources.Types
import           RPKI.Store.Types hiding (object)
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

data ResolvedFocusDto = TextDto Text 
                    | ObjectLink Text
                    | DirectLink Text
                    | TA_UI Text
    deriving stock (Show, Eq, Ord, Generic)      

data ValidationDto focus = ValidationDto {
        issues  :: [IssueDto],
        path    :: [focus],
        url     :: Focus
    } 
    deriving stock (Eq, Show, Generic)

newtype OriginalVDto = OriginalVDto (ValidationDto Focus)
    deriving stock (Eq, Show, Generic)

newtype ResolvedVDto = ResolvedVDto  { 
        unResolvedVDto :: ValidationDto ResolvedFocusDto
    }
    deriving stock (Eq, Show, Generic)

newtype MinimalVDto focus = MinimalVDto (ValidationDto focus)
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

data VrpExtDto = VrpExtDto {
        uri :: Text,
        vrp :: VrpDto        
    } 
    deriving stock (Eq, Show, Generic)

data AspaDto = AspaDto {
        customer  :: ASN,
        providers :: [ASN]        
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
                | SPLD (ObjectContentDto (CMSObjectDto SplPayloadDto))
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

data SplDto = SplDto {
        asn    :: ASN,
        prefix :: IpPrefix
    }  
    deriving stock (Eq, Show, Generic)

data SplPayloadDto = SplPayloadDto {
        asn      :: ASN,
        prefixes :: [IpPrefix]
    }  
    deriving stock (Eq, Show, Generic)

data RoaPrefixDto = RoaPrefixDto {
        prefix    :: IpPrefix,
        maxLength :: PrefixLength        
    }
    deriving stock (Eq, Show, Generic)

newtype GbrDto = GbrDto {
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


newtype MetricsDto = MetricsDto {
        groupedValidations :: GroupedMetric ValidationMetric        
    } 
    deriving stock (Eq, Show, Generic)

data PublicationPointsDto = PublicationPointsDto {
        rrdp  :: [(RrdpURL, RrdpRepository)],
        rsync :: [(RsyncURL, RepositoryMeta)]        
    } 
    deriving stock (Eq, Show, Generic)

data RepositoryDto = RsyncDto RsyncRepositoryDto
                   | RrdpDto RrdpRepositoryDto
    deriving stock (Eq, Show, Generic) 

data RsyncRepositoryDto = RsyncRepositoryDto {
        uri         :: RsyncURL,
        meta        :: RepositoryMeta,
        metrics     :: RsyncMetric,
        validations :: [ResolvedVDto]
    }
    deriving stock (Eq, Show, Generic)

data RrdpRepositoryDto = RrdpRepositoryDto {
        uri         :: RrdpURL,
        repository  :: RrdpRepository,
        metrics     :: RrdpMetric,
        validations :: [ResolvedVDto]
    }
    deriving stock (Eq, Show, Generic)

newtype JobsDto = JobsDto {
        jobs :: [(Text, Instant)]
    } 
    deriving stock (Eq, Show, Generic)

data ResourcesDto = ResourcesDto {
        tag                 :: Text,
        latestCpuTime      :: CPUTime,
        aggregatedCpuTime   :: CPUTime,
        aggregatedClockTime :: TimeMs,
        maxMemory           :: MaxMemory,        
        avgCpuTimeMsPerSecond :: Double
    }
    deriving stock (Eq, Show, Generic)

data WorkerInfoDto = WorkerInfoDto {
        pid       :: Int,
        endOfLife :: Instant,
        cli       :: Text
    }
    deriving stock (Eq, Show, Generic)

data SystemDto = SystemDto {
        proverVersion :: Text,
        gitInfo       :: Text,
        config        :: Config,
        startUpTime   :: Instant,
        resources     :: [ResourcesDto],
        rsyncClients  :: [WorkerInfoDto]
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


data ManifestChildDto = ManifestChildDto {
        fileName :: Text,
        child    :: MftChild
    }
    deriving stock (Eq, Show, Generic)

data CaShortcutDto = CaShortcutDto { 
        key            :: ObjectKey,        
        ski            :: SKI,
        publicationPoints :: [Text],
        notValidBefore :: Instant,
        notValidAfter  :: Instant
    }
    deriving stock (Show, Eq, Ord, Generic)

data ManifestShortcutDto = ManifestShortcutDto {
        key            :: ObjectKey,
        nonCrlChildren :: Map.Map ObjectKey ManifestChildDto,
        notValidBefore :: Instant,
        notValidAfter  :: Instant,        
        serial         :: Serial,
        manifestNumber :: Serial,
        crlShortcut    :: CrlShortcut
    }
    deriving stock (Eq, Show, Generic)

data ManifestsDto = ManifestsDto {
        shortcutMft :: Maybe ManifestShortcutDto,
        manifests   :: [ManifestDto]
    }
    deriving stock (Eq, Show, Generic)

data RouteDto = RouteDto {
        origin_asn :: ASN,
        prefix     :: Text
    }
    deriving stock (Eq, Show, Generic)

data ValidatedRouteDto = ValidatedRouteDto {
        route    :: RouteDto,
        validity :: ValidityDto
    }
    deriving stock (Eq, Show, Generic)

data ValidityResultDto = ValidityResultDto {
        validated_route :: ValidatedRouteDto,        
        generatedTime   :: String
    }
    deriving stock (Eq, Show, Generic)

data ValidityBulkResultDto = ValidityBulkResultDto {
        generatedTime :: String,
        results       :: [ValidatedRouteDto]        
    }
    deriving stock (Eq, Show, Generic)

data ValidityBulkInputDto = ValidityBulkInputDto {
        asn    :: Text,
        prefix :: Text
    }
    deriving stock (Eq, Show, Generic)

data MatchVrpDto = MatchVrpDto {
        asn        :: ASN,
        prefix     :: Text,
        max_length :: PrefixLength
    }
    deriving stock (Eq, Show, Generic)
    
data ValidityVrpsDto = ValidityVrpsDto {
        matched          :: [MatchVrpDto],
        unmatched_as     :: [MatchVrpDto],
        unmatched_length :: [MatchVrpDto]
    }
    deriving stock (Eq, Show, Generic)

data ValidityDto = ValidityDto {
        state :: Text,
        vrps  :: ValidityVrpsDto
    }
    deriving stock (Eq, Show, Generic)

data ManualCVS = ManualCVS
data ObjectBlob = ObjectBlob

newtype RawCSV = RawCSV { unRawCSV :: LBS.ByteString }
    deriving stock (Eq, Show, Generic)

instance Accept ManualCVS where
    contentType _ = "text" // "csv"

instance Accept ObjectBlob where
    contentType _ = "application" // "octet-stream"

instance MimeRender ManualCVS RawCSV where
    mimeRender _ = unRawCSV    

instance MimeRender ObjectBlob ObjectOriginal where
    mimeRender _ (ObjectOriginal b) = LBS.fromStrict b

instance ToSchema RawCSV where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema ObjectOriginal where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToJSON RObject
instance ToJSON ObjectDto where
    toJSON = \case
        CertificateD v -> object ["type" .= ("certificate" :: Text), "value" .= toJSON v]
        ManifestD v    -> object ["type" .= ("manifest" :: Text), "value" .= toJSON v]
        CRLD v         -> object ["type" .= ("CRL" :: Text), "value" .= toJSON v]
        BGPSecD v      -> object ["type" .= ("BGPSec" :: Text), "value" .= toJSON v]
        ROAD v  -> object ["type" .= ("ROA" :: Text), "value" .= toJSON v]
        SPLD v  -> object ["type" .= ("PrefixList" :: Text), "value" .= toJSON v]
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
instance ToJSON SplDto
instance ToJSON SplPayloadDto
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
instance ToJSON ManifestShortcutDto
instance ToJSON ManifestsDto
instance ToJSON CaShortcutDto
instance ToJSON ValidatedRouteDto
instance ToJSON ValidityResultDto
instance ToJSON ValidityBulkInputDto
instance ToJSON ValidityBulkResultDto
instance ToJSON ValidityVrpsDto
instance ToJSON MatchVrpDto
instance ToJSON RouteDto

instance FromJSON ValidityBulkInputDto

instance ToJSON ValidityDto where
    toJSON ValidityDto {..} = 
        object ["state" .= state, "VRPs" .= toJSON vrps ]

instance ToJSON ManifestChildDto where 
    toJSON ManifestChildDto {..} = 
        case child of 
            CaChild shortcut serial   -> toJsonObject "ca-shortcut" serial (toDto shortcut)                    
            RoaChild shortcut serial  -> toJsonObject "roa-shortcut" serial shortcut 
            SplChild shortcut serial  -> toJsonObject "spl-shortcut" serial shortcut
            GbrChild shortcut serial  -> toJsonObject "gbr-shortcut" serial shortcut                
            AspaChild shortcut serial -> toJsonObject "aspa-shortcut" serial shortcut                
            BgpSecChild shortcut serial -> toJsonObject "bgpsec-shortcut" serial shortcut            
            TroubledChild objectKey -> 
                object ["type" .= ("troubled-shortcut" :: Text), "key" .= toJSON objectKey ]
      where
        toDto CaShortcut {..} = let 
            publicationPoints = map (toText . getRpkiURL) 
                                $ NonEmpty.toList 
                                $ unPublicationPointAccess ppas
            in CaShortcutDto {..}

        toJsonObject :: ToJSON a => Text -> Serial -> a -> Value
        toJsonObject shortcutType serial value = 
            object [
                "type" .= (shortcutType :: Text), 
                "serial" .= toJSON serial, 
                "fileName" .= toJSON fileName, 
                "value" .= toJSON value ]
            
    
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
instance ToSchema SplPayloadDto
instance ToSchema SplDto
instance ToSchema RoaDto
instance ToSchema RoaPrefixDto
instance ToSchema GbrDto
instance ToSchema RscDto
instance ToSchema CheckListDto
instance ToSchema AspaDto
instance ToSchema BgpCertDto
instance ToSchema RtrDto
instance ToSchema TalDto
instance ToSchema ManifestShortcutDto
instance ToSchema ManifestChildDto
instance ToSchema ManifestsDto
instance ToSchema ValidatedRouteDto
instance ToSchema ValidityResultDto
instance ToSchema ValidityBulkInputDto
instance ToSchema ValidityBulkResultDto
instance ToSchema ValidityDto
instance ToSchema ValidityVrpsDto
instance ToSchema MatchVrpDto
instance ToSchema RouteDto
instance ToSchema TAL
instance ToSchema EncodedBase64 where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToJSON JobsDto     
instance ToSchema JobsDto   
instance ToJSON SystemDto  
instance ToJSON WorkerInfoDto  
instance ToJSON ResourcesDto
instance ToSchema SystemDto     
instance ToSchema WorkerInfoDto     
instance ToSchema ResourcesDto     

instance ToJSON a => ToJSON (ValidationsDto a)
instance ToSchema a => ToSchema (ValidationsDto a)

instance ToSchema OriginalVDto
instance ToSchema t => ToSchema (ValidationDto t)
instance ToJSON f => ToJSON (ValidationDto f) where
    toJSON ValidationDto {..} = object [         
            "url"       .= url,
            "full-path" .= path,
            "issues"    .= Array (V.fromList $ issuesJson issues)
        ]      

instance ToSchema ResolvedFocusDto where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance ToSchema ResolvedVDto
instance ToJSON ResolvedFocusDto
instance ToJSON ResolvedVDto
instance ToSchema f => ToSchema (MinimalVDto f)
instance ToJSON (MinimalVDto t) where
    toJSON (MinimalVDto ValidationDto {..}) = object [         
            "url"       .= url,
            "issues"    .= Array (V.fromList $ issuesJson issues)
        ]      

instance ToJSON OriginalVDto

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
instance ToJSON DeltaInfo
instance ToJSON RrdpIntegrity
instance ToJSON RrdpEnforcement
instance ToJSON RrdpMeta
instance ToJSON RrdpRepository
instance ToJSON RepositoryMeta
instance ToJSON PublicationPointsDto
instance ToJSON RepositoryDto
instance ToJSON RrdpRepositoryDto
instance ToJSON RsyncRepositoryDto


instance ToSchema MetricsDto
instance ToSchema FetchStatus where     
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema ETag where     
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema DeltaInfo
instance ToSchema RrdpIntegrity
instance ToSchema RrdpEnforcement where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
instance ToSchema RrdpMeta
instance ToSchema RrdpRepository
instance ToSchema RepositoryMeta
instance ToSchema PublicationPointsDto

instance ToJSONKey (DtoScope s) where 
    toJSONKey = toJSONKeyText $ \(DtoScope (Scope s)) -> focusToText $ NonEmpty.head s    

instance ToJSON (DtoScope s) where
    toJSON (DtoScope (Scope s)) = Array $ V.fromList $ map toJSON $ NonEmpty.toList s

instance ToSchema (DtoScope 'Metric)

toMinimalValidations :: Coercible dto (ValidationDto f) => 
                        ValidationsDto dto 
                     -> ValidationsDto (MinimalVDto f)
toMinimalValidations = #validations %~ coerce

toMetricsDto :: Metrics -> PerTA Metrics -> MetricsDto
toMetricsDto common perTa = MetricsDto {..}
  where
    -- TODO This is temporary -- grouping should take into account
    -- that metrics are already grouped by TA
    groupedValidations = groupedValidationMetric $ allTAs perTa <> common

toPublicationPointDto :: PublicationPoints -> PublicationPointsDto
toPublicationPointDto PublicationPoints {..} = PublicationPointsDto {
        rrdp  = Map.toList $ unRrdpMap rrdps,
        rsync = flattenRsyncTree rsyncs
    }


parseHash :: Text -> Either Text Hash
parseHash hashText = bimap 
    (Text.pack . ("Broken hex: " <>) . show)
    mkHash
    $ Hex.decode $ encodeUtf8 hashText

parseAki :: Text -> Either Text AKI
parseAki akiText = bimap 
    (Text.pack . ("Broken hex: " <>) . show)
    (AKI . KI . toShortBS) 
    $ Hex.decode $ encodeUtf8 akiText


resolvedFocusToText :: ResolvedFocusDto -> Text
resolvedFocusToText = \case  
    TextDto t    -> t 
    ObjectLink t -> t
    DirectLink t -> t
    TA_UI t      -> t
 
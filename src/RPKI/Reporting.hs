{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}


module RPKI.Reporting where
    
import           Control.Exception.Lifted
import           Control.Lens

import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import qualified Data.ByteString             as BS
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe, listToMaybe)
import           Data.Monoid

import           Data.Text                   as Text
import           Data.Tuple.Strict

import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Monoid.Generic
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.ASN1.Types (OID)

import           GHC.Generics

import           Data.Map.Monoidal.Strict (MonoidalMap(MonoidalMap))

import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Resources.Types
import           RPKI.Time

import           RPKI.Store.Base.Serialisation

newtype ParseError s = ParseError s
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data ValidationError =  SPKIMismatch SPKI SPKI |
                        UnknownObjectAsTACert |
                        ObjectIsTooSmall Integer |
                        ObjectIsTooBig Integer |
                        InvalidSignature Text |  
                        InvalidKI Text |  
                        CMSSignatureAlgorithmMismatch Text Text |                      
                        NoAKI |
                        NoValidatedVersion |
                        ParentCertificateNotFound |
                        ObjectNotOnManifest |
                        UnsupportedHashAlgorithm DigestAlgorithmIdentifier |
                        NotFoundOnChecklist Hash Text |
                        ChecklistFileNameMismatch Hash Text Text |
                        TACertAKIIsNotEmpty URI |
                        TACertOlderThanPrevious { 
                                before :: Instant,
                                after :: Instant,
                                prevBefore :: Instant,
                                prevAfter :: Instant
                            } |
                        CertNoPolicyExtension |
                        CertBrokenExtension OID BS.ByteString |
                        UnknownCriticalCertificateExtension OID BS.ByteString |
                        MissingCriticalExtension OID |
                        BrokenKeyUsage Text |
                        ObjectHasMultipleLocations [RpkiURL] |
                        NoMFT AKI |
                        NoMFTButCachedMft AKI |
                        NoCRLOnMFT AKI |
                        MoreThanOneCRLOnMFT AKI [T2 Text Hash] |
                        NoMFTSIA |
                        MFTOnDifferentLocation URI Locations |
                        BadFileNameOnMFT Text Text |
                        ZeroManifestEntries |
                        NonUniqueManifestEntries [(Hash, [Text])] |
                        NoCRLExists AKI Hash |
                        CRLOnDifferentLocation URI Locations |
                        CRLHashPointsToAnotherObject Hash |
                        CRL_AKI_DifferentFromCertSKI SKI AKI |
                        NextUpdateTimeNotSet |                        
                        NextUpdateTimeIsInThePast   { nextUpdateTime :: Instant, now :: Instant } |
                        ThisUpdateTimeIsInTheFuture { thisUpdateTime :: Instant, now :: Instant } |
                        NextUpdateTimeBeforeThisUpdateTime  { nextUpdateTime :: Instant, thisUpdateTime :: Instant } |
                        RevokedResourceCertificate |
                        ObjectValidityIsInTheFuture { before :: Instant, after :: Instant } |
                        ObjectIsExpired { before :: Instant, after :: Instant } |
                        AKIIsNotEqualsToParentSKI (Maybe AKI) SKI |
                        ManifestEntryDoesn'tExist Hash Text |
                        OverclaimedResources PrefixesAndAsns |
                        InheritWithoutParentResources |
                        ResourceSetMustBeInherit |
                        UnknownUriType URI | 
                        BrokenUri Text Text | 
                        CertificateDoesntHaveSIA | 
                        AIANotSameAsParentLocation Text Locations | 
                        CircularReference ObjectIdentity |
                        CertificatePathTooDeep Locations Int |
                        TreeIsTooBig Locations Int |
                        TooManyRepositories Locations Int |
                        ValidationTimeout Int |
                        ManifestLocationMismatch Text Locations | 
                        InvalidVCardFormatInGbr Text | 
                        RoaPrefixIsOutsideOfResourceSet IpPrefix PrefixesAndAsns |
                        RoaPrefixLenghtsIsBiggerThanMaxLength Vrp |
                        AspaOverlappingCustomerProvider ASN [ASN] | 
                        AspaAsNotOnEECert ASN [AsResource] | 
                        AspaNoAsn |
                        AspaIPv4Present |
                        AspaIPv6Present |      
                        BGPCertSIAPresent BS.ByteString | 
                        BGPCertIPv4Present |
                        BGPCertIPv6Present | 
                        BGPCertBrokenASNs
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    
data RrdpError = BrokenXml Text | 
                BrokenSerial Text |
                NoSessionId |
                NoSerial | 
                NoSnapshotHash | 
                NoSnapshotURI | 
                BrokenSnapshotUri Text | 
                SnapshotUriHostname Text Text | 
                NoDeltaSerial | 
                NoDeltaURI | 
                BrokenDeltaUri Text | 
                DeltaUriHostname Text Text | 
                NoDeltaHash |
                BadHash Text |
                NoVersion | 
                BadVersion Text | 
                NoPublishURI |
                BadBase64 Text Text |
                BadURL Text |
                NoHashInWithdraw |
                ContentInWithdraw Text Text |
                LocalSerialBiggerThanRemote RrdpSerial RrdpSerial |
                NonConsecutiveDeltaSerials [(RrdpSerial, RrdpSerial)] |
                CantDownloadFile Text |
                CantDownloadNotification Text |
                CantDownloadSnapshot Text |
                FailedToParseSnapshotItem Text |
                CantDownloadDelta Text |
                FailedToParseDeltaItem Text |
                SnapshotHashMismatch { actualHash :: Hash, expectedHash :: Hash } |
                SnapshotSessionMismatch { actualSessionId :: SessionId, expectedSessionId :: SessionId } |
                SnapshotSerialMismatch { actualSerial :: RrdpSerial, expectedSerial :: RrdpSerial } |
                DeltaSessionMismatch { actualSessionId :: SessionId, expectedSessionId :: SessionId } |
                DeltaSerialMismatch { actualSerial :: RrdpSerial, expectedSerial :: RrdpSerial } |
                DeltaSerialTooHigh { actualSerial :: RrdpSerial, expectedSerial :: RrdpSerial } |
                DeltaHashMismatch { actualHash :: Hash, expectedHash :: Hash, serial :: RrdpSerial } |
                NoObjectToReplace URI Hash |
                NoObjectToWithdraw URI Hash |
                ObjectExistsWhenReplacing URI Hash |
                UnsupportedObjectType Text | 
                RrdpDownloadTimeout Int64 | 
                UnknownRrdpProblem Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data RsyncError = RsyncProcessError Int Text |
                    FileReadError Text |
                    RsyncRunningError Text |         
                    RsyncDownloadTimeout Int64 | 
                    UnknownRsyncProblem Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data StorageError = StorageError Text |
                    DeserialisationError Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype TALError = TALError Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype InitError = InitError Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data InternalError = WorkerTimeout Text 
                   | WorkerOutOfMemory Text 
                   | InternalError Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data SlurmError = SlurmFileError Text Text |
                  SlurmParseError Text Text |
                  SlurmValidationError Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data AppError = ParseE (ParseError Text) | 
                TAL_E TALError | 
                RrdpE RrdpError |
                RsyncE RsyncError |
                StorageE StorageError |                     
                ValidationE ValidationError |
                InitE InitError |
                SlurmE SlurmError |
                InternalE InternalError |
                UnspecifiedE Text Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype VWarning = VWarning AppError
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data VIssue = VErr AppError | VWarn VWarning
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype AppException = AppException AppError
    deriving stock (Show, Eq, Ord, Generic)

instance Exception AppException

newtype Validations = Validations (Map VScope (Set VIssue))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving newtype Monoid

instance Semigroup Validations where
    (Validations m1) <> (Validations m2) = Validations $ Map.unionWith (<>) m1 m2


data Focus = TAFocus Text 
            | LocationFocus URI
            | LinkFocus URI
            | ObjectFocus ObjectKey
            | HashFocus Hash
            | PPFocus RpkiURL
            | RepositoryFocus RpkiURL
            | TextFocus Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    

newtype Scope (t :: ScopeKind) = Scope (NonEmpty Focus)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data ScopeKind = Validation | Metric
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    

type VScope      = Scope 'Validation    
type MetricScope = Scope 'Metric
    
data Scopes = Scopes {
        validationScope :: VScope,
        metricScope     :: MetricScope
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newScope :: Text -> Scope c
newScope = newScope' TextFocus

newScope' :: (a -> Focus) -> a -> Scope c
newScope' c u = Scope $ c u :| []

newScopes :: Text -> Scopes
newScopes = newScopes' TextFocus

newScopes' :: (a -> Focus) -> a -> Scopes
newScopes' c t = Scopes {
        validationScope = newScope' c t,
        metricScope     = newScope' c t
    }    

subScope :: Text -> Scope t -> Scope t
subScope = subScope' TextFocus        

subScope' :: (a -> Focus) -> a -> Scope t -> Scope t
subScope' constructor a ps@(Scope parentScope) = let
        focus = constructor a
    in case NonEmpty.filter (== focus) parentScope of 
        [] -> Scope $ NonEmpty.cons focus parentScope 
        _  -> ps     

validatorSubScope' :: forall a . (a -> Focus) -> a -> Scopes -> Scopes
validatorSubScope' constructor t vc = 
    vc & typed @VScope      %~ subScope' constructor t
       & typed @MetricScope %~ subScope' constructor t  
   

mError :: VScope -> AppError -> Validations
mError vc w = mProblem vc (VErr w)

mWarning :: VScope -> VWarning -> Validations
mWarning vc w = mProblem vc (VWarn w)

mProblem :: VScope -> VIssue -> Validations
mProblem vc p = Validations $ Map.singleton vc $ Set.singleton p

emptyValidations :: Validations -> Bool 
emptyValidations (Validations m) = List.all Set.null $ Map.elems m  

removeValidation :: VScope -> (AppError -> Bool) -> Validations -> Validations
removeValidation vScope predicate (Validations vs) =
    Validations $ Map.adjust removeFromSet vScope vs    
    where 
        removeFromSet = Set.filter $ \case 
            VErr e             -> not $ predicate e
            VWarn (VWarning e) -> not $ predicate e

getIssues :: VScope -> Validations -> Set VIssue
getIssues s (Validations vs) = fromMaybe mempty $ Map.lookup s vs


------------------------------------------------
------------ Metrics
------------------------------------------------

class Monoid metric => MetricC metric where
    -- lens to access the specific metric map in the total metric record    
    metricLens :: Lens' RawMetric (MetricMap metric)

newtype Count = Count { unCount :: Int64 }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary)   
    deriving newtype (Num)
    deriving Semigroup via Sum Count
    deriving Monoid via Sum Count

instance Show Count where 
    show (Count c) = show c

newtype HttpStatus = HttpStatus { unHttpStatus :: Int }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    

instance Monoid HttpStatus where
    mempty = HttpStatus 200

instance Semigroup HttpStatus where
    s1 <> s2 = if isHttpSuccess s1 then s2 else s1

data RrdpSource = RrdpNoUpdate | RrdpDelta | RrdpSnapshot
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)        

instance Monoid RrdpSource where
    mempty = RrdpNoUpdate

instance Semigroup RrdpSource where
    RrdpNoUpdate <> r           = r
    r           <> RrdpNoUpdate = r
    _           <> r           = r


data FetchFreshness = UpToDate | AttemptedFetch | FailedToFetch
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)        

instance Monoid FetchFreshness where
    mempty = UpToDate

instance Semigroup FetchFreshness where
    (<>) = max    

data RrdpMetric = RrdpMetric {
        added           :: Count,
        deleted         :: Count,        
        rrdpSource      :: RrdpSource,        
        lastHttpStatus  :: HttpStatus,        
        downloadTimeMs  :: TimeMs,
        saveTimeMs      :: TimeMs,
        totalTimeMs     :: TimeMs,
        fetchFreshness  :: FetchFreshness
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup RrdpMetric   
    deriving Monoid    via GenericMonoid RrdpMetric

data RsyncMetric = RsyncMetric {
        processed      :: Count,        
        totalTimeMs    :: TimeMs,
        fetchFreshness :: FetchFreshness
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup RsyncMetric   
    deriving Monoid    via GenericMonoid RsyncMetric

data ValidationMetric = ValidationMetric {
        vrpCounter      :: Count,        
        uniqueVrpNumber :: Count,        
        validCertNumber :: Count,
        validRoaNumber  :: Count,        
        validMftNumber  :: Count,
        validCrlNumber  :: Count,
        validGbrNumber  :: Count,
        validAspaNumber :: Count,
        validBgpNumber  :: Count,
        mftShortcutNumber :: Count,                
        totalTimeMs     :: TimeMs
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup ValidationMetric   
    deriving Monoid    via GenericMonoid ValidationMetric

instance MetricC RrdpMetric where
    metricLens = #rrdpMetrics

instance MetricC RsyncMetric where
    metricLens = #rsyncMetrics

instance MetricC ValidationMetric where 
    metricLens = #validationMetrics

newtype MetricMap a = MetricMap { unMetricMap :: MonoidalMap MetricScope a }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    
    deriving newtype Monoid    
    deriving newtype Semigroup

data VrpCounts = VrpCounts { 
        totalUnique :: Count,        
        perTaUnique :: MonoidalMap TaName Count
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup VrpCounts   
    deriving Monoid    via GenericMonoid VrpCounts

data RawMetric = RawMetric {
        rsyncMetrics      :: MetricMap RsyncMetric,
        rrdpMetrics       :: MetricMap RrdpMetric,
        validationMetrics :: MetricMap ValidationMetric,
        vrpCounts         :: VrpCounts
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup RawMetric   
    deriving Monoid    via GenericMonoid RawMetric


-- Misc

data Trace = WorkerTimeoutTrace                   
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    

data ValidationState = ValidationState {
        validations   :: Validations,
        topDownMetric :: RawMetric,
        traces        :: Set Trace
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)
    deriving Semigroup via GenericSemigroup ValidationState
    deriving Monoid    via GenericMonoid ValidationState

mTrace :: Trace -> Set Trace
mTrace t = Set.singleton t
    
vState :: Validations -> ValidationState
vState vs = ValidationState vs mempty mempty

updateMetricInMap :: Monoid a => 
                    MetricScope -> (a -> a) -> MetricMap a -> MetricMap a
updateMetricInMap ms f (MetricMap (MonoidalMap mm)) = 
    MetricMap $ MonoidalMap $ Map.alter (Just . f . fromMaybe mempty) ms mm

lookupMetric :: MetricScope -> MetricMap a -> Maybe a
lookupMetric ms (MetricMap (MonoidalMap mm)) = Map.lookup ms mm

isHttpSuccess :: HttpStatus -> Bool
isHttpSuccess (HttpStatus s) = s >= 200 && s < 300

focusToText :: Focus -> Text
focusToText = \case    
    LocationFocus   (getURL -> URI u) -> u
    PPFocus         (getURL -> URI u) -> u
    RepositoryFocus (getURL -> URI u) -> u
    LinkFocus (URI u) -> u
    TAFocus txt       -> txt
    ObjectFocus key   -> fmt key
    HashFocus hash_   -> fmt hash_    
    TextFocus txt     -> txt    
  where
    fmt = Text.pack . show 

scopeList :: Scope a -> [Focus]
scopeList (Scope s) = NonEmpty.toList s

{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedLabels           #-}


module RPKI.Reporting where
import           Control.Exception.Lifted
import           Control.Lens                (Lens', (%~), (&))

import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import qualified Data.ByteString             as BS
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromMaybe, listToMaybe)
import           Data.Monoid

import           Data.Text                   (Text)
import           Data.Tuple.Strict

import           Codec.Serialise
import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty (..))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Monoid.Generic
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           GHC.Generics

import           Data.Map.Monoidal.Strict
import           RPKI.Domain
import           RPKI.RRDP.Types
import           RPKI.Resources.Types
import           RPKI.Time


newtype ParseError s = ParseError s
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data ValidationError =  SPKIMismatch EncodedBase64 EncodedBase64 |
                        UnknownObjectAsTACert |
                        ObjectIsTooSmall Integer |
                        ObjectIsTooBig Integer |
                        InvalidSignature Text |  
                        CMSSignatureAlgorithmMismatch Text Text |                      
                        TACertAKIIsNotEmpty URI |
                        CertNoPolicyExtension |
                        CertWrongPolicyExtension BS.ByteString |
                        ObjectHasMultipleLocations [RpkiURL] |
                        NoMFT AKI Locations |
                        NoCRLOnMFT AKI Locations |
                        MoreThanOneCRLOnMFT AKI Locations [T2 Text Hash] |
                        NoMFTSIA Locations |
                        MFTOnDifferentLocation URI Locations |
                        BadFileNameOnMFT Text Text |
                        NonUniqueManifestEntries [(Hash, [Text])] |
                        NoCRLExists AKI Locations |
                        CRLOnDifferentLocation URI Locations |
                        CRLHashPointsToAnotherObject Hash Locations |
                        NextUpdateTimeNotSet |                        
                        NextUpdateTimeIsInThePast   { nextUpdateTime :: Instant, now :: Instant } |
                        ThisUpdateTimeIsInTheFuture { thisUpdateTime :: Instant, now :: Instant } |
                        RevokedResourceCertificate |
                        CertificateIsInTheFuture { before :: Instant, after :: Instant } |
                        CertificateIsExpired { before :: Instant, after :: Instant } |
                        AKIIsNotEqualsToParentSKI (Maybe AKI) SKI |
                        ManifestEntryDoesn'tExist Hash Text |
                        OverclaimedResources PrefixesAndAsns |
                        InheritWithoutParentResources |
                        UnknownUriType URI | 
                        CertificateDoesntHaveSIA | 
                        CircularReference Hash Locations |
                        CertificatePathTooDeep Locations Int |
                        TreeIsTooBig Locations Int |
                        TooManyRepositories Locations Int |
                        ManifestLocationMismatch Text Locations | 
                        InvalidVCardFormatInGbr Text | 
                        RoaPrefixIsOutsideOfResourceSet IpPrefix PrefixesAndAsns |
                        RoaPrefixLenghtsIsBiggerThanMaxLength Vrp
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    
data RrdpError = BrokenXml Text | 
                BrokenSerial Text |
                NoSessionId |
                NoSerial | 
                NoSnapshotHash | 
                NoSnapshotURI | 
                NoDeltaSerial | 
                NoDeltaURI | 
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
                CantDownloadDelta Text |
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
    deriving anyclass Serialise

data RsyncError = RsyncProcessError Int Text |
                    FileReadError Text |
                    RsyncRunningError Text |         
                    RsyncDownloadTimeout Int64 | 
                    UnknownRsyncProblem Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data StorageError = StorageError Text |
                    DeserialisationError Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype TALError = TALError Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype InitError = InitError Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data InternalError = WorkerTimeout Text 
                   | WorkerOutOfMemory Text 
                   | InternalError Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data SlurmError = SlurmFileError Text Text |
                  SlurmParseError Text Text |
                  SlurmValidationError Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

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
    deriving anyclass Serialise

newtype VWarning = VWarning AppError
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data VProblem = VErr AppError | VWarn VWarning
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype AppException = AppException AppError
    deriving stock (Show, Eq, Ord, Generic)

instance Exception AppException

newtype Validations = Validations (Map VPath (Set VProblem))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Monoid

instance Semigroup Validations where
    (Validations m1) <> (Validations m2) = Validations $ Map.unionWith (<>) m1 m2

newtype Path a = Path { unPath :: NonEmpty Text }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Semigroup

data PathKind = Validation | Metric
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    

type VPath      = Path 'Validation    
type MetricPath = Path 'Metric
    
data ValidatorPath = ValidatorPath {
        validationPath :: VPath,
        metricPath     :: MetricPath
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newPath :: Text -> Path c
newPath u = Path $ u :| []

subPath :: Text -> Path c -> Path c
subPath t parent = newPath t <> parent

newValidatorPath :: Text -> ValidatorPath
newValidatorPath t = ValidatorPath {
        validationPath = newPath t,
        metricPath     = newPath t
    }

-- | Step down     
validatorSubPath :: Text -> ValidatorPath -> ValidatorPath
validatorSubPath t vc = 
    vc & typed @VPath      %~ subPath t
       & typed @MetricPath %~ subPath t


mError :: VPath -> AppError -> Validations
mError vc w = mProblem vc (VErr w)

mWarning :: VPath -> VWarning -> Validations
mWarning vc w = mProblem vc (VWarn w)

mProblem :: VPath -> VProblem -> Validations
mProblem vc p = Validations $ Map.singleton vc $ Set.singleton p

emptyValidations :: Validations -> Bool 
emptyValidations (Validations m) = List.all Set.null $ Map.elems m  

findError :: Validations -> Maybe AppError
findError (Validations m) = 
    listToMaybe [ e | s <- Map.elems m, VErr e <- Set.toList s ]

removeValidation :: VPath -> (AppError -> Bool) -> Validations -> Validations
removeValidation vPath predicate (Validations vs) =
    Validations $ Map.adjust removeFromSet vPath vs    
    where 
        removeFromSet = Set.filter $ \case 
            VErr e             -> not $ predicate e
            VWarn (VWarning e) -> not $ predicate e


------------------------------------------------
------------ Metrics
------------------------------------------------

class Monoid metric => MetricC metric where
    -- lens to access the specific metric map in the total metric record    
    metricLens :: Lens' AppMetric (MetricMap metric)

newtype Count = Count { unCount :: Int64 }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise   
    deriving newtype (Num)
    deriving Semigroup via Sum Count
    deriving Monoid via Sum Count

instance Show Count where 
    show (Count c) = show c

newtype TimeMs = TimeMs { unTimeMs :: Int64 }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise    
    deriving newtype (Num)
    deriving Semigroup via Sum TimeMs
    deriving Monoid via Sum TimeMs

instance Show TimeMs where 
    show (TimeMs ms) = show ms

newtype HttpStatus = HttpStatus { unHttpStatus :: Int }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    

instance Monoid HttpStatus where
    mempty = HttpStatus 200

instance Semigroup HttpStatus where
    s1 <> s2 = if isHttpSuccess s1 then s2 else s1

data RrdpSource = RrdpNoUpdate | RrdpDelta | RrdpSnapshot
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise        

instance Monoid RrdpSource where
    mempty = RrdpNoUpdate

instance Semigroup RrdpSource where
    RrdpNoUpdate <> r           = r
    r           <> RrdpNoUpdate = r
    _           <> r           = r


data FetchFreshness = UpToDate | Fetched
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise        

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
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup RrdpMetric   
    deriving Monoid    via GenericMonoid RrdpMetric

data RsyncMetric = RsyncMetric {
        processed      :: Count,        
        totalTimeMs    :: TimeMs,
        fetchFreshness :: FetchFreshness
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup RsyncMetric   
    deriving Monoid    via GenericMonoid RsyncMetric

data ValidationMetric = ValidationMetric {
        vrpNumber       :: Count,        
        validCertNumber :: Count,
        validRoaNumber  :: Count,
        validMftNumber  :: Count,
        validCrlNumber  :: Count,
        validGbrNumber  :: Count,
        totalTimeMs     :: TimeMs
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup ValidationMetric   
    deriving Monoid    via GenericMonoid ValidationMetric

instance MetricC RrdpMetric where
    metricLens = #rrdpMetrics

instance MetricC RsyncMetric where
    metricLens = #rsyncMetrics

instance MetricC ValidationMetric where 
    metricLens = #validationMetrics


newtype MetricMap a = MetricMap { unMetricMap :: MonoidalMap MetricPath a }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    
    deriving newtype Monoid    
    deriving newtype Semigroup

data AppMetric = AppMetric {
        rsyncMetrics      :: MetricMap RsyncMetric,
        rrdpMetrics       :: MetricMap RrdpMetric,
        validationMetrics :: MetricMap ValidationMetric
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup AppMetric   
    deriving Monoid    via GenericMonoid AppMetric

data ValidationState = ValidationState {
        validations   :: Validations,
        topDownMetric :: AppMetric
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup ValidationState
    deriving Monoid    via GenericMonoid ValidationState

vState :: Validations -> ValidationState
vState vs = ValidationState vs mempty

validationsToList :: Validations -> [(VPath, Set VProblem)]
validationsToList (Validations vMap) = Map.toList vMap 

updateMetricInMap :: Monoid a => 
                MetricPath -> (a -> a) -> MetricMap a -> MetricMap a
updateMetricInMap metricPath f (MetricMap (MonoidalMap mm)) = 
    MetricMap $ MonoidalMap $ Map.alter (Just . f . fromMaybe mempty) metricPath mm

lookupMetric :: MetricPath -> MetricMap a -> Maybe a
lookupMetric metricPath (MetricMap (MonoidalMap mm)) = Map.lookup metricPath mm


isHttpSuccess :: HttpStatus -> Bool
isHttpSuccess (HttpStatus s) = s >= 200 && s < 300

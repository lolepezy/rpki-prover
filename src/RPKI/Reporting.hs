{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedLabels       #-}


module RPKI.Reporting where

import           Control.Lens                (Lens', (^.), (.~), (&), (%~))
import           Control.Exception.Lifted
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import qualified Data.ByteString             as BS
import           Data.Text                   (Text)
import           Data.Maybe                  (fromMaybe)

import           Codec.Serialise
import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty (..), (<|))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Monoid.Generic
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           GHC.Generics

import           Data.Maybe                  (listToMaybe)
import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Time
import Control.Lens.Setter (ASetter)
import Control.Monad.Reader.Class
import Data.Int (Int64)
import Data.Monoid


newtype ParseError s = ParseError s
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data ValidationError = InvalidCert Text |
                        ParentDoesntHaveResources |
                        NoAKIinManifest |
                        ROACannotBeAParent |
                        NoAKI | 
                        SPKIMismatch EncodedBase64 EncodedBase64 |
                        UnknownObjectAsTACert |
                        ObjectIsTooSmall Integer |
                        ObjectIsTooBig Integer |
                        TACertificateLocalIsNewer Serial Serial |
                        InvalidSignature Text |  
                        CMSSignatureAlgorithmMismatch Text Text |                      
                        TACertAKIIsNotEmpty URI |
                        CertNoPolicyExtension |
                        CertWrongPolicyExtension BS.ByteString |
                        NoMFT AKI Locations |
                        NoMFTNoRepository AKI Locations |
                        NoCRLOnMFT AKI Locations |
                        MoreThanOneCRLOnMFT AKI Locations [(Text, Hash)] |
                        NoMFTSIA Locations |
                        MFTOnDifferentLocation URI Locations |
                        NoCRLExists AKI Locations |
                        CRLOnDifferentLocation URI Locations |
                        CRLHashPointsToAnotherObject Hash Locations |
                        NextUpdateTimeNotSet |                        
                        NextUpdateTimeIsInThePast Instant Instant |
                        ThisUpdateTimeIsInTheFuture Instant Instant |
                        RevokedEECertificate |
                        RevokedResourceCertificate |
                        CertificateIsInTheFuture |
                        CertificateIsExpired |
                        AKIIsNotEqualsToParentSKI (Maybe AKI) SKI|
                        ManifestEntryDontExist Hash |
                        OverclaimedResources PrefixesAndAsns |
                        InheritWithoutParentResources |
                        UnknownUriType URI | 
                        CertificateDoesntHaveSIA | 
                        PublicationPointIsNotAvailable URI |
                        CircularReference Hash Locations |
                        ManifestLocationMismatch Text Locations
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
                BadPublish Text |
                BadURL Text |
                NoHashInWithdraw |
                ContentInWithdraw Text |
                LocalSerialBiggerThanRemote Serial Serial |
                NonConsecutiveDeltaSerials [(Serial, Serial)] |
                CantDownloadFile Text |
                CantDownloadNotification Text |
                CantDownloadSnapshot Text |
                CantDownloadDelta Text |
                SnapshotHashMismatch Hash Hash |
                SnapshotSessionMismatch { actualSessionId :: SessionId, expectedSessionId :: SessionId } |
                SnapshotSerialMismatch { actualSerial :: Serial, expectedSerial :: Serial } |
                DeltaSessionMismatch { actualSessionId :: SessionId, expectedSessionId :: SessionId } |
                DeltaSerialMismatch { actualSerial :: Serial, expectedSerial :: Serial } |
                DeltaSerialTooHigh { actualSerial :: Serial, expectedSerial :: Serial } |
                DeltaHashMismatch Hash Hash Serial |
                NoObjectToReplace URI Hash |
                NoObjectToWithdraw URI Hash |
                ObjectExistsWhenReplacing URI Hash |
                UnsupportedObjectType | 
                RrdpDownloadTimeout | 
                UnknownRrdpProblem Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data RsyncError = RsyncProcessError Int Text |
                    FileReadError Text |
                    RsyncRunningError Text |
                    RsyncDirError Text |               
                    RsyncDownloadTimeout | 
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
    deriving newtype Semigroup

newtype InitError = InitError Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Semigroup


data AppError = ParseE (ParseError Text) | 
                TAL_E TALError | 
                RrdpE RrdpError |
                RsyncE RsyncError |
                StorageE StorageError |                     
                ValidationE ValidationError |
                InitE InitError |
                UnspecifiedE Text Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype VWarning = VWarning AppError
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data VProblem = VErr AppError | VWarn VWarning
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

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


newtype AppException = AppException AppError
    deriving stock (Show, Eq, Ord, Generic)

instance Exception AppException

newtype Validations = Validations (Map VPath (Set VProblem))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Monoid

instance Semigroup Validations where
    (Validations m1) <> (Validations m2) = Validations $ Map.unionWith (<>) m1 m2

newtype Path a = Path (NonEmpty Text) 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Semigroup

data PathKind = Validation | Metric
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    

type VPath      = Path 'Validation    
type MetricPath = Path 'Metric
    
newPath :: Text -> Path c
newPath u = Path $ u :| []

childPath :: Path c -> Text -> Path c
childPath (Path ts) t = Path $ t <| ts


getPath :: HasType a s => s -> a
getPath = getTyped

subPath :: Text -> Path c -> Path c
subPath t parent = newPath t <> parent

data ValidatorPath = ValidatorPath {
        validationPath :: VPath,
        metricPath     :: MetricPath
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

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



------------------------------------------------
------------ Metrics
------------------------------------------------

class Monoid metric => MetricC metric where
    -- lens to access the specific metric map in the total metric record    
    metricLens :: Lens' AppMetric (MetricMap metric)

newtype Count = Count { unCount :: Int64 }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise   
    deriving newtype (Num)
    deriving Semigroup via Sum Count
    deriving Monoid via Sum Count

newtype TimeTakenMs = TimeTakenMs Int64
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise    
    deriving newtype (Num)
    deriving Semigroup via Sum TimeTakenMs
    deriving Monoid via Sum TimeTakenMs

instance Show TimeTakenMs where 
    show (TimeTakenMs ms) = show ms

data RrdpSource = RrdpDelta | RrdpSnapshot
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise        

instance Monoid RrdpSource where
    mempty = RrdpSnapshot

instance Semigroup RrdpSource where
    _ <> r = r

data RrdpMetric = RrdpMetric {
        added       :: Count,
        deleted     :: Count,        
        rrdpSource  :: RrdpSource,
        timeTakenMs :: TimeTakenMs
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving Semigroup via GenericSemigroup RrdpMetric   
    deriving Monoid    via GenericMonoid RrdpMetric

data RsyncMetric = RsyncMetric {
        processed   :: Count,        
        timeTakenMs :: TimeTakenMs
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
        timeTakenMs     :: TimeTakenMs
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


newtype MetricMap a = MetricMap (Map MetricPath a)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    
    deriving newtype Monoid    

instance Semigroup a => Semigroup (MetricMap a) where
    MetricMap m1 <> MetricMap m2 = MetricMap $ Map.unionWith (<>) m1 m2

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
updateMetricInMap metricPath f (MetricMap mm) = 
    MetricMap $ Map.alter (Just . f . fromMaybe mempty) metricPath mm

lookupMetric :: MetricPath -> MetricMap a -> Maybe a
lookupMetric metricPath (MetricMap mm) = Map.lookup metricPath mm

-- Experimental more economical version of Validations

data VTree = 
    VLeaf (Set VProblem) | 
    VNode (Map Text VTree)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise    


instance Monoid VTree where
    mempty = VLeaf mempty

instance Semigroup VTree where
    t1 <> t2 = t1

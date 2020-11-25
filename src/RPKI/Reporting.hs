{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedLabels       #-}


module RPKI.Reporting where

import           Control.Lens                ((.~), (&), (%~))
import           Control.Exception.Lifted
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import qualified Data.ByteString             as BS
import           Data.Text                   (Text)

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

mError :: VTrail -> AppError -> Validations
mError vc w = mProblem vc (VErr w)

mWarning :: VTrail -> VWarning -> Validations
mWarning vc w = mProblem vc (VWarn w)

mProblem :: VTrail -> VProblem -> Validations
mProblem vc p = Validations $ Map.singleton vc $ Set.singleton p

emptyValidations :: Validations -> Bool 
emptyValidations (Validations m) = List.all Set.null $ Map.elems m  

findError :: Validations -> Maybe AppError
findError (Validations m) = 
    listToMaybe [ e | s <- Map.elems m, VErr e <- Set.toList s ]


newtype AppException = AppException AppError
    deriving stock (Show, Eq, Ord, Generic)

instance Exception AppException

newtype Validations = Validations (Map VTrail (Set VProblem))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Monoid

instance Semigroup Validations where
    (Validations m1) <> (Validations m2) = Validations $ Map.unionWith (<>) m1 m2


data Validation
data Metric

newtype Context a = Context (NonEmpty Text) 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Semigroup


type VTrail      = Context Validation    
type MetricTrail = Context Metric
    
trail :: Text -> Context c
trail u = Context $ u :| []

childContext :: Context c -> Text -> Context c
childContext (Context ts) t = Context $ t <| ts


getContext :: HasType a s => s -> a
getContext = getTyped

-- TODO Fix it
subTrail :: Text -> Context c -> Context c
subTrail t parent = trail t <> parent

data ValidatorContext = ValidatorContext {
        validationContext :: VTrail,
        metricContext     :: MetricTrail
    }
    deriving stock (Show, Eq, Ord, Generic)
    -- deriving anyclass Serialise

newValidatorContext :: Text -> ValidatorContext
newValidatorContext t = ValidatorContext {
        validationContext = trail t,
        metricContext     = trail t
    }


-- | Step down     
validatorSubContext :: Text -> ValidatorContext -> ValidatorContext
validatorSubContext t vc = 
    vc & typed @VTrail      %~ subTrail t
       & typed @MetricTrail %~ subTrail t




------------------------------------------------
------------ Metrics
------------------------------------------------

class MetricC m where
    metricKey :: m -> MetricTrail 

data RepoMetric = RepoMetric {
        repoUrl     :: RpkiURL,
        added       :: Int,
        deleted     :: Int,
        timeTakenMs :: Int
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data RrdpMetric = RrdpMetric {
        repoMetric :: RepoMetric,
        usedDetla :: Bool
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data RsyncMetric = RsyncMetric {
        repoMetric :: RepoMetric
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data ValidationMetric = ValidationMetric {
        taName          :: TaName,
        vrpNumber       :: Int,        
        validCertNumber :: Int,
        validRoaNumber  :: Int,
        validMftNumber  :: Int,
        validCrlNumber  :: Int,
        timeTakenMs     :: Int
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

instance MetricC RrdpMetric where
    metricKey RrdpMetric { repoMetric = RepoMetric {..}} = trail $ toText repoUrl

instance MetricC RsyncMetric where
    metricKey RsyncMetric { repoMetric = RepoMetric {..}} = trail $ toText repoUrl

instance MetricC ValidationMetric where
    metricKey ValidationMetric {..} = trail $ unTaName taName

data AppMetric = AppMetric {
        rsyncMetrics      :: Map MetricTrail RsyncMetric,
        rrdpMetrics       :: Map MetricTrail RrdpMetric,
        validationMetrics :: Map MetricTrail ValidationMetric
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

newVMetric :: TaName -> ValidationMetric
newVMetric taName = ValidationMetric taName 0 0 0 0 0 0


appValidationMetric :: ValidationMetric -> AppMetric
appValidationMetric = setMetric #validationMetrics

appRsyncMetric :: RsyncMetric -> AppMetric
appRsyncMetric = setMetric #rsyncMetrics

appRrdpMetric :: RrdpMetric -> AppMetric
appRrdpMetric = setMetric #rrdpMetrics


setMetric :: (Monoid s, MetricC a1) => ASetter s b a2 (Map MetricTrail a1) -> a1 -> b
setMetric theLens metric = mempty & theLens .~ Map.singleton (metricKey metric) metric


-- modifyAMetric :: AppMetric -> MetricTrail -> (AMetric -> Maybe AMetric) -> AppMetric
-- modifyAMetric am@(AppMetric ms) key f =
--     case Map.lookup key ms of 
--         Nothing -> am 
--         Just m  -> 
--             case f m of 
--                 Nothing -> am
--                 Just fm -> mMetric fm <> am


validationsToList :: Validations -> [(VTrail, Set VProblem)]
validationsToList (Validations vMap) = Map.toList vMap 


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

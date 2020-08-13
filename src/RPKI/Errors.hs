{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Errors where
    
import           Control.Exception.Lifted

import qualified Data.ByteString             as BS
import           Data.Text                   (Text)

import           Codec.Serialise
import qualified Data.List                   as List
import           Data.List.NonEmpty          (NonEmpty (..), (<|))
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set

import           Data.Generics.Product.Typed

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Time
import           RPKI.Resources.Types



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
                        CRLHashPointsToAnotherObject Hash Locations |
                        NextUpdateTimeNotSet |
                        NextUpdateTimeIsInThePast Instant |
                        ThisUpdateTimeIsInTheFuture Instant |
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
                DeltaHashMismatch Hash Hash Serial |
                NoObjectToReplace URI Hash |
                ObjectExistsWhenReplacing URI Hash |
                UnsupportedObjectType | 
                RrdpDownloadTimeout
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data RsyncError = RsyncProcessError Int Text |
                    FileReadError Text |
                    RsyncRunningError Text |
                    RsyncDirError Text |               
                    RsyncDownloadTimeout
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

newtype VContext = VContext (NonEmpty Text) 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data AppError = ParseE (ParseError Text) | 
                TAL_E TALError | 
                RrdpE RrdpError |
                RsyncE RsyncError |
                StorageE StorageError |                     
                ValidationE ValidationError |
                InitE InitError |
                UnspecifiedE Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype VWarning = VWarning AppError
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

vContext :: Text -> VContext
vContext u = VContext $ u :| []

childVContext :: VContext -> Text -> VContext
childVContext (VContext us) u = VContext $ u <| us

data VProblem = VErr AppError | VWarn VWarning
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Validations = Validations (Map VContext (Set VProblem))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Monoid

instance Semigroup Validations where
    (Validations m1) <> (Validations m2) = Validations $ Map.unionWith (<>) m1 m2

mError :: VContext -> AppError -> Validations
mError vc e = Validations $ Map.singleton vc $ Set.fromList [VErr e]

mWarning :: VContext -> VWarning -> Validations
mWarning vc w = Validations $ Map.singleton vc $ Set.fromList [VWarn w]

emptyValidations :: Validations -> Bool 
emptyValidations (Validations m) = List.all Set.null $ Map.elems m  

class WithVContext v where
    getVC :: v -> VContext
    childVC :: Text -> v -> v

instance {-# OVERLAPPING #-} WithVContext VContext where
    getVC = id
    childVC = flip childVContext

-- TODO This one needs UnddecidableInstances which is probably bad, 
-- so do something about it
instance (Generic a, HasType VContext a) => WithVContext a where 
    getVC = getTyped
    childVC u v = setTyped (childVContext (getVC v) u) v

newtype AppException = AppException AppError
    deriving stock (Show, Eq, Ord, Generic)

instance Exception AppException
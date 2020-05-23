{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Errors where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)

import           Codec.Serialise
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List          as List
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import Data.Generics.Product.Typed

import           Data.Hourglass       (DateTime)

import           GHC.Generics

import           RPKI.Domain
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
                        NoCRLExists AKI Locations |
                        CRLHashPointsToAnotherObject Hash Locations |
                        NextUpdateTimeNotSet |
                        NextUpdateTimeIsInThePast DateTime |
                        ThisUpdateTimeIsInTheFuture DateTime |
                        RevokedEECertificate |
                        RevokedResourceCertificate |
                        CertificateIsInTheFuture |
                        CertificateIsExpired |
                        AKIIsNotEqualsToParentSKI (Maybe AKI) SKI|
                        ManifestEntryDontExist Hash |
                        OverclaimedResources PrefixesAndAsns |
                        InheritWithoutParentResources |
                        UnknownUriType URI | 
                        CertificateDoesn'tHaveSIA | 
                        PublicationPointIsNotAvailable URI
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    
data RrdpError = BrokenXml Text | 
                BrokenSerial BS.ByteString |
                NoSessionId |
                NoSerial | 
                NoSnapshotHash | 
                NoSnapshotURI | 
                NoDeltaSerial | 
                NoDeltaURI | 
                NoDeltaHash |
                BadHash BS.ByteString |
                NoVersion | 
                BadVersion BS.ByteString | 
                NoPublishURI |
                BadBase64 Text BS.ByteString |
                BadPublish BS.ByteString |
                NoHashInWithdraw |
                ContentInWithdraw BS.ByteString |
                LocalSerialBiggerThanRemote Serial Serial |
                NonConsecutiveDeltaSerials [(Serial, Serial)] |
                CantDownloadNotification String |
                CantDownloadSnapshot String |
                CantDownloadDelta String |
                SnapshotHashMismatch Hash Hash |
                DeltaHashMismatch Hash Hash Serial |
                NoObjectToReplace URI Hash |
                ObjectExistsWhenReplacing URI Hash
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data RsyncError = RsyncProcessError Int LBS.ByteString |
                    FileReadError Text |
                    RsyncRunningError Text |
                    RsyncDirError Text
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

newtype VContext = VContext (NonEmpty URI) 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data AppError = ParseE (ParseError Text) | 
                TAL_E TALError | 
                RrdpE RrdpError |
                RsyncE RsyncError |
                StorageE StorageError |                     
                ValidationE ValidationError |
                UnspecifiedE Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype VWarning = VWarning AppError
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

vContext :: URI -> VContext
vContext u = VContext $ u :| []

childVContext :: VContext -> URI -> VContext
childVContext (VContext us) u = VContext $ u <| us

data VProblem = VErr AppError | VWarn VWarning
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Validations = Validations (Map VContext (Set VProblem))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid)

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
    childVC :: URI -> v -> v

instance {-# OVERLAPPING #-} WithVContext VContext where
    getVC = id
    childVC = flip childVContext

instance (Generic a, HasType VContext a) => WithVContext a where 
    getVC = getTyped
    childVC u v = setTyped (childVContext (getVC v) u) v
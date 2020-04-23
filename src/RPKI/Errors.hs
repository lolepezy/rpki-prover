{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Errors where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text

import           Codec.Serialise
import           Control.Exception
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List          as List
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import           Data.Hourglass       (DateTime)

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Resources.Types


newtype ParseError s = ParseError s
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data ValidationError = InvalidCert !Text.Text |
            ParentDoesntHaveResources |
            NoAKIinManifest |
            ROACannotBeAParent |
            NoAKI | 
            SPKIMismatch !EncodedBase64 !EncodedBase64 |
            UnknownObjectAsTACert |
            ObjectIsTooSmall !Integer |
            ObjectIsTooBig !Integer |
            TACertificateLocalIsNewer !Serial !Serial |
            InvalidSignature !Text.Text |                        
            TACertAKIIsNotEmpty !URI |
            CertNoPolicyExtension |
            CertWrongPolicyExtension !BS.ByteString |
            NoMFT !AKI !Locations |
            NoMFTNoRepository !AKI !Locations |
            NoCRLOnMFT !AKI !Locations |
            MoreThanOneCRLOnMFT !AKI !Locations |
            NoCRLExists !AKI !Locations |
            CRLHashPointsToAnotherObject !Hash !Locations |
            NextUpdateTimeNotSet |
            NextUpdateTimeIsBeforeNow !DateTime |
            RevokedEECertificate |
            RevokedResourceCertificate |
            CertificateIsInTheFuture |
            CertificateIsExpired |
            AKIIsNotEqualsToParentSKI !(Maybe AKI) !SKI|
            ManifestEntryDontExist !Hash |
            OverclaimedResources PrefixesAndAsns |
            InheritWithoutParentResources |
            UnknownUriType !URI | 
            CertificateDoesn'tHaveSIA
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    
data RrdpError = BrokenXml !Text.Text | 
                BrokenSerial !BS.ByteString |
                NoSessionId |
                NoSerial | 
                NoSnapshotHash | 
                NoSnapshotURI | 
                NoDeltaSerial | 
                NoDeltaURI | 
                NoDeltaHash |
                BadHash !BS.ByteString |
                NoVersion | 
                BadVersion !BS.ByteString | 
                NoPublishURI |
                BadBase64 String !BS.ByteString |
                BadPublish !BS.ByteString |
                NoHashInWithdraw |
                ContentInWithdraw !BS.ByteString |
                LocalSerialBiggerThanRemote !Serial !Serial |
                NonConsecutiveDeltaSerials ![(Serial, Serial)] |
                CantDownloadNotification !String |
                CantDownloadSnapshot !String |
                CantDownloadDelta !String |
                SnapshotHashMismatch !Hash !Hash |
                DeltaHashMismatch !Hash !Hash !Serial |
                NoObjectToReplace !URI !Hash |
                ObjectExistsWhenReplacing !URI !Hash
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data RsyncError = RsyncProcessError !Int !LBS.ByteString |
                    FileReadError !Text.Text |
                    RsyncDirError !Text.Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data StorageError = StorageError !Text.Text |
                    DeserialisationError !Text.Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype TALError = TALError Text.Text 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Semigroup

newtype VContext = VContext (NonEmpty URI) 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

data AppError = ParseE (ParseError Text.Text) | 
                    TAL_E TALError | 
                    RrdpE RrdpError |
                    RsyncE RsyncError |
                    StorageE StorageError |                     
                    ValidationE ValidationError |
                    UnspecifiedE Text.Text
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

instance Exception AppError

newtype VWarning = VWarning AppError
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

vContext :: URI -> VContext
vContext u = VContext $ u :| []

childVContext :: VContext -> URI -> VContext
childVContext (VContext us) u = VContext $ u `NonEmpty.cons` us

data VProblem = VErr !AppError | VWarn !VWarning
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

newtype Validations = Validations (Map VContext (Set VProblem))
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype (Monoid, Semigroup)

mError :: VContext -> AppError -> Validations
mError vc e = Validations $ Map.singleton vc $ Set.fromList [VErr e]

mWarning :: VContext -> VWarning -> Validations
mWarning vc w = Validations $ Map.singleton vc $ Set.fromList [VWarn w]

emptyValidations :: Validations -> Bool 
emptyValidations (Validations m) = List.all Set.null $ Map.elems m  
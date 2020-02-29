{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Errors where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           Codec.Serialise
import           Control.DeepSeq
import           Control.Exception
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           Data.Data            (Typeable)
import           Data.Hourglass       (DateTime)

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Resources.Types


newtype ParseError s = ParseError s
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data ValidationError = InvalidCert !T.Text |
            ParentDoesntHaveResources |
            NoAKIinManifest |
            ROACannotBeAParent |
            NoAKI | 
            SPKIMismatch !EncodedBase64 !EncodedBase64 |
            UnknownObjectAsTACert |
            ObjectIsTooSmall !Integer |
            ObjectIsTooBig !Integer |
            TACertificateLocalIsNewer !Serial !Serial |
            InvalidSignature !T.Text |                        
            TACertAKIIsNotEmpty !URI |
            CertNoPolicyExtension |
            CertWrongPolicyExtension !B.ByteString |
            NoMFT !AKI !Locations |
            NoCRLOnMFT !AKI !Locations |
            MoreThanOneCRLOnMFT !AKI !Locations |
            NoCRLExists !AKI !Locations |
            CRLHashPointsToAnotherObject !Hash !Locations |
            NextUpdateTimeNotSet |
            NextUpdateTimeIsBeforeNow !DateTime |
            RevokedEECertificate |
            RevokedResourceCertificate |
            AKIIsNotEqualsToParentSKI !(Maybe AKI) !SKI|
            ManifestEntryDontExist !Hash |
            OverclaimedResources PrefixesAndAsns |
            InheritWithoutParentResources 
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)
    
data RrdpError = BrokenXml !T.Text | 
                BrokenSerial !B.ByteString |
                NoSessionId |
                NoSerial | 
                NoSnapshotHash | 
                NoSnapshotURI | 
                NoDeltaSerial | 
                NoDeltaURI | 
                NoDeltaHash |
                BadHash !B.ByteString |
                NoVersion | 
                BadVersion !B.ByteString | 
                NoPublishURI |
                BadBase64 String !B.ByteString |
                BadPublish !B.ByteString |
                NoHashInWithdraw |
                ContentInWithdraw !B.ByteString |
                LocalSerialBiggerThanRemote !Serial !Serial |
                NonConsecutiveDeltaSerials ![(Serial, Serial)] |
                CantDownloadNotification !String |
                CantDownloadSnapshot !String |
                CantDownloadDelta !String |
                SnapshotHashMismatch !Hash !Hash |
                DeltaHashMismatch !Hash !Hash !Serial
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data RsyncError = RsyncProcessError !Int !BL.ByteString |
                    FileReadError !T.Text |
                    RsyncDirError !T.Text
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data StorageError = StorageError !T.Text |
                    DeserialisationError !T.Text
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

newtype TALError = TALError T.Text 
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)
    deriving newtype Semigroup

newtype VContext = VContext (NE.NonEmpty URI) 
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data SomeError = ParseE (ParseError T.Text) | 
                    TAL_E TALError | 
                    RrdpE RrdpError |
                    RsyncE RsyncError |
                    StorageE StorageError | 
                    ValidationE ValidationError
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

instance Exception SomeError

newtype VWarning = VWarning SomeError
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

vContext :: URI -> VContext
vContext u = VContext $ u :| []

childVContext :: VContext -> URI -> VContext
childVContext (VContext us) u = VContext $ u `NE.cons` us
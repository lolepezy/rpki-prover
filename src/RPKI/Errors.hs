{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Errors where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

import           Data.List.NonEmpty

import           Control.DeepSeq
import           Control.Exception

import           Data.Data            (Typeable)
import           Data.Hourglass       (DateTime)

import           GHC.Generics

import           RPKI.Domain



newtype ParseError s = ParseError s
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data ValidationError = InvalidCert !T.Text |
                        ParentDoesntHaveResources |
                        NoAKIinManifest |
                        ROACannotBeAParent |
                        NoAKI | 
                        SPKIMismatch !EncodedBase64 !EncodedBase64 |
                        UnknownObjectAsTACert |
                        TACertificateIsTooSmall !Int |
                        TACertificateIsTooBig !Int |
                        TACertificateLocalIsNewer !Serial !Serial |
                        InvalidSignature !T.Text |                        
                        TACertAKIIsNotEmpty !URI |
                        CertNoPolicyExtension |
                        CertWrongPolicyExtension !B.ByteString |
                        NoMFT !AKI !(NonEmpty URI) |
                        NoCRLOnMFT !AKI !(NonEmpty URI) |
                        MoreThanOneCRLOnMFT !AKI !(NonEmpty URI) |
                        NoCRLExists !AKI !(NonEmpty URI) |
                        CRLHashPointsToAnotherObject !Hash !(NonEmpty URI) |
                        NextUpdateTimeNotSet |
                        NextUpdateTimeIsBeforeNow !DateTime |
                        RevokedEECertificate !(NonEmpty URI)
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)
    
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
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data RsyncError = RsyncProcessError !Int !BL.ByteString |
                  FileReadError !T.Text |
                  RsyncDirError !T.Text
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data StorageError = StorageError T.Text 
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

newtype TALError = TALError T.Text 
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)
    deriving newtype Semigroup

newtype ValidationContext = ValidationContext T.Text 
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data SomeError = ParseE (ParseError T.Text) | 
                   TAL_E TALError | 
                   RrdpE RrdpError |
                   RsyncE RsyncError |
                   StorageE StorageError | 
                   ValidationE ValidationError
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

instance Exception SomeError

newtype ValidationWarning = ValidationWarning SomeError
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

vContext :: URI -> ValidationContext
vContext (URI u) = ValidationContext u
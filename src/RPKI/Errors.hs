{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Errors where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Control.DeepSeq
import Control.Exception

import Data.Data (Typeable)

import GHC.Generics

import RPKI.Domain


newtype ParseError s = ParseError s
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data ValidationError = InvalidCert !T.Text |
                        ParentDoesntHaveResources |
                        NoAKIinManifest |
                        ROACannotBeAParent |
                        NoAKI | 
                        SPKIMismatch EncodedBase64 EncodedBase64 |
                        UnknownObjectAsTACert |
                        TACertificateIsTooSmall !Int |
                        TACertificateIsTooBig !Int |
                        InvalidSignature !T.Text |
                        AKIIsNotEmpty |
                        CertNoPolicyExtension |
                        CertNoWrongPolicyExtension B.ByteString
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
                 LocalSerialBiggerThanRemote Serial Serial |
                 NonConsecutiveDeltaSerials [(Serial, Serial)] |
                 CantDownloadNotification String |
                 CantDownloadSnapshot String |
                 CantDownloadDelta String |
                 SnapshotHashMismatch Hash Hash |
                 DeltaHashMismatch Hash Hash Serial
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

data SomeError = ParseE (ParseError T.Text) | 
                   TAL_E TALError | 
                   RrdpE RrdpError |
                   RsyncE RsyncError |
                   StorageE StorageError | 
                   ValidationE ValidationError |
                   ComposeE [SomeError]
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)

instance Exception SomeError

newtype ValidationWarning = ValidationWarning T.Text
    deriving (Show, Eq, Ord, Typeable, Generic, NFData)
    deriving newtype (Monoid, Semigroup)

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}

module RPKI.RRDP.Types where

import qualified Data.ByteString as BS
import           GHC.Generics
import           RPKI.Domain
import           RPKI.Store.Base.Serialisation


-- NOTE: All the data here is strict

newtype RrdpSerial = RrdpSerial Integer
    deriving stock (Eq, Ord, Generic)
    deriving anyclass TheBinary

instance Show RrdpSerial where
    show (RrdpSerial s) = show s

data Notification = Notification {
    version      :: Version,
    sessionId    :: SessionId,
    serial       :: RrdpSerial,
    snapshotInfo :: SnapshotInfo,
    deltas       :: [DeltaInfo]
} deriving stock (Show, Eq, Ord, Generic)

data SnapshotInfo = SnapshotInfo URI Hash
    deriving stock (Show, Eq, Ord, Generic)

data SnapshotPublish = SnapshotPublish URI EncodedBase64
    deriving stock (Show, Eq, Ord, Generic)

data Snapshot = Snapshot Version SessionId RrdpSerial [SnapshotPublish]
    deriving stock (Show, Eq, Ord, Generic)

data DeltaInfo = DeltaInfo URI Hash RrdpSerial
    deriving stock (Show, Eq, Ord, Generic)

data DeltaItem = DP DeltaPublish | DW DeltaWithdraw
    deriving stock (Show, Eq, Ord, Generic)

data DeltaPublish = DeltaPublish URI (Maybe Hash) EncodedBase64
    deriving stock (Show, Eq, Ord, Generic)

data DeltaWithdraw = DeltaWithdraw URI Hash
    deriving stock (Show, Eq, Ord, Generic)

data Delta = Delta Version SessionId RrdpSerial [DeltaItem]
    deriving stock (Show, Eq, Ord, Generic)

newtype ETag = ETag BS.ByteString
    deriving stock (Show, Eq, Ord, Generic)    
    deriving anyclass TheBinary        


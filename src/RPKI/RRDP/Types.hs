{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StrictData #-}

module RPKI.RRDP.Types where

import           GHC.Generics
import           Codec.Serialise
import           RPKI.Domain


-- NOTE: All the data here is strict

newtype RrdpSerial = RrdpSerial Integer
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise

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


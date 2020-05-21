{-# LANGUAGE DerivingStrategies #-}

module RPKI.RRDP.Types where

import           GHC.Generics
import           RPKI.Domain

data Notification = Notification {
    version      :: Version,
    sessionId    :: !SessionId,
    serial       :: !Serial,
    snapshotInfo :: !SnapshotInfo,
    deltas       :: [DeltaInfo]
} deriving (Show, Eq, Ord, Generic)

data SnapshotInfo = SnapshotInfo !URI !Hash
    deriving stock (Show, Eq, Ord, Generic)

data SnapshotPublish = SnapshotPublish !URI !EncodedBase64
    deriving stock (Show, Eq, Ord, Generic)

data Snapshot = Snapshot !Version !SessionId !Serial [SnapshotPublish]
    deriving stock (Show, Eq, Ord, Generic)

data DeltaInfo = DeltaInfo !URI !Hash !Serial
    deriving stock (Show, Eq, Ord, Generic)

data DeltaItem = DP DeltaPublish | DW DeltaWithdraw
    deriving stock (Show, Eq, Ord, Generic)

data DeltaPublish = DeltaPublish !URI !(Maybe Hash) !EncodedBase64
    deriving stock (Show, Eq, Ord, Generic)

data DeltaWithdraw = DeltaWithdraw !URI !Hash
    deriving stock (Show, Eq, Ord, Generic)

data Delta = Delta !Version !SessionId !Serial [DeltaItem]
    deriving stock (Show, Eq, Ord, Generic)


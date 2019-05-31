{-# LANGUAGE DuplicateRecordFields #-}

module RPKI.RRDP.Types where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

import           RPKI.Domain

data Notification = Notification {
  sessionId    :: !SessionId,
  serial       :: !Serial,
  snapshotUri  :: !URI,
  snapshotHash :: !Hash,
  deltas       :: [DeltaInfo]
}

newtype Content = Content B.ByteString deriving (Show, Eq, Ord)

data SnapshotInfo = SnapshotInfo !URI !Hash
  deriving (Show, Eq, Ord)

data SnapshotPublish = SnapshotPublish !URI !Content
  deriving (Show, Eq, Ord)

data Snapshot = Snapshot !Version !SessionId !Serial [SnapshotPublish]
  deriving (Show, Eq, Ord)

data DeltaInfo = DeltaInfo !URI !Hash !Serial
  deriving (Show, Eq, Ord)

data DeltaEntry = DP DeltaPublish | DW DeltaWithdraw
  deriving (Show, Eq, Ord)

data DeltaPublish = DeltaPublish !URI !(Maybe Hash) !Content
  deriving (Show, Eq, Ord)

data DeltaWithdraw = DeltaWithdraw !URI !Hash
  deriving (Show, Eq, Ord)

data Delta = Delta !DeltaInfo [DeltaEntry]
  deriving (Show, Eq, Ord)

{-# LANGUAGE DuplicateRecordFields #-}

module RPKI.RRDP.Types where

import qualified Data.ByteString.Lazy as BL
import           RPKI.Domain

data Notification = Notification {
  sessionId    :: !SessionId,
  serial       :: !Serial,
  snapshotUri  :: !URI,
  snapshotHash :: !Hash,
  deltas       :: [DeltaInfo]
}

data SnapshotInfo = SnapshotInfo !URI !Hash

data SnapshotPublish = SnapshotPublish !URI !BL.ByteString

data Snapshot = Snapshot !SnapshotInfo [SnapshotPublish]

data DeltaInfo = DeltaInfo !URI !Hash !Serial

data DeltaEntry = DP DeltaPublish | DW DeltaWithdraw

data DeltaPublish = DeltaPublish !URI !(Maybe Hash) !BL.ByteString
data DeltaWithdraw = DeltaWithdraw !URI !Hash

data Delta = Delta !DeltaInfo [DeltaEntry]

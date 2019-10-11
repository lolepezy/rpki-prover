{-# LANGUAGE DeriveAnyClass #-}

module RPKI.RRDP.Types where

import           Control.DeepSeq
import qualified Data.ByteString      as B
import           GHC.Generics

import           RPKI.Domain

data Notification = Notification {
  version      :: Version,
  sessionId    :: !SessionId,
  serial       :: !Serial,
  snapshotInfo :: !SnapshotInfo,
  deltas       :: [DeltaInfo]
} deriving (Show, Eq, Ord, Generic, NFData)

newtype EncodedBase64 = EncodedBase64 B.ByteString
  deriving (Show, Eq, Ord, Generic, Monoid, Semigroup, NFData)

newtype DecodedBase64 = DecodedBase64 B.ByteString
  deriving (Show, Eq, Ord, Generic, Monoid, Semigroup, NFData)

data SnapshotInfo = SnapshotInfo !URI !Hash
  deriving (Show, Eq, Ord, Generic, NFData)

data SnapshotPublish = SnapshotPublish !URI !EncodedBase64
  deriving (Show, Eq, Ord, Generic, NFData)

data Snapshot = Snapshot !Version !SessionId !Serial [SnapshotPublish]
  deriving (Show, Eq, Ord, Generic, NFData)

data DeltaInfo = DeltaInfo !URI !Hash !Serial
  deriving (Show, Eq, Ord, Generic, NFData)

data DeltaItem = DP DeltaPublish | DW DeltaWithdraw
  deriving (Show, Eq, Ord, Generic, NFData)

data DeltaPublish = DeltaPublish !URI !(Maybe Hash) !EncodedBase64
  deriving (Show, Eq, Ord, Generic, NFData)

data DeltaWithdraw = DeltaWithdraw !URI !Hash
  deriving (Show, Eq, Ord, Generic, NFData)

data Delta = Delta !Version !SessionId !Serial [DeltaItem]
  deriving (Show, Eq, Ord, Generic, NFData)


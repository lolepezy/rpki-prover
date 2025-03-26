
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.IPC.Types where

import Data.Text

import GHC.Generics (Generic)

import System.Posix.Types

import RPKI.Metrics.System
import RPKI.Store.Base.Serialisation
import RPKI.Time


data LogLevel = ErrorL | WarnL | InfoL | DebugL
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary)

instance Show LogLevel where
    show = \case 
        ErrorL -> "Error"
        WarnL  -> "Warn"
        InfoL  -> "Info"
        DebugL -> "Debug"

data LogMessage = LogMessage {
        logLevel  :: LogLevel,
        message   :: Text,
        processId :: ProcessID,
        timestamp :: Instant
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data IpcMessage = LogIpc LogMessage 
                | RtrLogIpc LogMessage 
                | SystemIpc SystemMetrics
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)


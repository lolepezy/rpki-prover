{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.IPC.Types where

import GHC.Generics (Generic)

import System.Posix.Types

import RPKI.Metrics.System
import RPKI.Store.Base.Serialisation
import RPKI.Time
import RPKI.Logging.Types

data IpcMessage = LogIpc LogMessage 
                | RtrLogIpc LogMessage 
                | SystemIpc SystemMetrics
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

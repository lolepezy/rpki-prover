{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.IPC.Types where

import GHC.Generics (Generic)

import System.Posix.Types

import RPKI.Domain
import RPKI.Metrics.System
import RPKI.Store.Base.Serialisation
import RPKI.Time
import RPKI.Logging.Types

newtype CommandId = CommandId { unCommandId :: Integer }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (TheBinary)

data Command = Command {
        commandId   :: CommandId,
        commandSpec :: CommandSpec
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data CommandSpec = RunFetcher RpkiURL Timebox 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data CommandResult = CommandResult {
        commandId :: CommandId,
        result    :: CommandResultSpec
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data CommandResultSpec = RunFetcherR RunFetcherResult
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)


data RunFetcherResult = RunFetcherResult {        
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data IpcMessage = LogIpc LogMessage 
                | RtrLogIpc LogMessage 
                | SystemIpc SystemMetrics
                | CommandIpc Command
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

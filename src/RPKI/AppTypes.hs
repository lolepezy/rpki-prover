{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppTypes where
    
import           Control.Concurrent.STM

import           Codec.Serialise
import           Data.Int
import           GHC.Generics

import           Data.Set
import           RPKI.Domain
-- import           RPKI.SLURM.Types

-- It's a sequence of versions that is equal to some monotonic  
-- clock timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data VersionState = NewVersion | CompletedVersion
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data WorldState = WorldState WorldVersion VersionState
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

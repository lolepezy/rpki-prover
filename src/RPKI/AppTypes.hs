{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppTypes where
import           Codec.Serialise
import           Data.Int
import           GHC.Generics

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

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppTypes where

import           Codec.Serialise
import           Data.Int
import           Data.Text (Text)
import           GHC.Generics

-- It's a sequence of versions that is equal to some monotonic  
-- clock timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

-- Keep it just text for now since we don't
-- know what may be needed there in the future.
data VersionState = VersionState Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData         #-}

module RPKI.AppTypes where
    
import           Data.Int
import           Data.Text (Text)
import           Data.Semigroup
import           GHC.Generics

import           RPKI.Store.Base.Serialisation

-- It's a sequence of versions that is equal to some monotonic  
-- clock timestamp in nanoseconds.
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

-- Keep it just text for now since we don't
-- know what may be needed there in the future.
data VersionState = VersionState Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)


-- TODO Probably move it to some other module
newtype MaxMemory = MaxMemory { unMaxMemory :: Int }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    
    deriving newtype (Num, Bounded)
    deriving Semigroup via Max MaxMemory
    deriving Monoid via Max MaxMemory    
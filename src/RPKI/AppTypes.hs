{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.AppTypes where
    
import           Control.DeepSeq
import           Data.Int
import           Data.Text (Text)
import           Data.Semigroup
import           GHC.Generics

import           RPKI.Store.Base.Serialisation

{- 
    Sequence of versions that are equal to some monotonic clock timestamp in nanoseconds. 
    Used to attribute pretty much everything (validation runs, (async) repository fetches, 
    etc.) to. Every validation happens for a specific world version which is also used 
    as 'now' for validity period comparisons. Also, most of the data in the cache is
    associated with a world version (VRPs, metrics, SLURM data, etc.).
   -}
newtype WorldVersion = WorldVersion Int64
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

instance Show WorldVersion where 
    show (WorldVersion v) = show v

-- Keep it just text for now since we don't
-- know what may be needed there in the future.
data VersionKind = VersionKind Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

-- Version of the executable, ideally it is supposed to 
-- be different for every build of the program where 
-- source or library versions are different.
newtype ExecutableVersion = ExecutableVersion Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)


-- TODO Probably move it to some other module
newtype MaxMemory = MaxMemory Int
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary)    
    deriving newtype (Num, Bounded)
    deriving Semigroup via Max MaxMemory
    deriving Monoid via Max MaxMemory    

instance Show MaxMemory where 
    show (MaxMemory m) = show (m `div` (1024*1024)) <> "mb"


validationKind, generalKind, asyncFetchKind :: VersionKind
validationKind = VersionKind "validation"
generalKind = VersionKind "general"
asyncFetchKind = VersionKind "async-fetch"

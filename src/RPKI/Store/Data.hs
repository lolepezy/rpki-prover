{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.Data where

import           Codec.Serialise
import           GHC.Generics

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.TAL


class WithKey a k where
    key :: a -> k

data VProblem = VErr SomeError | VWarn VWarning
    deriving (Show, Eq, Generic, Serialise)

-- TODO Add versioning here
data VResult = VResult {
    problem :: ![VProblem],
    path    :: !VContext
} deriving (Show, Eq, Generic, Serialise)


data StoredTA = StoredTA {
    tal        :: !TAL,
    taCert     :: !CerObject
} deriving (Show, Eq, Generic, Serialise)


data RepositoryStatus = NEW | FAILED | COMPLETED
    deriving (Show, Eq, Generic, Serialise)

data SRepository = SRepository {
    repo   :: !Repository,    
    status :: !RepositoryStatus
} deriving (Show, Eq, Generic, Serialise)


instance WithKey SRepository URI where
    key (SRepository r _) = repositoryURI r

instance WithKey VResult VContext where
    key vr = path vr

instance WithKey StoredTA Locations where
    key ta = certLocations $ tal ta
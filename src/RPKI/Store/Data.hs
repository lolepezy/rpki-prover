{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.Data where

import           Codec.Serialise
import           GHC.Generics

import Data.List.NonEmpty

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.TAL


class WithKey a k where
    key :: a -> k

-- TODO Add versioning here
data VResult = VResult {
    problem :: ![VProblem],
    path    :: !VContext
} deriving (Show, Eq, Ord, Generic, Serialise)


data STA = STA {
    tal                 :: !TAL,
    taCert              :: !CerObject,
    initialRepositories :: NonEmpty Repository
} deriving (Show, Eq, Generic, Serialise)


data RepositoryStatus = NEW | FAILED | FETCHED
    deriving (Show, Eq, Ord, Generic, Serialise)

data SRepository = SRepository {
    repo   :: !Repository,    
    status :: !RepositoryStatus
} deriving (Show, Eq, Ord, Generic, Serialise)


instance WithKey SRepository URI where
    key (SRepository r _) = repositoryURI r

instance WithKey VResult VContext where
    key vr = path vr

instance WithKey STA Locations where
    key ta = certLocations $ tal ta

{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module RPKI.Store.Data where

import           Codec.Serialise
import           GHC.Generics

import           Data.List.NonEmpty

import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Repository
import           RPKI.TAL

data StorableTA = StorableTA {
    tal                 :: TAL,
    taCert              :: CerObject,
    fetchStatus         :: FetchStatus,
    initialRepositories :: NonEmpty Repository
} deriving (Show, Eq, Generic, Serialise)

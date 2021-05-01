{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}

module RPKI.CommonTypes where

import           Codec.Serialise
import GHC.Generics

import Data.Map.Strict (Map)
import qualified Data.Map.Strict                  as Map


newtype MonoidMap k v = MonoidMap { unMonoidMap :: Map k v }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Monoid

instance (Ord k, Monoid v) => Semigroup (MonoidMap k v) where
    (MonoidMap w1) <> (MonoidMap w2) = MonoidMap $ Map.unionWith (<>) w1 w2

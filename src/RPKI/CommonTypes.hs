{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingVia #-}

module RPKI.CommonTypes where

import           Codec.Serialise
import GHC.Generics
import           Data.Monoid.Generic
import Data.Map.Strict (Map)
import qualified Data.Map.Strict                  as Map

data T2 a b = T2 a b
    deriving stock (Show, Eq, Ord, Generic)
    deriving Semigroup via GenericSemigroup (T2 a b)
    deriving Monoid    via GenericMonoid (T2 a b)
    
data T3 a b c = T3 a b c
    deriving stock (Show, Eq, Ord, Generic)
    deriving Semigroup via GenericSemigroup (T3 a b c)
    deriving Monoid    via GenericMonoid (T3 a b c)
    

newtype MonoidMap k v = MonoidMap { unMonoidMap :: Map k v }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass Serialise
    deriving newtype Monoid

instance (Ord k, Monoid v) => Semigroup (MonoidMap k v) where
    (MonoidMap w1) <> (MonoidMap w2) = MonoidMap $ Map.unionWith (<>) w1 w2

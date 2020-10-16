{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module RPKI.StrictTuple where

import GHC.Generics

data T2 a b = T2 a b
    deriving stock (Show, Eq, Ord, Generic)
    
instance (Monoid a, Monoid b) => Monoid (T2 a b) where
    mempty = T2 mempty mempty

instance (Semigroup a, Semigroup b) => Semigroup (T2 a b) where
    T2 a1 b1 <> T2 a2 b2 = T2 (a1 <> a2) (b1 <> b2)

data T3 a b c = T3 a b c
    deriving stock (Show, Eq, Ord, Generic)
    
instance (Monoid a, Monoid b, Monoid c) => Monoid (T3 a b c) where
    mempty = T3 mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (T3 a b c) where
    T3 a1 b1 c1 <> T3 a2 b2 c2 = T3 (a1 <> a2) (b1 <> b2) (c1 <> c2)

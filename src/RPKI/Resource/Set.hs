{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Resource.Set where

import           Codec.Serialise

import           Data.Data              (Typeable)
import           Data.List
import qualified Data.Vector            as V
import           GHC.Generics

import           RPKI.Resource.Resource


-- | Small set representation. In practice most of the resource 
-- set are of the size 1-3, so we are trying to save some bytes here and there
data SmallSet a = EmptyS 
                | OneS 
                    {-# UNPACK #-} !a 
                | TwoS
                    {-# UNPACK #-} !a 
                    {-# UNPACK #-} !a 
                | ThreeS
                    {-# UNPACK #-} !a 
                    {-# UNPACK #-} !a 
                    {-# UNPACK #-} !a 
                | FourS
                    {-# UNPACK #-} !a 
                    {-# UNPACK #-} !a 
                    {-# UNPACK #-} !a 
                    {-# UNPACK #-} !a 
                | ManyS !(V.Vector a)
  deriving (Show, Eq, Ord, Typeable, Generic)


fromList :: Ord a => [a] -> SmallSet a
fromList = \case
  []               -> EmptyS
  [a1]             -> OneS a1
  [a1, a2]         -> TwoS a1 a2
  [a1, a2, a3]     -> ThreeS a1 a2 a3
  [a1, a2, a3, a4] -> FourS a1 a2 a3 a4
  rs               -> ManyS $ V.fromList $ sort rs

empty :: SmallSet a
empty = EmptyS

instance Serialise a => Serialise (SmallSet a)
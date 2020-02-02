{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Common.SmallSet where

import Prelude hiding (filter, subtract, null)

import           Codec.Serialise

import           Data.Data                             (Typeable)
import           Data.Bits
import           Data.Maybe
import           Data.Either
import           Data.Kind
import qualified Data.List                             as L
import qualified Data.Vector                           as V
import           Data.Word
import           GHC.Generics

data SmallSet a = SS !(V.Vector a)
  deriving stock (Show, Eq, Ord, Typeable, Generic) 
  deriving anyclass Serialise

fromList :: [a] -> SmallSet a
fromList = SS . V.fromList

toList :: SmallSet a -> [a]
toList (SS v) = V.toList v

toVector :: SmallSet a -> V.Vector a
toVector (SS v) = v

empty :: SmallSet a
empty = fromList []

find :: (a -> Bool) -> SmallSet a -> Maybe a
find p (SS v) = V.find p v    

filter :: (a -> Bool) -> SmallSet a -> SmallSet a
filter p (SS v) = SS $ V.filter p v    

elem :: Eq a => a -> SmallSet a -> Bool   
elem a = isJust . find (==a)

null :: SmallSet a -> Bool
null (SS v) = V.null v
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Resources.IntervalSet where

import           Codec.Serialise

import           Common.SmallSet          (SmallSet(..))
import qualified Common.SmallSet          as SmallSet

import qualified Data.Vector              as V
import           Data.Kind
import           GHC.Generics
import           RPKI.Resources.Resources


class (WithSetOps p, Eq (Point p), Ord (Point p)) => Interval p where
  type Point p :: Type
  start :: p -> (Point p)


instance Interval Ipv4Prefix where
  type Point Ipv4Prefix = Address Ipv4Prefix
  start = startV4

instance Interval Ipv6Prefix where
  type Point Ipv6Prefix = Address Ipv6Prefix
  start = startV6

instance Interval AsResource where
  type Point AsResource = ASN
  start (AS a) = a
  start (ASRange a _) = a


-- | Representation of the resource set
newtype IntervalSet a = IntervalSet (SmallSet a) 
  deriving stock (Show, Eq, Ord, Generic) 
  deriving anyclass Serialise

fromList :: WithSetOps a => [a] -> IntervalSet a
fromList = IntervalSet . SmallSet.fromList . normalise 


findIntersections :: Interval a => a -> IntervalSet a -> [a]
findIntersections a (IntervalSet (SS v)) = 
  case (V.unsafeIndex v 0) `intersection` a of
      [] -> case (V.unsafeIndex v (V.length v - 1)) `intersection` a of
        [] -> goBinary 0 lastIndex
        _  -> goBackwards lastIndex
      _  -> goForward 0
  where 
    goBinary b e
      | e <= b = []
      | otherwise =           
          case middle `intersection` a of
            [] -> 
              case compare (start a) (start middle) of
                LT -> goBinary b middleIndex 
                GT -> goBinary (middleIndex + 1) e
                -- this will never happen
                EQ -> goBackwards middleIndex <> goForward (middleIndex + 1)
            _ -> goBackwards middleIndex <> goForward (middleIndex + 1)
      where                  
        middle = V.unsafeIndex v middleIndex
        middleIndex = fromIntegral (((fromIntegral b :: Word) + (fromIntegral e :: Word)) `div` 2) :: Int
    
    goForward index 
      | index >= len = []
      | otherwise = 
          case V.unsafeIndex v index `intersection` a of
            [] -> []
            is -> is <> goForward (index + 1)
    
    goBackwards index
      | index <= 0 = []
      | otherwise = 
          case V.unsafeIndex v index `intersection` a of
            [] -> []
            is -> goBackwards (index - 1) <> is
          
    len = V.length v
    lastIndex = V.length v - 1        
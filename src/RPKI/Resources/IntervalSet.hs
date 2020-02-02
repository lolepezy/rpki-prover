{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Resources.IntervalSet where

import Prelude hiding (subtract, null)

import           Codec.Serialise

import           Common.SmallSet          (SmallSet (..))
import qualified Common.SmallSet          as SmallSet

import           Data.Either              (partitionEithers)
import           Data.Kind
import qualified Data.List                as L
import qualified Data.Vector              as V
import           GHC.Generics
import           RPKI.Resources.Types

empty ::IntervalSet a
empty = IntervalSet SmallSet.empty

null ::IntervalSet a -> Bool
null (IntervalSet s) = SmallSet.null s

fromList :: WithSetOps a => [a] -> IntervalSet a
fromList = IntervalSet . SmallSet.fromList . normalise 

findIntersections :: Interval a => a -> IntervalSet a -> [a]
findIntersections a as = concatMap fst $ findFullIntersections a as

-- | Use binary search to find intersections of an intetval within an interval set.
-- | Return also the orginal intervals it intersects with.
findFullIntersections :: Interval a => a -> IntervalSet a -> [([a], a)]
findFullIntersections a (IntervalSet (SS v)) = 
  case (V.unsafeIndex v 0) `intersection` a of
      [] -> case (V.unsafeIndex v lastIndex) `intersection` a of
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
        middleIndex = fromIntegral ((word b + word e) `div` 2) :: Int
          where
            word n = fromIntegral n :: Word
            {-# INLINE word #-}
    
    goForward index 
      | index >= len = []
      | otherwise = 
          case big `intersection` a of
            [] -> []
            is -> (is, big) : goForward (index + 1)
      where
        big = V.unsafeIndex v index
    
    goBackwards index
      | index <= 0 = []
      | otherwise = 
          case big `intersection` a of
            [] -> []
            is -> goBackwards (index - 1) <> [(is, big)]
      where
        big = V.unsafeIndex v index
          
    len = V.length v
    lastIndex = len - 1        


type ResourceCheckResult a = Either 
    (Nested (IntervalSet a)) 
    (Nested (IntervalSet a), Overclaiming (IntervalSet a))


-- | For two sets, find intersecting and overclaming resource subsets
-- 
intersectionAndOverclaimedIntervals :: Interval a =>    
                                      IntervalSet a -> IntervalSet a -> 
                                      (Nested (IntervalSet a), Overclaiming (IntervalSet a))
intersectionAndOverclaimedIntervals (IntervalSet (SS smaller)) bigger =     
    (Nested $ fromList intersectionRS, Overclaiming $ fromList overclaimingRS)
  where
    intersectionRS = good <> concatMap fst problematic 
    overclaimingRS = concatMap snd problematic

    (problematic, good) = partitionEithers $ concatMap overclamingPart smaller

    overclamingPart smallerInterval = 
      case findFullIntersections smallerInterval bigger of
        []             -> [Left ([], [smallerInterval])]
        intersections  -> (flip L.map) intersections $ 
          \(intersection, biggerInterval) ->        
              if biggerInterval `contains` smallerInterval
                then Right smallerInterval
                else Left (intersection, smallerInterval `subtract` biggerInterval)       


subsetCheck :: Interval a =>
                IntervalSet a -> IntervalSet a -> ResourceCheckResult a              
subsetCheck s b = 
  if null o 
    then Left $ Nested i
    else Right (Nested i, Overclaiming o)
  where
    (Nested i, Overclaiming o) = intersectionAndOverclaimedIntervals s b                  
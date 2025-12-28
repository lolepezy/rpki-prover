{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Resources.IntervalContainers where

import Prelude hiding (subtract, null)

import           Data.Either              (partitionEithers)
import qualified Data.List                as List
import qualified Data.Vector              as V
import           RPKI.Resources.Types

empty :: IntervalSet a
empty = IntervalSet V.empty

null :: IntervalSet a -> Bool
null (IntervalSet s) = V.null s

fromList :: WithSetOps a => [a] -> IntervalSet a
fromList = IntervalSet . V.fromList . normalise

toList :: IntervalSet a -> [a]
toList (IntervalSet s) = V.toList s

findIntersections :: Interval a => a -> IntervalSet a -> [a]
findIntersections a as = concatMap fst $ findFullIntersections a as

instance WithSetOps a => Semigroup (IntervalSet a) where
    is1 <> is2 = fromList $ toList is1 <> toList is2

isInside :: Interval a => a -> IntervalSet a -> Bool
isInside i is = 
    case findFullIntersections i is of
        [([z], _)] -> i == z
        _          -> False

-- | Use binary search to find intersections of an interval within an interval set.
-- | Return both interesections -- '[a]' and the intervals it intersects with -- 'a'.
findFullIntersections :: Interval a => a -> IntervalSet a -> [([a], a)]
findFullIntersections a is@(IntervalSet v) = 
    if null is 
        then []
    else 
        case V.unsafeIndex v 0 `intersection` a of
            [] -> case V.unsafeIndex v lastIndex `intersection` a of
                [] -> goBinarySearch 0 lastIndex
                _  -> goBackwards lastIndex
            _  -> goForward 0
  where 
    goBinarySearch b e
        | e <= b = []
        | otherwise =           
            case middle `intersection` a of
                [] -> 
                    case compare (start a) (start middle) of
                        LT -> goBinarySearch b middleIndex 
                        GT -> goBinarySearch (middleIndex + 1) e
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
                is' -> (is', big) : goForward (index + 1)
      where
        big = V.unsafeIndex v index
    
    goBackwards index
        | index <= 0 = []
        | otherwise = 
            case big `intersection` a of
                [] -> []
                is' -> goBackwards (index - 1) <> [(is', big)]
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
intersectionAndOverclaimedIntervals (IntervalSet smaller) bigger =     
    (Nested $ fromList intersectionRS, Overclaiming $ fromList overclaimingRS)
    where
        intersectionRS = good <> concatMap fst problematic 
        overclaimingRS = concatMap snd problematic

        (problematic, good) = partitionEithers $ concatMap overclamingPart smaller

        overclamingPart smallerInterval = 
            case findFullIntersections smallerInterval bigger of
                []             -> [Left ([], [smallerInterval])]
                intersections  -> flip List.map intersections $ 
                    \(intersection', biggerInterval) ->        
                        if biggerInterval `contains` smallerInterval
                            then Right smallerInterval
                            else Left (intersection', smallerInterval `subtract` biggerInterval)       


subsetCheck :: Interval a =>
                IntervalSet a -> IntervalSet a -> ResourceCheckResult a              
subsetCheck s b = 
    if null o 
        then Left $ Nested i
        else Right (Nested i, Overclaiming o)
    where
        (Nested i, Overclaiming o) = intersectionAndOverclaimedIntervals s b                  

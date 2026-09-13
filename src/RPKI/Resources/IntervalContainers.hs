{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
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
--
-- `intersection` can be expensive (e.g. Ipv6Prefix's goes through hw-ip's
-- Word128 range-splitting), so every branch below reuses the `intersection`
-- value it already tested instead of letting goForward/goBackwards recompute
-- it for the very same index.
findFullIntersections :: Interval a => a -> IntervalSet a -> [([a], a)]
findFullIntersections a is@(IntervalSet v) =
    if null is
        then []
    else
        let first' = V.unsafeIndex v 0
        in case first' `intersection` a of
            [] ->
                let last' = V.unsafeIndex v lastIndex
                in case last' `intersection` a of
                    [] -> goBinarySearch 0 lastIndex
                    lastX -> continueBackwards lastIndex lastX last'
            firstX -> continueForward 0 firstX first'
  where
    goBinarySearch b e
        | e <= b = []
        | otherwise =
            case middle `intersection` a of
                [] ->
                    case compare (start a) (start middle) of
                        LT -> goBinarySearch b middleIndex
                        GT -> goBinarySearch (middleIndex + 1) e
                        -- `middle` itself doesn't intersect (the [] case above),
                        -- so a backwards search starting at it contributes nothing.
                        EQ -> goForward (middleIndex + 1)
                middleX -> continueBackwards middleIndex middleX middle <> goForward (middleIndex + 1)
      where
        middle = V.unsafeIndex v middleIndex
        middleIndex = fromIntegral ((word b + word e) `div` 2) :: Int
            where
                word n = fromIntegral n :: Word
                {-# INLINE word #-}

    goForward index
        | index >= len = []
        | otherwise = continueForward index (big `intersection` a) big
      where
        big = V.unsafeIndex v index

    -- `x` must be `big \`intersection\` a`, already computed by the caller.
    continueForward index x big =
        case x of
            [] -> []
            is' -> (is', big) : goForward (index + 1)

    goBackwards index
        | index <= 0 = []
        | otherwise = continueBackwards index (big `intersection` a) big
      where
        big = V.unsafeIndex v index

    -- `x` must be `big \`intersection\` a`, already computed by the caller.
    continueBackwards index x big =
        case x of
            [] -> []
            is' -> goBackwards (index - 1) <> [(is', big)]

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

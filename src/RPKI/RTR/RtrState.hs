{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedLabels #-}

module RPKI.RTR.RtrState where

import           Control.Lens

import           Data.Foldable  (toList)
import           Data.Set       (Set, (\\))
import qualified Data.Set       as Set
import qualified Data.List      as List
import qualified Data.Vector    as V
import           Data.Generics.Labels

import           Deque.Strict   as Deq

import           RPKI.AppTypes
import           RPKI.Time      (nanosPerSecond)
import           RPKI.RTR.Types
import           RPKI.RTR.Protocol


newDiff :: Ord a => Diff a
newDiff = Diff mempty mempty

newRtrDiff :: RtrDiffs
newRtrDiff = GenDiffs newDiff newDiff

isEmptyDiff :: Diff a -> Bool
isEmptyDiff Diff {..} = Set.null added && Set.null deleted

emptyDiffs :: RtrDiffs -> Bool
emptyDiffs GenDiffs {..} = isEmptyDiff vrpDiff && isEmptyDiff bgpSecDiff


newRtrState :: WorldVersion -> Int -> RtrState
newRtrState worldVersion maxTotalVrps =
    RtrState {
        lastKnownWorldVersion = worldVersion,
        currentSessionId      = worldVersionToRtrSessionId worldVersion,
        currentSerial         = initialSerial,
        maxSerialsPerSession  = 20,
        diffs                 = mempty,
        totalDiffSize         = 0,
        maxTotalDiffSize      = maxTotalVrps
    }

-- | Create a new RTR state based on the previous one and a new diff.
-- 
updatedRtrState :: RtrState -> WorldVersion -> RtrDiffs -> RtrState
updatedRtrState RtrState {..} worldVersion diff =
    RtrState {
        lastKnownWorldVersion = worldVersion,
        diffs = diffs',
        currentSerial = newSerial,
        totalDiffSize = newTotalSize,
        ..
    }
    where
        newSerial = nextSerial currentSerial

        -- strip off the oldest serial if either
        --   * the list of diffs is getting too long
        --   * or the total amount of VRPs in all diffs is getting too big
        --   * never remove the latest diff, regardless of its size
        (diffs', newTotalSize) =
            shrinkUntilSizeFits
                (let !z = (newSerial, diff) in Deq.cons z diffs)
                (let v = vrpDiff diff
                     b = bgpSecDiff diff
                    in totalDiffSize +
                        Set.size (added v) + Set.size (deleted v) +
                        Set.size (added b) + Set.size (deleted b))
            where
                shrinkUntilSizeFits newDiffs@[_] newSize = (newDiffs, newSize)
                shrinkUntilSizeFits newDiffs newSize =
                    if length newDiffs > maxSerialsPerSession || newSize > maxTotalDiffSize
                        then
                            case Deq.unsnoc newDiffs of
                                Nothing -> (newDiffs, newSize)
                                Just ((_, removedDiff), restDiffs) -> let
                                        v = vrpDiff removedDiff
                                        b = bgpSecDiff removedDiff
                                    in shrinkUntilSizeFits restDiffs $
                                            newSize - Set.size (added v) - Set.size (deleted v) -
                                                      Set.size (added b) - Set.size (deleted b)
                        else (newDiffs, newSize)


-- | Return all the diffs starting from some serial if we have this data.
-- 
diffsFromSerial :: RtrState -> SerialNumber -> Maybe [(SerialNumber, RtrDiffs)]
diffsFromSerial RtrState {..} clientSerial =
    case List.takeWhile ((> clientSerial) . fst) (toList diffs) of
        [] -> Nothing
        z -> Just z


-- | Transform a list of diffs into one diff that doesn't contain duplicates
-- or alternating 'add' and 'remove' operations for the same RTR payload (VRP or BGPSec).
-- 
squashDiffs :: (Ord a, Ord b) => [(SerialNumber, GenDiffs a b)] -> GenDiffs a b
squashDiffs diffs =
    GenDiffs {
        vrpDiff    = foldr (squash . (^. #vrpDiff)) newDiff sortedDiffs,
        bgpSecDiff = foldr (squash . (^. #bgpSecDiff)) newDiff sortedDiffs
    }
  where
    sortedDiffs = map snd $ List.sortOn fst diffs
    squash diff resultDiff = let
         added'   = added diff   <> added resultDiff
         deleted' = deleted diff <> deleted resultDiff
         in Diff {
             added   = added'  \\ deleted resultDiff,
             deleted = deleted' \\ added resultDiff
         }


-- | Create a diff, optimising typical corner-cases.
-- 
setDiff :: Ord a => Set a -> Set a -> Diff a
setDiff previous current
    | Set.null previous       && Set.null current       = newDiff
    | Set.null previous       && not (Set.null current) = newDiff { added = current }
    | not (Set.null previous) && Set.null current       = newDiff { deleted = previous }
    | otherwise = Diff {
                added   = current \\ previous,
                deleted = previous \\ current
            }

setDiffV :: Ord a => V.Vector a -> V.Vector a -> Diff a
setDiffV previous current = 
    setDiff (Set.fromList $ V.toList previous)
            (Set.fromList $ V.toList current) 

evalDiffs :: RtrPayloads -> RtrPayloads -> RtrDiffs
evalDiffs previous current =
    GenDiffs {
        vrpDiff    = setDiffV (uniqueVrps previous) (uniqueVrps current),
        bgpSecDiff = setDiff (bgpSec previous) (bgpSec current)
    }


-- Wrap around at 2^31 - 1
-- https://tools.ietf.org/html/rfc8210#page-5
-- 
nextSerial :: SerialNumber -> SerialNumber
nextSerial (SerialNumber n) =
    SerialNumber $
        if (fromIntegral n :: Integer) == wrapAroundSerial
            then 0
            else n + 1

initialSerial :: SerialNumber
initialSerial = SerialNumber 1

wrapAroundSerial :: Integer
wrapAroundSerial = (2 :: Integer)^(31 :: Integer) - 1

-- 1) strip everything except from seconds from the the version
-- 2) Make it 2 bytes using `mod`    
worldVersionToRtrSessionId :: WorldVersion -> RtrSessionId
worldVersionToRtrSessionId (WorldVersion nanoseconds) =
    RtrSessionId $ fromIntegral $
        (nanoseconds `div` nanosPerSecond) `mod` (256 * 256)

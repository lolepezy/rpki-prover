{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.RtrState where

import           Data.Foldable  (toList)
import           Data.Set       (Set, (\\))
import qualified Data.Set       as Set

import           GHC.Generics

import qualified Data.List      as List

import           Deque.Strict   as Deq

import           RPKI.Domain
import           RPKI.RTR.Types

import           RPKI.AppTypes
import           RPKI.Time      (nanosPerSecond)



data Diff a = Diff { 
        added   :: Set a, 
        deleted :: Set a
    } 
    deriving stock (Show, Eq, Ord, Generic)

type VrpDiff = Diff Vrp

data RtrState = RtrState {
        lastKnownWorldVersion :: WorldVersion,
        currentSessionId      :: RtrSessionId,
        currentSerial         :: SerialNumber,
        maxSerialsPerSession  :: Int,
        diffs                 :: Deq.Deque (SerialNumber, VrpDiff),
        totalDiffSize         :: Int,
        maxTotalDiffSize      :: Int
    } 
    deriving stock (Show, Eq, Generic)

newVrpDiff :: Ord a => Diff a
newVrpDiff = Diff mempty mempty

isEmptyDiff :: Diff a -> Bool
isEmptyDiff Diff {..} = Set.null added && Set.null deleted


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
updatedRtrState :: RtrState -> WorldVersion -> VrpDiff -> RtrState
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
                (totalDiffSize + Set.size (added diff) + Set.size (deleted diff))
            where 
                shrinkUntilSizeFits newDiffs@[_] newSize = (newDiffs, newSize)
                shrinkUntilSizeFits newDiffs newSize = 
                    if length newDiffs > maxSerialsPerSession || newSize > maxTotalDiffSize
                        then 
                            case Deq.unsnoc newDiffs of 
                                Nothing -> (newDiffs, newSize)
                                Just ((_, removedDiff), restDiffs) ->
                                    shrinkUntilSizeFits restDiffs $
                                        newSize - Set.size (added removedDiff) - Set.size (deleted removedDiff)
                        else (newDiffs, newSize)


-- | Return all the diffs starting from some serial if we have this data.
-- 
diffsFromSerial :: RtrState -> SerialNumber -> Maybe [(SerialNumber, VrpDiff)] 
diffsFromSerial RtrState {..} clientSerial = 
    case List.takeWhile ((> clientSerial) . fst) (toList diffs) of
        [] -> Nothing
        z -> Just z


-- | Transform a list of diffs into one diff that doesn't contain duplicates
-- or alternating 'add' and 'remove' operations for the same VRPs.
-- 
squashDiffs :: Ord a => [(SerialNumber, Diff a)] -> Diff a
squashDiffs diffs = 
    List.foldr (squash . snd) newVrpDiff $ List.sortOn fst diffs
  where
     squash diff resultDiff = let
         added'   = added diff   <> added resultDiff
         deleted' = deleted diff <> deleted resultDiff
         in Diff {
             added   = added'  \\ deleted resultDiff,
             deleted = deleted' \\ added resultDiff
         }

-- | Create a diff, optimising typical corner-cases.
-- 
evalVrpDiff :: Set Vrp -> Set Vrp -> VrpDiff
evalVrpDiff previous current
    | Set.null previous       && Set.null current       = newVrpDiff
    | Set.null previous       && not (Set.null current) = newVrpDiff { added = current }
    | not (Set.null previous) && Set.null current       = newVrpDiff { deleted = previous }
    | otherwise = Diff { 
                added   = current \\ previous,
                deleted = previous \\ current
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
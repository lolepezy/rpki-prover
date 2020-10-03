{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.RtrState where

import           Data.Foldable          (toList)
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           GHC.Generics

import qualified Data.List              as List

import           Deque.Strict           as Deq

import           RPKI.Domain
import           RPKI.RTR.Types

import           RPKI.AppState


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
        earliestSerial        :: SerialNumber,
        maxSerialsPerSession  :: Int,
        diffs                 :: Deq.Deque (SerialNumber, VrpDiff)
    } 
    deriving stock (Eq, Generic)

newVrpDiff :: Diff a
newVrpDiff = Diff Set.empty Set.empty

isEmptyDiff :: Diff a -> Bool
isEmptyDiff Diff {..} = Set.null added && Set.null deleted


newRtrState :: WorldVersion -> IO RtrState 
newRtrState worldVersion = do
    session' <- generateSessionId
    serial'  <- genarateSerial
    pure $ RtrState worldVersion session' serial' serial' 100 mempty
    where
        -- TODO Generate them according to the RFC
        generateSessionId = pure $ RtrSessionId 133
        genarateSerial    = pure initialSerial


updatedRtrState :: RtrState -> WorldVersion -> VrpDiff -> RtrState
updatedRtrState RtrState {..} worldVersion diff = 
    RtrState {        
        lastKnownWorldVersion = worldVersion,        
        diffs = diffs',
        currentSerial = newSerial,
        earliestSerial = earliestSerial',
        ..
    }
    where
        newSerial       = nextSerial currentSerial
        earliestSerial' = nextSerial earliestSerial
        diffs' = let
            -- strip off the oldest serial if the list of diffs is getting too long
            newDiffs = let !z = (newSerial, diff) in Deq.cons z diffs
            in if length newDiffs > maxSerialsPerSession
                    then maybe [] snd $ Deq.unsnoc newDiffs
                    else newDiffs


-- | Return all the diffs starting from some serial if we have this data.
-- 
diffsFromSerial :: RtrState -> SerialNumber -> Maybe [(SerialNumber, VrpDiff)] 
diffsFromSerial RtrState {..} serial = 
    if serial < earliestSerial
        then Nothing
        else Just $ List.filter (\(s, _) -> s > serial) $ toList diffs


-- | Transform a list of diffs into one diff that doesn't contain duplicates
-- or alternating 'add' and 'remove' operations for the same VRPs.
-- 
squashDiffs :: Ord a => [(SerialNumber, Diff a)] -> Diff a
squashDiffs diffs = 
    List.foldr (squash . snd) newVrpDiff $ List.sortOn fst diffs
  where
     squash diff resultDiff = Diff {
             added   = Set.difference (Set.union (added diff)   (added resultDiff))   (deleted resultDiff),
             deleted = Set.difference (Set.union (deleted diff) (deleted resultDiff)) (added resultDiff)
         }

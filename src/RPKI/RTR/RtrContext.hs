{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.RtrContext where

import           Control.Concurrent.STM
import           Data.Foldable          (toList)
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Data.Maybe             (fromMaybe)
import           GHC.Generics

import qualified Data.List              as List
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

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

data RtrContext = RtrContext {
        lastKnownVersion     :: Maybe WorldVersion,
        currentSessionId     :: RtrSessionId,
        currentSerial        :: SerialNumber,
        earliestSerial       :: SerialNumber,
        maxSerialsPerSession :: Int,
        diffs                :: Deq.Deque (SerialNumber, VrpDiff)
    } 
    deriving stock (Eq, Generic)

newVrpDiff :: Diff a
newVrpDiff = Diff Set.empty Set.empty


newRtrContext :: IO RtrContext 
newRtrContext = do
    session' <- generateSessionId
    serial'  <- genarateSerial
    pure $ RtrContext Nothing session' serial' serial' 100 mempty
    where
        -- TODO Generate them according to the RFC
        generateSessionId = pure $ RtrSessionId 133
        genarateSerial    = pure initialSerial



updateContext :: RtrContext -> WorldVersion -> VrpDiff -> RtrContext
updateContext RtrContext {..} worldVersion diff = 
    RtrContext {        
        lastKnownVersion = Just worldVersion,        
        diffs = diffs',
        currentSerial = newSerial,
        earliestSerial = earliestSerial',
        ..
    }
    where
        newSerial = nextSerial currentSerial
        earliestSerial' = nextSerial earliestSerial
        diffs' = let
            -- strip off the oldest serial if the list of diffs is getting too long
            newDiffs = let !z = (newSerial, diff) in Deq.cons z diffs
            in if length newDiffs > maxSerialsPerSession
                    then maybe [] snd $ Deq.unsnoc newDiffs
                    else newDiffs


-- | Return 
diffsFromSerial :: RtrContext -> SerialNumber -> Maybe [(SerialNumber, VrpDiff)] 
diffsFromSerial RtrContext {..} serial = 
    if serial < earliestSerial
        then Nothing
        else Just $ List.filter (\(s, _) -> s > serial) $ toList diffs


squashDiffs :: Ord a => [(SerialNumber, Diff a)] -> Diff a
squashDiffs diffs = 
    List.foldr squash newVrpDiff
    $ map snd 
    $ List.sortOn fst diffs
  where
     squash diff resultDiff = 
         resultDiff {
             added   = Set.difference (Set.union (added diff) (added resultDiff)) (deleted resultDiff),
             deleted = Set.difference (Set.union (deleted diff) (deleted resultDiff)) (added resultDiff)
         }


{-
import Data.Set as Set
let d1 = Diff { added = Set.fromList [1,2], deleted = Set.fromList [3,4] }
let d2 = Diff { added = Set.fromList [4,5], deleted = Set.fromList [2] }
squashDiffs [(SerialNumber 1, d1), (SerialNumber 3, d2)]
-}
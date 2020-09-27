{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.RtrContext where

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

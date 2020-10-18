{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.RtrState where

import           Data.Foldable          (toList)
import           Data.Set               ((\\), Set)
import qualified Data.Set               as Set

import           GHC.Generics

import qualified Data.List              as List

import           Deque.Strict           as Deq

import           RPKI.Domain
import           RPKI.RTR.Types

import           RPKI.AppState


data Diff a = Diff { 
        added   :: [a], 
        deleted :: [a]
    } 
    deriving stock (Show, Eq, Ord, Generic)

type VrpDiff = Diff Vrp

data RtrState = RtrState {
        lastKnownWorldVersion :: WorldVersion,
        currentSessionId      :: RtrSessionId,
        currentSerial         :: SerialNumber,
        maxSerialsPerSession  :: Int,
        diffs                 :: Deq.Deque (SerialNumber, VrpDiff)
    } 
    deriving stock (Show, Eq, Generic)

newVrpDiff :: Diff a
newVrpDiff = Diff [] []

isEmptyDiff :: Diff a -> Bool
isEmptyDiff Diff {..} = List.null added && List.null deleted


newRtrState :: WorldVersion -> IO RtrState 
newRtrState worldVersion = do
    session' <- generateSessionId
    serial'  <- genarateSerial
    pure $ RtrState worldVersion session' serial' 100 mempty
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
        ..
    }
    where
        newSerial = nextSerial currentSerial        

        diffs' = let
            -- strip off the oldest serial if the list of diffs is getting too long
            newDiffs = let !z = (newSerial, diff) in Deq.cons z diffs
            in if length newDiffs > maxSerialsPerSession
                    then maybe newDiffs snd $ Deq.unsnoc newDiffs            
                    else newDiffs


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
         added'   = Set.fromList $ added diff   <> added resultDiff
         deleted' = Set.fromList $ deleted diff <> deleted resultDiff
         in Diff {
             added   = Set.toList $ added'  \\ Set.fromList (deleted resultDiff),
             deleted = Set.toList $ deleted' \\ Set.fromList (added resultDiff)
         }

evalVrpDiff :: [Vrp] -> [Vrp] -> VrpDiff
evalVrpDiff [] [] = Diff [] []    
evalVrpDiff [] n = Diff { 
                    added   = n,
                    deleted = []
                }
evalVrpDiff p [] = Diff { 
                    added   = [],
                    deleted = p
                }
evalVrpDiff p n = let
    newVrpsSet      = Set.fromList n
    previousVrpsSet = Set.fromList p
    in Diff { 
        added   = Set.toList $ newVrpsSet \\ previousVrpsSet,
        deleted = Set.toList $ previousVrpsSet \\ newVrpsSet
    }

-- 
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
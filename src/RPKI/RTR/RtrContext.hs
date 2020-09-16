{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.RtrContext where

import           Control.Concurrent.STM

import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Data.Maybe             (fromMaybe)
import           GHC.Generics

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Deque.Strict           as Deq

import           RPKI.Domain            hiding (SessionId)
import           RPKI.RTR.Types

import           RPKI.AppState



data VrpDiff = VrpDiff { 
        added   :: Set Vrp, 
        deleted :: Set Vrp
    } 
    deriving stock (Eq, Generic)

data RtrContext = RtrContext {
        lastKnownVersion     :: Maybe WorldVersion,
        session              :: SessionId,
        serial               :: SerialNumber,
        earliestSerial       :: SerialNumber,
        maxSerialsPerSession :: Int,
        diffs                :: Deq.Deque (SerialNumber, VrpDiff)
    } 
    deriving stock (Eq, Generic)


newRtrContext :: IO RtrContext 
newRtrContext = do
    session' <- generateSessionId
    serial'  <- genarateSerial
    pure $ RtrContext Nothing session' serial' serial' 100 mempty
    where
        -- TODO Generate them according to the RFC
        generateSessionId = pure $ SessionId 133
        genarateSerial    = pure initialSerial



updateContext :: RtrContext -> WorldVersion -> VrpDiff -> RtrContext
updateContext RtrContext {..} worldVersion !diff = 
    RtrContext {        
        lastKnownVersion = Just worldVersion,        
        diffs = diffs',
        serial = newSerial,
        ..
    }
    where
        newSerial = nextSerial serial
        diffs' = let
            -- strip off the oldest serial if the list of diffs is getting too long
            newDiffs = let !z = (newSerial, diff) in Deq.cons z diffs
            in if length newDiffs > maxSerialsPerSession
                    then maybe [] snd $ Deq.unsnoc newDiffs
                    else newDiffs

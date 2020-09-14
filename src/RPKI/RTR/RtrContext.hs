{-# LANGUAGE RecordWildCards #-}
module RPKI.RTR.RtrContext where

import           Control.Concurrent.STM

import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           RPKI.Domain            hiding (SessionId)
import           RPKI.RTR.Types

data RtrUpdate = 
      RtrAll (Set Vrp) 
    | RtrDiff { 
        added   :: Set Vrp, 
        deleted :: Set Vrp
    }

data VrpDiff = VrpDiff { 
        added   :: Set Vrp, 
        deleted :: Set Vrp
    } 

data RtrState = RtrState {
    vrps :: Set Vrp,
    deltas :: Map SerialNumber VrpDiff
}

emptyState :: RtrState
emptyState = RtrState mempty mempty

updatedRtrState :: RtrState -> Set Vrp -> RtrState
updatedRtrState RtrState {..} newVrps = RtrState newVrps newDeltas
    where
        newDeltas = 
            case Map.maxViewWithKey deltas of
                Nothing ->
                    let newDiff = VrpDiff { 
                                    added = newVrps, 
                                    deleted = mempty
                                }
                    in Map.insert (SerialNumber 1) newDiff deltas
                Just ((maxSerial, _), _) -> 
                    let newDiff = VrpDiff { 
                                    added = Set.difference newVrps vrps,
                                    deleted = Set.difference vrps newVrps
                                }
                    in Map.insert (nextSerial maxSerial) newDiff deltas


data RtrContext = RtrContext {
    currentState :: TVar RtrState,
    worldVersionUpdateChan :: TChan RtrUpdate,
    session :: TVar SessionId,
    serial  :: TVar SerialNumber,
    earliestSerial :: TVar SerialNumber,
    maxSerialsPerSession :: Int
}

newRtrContext :: IO RtrContext 
newRtrContext = do
    session' <- generateSessionId
    serial'  <- genarateSerial
    RtrContext <$> 
        newTVarIO emptyState <*>
        newBroadcastTChanIO <*> 
        newTVarIO session' <*> 
        newTVarIO serial' <*>
        newTVarIO serial' <*>
        pure 100
    where
        -- TODO Generate them according to the RFC
        generateSessionId = pure $ SessionId 133
        genarateSerial    = pure $ SerialNumber 1

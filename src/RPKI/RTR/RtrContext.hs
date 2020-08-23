module RPKI.RTR.RtrContext where

import           Control.Concurrent.STM

import           Data.Set               (Set)
import           RPKI.Domain            hiding (SessionId)

import           RPKI.RTR.Types

                                            
data RtrUpdate = 
      RtrAll (Set Roa) 
    | RtrDiff { 
        added   :: Set Roa, 
        deleted :: Set Roa
    }

data RtrContext = RtrContext {
    worldVersionUpdateChan :: TChan RtrUpdate,
    session :: TVar SessionId,
    serial  :: TVar SerialNumber
}

newRtrContext :: IO RtrContext 
newRtrContext = do
    session' <- generateSessionId
    serial'  <- genarateSerial
    RtrContext <$> 
        newBroadcastTChanIO <*> 
        newTVarIO session' <*> 
        newTVarIO serial'
    where
        -- TODO Generate them according to the RFC
        generateSessionId = pure $ SessionId 133
        genarateSerial    = pure $ SerialNumber 1

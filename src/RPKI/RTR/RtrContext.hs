{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts    #-}

module RPKI.RTR.RtrContext where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           RPKI.Version

data RtrContext = RtrContext {
    worldVersionUpdateChan :: TChan WorldVersion    
}

newRtrContext :: IO RtrContext 
newRtrContext = 
    RtrContext <$> newBroadcastTChanIO

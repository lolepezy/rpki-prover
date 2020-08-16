{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrServer where

import           Control.Monad.IO.Class

import           Network.Simple.TCP

import           RPKI.RTR.Types
import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Store.Base.Storage


data RtrCntext = RtrCntext {

}

rtrListen :: (Storage s, MonadIO m) => 
            AppContext s -> HostPreference -> ServiceName -> m a
rtrListen appContext address port = do 
    serve address port $ \(connectionSocket, remoteAddr) -> do        
        putStrLn $ "TCP connection established from " ++ show remoteAddr
        -- add client to the set of connected clients

        -- remove client on excep[tion]


        -- Now you may use connectionSocket as you please within this scope,
        -- possibly using recv and send to interact with the remote end.

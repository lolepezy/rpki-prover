{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrServer where

import           Control.Concurrent        (forkFinally)

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Exception.Lifted

import qualified Data.ByteString           as BS

import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

import           RPKI.Config
import           RPKI.AppContext
import           RPKI.RTR.RtrContext
import           RPKI.RTR.Types
import           RPKI.RTR.Binary


runRtrServer :: RtrConfig -> RtrContext -> IO ()
runRtrServer _ rtrContext = 
    withSocketsDo $ do         
        addr <- resolve "3000"
        bracket (open addr) close (loop rtrContext)        
    where
        resolve port = do
            let hints = defaultHints {
                    addrFlags = [AI_PASSIVE], 
                    addrSocketType = Stream
                }
            addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
            return addr
        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress addr)            
            let fd = fdSocket sock
            setCloseOnExecIfNeeded fd
            listen sock 10
            return sock
        loop rtrContext sock = forever $ do
            (conn, peer) <- accept sock
            putStrLn $ "Connection from " ++ show peer
            void $ forkFinally 
                (connectionProcessor rtrContext conn) 
                (\_ -> close conn)

        connectionProcessor RtrContext {..} connection = 
            race receiveFromClient sendToClient
            where
                receiveFromClient = forever $ do
                     pduBytes <- recv connection 1024
                     unless (BS.null pduBytes) $ do                         
                         case bytesToPdu pduBytes of 
                            Left e -> do
                                 -- TODO Complain
                                 pure ()
                            Right pdu -> do
                                -- TODO Process PDU
                                 pure ()                                

                sendToClient = do
                    localChan <- atomically $ dupTChan worldVersionUpdateChan
                    forever $ do 
                        updated <- atomically $ readTChan localChan
                        let msg = toBytes updated
                        sendAll connection msg
                    where
                        -- TODO Implement
                        toBytes _ = BS.empty
                    

{-# LANGUAGE OverloadedLabels    #-}
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

import           Control.Lens
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import           Data.String.Interpolate.IsString

import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (recv, sendAll)

import           RPKI.Config
import           RPKI.AppContext
import           RPKI.Logging
import           RPKI.RTR.RtrContext
import           RPKI.RTR.Types
import           RPKI.RTR.Binary


runRtrServer :: AppContext s -> RtrConfig -> RtrContext -> IO ()
runRtrServer AppContext {..} RtrConfig {..} rtrContext = 
    withSocketsDo $ do                 
        address <- resolve (show rtrPort)
        bracket (open address) close loop
    where
        resolve port = do
            let hints = defaultHints {
                    addrFlags = [AI_PASSIVE], 
                    addrSocketType = Stream
                }
            addr : _ <- getAddrInfo (Just hints) Nothing (Just port)
            return addr
        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress addr)            
            let fd = fdSocket sock
            setCloseOnExecIfNeeded fd
            listen sock 10
            return sock
        loop sock = forever $ do
            (conn, peer) <- accept sock
            logDebug_ logger [i|Connection from|]        
            putStrLn $ "Connection from " ++ show peer
            void $ forkFinally 
                (connectionProcessor rtrContext conn) 
                (\_ -> close conn)

        -- For every connection run 3 threads:
        --      receiveFromClient blocks on `recv` and accepts client requests
        --      updateFromRtrState blocks on updates from the new data coming from the validation process
        --      sendToClient simply sends all PDU to the client
        connectionProcessor RtrContext {..} connection = do
            sendChan <- atomically newTChan
            race 
                (receiveFromClient sendChan)
                (race 
                    (updateFromRtrState sendChan)
                    (sendToClient sendChan))
            where
                sendToClient sendChan = do 
                    pdu <- atomically $ readTChan sendChan                    
                    sendAll connection (toBytes pdu)
                    where
                        -- TODO Implement
                        toBytes _ = BS.empty

                receiveFromClient sendChan = forever $ do
                     pduBytes <- recv connection 1024
                     unless (BS.null pduBytes) $ do                         
                         case bytesToPdu pduBytes of 
                            Left e -> do
                                 -- TODO Complain
                                 pure ()
                            Right pdu -> do
                                -- TODO Process PDU
                                 pure ()                                

                updateFromRtrState sendChan = do
                    localChan <- atomically $ dupTChan worldVersionUpdateChan
                    forever $ atomically $ do
                        update <- readTChan localChan
                        writeTChan sendChan update
                        
                                                
                    
                    

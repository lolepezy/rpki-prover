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
import qualified Data.ByteString.Lazy      as BSL

import           Data.Text
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as TE


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
import           RPKI.RTR.Pdus

import           RPKI.Util (convert)
import Data.Maybe (fromMaybe, fromJust)


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
            logDebug_ logger [i|Connection from #{peer}|]
            void $ forkFinally 
                (connectionProcessor rtrContext conn peer) 
                (\_ -> do 
                    logDebug_ logger [i|Closing connection with #{peer}|]
                    close conn)

        -- For every connection run 3 threads:
        --      receiveFromClient blocks on `recv` and accepts client requests
        --      updateFromRtrState blocks on updates from the new data coming from the validation process
        --      sendToClient simply sends all PDU to the client
        connectionProcessor RtrContext {..} connection peer = do
            sendChan :: TChan APdu <- atomically newTChan
            race 
                (receiveFromClient sendChan)
                (race 
                    (updateFromRtrState sendChan)
                    (sendToClient sendChan))
            where
                sendToClient sendChan = do 
                    pdu <- atomically $ readTChan sendChan                    
                    sendAll connection (BSL.toStrict $ withPdu pdu pduToBytes)                    

                receiveFromClient sendChan = do
                    firstPdu <- recv connection 1024
                    r <- processFirstPdu firstPdu
                    case r of
                        Left e -> do 
                            logError_ logger [i|First PDU is wrong: #{e}.|]
                        Right (pdu, session') -> do 
                            let responsePdu = withSession session' (respondWith pdu)  
                            atomically $ writeTChan sendChan responsePdu
                            go 
                    where 
                        go = do
                            logDebug_ logger [i|Waiting data from the client #{peer}|]
                            pduBytes <- recv connection 1024
                            logDebug_ logger [i|Received #{BS.length pduBytes} bytes from #{peer}|]
                            if BS.null pduBytes
                                then
                                    logDebug_ logger [i|Connection with #{peer} is closed.|] 
                                else do                         
                                    case bytesToPdu pduBytes of 
                                        Left e -> do
                                            logError_ logger [i|Error parsing a PDU #{e}.|]
                                        Right pdu -> do
                                            logDebug_ logger [i|Parsed PDU: #{pdu}.|]
                                            -- respond pdu
                                            pure ()                                
                                    go

                updateFromRtrState sendChan = do
                    localChan <- atomically $ dupTChan worldVersionUpdateChan
                    forever $ atomically $ do
                        update <- readTChan localChan
                        -- writeTChan sendChan update
                        pure ()                        
                                                
                
-- 
-- | Process the first PDU and do the protocol version negotiation
-- 
processFirstPdu :: BS.ByteString -> IO (Either (APdu, Text) (APdu, ASession))
processFirstPdu pduBytes = do    
    case bytesToPdu pduBytes of    
        Left (Just code, maybeText) -> do
            let 
                text = fromMaybe "Wrong PDU" maybeText
                in pure $ Left (APdu $ ErrorPdu @'V0 code (convert pduBytes) (convert text), text)

        Right (APdu pdu) -> 
            case pdu of 
                (SerialQueryPdu sessionId serial :: Pdu protocolVersion code) -> 
                    pure $ Right (APdu pdu, ASession (Session @protocolVersion sessionId))

                (ResetQueryPdu :: Pdu protocolVersion code) -> 
                    pure $ Right (APdu pdu, ASession (Session @protocolVersion $ SessionId 1))

                (otherPdu :: Pdu protocolVersion code) -> 
                    let 
                        text = convert $ "First received PDU must be SerialQueryPdu or ResetQueryPdu, but received " <> show otherPdu
                        in pure $ Left (APdu $ ErrorPdu @protocolVersion InvalidRequest (convert pduBytes) (convert text), text)                    



respondWith :: APdu -> 
            Session (protocolVersion :: ProtocolVersion) -> 
            APdu
respondWith (APdu pdu) s = APdu pdu
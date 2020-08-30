{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrServer where

import           Control.Concurrent               (forkFinally)

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL

import           Data.Text
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as TE

import           Data.Typeable

import           Control.Lens
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import           Data.String.Interpolate.IsString

import           Network.Socket                   hiding (recv)
import           Network.Socket.ByteString        (recv, sendAll)

import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Logging
import           RPKI.RTR.Pdus
import           RPKI.RTR.RtrContext
import           RPKI.RTR.Types

import           RPKI.Util                        (convert)

import           Data.Maybe                       (fromMaybe)
import           Data.Word                        (Word8)
import           GHC.TypeLits                     (Nat)

-- 
-- | Main entry point, here we start the RTR server. 
-- 
-- RtrContext must be created separately.
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
                    r <- processFirstPdu rtrContext firstPdu
                    case r of
                        Left (responsePdu, message) -> do 
                            logError_ logger message
                            atomically $ writeTChan sendChan responsePdu
                        Right (responsePdu, session') -> do
                            forM_ responsePdu $ atomically . writeTChan sendChan
                            withSession session' go
                    where
                        go :: forall version . 
                                IsProtocolVersion version => 
                                Session version -> IO () 
                        go session' = do
                            logDebug_ logger [i|Waiting data from the client #{peer}|]
                            pduBytes <- recv connection 1024
                            logDebug_ logger [i|Received #{BS.length pduBytes} bytes from #{peer}|]
                            unless (BS.null pduBytes) $ do      
                                case bytesToVersionedPdu session' pduBytes of 
                                    Left e -> do
                                        logError_ logger [i|Error parsing a PDU #{e}.|]
                                    Right pdu -> do
                                        logDebug_ logger [i|Parsed PDU: #{pdu}.|]
                                        -- respond pdu
                                        pure ()                                
                                go session'

                updateFromRtrState sendChan = do
                    localChan <- atomically $ dupTChan worldVersionUpdateChan
                    forever $ atomically $ do
                        update <- readTChan localChan
                        -- writeTChan sendChan update
                        pure ()                        
                                                
                
-- 
-- | Process the first PDU and do the protocol version negotiation
-- 
processFirstPdu :: RtrContext 
                -> BS.ByteString 
                -> IO (Either (APdu, Text) ([APdu], ASession))
processFirstPdu rtrContext@RtrContext {..} pduBytes = do    
    case bytesToPdu pduBytes of    
        Left (Just code, maybeText) -> let 
            text = fromMaybe "Wrong PDU" maybeText
            in pure $ Left (APdu $ ErrorPdu @'V0 code (convert pduBytes) (convert text), text)

        Right pdu -> 
            case pdu of 
                APdu (SerialQueryPdu _ _ :: Pdu protocolVersion code) -> do
                    let session' = Session @protocolVersion
                    r <- respondToPdu rtrContext pdu pduBytes session'
                    pure $ (, ASession session') <$> r                    

                APdu (ResetQueryPdu :: Pdu protocolVersion code) -> do
                    let session' = Session @protocolVersion
                    r <- respondToPdu rtrContext pdu pduBytes session'
                    pure $ (, ASession session') <$> r

                APdu (otherPdu :: Pdu protocolVersion code) -> 
                    let 
                        text = convert $ "First received PDU must be SerialQueryPdu or ResetQueryPdu, but received " <> show otherPdu
                        in pure $ Left (APdu $ ErrorPdu @protocolVersion InvalidRequest (convert pduBytes) (convert text), text)


respondToPdu :: forall protocolVersion . 
               IsProtocolVersion protocolVersion => 
                RtrContext
                -> APdu 
                -> BS.ByteString 
                -> Session (protocolVersion :: ProtocolVersion) 
                -> IO (Either (APdu, Text) [APdu])
respondToPdu RtrContext {..} (APdu pdu) pduBytes _ = do                  
    case pdu of 
        (SerialQueryPdu sessionId serial :: Pdu version code) -> let
                text :: Text = [i|Unexpected serial query PDU.|]
                in pure $ Left (APdu $ 
                        ErrorPdu @protocolVersion CorruptData (convert pduBytes) (convert text), text)
            

        (ResetQueryPdu :: Pdu version code) -> 
            withProtocolVersionCheck pdu $ pure $ Right []

        (RouterKeyPduV1 _ _ _ _)  -> 
            pure $ Right []
        
    where
        withProtocolVersionCheck :: forall 
                            (version :: ProtocolVersion) 
                            (pduCode :: Nat) a                             
                            . (Typeable protocolVersion, Typeable version) => 
                            Pdu version pduCode 
                            -> IO (Either (APdu, Text) a) 
                            -> IO (Either (APdu, Text) a)
        withProtocolVersionCheck _ respond = 
            case eqT @protocolVersion @version of
                Just _  -> respond
                Nothing -> let
                    text :: Text = [i|Protocol version is not the same.|]                                
                    in pure $ Left (APdu $ 
                        ErrorPdu @protocolVersion UnexpectedProtocolVersion (convert pduBytes) (convert text), text)

        withSessionIdCheck :: SessionId 
                            -> IO (Either (APdu, Text) a) 
                            -> IO (Either (APdu, Text) a)
        withSessionIdCheck sessionId respond = do 
            currentSessionId <- readTVarIO session
            if currentSessionId == sessionId
                then respond
                else let 
                    text = [i|Wrong sessionId from PDU #{sessionId}, cache sessionId is #{currentSessionId}.|]
                    in pure $ Left (APdu $ 
                        ErrorPdu @protocolVersion CorruptData (convert pduBytes) (convert text), text)

                
                
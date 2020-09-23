{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrServer where

import           Control.Concurrent               (forkFinally)

import           Control.Lens                     ((^.))

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL

import           Data.Text                        (Text)

import qualified Data.Set                         as Set

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

import           RPKI.AppState
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database



-- 
-- | Main entry point, here we start the RTR server. 
-- 
runRtrServer :: Storage s => AppContext s -> RtrConfig -> IO ()
runRtrServer AppContext {..} RtrConfig {..} = do 
    rtrContext <- newRtrContext 
    -- re-initialise the rtrContext and create a broadcast 
    -- channel to publish update for every clients
    perClientUpdateChan <- atomically $ do 
                                writeTVar rtrState rtrContext      
                                newBroadcastTChan 
    void $ race 
            (runSocketBusiness rtrState perClientUpdateChan)
            (listenToAppStateUpdates rtrState perClientUpdateChan)
  where    

    runSocketBusiness rtrState perClientUpdateChan = 
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
                (connectionProcessor rtrState conn peer perClientUpdateChan) 
                (\_ -> do 
                    logDebug_ logger [i|Closing connection with #{peer}|]
                    close conn)

    -- Block till every new update of the current world version and the VRP set.
    listenToAppStateUpdates rtrState updateChan = do 
        rtrContext <- readTVarIO rtrState
        case rtrContext ^. #lastKnownVersion of
            Nothing -> 
                -- nothing to update from yet, clients will ask for data themselves
                pure ()
            Just knownVersion -> do
                (newVersion, newVrps) <- listenWorldVersion appState knownVersion                            
                previousVRPs <- roTx database $ \tx -> getVrps tx database knownVersion
                let vrpDiff = VrpDiff { 
                        added = Set.difference newVrps previousVRPs,
                        deleted = Set.difference previousVRPs newVrps
                    }
                let newRtrContext = updateContext rtrContext newVersion vrpDiff
                atomically $ do 
                    writeTVar rtrState newRtrContext
                    writeTChan updateChan $ 
                        NotifyPdu (newRtrContext ^. #session) (newRtrContext ^. #serial)


    -- For every connection run 3 threads:
    --      receiveFromClient blocks on `recv` and accepts client requests
    --      updateFromAppState blocks on updates from the new data coming from the validation process
    --      sendToClient simply sends all PDU to the client
    -- 
    connectionProcessor rtrState connection peer perClientUpdateChan = do
        firstPdu   <- recv connection 1024
        rtrContext <- readTVarIO rtrState        
        sendChan   <- atomically newTChan

        processFirstPdu rtrContext firstPdu >>= \case
            Left (responsePdu, message) -> do 
                logError_ logger message
                atomically $ writeTChan sendChan responsePdu
            Right (responsePdu, session) -> do                
                forM_ responsePdu $ atomically . writeTChan sendChan
                void $ race 
                    (serve session)
                    (race 
                        (updateFromAppState sendChan)
                        (sendToClient session sendChan))
        where
            sendToClient session sendChan = do 
                pdu <- atomically $ readTChan sendChan                    
                sendAll connection $ BSL.toStrict $ pduToBytes pdu session

            serve session = do
                logDebug_ logger [i|Waiting data from the client #{peer}|]
                pduBytes <- recv connection 128
                logDebug_ logger [i|Received #{BS.length pduBytes} bytes from #{peer}|]
                -- Empty bytestring means connection is closed, so if it's 
                -- empty just silently stop doing anything
                unless (BS.null pduBytes) $ do                    
                    case bytesToPduOfKnownVersion session pduBytes of 
                        Left e -> do
                            logError_ logger [i|Error parsing a PDU #{e}.|]
                        Right pdu -> do
                            logDebug_ logger [i|Parsed PDU: #{pdu}.|]
                            -- respond pdu
                            pure ()                                
                    serve session           
                    

            -- send around PDUs that RTR cache initiates to all the clients
            updateFromAppState :: TChan Pdu -> IO ()
            updateFromAppState sendChan = do 
                localChan <- atomically $ dupTChan perClientUpdateChan
                forever $ atomically $ 
                    readTChan localChan >>= writeTChan sendChan 
                    
                                                
-- 
-- | Process the first PDU and do the protocol version negotiation
-- 
processFirstPdu :: RtrContext 
                -> BS.ByteString 
                -> IO (Either (Pdu, Text) ([Pdu], Session))
processFirstPdu rtrContext@RtrContext {..} pduBytes =    
    case bytesToVersionedPdu pduBytes of
        Left (code, maybeText) -> let 
            text = fromMaybe "Wrong PDU" maybeText
            in pure $ Left (ErrorPdu code (convert pduBytes) (convert text), text)

        Right versionedPdu@(VersionedPdu pdu protocolVersion) -> 
            case pdu of 
                SerialQueryPdu _ _ -> do
                    let session' = Session protocolVersion
                    r <- respondToPdu rtrContext versionedPdu pduBytes session'
                    pure $ (, session') <$> r

                ResetQueryPdu -> do
                    let session' = Session protocolVersion
                    r <- respondToPdu rtrContext versionedPdu pduBytes session'
                    pure $ (, session') <$> r

                otherPdu -> 
                    let 
                        text = convert $ "First received PDU must be SerialQueryPdu or ResetQueryPdu, but received " <> show otherPdu
                        in pure $ Left (ErrorPdu InvalidRequest (convert pduBytes) (convert text), text)


-- | Generate a PDU that would be a appropriate response the request PDU.
-- 
respondToPdu :: RtrContext
                -> VersionedPdu 
                -> BS.ByteString 
                -> Session
                -> IO (Either (Pdu, Text) [Pdu])
respondToPdu RtrContext {..} (VersionedPdu pdu pduProtocol) pduBytes (Session sessionProtocol) =                  
    case pdu of 
        SerialQueryPdu sessionId serial -> let
                text :: Text = [i|Unexpected serial query PDU.|]
                in pure $ Left (ErrorPdu CorruptData (convert pduBytes) (convert text), text)            

        ResetQueryPdu -> 
            withProtocolVersionCheck pdu $ pure $ Right []

        RouterKeyPdu _ _ _ _  -> 
            pure $ Right []
        
    where        
        withProtocolVersionCheck _ respond = 
            if sessionProtocol == pduProtocol
                then respond
                else do            
                    let text :: Text = [i|Protocol version is not the same.|]                                
                    pure $ Left (ErrorPdu UnexpectedProtocolVersion (convert pduBytes) (convert text), text)

        withSessionIdCheck :: SessionId 
                            -> IO (Either (Pdu, Text) a) 
                            -> IO (Either (Pdu, Text) a)
        withSessionIdCheck sessionId respond =
            if session == sessionId
                then respond
                else let 
                    text = [i|Wrong sessionId from PDU #{sessionId}, cache sessionId is #{session}.|]
                    in pure $ Left (ErrorPdu CorruptData (convert pduBytes) (convert text), text)

                
                
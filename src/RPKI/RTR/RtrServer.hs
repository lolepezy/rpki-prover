{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

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

import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import           Data.String.Interpolate.IsString

import           Network.Socket                   hiding (recv)
import           Network.Socket.ByteString        (recv, sendAll, sendMany)

import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Resources.Types
import           RPKI.RTR.Pdus
import           RPKI.RTR.RtrState
import           RPKI.RTR.Types

import           RPKI.Parallel
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
    rtrState <- atomically $ newTVar Nothing
    -- re-initialise the rtrContext and create a broadcast 
    -- channel to publish update for every clients
    perClientUpdateChan <- atomically newBroadcastTChan 

    void $ race 
            (runSocketBusiness rtrState perClientUpdateChan)
            (listenToAppStateUpdates rtrState perClientUpdateChan)
  where    

    -- | Handling TCP conections happens here
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
                (connectionProcessor conn peer perClientUpdateChan rtrState) 
                (\_ -> do 
                    logDebug_ logger [i|Closing connection with #{peer}|]
                    close conn)

    -- Block till every new update of the current world version and the VRP set.
    listenToAppStateUpdates rtrState updateChan = do

        -- Wait till some VRPs are generated (or are read from the cache)
        -- This also means that there's a world version in appState.
        --
        -- TODO Make it more strict and obvious if possible (but that will 
        -- require some generic refactoring).
        worldVersion <- atomically $ do 
            hasVrps' <- hasVrps appState
            if not hasVrps' 
                then retry                
                else getWorldVerion appState

        -- Now we can initialise rtrState with a new RtrState.
        atomically . writeTVar rtrState . Just =<< newRtrContext worldVersion      
            
        -- In loop
        --  - wait for a new world version (`listenWorldVersion` does that)
        --  - when a version update happens, update RtrState with calculated diff
        -- 
        -- TODO Make sure there are no lazyness anywhere in the RtrState, so no space leaks
        forever $ do
            (rtrContext, previousVersion, newVersion, newVrps) 
                <- atomically $ 
                    readTVar rtrState >>= \case 
                        Nothing         -> retry
                        Just rtrContext -> do
                            let knownVersion = rtrContext ^. #lastKnownWorldVersion
                            (newVersion, newVrps) <- listenWorldVersion appState knownVersion
                            pure (rtrContext, knownVersion, newVersion, newVrps)

            previousVRPs <- roTx database $ \tx -> getVrps tx database previousVersion
            let vrpDiff = Diff { 
                    added = Set.difference newVrps previousVRPs,
                    deleted = Set.difference previousVRPs newVrps
                }

            -- force evaluation of the new RTR context so that the old ones could be GC-ed.
            let !rtrContext' = updateContext rtrContext newVersion vrpDiff
            atomically $ do 
                writeTVar rtrState (Just rtrContext')
                writeTChan updateChan $ 
                    NotifyPdu (rtrContext' ^. #currentSessionId) (rtrContext' ^. #currentSerial)


    -- For every connection run 3 threads:
    --      receiveFromClient blocks on `recv` and accepts client requests
    --      updateFromAppState blocks on updates from the new data coming from the validation process
    --      sendToClient simply sends all PDU to the client
    -- 
    -- TODO Make it only 2 threads, it should be possible to get rid of `updateFromAppState`
    -- and listen to the `perClientUpdateChan` in `sendToClient`.
    connectionProcessor connection peer perClientUpdateChan rtrState = do

        -- TODO This is bad, first `recv` is called in this thread
        -- and then inside of `serveLoop`, refactor it.
        firstPdu   <- recv connection 1024
        (rtrContext, currentVrps', sendQueue) <- 
            atomically $ (,,) <$> 
                    readTVar rtrState <*>
                    readTVar (currentVrps appState) <*>
                    newCQueue 10

        case processFirstPdu rtrContext currentVrps' firstPdu of
            Left (responsePdu, message) -> do 
                logError_ logger message
                atomically $ writeCQueue sendQueue [responsePdu]
            Right (responsePdus, session) -> do                
                -- logDebug_ logger [i|responsePdus = #{take 10 responsePdus}|]
                atomically $ writeCQueue sendQueue responsePdus

                -- `concurrently` is used here instead of `race` because we want to 
                -- make sure that all the PDUs that ended up in the `sendQueue` will 
                -- be sent to the client.   
                void $ concurrently 
                    (sendToClient session sendQueue)
                    $ race 
                        (serveLoop session sendQueue)                    
                        $ updateFromAppState sendQueue
        where
            sendToClient session sendQueue = do
                logDebug_ logger [i|sendToClient|]
                atomically (readCQueue sendQueue) >>= \case 
                    Nothing   -> pure ()
                    Just pdus ->                        
                        sendMany connection 
                            $ map (\pdu -> BSL.toStrict $ pduToBytes pdu session) pdus

            serveLoop session sendQueue = do
                logDebug_ logger [i|Waiting data from the client #{peer}|]
                pduBytes <- recv connection 128
                logDebug_ logger [i|Received #{BS.length pduBytes} bytes from #{peer}|]
                -- Empty bytestring means connection is closed, so if it's 
                -- empty just silently stop doing anything
                unless (BS.null pduBytes) $
                    join $ atomically $ 
                        case bytesToPduOfKnownVersion session pduBytes of 
                            Left (errorCode, errorMessage) -> do                                
                                let errorPdu = ErrorPdu errorCode (convert pduBytes) (convert errorMessage)
                                writeCQueue sendQueue [errorPdu]
                                closeQueue sendQueue
                                pure $ logError_ logger [i|Error parsing a PDU #{errorMessage}.|]

                            Right pdu -> do                                                                
                                rtrContext  <- readTVar rtrState
                                currentVrps <- readTVar $ currentVrps appState
                                let response = respondToPdu 
                                                    rtrContext
                                                    currentVrps
                                                    (toVersioned session pdu)
                                                    pduBytes
                                                    session                                    

                                case response of 
                                    Left (errorPdu, message) -> do
                                        writeCQueue sendQueue [errorPdu]
                                        closeQueue sendQueue
                                        pure $ do                                             
                                            logDebug_ logger [i|Parsed PDU: #{pdu}.|]
                                            logError_ logger [i|Response: #{pdu}.|]
                                    Right pdus -> do
                                        writeCQueue sendQueue pdus
                                        closeQueue sendQueue
                                        pure $ do 
                                            logDebug_ logger [i|Parsed PDU: #{pdu}.|]                                
                                            serveLoop session sendQueue                                                

            -- send around PDUs that RTR cache initiates to all the clients            
            updateFromAppState sendQueue = do
                logDebug_ logger [i|updateFromAppState|]
                localChan <- atomically $ dupTChan perClientUpdateChan
                forever $ atomically $ do
                    pdu <- readTChan localChan
                    writeCQueue sendQueue [pdu]
                    
                                                
-- 
-- | Process the first PDU and do the protocol version negotiation
-- 
processFirstPdu :: Maybe RtrState 
                -> Set Vrp
                -> BS.ByteString 
                -> Either (Pdu, Text) ([Pdu], Session)
processFirstPdu rtrContext currentVrps pduBytes =    
    case bytesToVersionedPdu pduBytes of
        Left (code, maybeText) -> let 
            text = fromMaybe "Wrong PDU" maybeText
            in Left (ErrorPdu code (convert pduBytes) (convert text), text)

        Right versionedPdu@(VersionedPdu pdu protocolVersion) -> 
            case pdu of 
                SerialQueryPdu _ _ -> let 
                    session' = Session protocolVersion
                    r = respondToPdu rtrContext currentVrps versionedPdu pduBytes session'
                    in (, session') <$> r

                ResetQueryPdu -> let
                    session' = Session protocolVersion
                    r = respondToPdu rtrContext currentVrps versionedPdu pduBytes session'
                    in (, session') <$> r

                otherPdu -> 
                    let 
                        text = convert $ "First received PDU must be SerialQueryPdu or ResetQueryPdu, but received " <> show otherPdu
                        in Left (ErrorPdu InvalidRequest (convert pduBytes) (convert text), text)


-- | Generate a PDU that would be a appropriate response the request PDU.
-- 
respondToPdu :: Maybe RtrState
                -> Set Vrp
                -> VersionedPdu 
                -> BS.ByteString 
                -> Session
                -> Either (Pdu, Text) [Pdu]
respondToPdu 
    rtrContext
    currentVrps 
    (VersionedPdu pdu pduProtocol) 
    pduBytes 
    (Session sessionProtocol) =
        case rtrContext of 
            Nothing -> let
                text :: Text = "VRP set is empty, the RTR cache is not ready yet."
                in Left (ErrorPdu NoDataAvailable (convert pduBytes) (convert text), text)
            Just rtrState@RtrState {..} ->             
                case pdu of 
                    SerialQueryPdu sessionId serial -> 
                        withProtocolVersionCheck pdu $ withSessionIdCheck currentSessionId sessionId $
                            case diffsFromSerial rtrState serial of
                                Nothing -> 
                                    -- we don't have the data, you are too far behind
                                    Right [CacheResetPdu]
                                Just (squashDiffs -> diff) -> 
                                    Right $ [CacheResponsePdu sessionId] 
                                            <> diffPayloadPdus diff
                                            -- TODO Figure out how to instantiate intervals
                                            -- Should they be configurable?                                                                     
                                            <> [EndOfDataPdu sessionId currentSerial defIntervals]
                                        
                    ResetQueryPdu -> 
                        withProtocolVersionCheck pdu $              
                            Right $ [CacheResponsePdu currentSessionId] 
                                    <> currentCachePayloadPdus currentVrps
                                    <> [EndOfDataPdu currentSessionId currentSerial defIntervals]

                    RouterKeyPdu _ _ _ _  -> 
                        Right []
        
    where        
        withProtocolVersionCheck _ respond = 
            if sessionProtocol == pduProtocol
                then respond
                else do            
                    let text :: Text = [i|Protocol version is not the same.|]                                
                    Left (ErrorPdu UnexpectedProtocolVersion (convert pduBytes) (convert text), text)

        withSessionIdCheck currentSessionId sessionId respond =
            if currentSessionId == sessionId
                then respond
                else let 
                    text = [i|Wrong sessionId from PDU #{sessionId}, cache sessionId is #{currentSessionId}.|]
                    in Left (ErrorPdu CorruptData (convert pduBytes) (convert text), text)


-- Create VRP PDUs 
-- TODO Add router certificate PDU here.
diffPayloadPdus :: VrpDiff -> [Pdu]
diffPayloadPdus Diff {..} = 
    map (toPdu Withdrawal) (Set.toList deleted) <>
    map (toPdu Announcement) (Set.toList added)  
    
currentCachePayloadPdus :: Set Vrp -> [Pdu]
currentCachePayloadPdus vrps =
    map (toPdu Announcement) $ Set.toList vrps

toPdu :: Flags -> Vrp -> Pdu
toPdu flags (Vrp asn prefix maxLength) = 
    case prefix of
        Ipv4P v4p -> IPv4PrefixPdu flags v4p asn maxLength
        Ipv6P v6p -> IPv6PrefixPdu flags v6p asn maxLength
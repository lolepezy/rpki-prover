{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.RTR.RtrServer where

import           Control.Concurrent               (forkFinally)

import           Control.Lens                     ((^.))
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

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

import           System.Timeout                   (timeout)


-- 
-- | Main entry point, here we start the RTR server. 
-- 
runRtrServer :: Storage s => AppContext s -> RtrConfig -> IO ()
runRtrServer AppContext {..} RtrConfig {..} = do     
    rtrState <- atomically $ newTVar Nothing
    -- re-initialise the rtrContext and create a broadcast 
    -- channel to publish update for all clients
    updateBroadcastChan <- atomically newBroadcastTChan 

    void $ race 
            (runSocketBusiness rtrState updateBroadcastChan)
            (listenToAppStateUpdates rtrState updateBroadcastChan)
  where    

    -- | Handling TCP conections happens here
    runSocketBusiness rtrState updateBroadcastChan = 
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
                (serveConnection conn peer updateBroadcastChan rtrState) 
                (\_ -> do 
                    logDebug_ logger [i|Closing connection with #{peer}|]
                    close conn)
    
    -- | Block on updates on `appState` and when these update happen
    -- generate the diff, update diffs, increment serials, etc. in RtrState 
    -- and send 'notify PDU' to all clients using `broadcastChan`.
    listenToAppStateUpdates rtrState updateBroadcastChan = do

        -- Wait until a complete world version is generted (or are read from the cache)      
        worldVersion <- atomically $ waitForCompleteVersion appState            

        -- Now we can initialise rtrState with a new RtrState.
        atomically . writeTVar rtrState . Just =<< newRtrState worldVersion      
                    
        forever $ do
            --  wait for a new complete world version
            (rtrContext, previousVersion, newVersion, newVrps) 
                <- atomically $ 
                    readTVar rtrState >>= \case 
                        -- this shouldn't really happen
                        Nothing         -> retry
                        Just rtrContext -> do
                            let knownVersion = rtrContext ^. #lastKnownWorldVersion
                            (newVersion, newVrps) <- waitForNewCompleteVersion appState knownVersion
                            pure (rtrContext, knownVersion, newVersion, newVrps)
        
            previousVRPs <- roTx database $ \tx -> getVrps tx database previousVersion
            let vrpDiff = Diff { 
                    added = Set.difference newVrps previousVRPs,
                    deleted = Set.difference previousVRPs newVrps
                }
                         
            let thereAreVrpUpdates = not $ isEmptyDiff vrpDiff

            -- force evaluation of the new RTR context so that the old ones could be GC-ed.
            let !rtrContext' = 
                    if thereAreVrpUpdates
                        then updatedRtrState rtrContext newVersion vrpDiff
                        else rtrContext { lastKnownWorldVersion = newVersion }                        

            logDebug_ logger [i|Generated new diff in VRP set: added #{Set.size (added vrpDiff)}, deleted #{Set.size (deleted vrpDiff)}.|]

            atomically $ do                
                writeTVar rtrState $ Just rtrContext'
                --  TODO Do not send notify PDUs more often than 1 minute (RFC says so)                    
                when thereAreVrpUpdates $ 
                    writeTChan updateBroadcastChan 
                        [NotifyPdu (rtrContext' ^. #currentSessionId) (rtrContext' ^. #currentSerial)]


    -- For every connection run 3 threads:
    --      receiveFromClient blocks on `recv` and accepts client requests
    --      sendToClient simply sends all PDU to the client
    --
    serveConnection connection peer updateBroadcastChan rtrState = do

        -- TODO This is bad, first `recv` is called in this thread
        -- and then inside of `serveLoop`, refactor it.
        firstPdu <- recv connection 128
        (rtrContext, currentVrps', outboxQueue) <- 
            atomically $ (,,) <$> 
                    readTVar rtrState <*>
                    readTVar (currentVrps appState) <*>
                    newCQueue 10

        case processFirstPdu rtrContext currentVrps' firstPdu of
            Left (errorPdu, message) -> do 
                logError_ logger [i|First PDU from the #{peer} was wrong: #{message}|]
                sendAll connection $ BSL.toStrict $ pduToBytes errorPdu V0

            Right (responsePdus, session) -> do                
                -- logDebug_ logger [i|responsePdus = #{take 10 responsePdus}|]
                atomically $ writeCQueue outboxQueue responsePdus

                -- run `sendToClient` in parallel to the serveLoop
                withAsync (sendToClient session outboxQueue) $ \sender -> do
                    serveLoop session outboxQueue 
                    `finally` do 
                        atomically (closeCQueue outboxQueue)
                        -- Wait before returning from this functions and thus getting the sender killed
                        -- Let it first drain `outboxQueue` to the socket.
                        timeout 30_000_000 $ wait sender
                        
        where
            sendToClient session outboxQueue =
                loop =<< atomically (dupTChan updateBroadcastChan)
                where
                    loop stateUpdateChan = do 
                        -- wait for queued PDUs or for state updates
                        r <- atomically $ 
                                    readCQueue outboxQueue 
                                `orElse` 
                                    (Just <$> readTChan stateUpdateChan)

                        case r of
                            Nothing   -> pure ()
                            Just pdus -> do 
                                sendMany connection $ map 
                                    (\pdu -> BSL.toStrict $ 
                                                pduToBytes pdu (session ^. typed @ProtocolVersion)) 
                                    pdus
                                loop stateUpdateChan

            -- Main loop of the client-server interaction: wait for a PDU from the client, 
            -- 
            serveLoop session outboxQueue = do
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
                                writeCQueue outboxQueue [errorPdu]
                                pure $ logError_ logger [i|Error parsing a PDU #{errorMessage}.|]

                            Right pdu -> do                                                                
                                rtrState'   <- readTVar rtrState
                                currentVrps <- readTVar $ currentVrps appState
                                let response = respondToPdu 
                                                    rtrState'
                                                    currentVrps
                                                    (toVersioned session pdu)
                                                    pduBytes
                                                    session                                    

                                case response of 
                                    Left (errorPdu, message) -> do
                                        writeCQueue outboxQueue [errorPdu]                                        
                                        pure $ logDebug_ logger 
                                            [i|Parsed PDU: #{pdu}, error = #{message}, responding with #{errorPdu}.|]
                                    Right pdus -> do
                                        writeCQueue outboxQueue pdus                                        
                                        pure $ do 
                                            logDebug_ logger [i|Parsed PDU: #{pdu}, responding with #{pdus}.|]
                                            serveLoop session outboxQueue                                                
                               
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
                                Just diffs' -> 
                                    Right $ [CacheResponsePdu sessionId] 
                                            <> diffPayloadPdus (squashDiffs diffs')
                                            -- TODO Figure out how to instantiate intervals
                                            -- Should they be configurable?                                                                     
                                            <> [EndOfDataPdu sessionId currentSerial defIntervals]
                                        
                    ResetQueryPdu -> 
                        withProtocolVersionCheck pdu $              
                            Right $ [CacheResponsePdu currentSessionId] 
                                    <> currentCachePayloadPdus currentVrps
                                    <> [EndOfDataPdu currentSessionId currentSerial defIntervals]

                    other -> let
                        text = "Unexpected PDU received from the client: " <> show other
                        in Left (ErrorPdu NoDataAvailable (convert pduBytes) (convert text), convert text)
                                
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
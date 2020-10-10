{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.RTR.RtrServer where

import           Control.Concurrent               (forkFinally)

import           Control.Lens                     ((^.))
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import           Data.Foldable          (toList)

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           Data.Hex

import           Data.Text                        (Text)

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
import Data.ByteString.Lazy (fromStrict)


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
            (rtrState', previousVersion, newVersion, newVrps) 
                <- atomically $ 
                    readTVar rtrState >>= \case 
                        -- this shouldn't really happen
                        Nothing         -> retry
                        Just rtrState' -> do
                            let knownVersion = rtrState' ^. #lastKnownWorldVersion
                            (newVersion, newVrps) <- waitForNewCompleteVersion appState knownVersion
                            pure (rtrState', knownVersion, newVersion, newVrps)
        
            previousVRPs <- roTx database $ \tx -> getVrps tx database previousVersion
            let vrpDiff = let 
                    newVrpsSet      = Set.fromList newVrps
                    previousVrpsSet = Set.fromList previousVRPs
                    in Diff { 
                        added = Set.difference newVrpsSet previousVrpsSet,
                        deleted = Set.difference previousVrpsSet newVrpsSet
                    }
                         
            let thereAreVrpUpdates = not $ isEmptyDiff vrpDiff

            -- force evaluation of the new RTR context so that the old ones could be GC-ed.
            let !nextRtrState = 
                    if thereAreVrpUpdates
                        then updatedRtrState rtrState' newVersion vrpDiff
                        else rtrState' { lastKnownWorldVersion = newVersion }                        

            logDebug_ logger [i|Generated new diff in VRP set: added #{Set.size (added vrpDiff)}, deleted #{Set.size (deleted vrpDiff)}.|]
            when thereAreVrpUpdates $ do 
                let RtrState {..} = nextRtrState
                let diffs' = map fst $ toList diffs
                logDebug_ logger [i|Updated RTR state: currentSerial #{currentSerial}, diffs #{diffs'}.|]

            atomically $ do                
                writeTVar rtrState $ Just nextRtrState
                --  TODO Do not send notify PDUs more often than 1 minute (RFC says so)                    
                when thereAreVrpUpdates $ 
                    writeTChan updateBroadcastChan 
                        [NotifyPdu (nextRtrState ^. #currentSessionId) (nextRtrState ^. #currentSerial)]


    -- For every connection run 3 threads:
    --      receiveFromClient blocks on `recv` and accepts client requests
    --      sendToClient simply sends all PDU to the client
    --
    serveConnection connection peer updateBroadcastChan rtrState = do

        -- TODO This is bad, first `recv` is called in this thread
        -- and then inside of `serveLoop`, refactor it.
        firstPdu <- recv connection 128
        (rtrState', currentVrps', outboxQueue) <- 
            atomically $ (,,) <$> 
                    readTVar rtrState <*>
                    readTVar (currentVrps appState) <*>
                    newCQueue 10

        let firstPduLazy = fromStrict firstPdu

        -- TODO This piece of code is very similar to the one in `serveLoop`, 
        -- generalise it if it doesn't involve to much fancy monadic magic.
        case bytesToVersionedPdu firstPduLazy of 
            Left (ParsedNothing errorCode errorMessage) -> do 
                let errorPdu = ErrorPdu errorCode (convert firstPdu) (Just $ convert errorMessage)
                logError_ logger [i|First PDU from the #{peer} was wrong: #{errorMessage}, error PDU: #{errorPdu}|]                
                sendAll connection $ BSL.toStrict $ pduToBytes errorPdu V0

            Left (ParsedOnlyHeader errorCode errorMessage header@(PduHeader _ pduType)) -> do   
                -- Do no send an error PDU as a response to an error PDU, it's prohibited by RFC
                unless (pduType == errorPduType) $ do 
                    let errorPdu = ErrorPdu errorCode (convert firstPdu) (Just $ convert errorMessage)
                    sendAll connection $ BSL.toStrict $ pduToBytes errorPdu V0
                logError_ logger [i|Error parsing a PDU #{errorMessage}, parsed header #{header}.|]       

            Right (VersionedPdu errorPdu@(ErrorPdu {}) _) -> do
                logError_ logger [i|Received an error from #{peer}: #{errorPdu}.|]

            Right versionedPdu ->            
                case processFirstPdu rtrState' currentVrps' versionedPdu firstPduLazy of 
                    Left (errorPdu, errorMessage) -> do
                        let errorBytes = pduToBytes errorPdu V0
                        logError_ logger [i|Cannot respond to the first PDU from the #{peer}: #{errorMessage}, error PDU: #{errorPdu}, errorBytes = #{hex errorBytes}, length = #{pduLength errorPdu V0}|]
                        sendAll connection $ BSL.toStrict $ pduToBytes errorPdu V0

                    Right (responsePdus, session) -> do
                        atomically $ writeCQueue outboxQueue responsePdus

                        -- run `sendToClient` in parallel to the serveLoop
                        withAsync (sendToClient session outboxQueue) $ \sender -> do
                            serveLoop session outboxQueue 
                            `finally` do 
                                atomically (closeCQueue outboxQueue)
                                -- Wait before returning from this functions and thus getting the sender killed
                                -- Let it first drain `outboxQueue` to the socket.
                                void $ timeout 30_000_000 $ wait sender

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
                    join $ atomically $ do 
                        let pduBytesLazy = fromStrict pduBytes
                        case bytesToVersionedPdu pduBytesLazy of 
                            -- Couldn't parse anything at all
                            Left (ParsedNothing errorCode errorMessage) -> do
                                let errorPdu = ErrorPdu errorCode (convert pduBytes) (Just $ convert errorMessage)
                                writeCQueue outboxQueue [errorPdu]
                                pure $ logError_ logger [i|Error parsing a PDU #{errorMessage}.|]

                            -- Managed to parse at elast the header (protocol version and PDU code),
                            -- it will help with logging.
                            Left (ParsedOnlyHeader errorCode errorMessage header@(PduHeader _ pduType)) -> do   
                                -- Do no send an error PDU as a response to an error PDU, it's prohibited by RFC
                                unless (pduType == errorPduType) $ do 
                                    let errorPdu = ErrorPdu errorCode (convert pduBytes) (Just $ convert errorMessage)
                                    writeCQueue outboxQueue [errorPdu]
                                pure $ logError_ logger [i|Error parsing a PDU #{errorMessage}, parsed header #{header}.|]

                            -- Received an ErrorPdu which means client is not happy with us.
                            -- Log it and stop here, we don't want to screw things up even more,
                            -- maybe the client will try again.
                            Right (VersionedPdu errorPdu@(ErrorPdu {}) _) -> do
                                pure $ logError_ logger [i|Received an error from #{peer}: #{errorPdu}.|]

                            -- There's PDU we can potentially react to
                            Right (VersionedPdu pdu _) -> do
                                rtrState'   <- readTVar rtrState
                                currentVrps <- readTVar $ currentVrps appState
                                let response = respondToPdu 
                                                    rtrState'
                                                    currentVrps
                                                    (toVersioned session pdu)
                                                    pduBytesLazy
                                                    session                                    

                                case response of 
                                    Left (errorPdu, message) -> do
                                        writeCQueue outboxQueue [errorPdu]                                        
                                        pure $ logDebug_ logger 
                                            [i|Parsed PDU: #{pdu}, error = #{message}, responding with #{errorPdu}.|]
                                    Right pdus -> do
                                        writeCQueue outboxQueue pdus                                        
                                        pure $ do 
                                            logDebug_ logger [i|Parsed PDU: #{pdu}, responding with #{take 2 pdus}.. PDUs.|]
                                            serveLoop session outboxQueue                                                


-- 
-- | Process the first PDU and do the protocol version negotiation
-- 
processFirstPdu :: Maybe RtrState 
                -> [Vrp]
                -> VersionedPdu
                -> BSL.ByteString 
                -> Either (Pdu, Text) ([Pdu], Session)
processFirstPdu 
    rtrContext 
    currentVrps 
    versionedPdu@(VersionedPdu pdu protocolVersion) 
    pduBytes =                
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
                in Left (ErrorPdu InvalidRequest (convert pduBytes) (Just $ convert text), text)


-- | Generate a PDU that would be a appropriate response the request PDU.
-- 
respondToPdu :: Maybe RtrState
                -> [Vrp]
                -> VersionedPdu 
                -> BSL.ByteString 
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
                in Left (ErrorPdu NoDataAvailable (convert pduBytes) (Just $ convert text), text)
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

                    -- TODO Refactor that stuff 
                    --  - react properly on ErrorPdu
                    --  - Log 
                    -- Dont't send back anything 
                    ErrorPdu {} -> 
                        Right []

                    other -> let
                        text = "Unexpected PDU received from the client: " <> show other
                        in Left (ErrorPdu NoDataAvailable (convert pduBytes) (Just $ convert text), convert text)
                                
    where        
        withProtocolVersionCheck _ respond = 
            if sessionProtocol == pduProtocol
                then respond
                else do            
                    let text :: Text = [i|Protocol version is not the same.|]                                
                    Left (ErrorPdu UnexpectedProtocolVersion (convert pduBytes) (Just $ convert text), text)

        withSessionIdCheck currentSessionId sessionId respond =
            if currentSessionId == sessionId
                then respond
                else let 
                    text = [i|Wrong sessionId from PDU #{sessionId}, cache sessionId is #{currentSessionId}.|]
                    in Left (ErrorPdu CorruptData (convert pduBytes) (Just $ convert text), text)


-- Create VRP PDUs 
-- TODO Add router certificate PDU here.
diffPayloadPdus :: VrpDiff -> [Pdu]
diffPayloadPdus Diff {..} = 
    map (toPdu Withdrawal) (Set.toList deleted) <>
    map (toPdu Announcement) (Set.toList added)  
    
currentCachePayloadPdus :: [Vrp] -> [Pdu]
currentCachePayloadPdus vrps =
    map (toPdu Announcement) vrps

toPdu :: Flags -> Vrp -> Pdu
toPdu flags (Vrp asn prefix maxLength) = 
    case prefix of
        Ipv4P v4p -> IPv4PrefixPdu flags v4p asn maxLength
        Ipv6P v6p -> IPv6PrefixPdu flags v6p asn maxLength
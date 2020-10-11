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

import           Network.Socket                   hiding (recv, recvFrom)
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
            pure addr

        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress addr)            
            let fd = fdSocket sock
            setCloseOnExecIfNeeded fd
            listen sock 10
            pure sock

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
                writeTVar rtrState $! let !z = Just nextRtrState in z
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
        logDebug_ logger [i|Waiting first PDU from the client #{peer}|]
        firstPdu <- recv connection 1024
        (rtrState', currentVrps', outboxQueue) <- 
            atomically $ (,,) <$> 
                    readTVar rtrState <*>
                    readTVar (currentVrps appState) <*>
                    newCQueue 10

        let firstPduLazy = BSL.fromStrict firstPdu

        let (errorPdu, message, versionedPdu) = 
                analyzePdu peer firstPduLazy $ bytesToVersionedPdu firstPduLazy        

        ifJust errorPdu $ \errorPdu' -> 
            sendAll connection $ BSL.toStrict $ pduToBytes errorPdu' V0

        ifJust message $ logError_ logger

        ifJust versionedPdu $ \versionedPdu' -> do
            case processFirstPdu rtrState' currentVrps' versionedPdu' firstPduLazy of 
                Left (errorPdu, errorMessage) -> do
                    let errorBytes = pduToBytes errorPdu V0
                    logError_ logger $ [i|Cannot respond to the first PDU from the #{peer}: #{errorMessage},|] <> 
                                       [i|error PDU: #{errorPdu}, errorBytes = #{hex errorBytes}, length = #{pduLength errorPdu V0}|]
                    sendAll connection $ BSL.toStrict $ pduToBytes errorPdu V0

                Right (responsePdus, session) -> do
                    atomically $ writeCQueue outboxQueue responsePdus

                    withAsync (sendToClient session outboxQueue) $ \sender -> do
                        serveLoop session outboxQueue 
                            `finally` do 
                                logDebug_ logger [i|111111|]
                                atomically (closeCQueue outboxQueue)
                                -- Wait before returning from this functions and thus getting the sender killed
                                -- Let it first drain `outboxQueue` to the socket.
                                void $ timeout 30_000_000 $ wait sender
                                logDebug_ logger [i|222222|]

                    logDebug_ logger [i|33333|]

        where
            sendToClient session outboxQueue =
                loop =<< atomically (dupTChan updateBroadcastChan)
                where
                    loop stateUpdateChan = do 
                        logDebug_ logger [i|sendToClient loop|]
                        -- wait for queued PDUs or for state updates
                        r <- atomically $ 
                                    readCQueue outboxQueue 
                                `orElse` 
                                    (Just <$> readTChan stateUpdateChan)

                        case r of
                            Nothing   -> pure ()
                            Just pdus -> do 
                                logDebug_ logger [i|sendToClient loop, before send.|]
                                sendMany connection $ map 
                                    (\pdu -> BSL.toStrict $ 
                                                pduToBytes pdu (session ^. typed @ProtocolVersion)) 
                                    pdus
                                logDebug_ logger [i|sendToClient loop, after send.|]                                    
                                loop stateUpdateChan

            -- Main loop of the client-server interaction: wait for a PDU from the client, 
            -- 
            serveLoop session outboxQueue = do
                logDebug_ logger [i|Waiting data from the client #{peer}|]                
                -- pduBytes <- recv connection 1024
                pduBytes <- recv connection 1024
                -- let pduBytes = mempty :: BS.ByteString
                -- threadDelay 1000_000_000
                logDebug_ logger [i|Received #{BS.length pduBytes} bytes from #{peer}|]
                -- Empty bytestring means connection is closed, so if it's 
                -- empty just silently stop doing anything
                unless (BS.null pduBytes) $
                    join $ atomically $ do 
                        let pduBytesLazy = BSL.fromStrict pduBytes

                        let (errorPdu, message, versionedPdu) = 
                                analyzePdu peer pduBytesLazy $ bytesToVersionedPdu pduBytesLazy

                        let ifJust1 z def f = maybe def f z                            
                        let noop = pure $ pure ()                        

                        let logError = ifJust1 message noop $ \m -> pure $ logError_ logger m

                        let respond = ifJust1 versionedPdu noop $ \(VersionedPdu pdu _) -> do
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
                                                                
                        ifJust1 errorPdu (pure ()) $ \errorPdu' -> 
                            writeCQueue outboxQueue [errorPdu']                                        

                        liftM2 (>>) logError respond


-- | Helper function to reduce repeated code
-- 
analyzePdu :: Show peer => 
                peer 
            -> BSL.ByteString 
            -> Either PduParseError VersionedPdu 
            -> (Maybe Pdu, Maybe Text, Maybe VersionedPdu)
analyzePdu peer pduBytes = \case
        Left (ParsedNothing errorCode errorMessage) -> let
            errorPdu = ErrorPdu errorCode (Just $ convert pduBytes) (Just $ convert errorMessage)
            in (Just errorPdu , 
                Just [i|First PDU from the #{peer} was wrong: #{errorMessage}, error PDU: #{errorPdu}|], 
                Nothing)

        Left (ParsedOnlyHeader errorCode errorMessage header@(PduHeader _ pduType)) ->                 
            let message = [i|Error parsing a PDU #{errorMessage}, parsed header #{header}.|]
            in if pduType == errorPduType
                then let 
                    errorPdu = ErrorPdu errorCode (Just $ convert pduBytes) (Just $ convert errorMessage)
                    in (Just errorPdu, Just message, Nothing)                        
                else 
                    -- Do no send an error PDU as a response to an error PDU, it's prohibited by RFC                        
                    (Nothing, Just message, Nothing)                            

        Right (VersionedPdu errorPdu@(ErrorPdu {}) _) ->
            (Nothing, Just [i|Received an error from #{peer}: #{errorPdu}.|], Nothing)                

        Right versionedPdu -> (Nothing, Nothing, Just versionedPdu)


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
                in Left (ErrorPdu InvalidRequest (Just $ convert pduBytes) (Just $ convert text), text)


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
                in Left (ErrorPdu NoDataAvailable (Just $ convert pduBytes) (Just $ convert text), text)
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
                        in Left (ErrorPdu NoDataAvailable (Just $ convert pduBytes) (Just $ convert text), convert text)
                                
    where        
        withProtocolVersionCheck _ respond = 
            if sessionProtocol == pduProtocol
                then respond
                else do            
                    let text :: Text = [i|Protocol version is not the same.|]                                
                    Left (ErrorPdu UnexpectedProtocolVersion (Just $ convert pduBytes) (Just $ convert text), text)

        withSessionIdCheck currentSessionId sessionId respond =
            if currentSessionId == sessionId
                then respond
                else let 
                    text = [i|Wrong sessionId from PDU #{sessionId}, cache sessionId is #{currentSessionId}.|]
                    in Left (ErrorPdu CorruptData (Just $ convert pduBytes) (Just $ convert text), text)


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
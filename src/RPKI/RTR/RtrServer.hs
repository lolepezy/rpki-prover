{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.RTR.RtrServer where

import           Control.Concurrent               (forkFinally)

import           Control.Lens                     ((^.))

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad

import           Data.Generics.Product.Typed

import           Data.Foldable                    (for_)

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS

import           Data.List.Split                  (chunksOf)

import qualified Data.Set                         as Set
import qualified Data.List                        as List
import           Data.Ord
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)

import           Network.Socket                   
import           Network.Socket.ByteString        (recv, sendAll, sendMany)

import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Resources.Types
import           RPKI.RTR.Pdus
import           RPKI.RTR.RtrState
import           RPKI.RTR.Types
import           RPKI.SLURM.SlurmProcessing

import           RPKI.Parallel
import           RPKI.Time
import           RPKI.Util                        (convert, hexL, decodeBase64)

import           RPKI.AppState
import           RPKI.AppTypes
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database

import           System.Timeout                   (timeout)
import           Time.Types

-- 
-- | Main entry point, here we start the RTR server. 
-- 
runRtrServer :: Storage s => AppContext s -> RtrConfig -> IO ()
runRtrServer appContext@AppContext {..} RtrConfig {..} = do         
    -- re-initialise `rtrState` and create a broadcast 
    -- channel to publish update for all clients
    rtrState <- newTVarIO Nothing
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
            head <$> getAddrInfo (Just hints) (Just rtrAddress) (Just port)            

        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 1024
            pure sock

        loop sock = forever $ do
            (conn, peer) <- accept sock
            logDebug logger [i|Connection from #{peer}|]
            void $ forkFinally 
                (serveConnection conn peer updateBroadcastChan rtrState) 
                (\_ -> do 
                    logDebug logger [i|Closing connection with #{peer}|]
                    close conn)
    
    -- | Block on updates on `appState` and when these update happen
    --
    --   - generate the diff, update stored diffs, increment serials, etc. in RtrState 
    --   - send 'notify PDU' to all clients using `broadcastChan`.
    --
    listenToAppStateUpdates rtrState updateBroadcastChan = do        

        -- If a version is recovered from the storage after the start, 
        -- use it, otherwise wait until some complete version is generated       
        -- by a validation process 
        logInfo logger [i|RTR server: waiting for the first complete world version.|] 
        worldVersion <- atomically $ waitForVersion appState                
    
        -- Do not store more the thrise the amound of VRPs in the diffs as the initial size.
        -- It's totally heuristical way of avoiding memory bloat
        rtrPayloads <- atomically $ getRtrPayloads appState
        let maxStoredDiffs = Set.size (rtrPayloads ^. #flatVrps)
                
        logDebug logger [i|RTR started with version #{worldVersion}, maxStoredDiffs = #{maxStoredDiffs}.|] 

        atomically $ writeTVar rtrState $ 
                        Just $ newRtrState worldVersion maxStoredDiffs

        lastTimeNotified <- newTVarIO Nothing

        forever $ do
            (rtrState', previousVersion, newVersion, currentRtrPayload) <- waitForLatestRtrPayload appContext rtrState
            previousRtrPayload <- readRtrPayload appContext previousVersion
            
            let rtrDiff = evalDiffs previousRtrPayload currentRtrPayload
            let thereAreRtrUpdates = not $ emptyDiffs rtrDiff

            let 
                previousVrpSize = Set.size $ previousRtrPayload ^. #flatVrps 
                currentVrpSize  = Set.size $ currentRtrPayload ^. #flatVrps
                previousBgpSecSize = Set.size $ previousRtrPayload ^. #bgpSec 
                currentBgpSecSize  = Set.size $ currentRtrPayload ^. #bgpSec 
                in logDebug logger $ [i|Notified about an update: #{previousVersion} -> #{newVersion}, |] <> 
                              [i|VRPs: #{previousVrpSize} -> #{currentVrpSize}, |] <>
                              [i|BGPSecs: #{previousBgpSecSize} -> #{currentBgpSecSize}.|]

            -- force evaluation of the new RTR state so that the old ones could be GC-ed.
            let !nextRtrState = if thereAreRtrUpdates
                    then updatedRtrState rtrState' newVersion rtrDiff
                    else rtrState' { lastKnownWorldVersion = newVersion }

            -- logDebug logger [i|Generated new diff, VRPs: added #{length (added vrpDiff)}, deleted #{length (deleted vrpDiff)}.|]
            -- when thereAreRtrUpdates $ do 
            --     let RtrState {..} = nextRtrState
                -- let diffsStr = concatMap (\(n, d) -> [i|(#{n}, added = #{Set.size $ added d}, deleted = #{Set.size $ deleted d})|]) $ toList diffs
                -- logDebug logger [i|Updated RTR state: currentSerial #{currentSerial}, diffs #{diffsStr}.|]

            Now now <- thisInstant            

            atomically $ do                
                writeTVar rtrState $! Just $! nextRtrState

                -- https://datatracker.ietf.org/doc/html/rfc8210#section-8.2
                -- "The cache MUST rate-limit Serial Notifies to no more frequently than one per minute."
                -- 
                let moreThanMinuteAgo lastTime = not $ closeEnoughMoments lastTime now (Seconds 60)
                sendNotify <- maybe True moreThanMinuteAgo <$> readTVar lastTimeNotified                 

                when (sendNotify && thereAreRtrUpdates) $ do                    
                    let notifyPdu = NotifyPdu (nextRtrState ^. #currentSessionId) (nextRtrState ^. #currentSerial)
                    writeTChan updateBroadcastChan [notifyPdu]
                    writeTVar lastTimeNotified $ Just now


    -- For every connection run 2 threads:
    --      serveLoop blocks on `recv` and accepts client's requests
    --      sendToClient simply sends all PDUs to the client
    -- They communicate using the outboxChan
    serveConnection connection peer updateBroadcastChan rtrState = do        
        logDebug logger [i|Waiting for the first PDU from client #{peer}|]
        firstPdu <- recv connection 1024
        (rtrState', rtrPayloads, outboxQueue) <- 
            atomically $ (,,) <$> 
                    readTVar rtrState <*>
                    getRtrPayloads appState <*>
                    newCQueue 10

        let firstPduLazy = LBS.fromStrict firstPdu

        let (errorPdu, message, versionedPdu) = 
                analyzePdu peer firstPduLazy $ bytesToVersionedPdu firstPduLazy        

        for_ errorPdu $ \errorPdu' -> 
            sendAll connection $ LBS.toStrict $ pduToBytes errorPdu' V0

        for_ message $ logError logger

        for_ versionedPdu $ \versionedPdu' -> do
            case processFirstPdu rtrState' rtrPayloads versionedPdu' firstPduLazy of 
                Left (errorPdu', errorMessage) -> do
                    let errorBytes = pduToBytes errorPdu' V0
                    logError logger $ [i|Cannot respond to the first PDU from the #{peer}: #{errorMessage},|] <> 
                                      [i|error PDU: #{errorPdu'}, errorBytes = #{hexL errorBytes}, length = #{pduLength errorPdu' V0}|]
                    sendAll connection $ LBS.toStrict $ pduToBytes errorPdu' V0

                Right (responsePdus, session, warning) -> do
                    for_ warning $ logWarn logger
                    atomically $ writeCQueue outboxQueue responsePdus

                    withAsync (sendToClient session outboxQueue) $ \sender -> do
                        serveLoop session outboxQueue 
                            `finally` do 
                                atomically (closeCQueue outboxQueue)
                                -- Wait before returning from this functions and thus getting the sender killed
                                -- Let it first drain `outboxQueue` to the socket.
                                void $ timeout 30_000_000 $ wait sender
        where
            -- | Wait for PDUs to appear in either broadcast chan or 
            -- in this session's outbox and send them to the socket.
            sendToClient session outboxQueue =
                loop =<< atomically (dupTChan updateBroadcastChan)
                where
                    loop stateUpdateChan = do                         
                        -- wait for queued PDUs or for state updates
                        r <- atomically $ 
                                    readCQueue outboxQueue 
                                <|> (Just <$> readTChan stateUpdateChan)

                        for_ r $ \pdus -> do 
                            for_ (chunksOf 1000 pdus) $ \chunk -> 
                                sendMany connection 
                                    $ map (\pdu -> LBS.toStrict $ 
                                            pduToBytes pdu (session ^. typed @ProtocolVersion)) chunk

                            loop stateUpdateChan

            -- Main loop of the client-server interaction: wait for a PDU from the client, 
            -- 
            serveLoop session outboxQueue = do
                logDebug logger [i|Waiting data from the client #{peer}|]
                pduBytes <- recv connection 1024                
                logDebug logger [i|Received #{BS.length pduBytes} bytes from #{peer}|]
                -- Empty bytestring means connection is closed, so if it's 
                -- empty just silently stop doing anything
                unless (BS.null pduBytes) $
                    join $ atomically $ do 
                        -- all the real work happens inside of pure `responseAction`, 
                        -- for better testability.
                        rtrState'   <- readTVar rtrState                        
                        rtrPayloads <- getRtrPayloads appState
                        case responseAction logger peer session rtrState' rtrPayloads pduBytes of 
                            Left (pdus, io) -> do 
                                writeCQueue outboxQueue pdus
                                pure io
                            Right (pdus, io) -> do 
                                writeCQueue outboxQueue pdus
                                pure $ io <> serveLoop session outboxQueue


readRtrPayload :: Storage s => AppContext s -> WorldVersion -> IO RtrPayloads 
readRtrPayload AppContext {..} worldVersion = do 
    db <- readTVarIO database

    (vrps, bgpSec) <- roTx db $ \tx -> do 
                slurm <- slurmForVersion tx db worldVersion
                vrps <- getVrps tx db worldVersion >>= \case 
                            Nothing   -> pure mempty
                            Just vrps -> pure $ maybe vrps (`applySlurmToVrps` vrps) slurm

                bgps <- getBgps tx db worldVersion
                let bgpSec = maybe bgps (`applySlurmBgpSec` bgps) slurm
                
                pure (vrps, bgpSec)

    pure $ mkRtrPayloads vrps bgpSec


waitForLatestRtrPayload :: Storage s => AppContext s 
        -> TVar (Maybe RtrState) 
        -> IO (RtrState, WorldVersion, WorldVersion, RtrPayloads)
waitForLatestRtrPayload AppContext {..} rtrState = do 
    atomically $ 
        readTVar rtrState >>= \case 
            Nothing        -> retry
            Just rtrState' -> do
                let knownVersion = rtrState' ^. #lastKnownWorldVersion
                (newVersion, rtrPayloads) <- waitForNewVersion appState knownVersion                
                pure (rtrState', knownVersion, newVersion, rtrPayloads)

-- | Auxiliarry pure function to avoid doing too much inside of IO.
-- Here IO is only doing logging
--
-- TODO Do something with contravariant logging here, it's the right place.
-- 
responseAction :: Show peer => 
                   AppLogger 
                -> peer 
                -> Session 
                -> Maybe RtrState 
                -> RtrPayloads
                -> BS.ByteString 
                -> Either ([Pdu], IO ()) ([Pdu], IO ())
responseAction logger peer session rtrState vrps pduBytes = 
    let 
        pduBytesLazy = LBS.fromStrict pduBytes

        (errorPdu, message, versionedPdu) = 
                analyzePdu peer pduBytesLazy $ bytesToVersionedPdu pduBytesLazy
        
        logError' = maybe mempty (logError logger) message

        errorResponse = maybe mempty (: []) errorPdu
                
        response pdu = let 
            r = respondToPdu 
                    rtrState
                    vrps
                    (toVersioned session pdu)
                    pduBytesLazy
                    session
            in case r of 
                Left (errorPdu', message') -> let
                    ioAction = logDebug logger [i|Parsed PDU: #{pdu}, error = #{message'}, responding with #{errorPdu}.|]
                    in Left ([errorPdu'], ioAction)                                     
                Right (pdus, warning) -> let
                    ioAction = do 
                        for_ warning $ logWarn logger
                        logDebug logger [i|Parsed PDU: #{pdu}, responding with (first 10) #{take 10 pdus}.. PDUs.|]                            
                    in Right (pdus, ioAction)                                     

        errors = (mempty, logError') <> (errorResponse, mempty)
    in 
        case versionedPdu of 
            Nothing                   -> Left errors
            Just (VersionedPdu pdu _) -> 
                (errors <> ) <$> response pdu 


-- | Helper function to reduce repeated code.
-- 
analyzePdu :: Show peer => 
               peer 
            -> LBS.ByteString 
            -> Either PduParseError VersionedPdu 
            -> (Maybe Pdu, Maybe Text, Maybe VersionedPdu)
analyzePdu peer pduBytes = \case
        Left (ParsedNothing errorCode errorMessage) -> let
            errorPdu = ErrorPdu errorCode (Just $ convert pduBytes) (Just $ convert errorMessage)
            in (Just errorPdu, 
                Just [i|PDU from the #{peer} was wrong: #{errorMessage}, error PDU: #{errorPdu}|], 
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

        Right (VersionedPdu errorPdu@ErrorPdu {} _) ->
            (Nothing, Just [i|Received an error from #{peer}: #{errorPdu}.|], Nothing)                

        Right versionedPdu -> (Nothing, Nothing, Just versionedPdu)


-- 
-- | Process the first PDU and do the protocol version negotiation.
-- 
processFirstPdu :: Maybe RtrState 
                -> RtrPayloads
                -> VersionedPdu
                -> LBS.ByteString 
                -> Either (Pdu, Text) ([Pdu], Session, Maybe Text)
processFirstPdu 
    rtrState 
    vrps 
    versionedPdu@(VersionedPdu pdu protocolVersion) 
    pduBytes =                
    case pdu of 
        SerialQueryPdu _ _ -> let 
            session' = Session protocolVersion
            r = respondToPdu rtrState vrps versionedPdu pduBytes session'
            in fmap (\(pdus, message) -> (pdus, session', message)) r

        ResetQueryPdu -> let
            session' = Session protocolVersion
            r = respondToPdu rtrState vrps versionedPdu pduBytes session'
            in fmap (\(pdus, message) -> (pdus, session', message)) r

        otherPdu -> let 
            text = convert $ "First received PDU must be SerialQueryPdu or ResetQueryPdu, " <> 
                             "but received " <> show otherPdu
            in Left (ErrorPdu InvalidRequest (Just $ convert pduBytes) (Just $ convert text), text)


-- | Generate PDUs that would be an appropriate response the request PDU.
-- 
respondToPdu :: Maybe RtrState
                -> RtrPayloads
                -> VersionedPdu 
                -> LBS.ByteString 
                -> Session
                -> Either (Pdu, Text) ([Pdu], Maybe Text)
respondToPdu 
    rtrState
    vrps 
    (VersionedPdu pdu pduProtocol) 
    pduBytes 
    (Session sessionProtocol) =
        case rtrState of 
            Nothing -> let
                text :: Text = "VRP set is empty, the RTR cache is not ready yet."
                in Left (ErrorPdu NoDataAvailable (Just $ convert pduBytes) (Just $ convert text), text)
            Just rtrState'@RtrState {..} ->             
                case pdu of 
                    SerialQueryPdu sessionId clientSerial -> 
                        withProtocolVersionCheck pdu $ withSessionIdCheck currentSessionId sessionId $
                            if clientSerial == currentSerial 
                                then let 
                                    pdus = [CacheResponsePdu sessionId] 
                                        <> [EndOfDataPdu sessionId currentSerial defIntervals]
                                    in Right (pdus, Nothing)
                                else 
                                    case diffsFromSerial rtrState' clientSerial of
                                        Nothing -> 
                                            -- we don't have the data, you are too far behind
                                            Right (
                                                [CacheResetPdu], 
                                                Just [i|No data for serial #{clientSerial}.|])
                                        Just diffs' -> let                                            
                                            pdus = [CacheResponsePdu sessionId] 
                                                    <> diffPayloadPdus (squashDiffs diffs')
                                                    -- TODO Figure out how to instantiate intervals
                                                    -- Should they be configurable?                                                                     
                                                    <> [EndOfDataPdu sessionId currentSerial defIntervals]
                                            in Right (pdus, Nothing)

                    ResetQueryPdu -> 
                        withProtocolVersionCheck pdu $ let
                            pdus = [CacheResponsePdu currentSessionId] 
                                    <> currentCachePayloadPdus vrps
                                    <> [EndOfDataPdu currentSessionId currentSerial defIntervals]
                            in Right (pdus, Nothing) 

                    -- TODO Refactor that stuff 
                    --  - react properly on ErrorPdu
                    --  - Log 
                    -- Dont't send back anything 
                    ErrorPdu {} -> 
                        Right ([], Nothing)

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
diffPayloadPdus :: RtrDiffs -> [Pdu]
diffPayloadPdus GenDiffs {..} = 
    vrpWithdrawn <> vrpPdusAnn <> 
    mconcat bgpSecWithdrawn <> mconcat bgpSecPdusAnn
  where
    vrpWithdrawn = map (vrpToPdu Withdrawal) (Set.toList $ vrpDiff ^. #deleted)
    vrpPdusAnn   = map (vrpToPdu Announcement) $ sortVrps $ Set.toList $ vrpDiff ^. #added 
    bgpSecWithdrawn = map (bgpSecToPdu Withdrawal) $ Set.toList $ bgpSecDiff ^. #deleted
    bgpSecPdusAnn   = map (bgpSecToPdu Announcement) $ Set.toList $ bgpSecDiff ^. #added

    
currentCachePayloadPdus :: RtrPayloads -> [Pdu]
currentCachePayloadPdus RtrPayloads {..} =
    vrpPdusAnn <> mconcat bgpSecPdusAnn
  where    
    vrpPdusAnn = map (vrpToPdu Announcement) $ sortVrps $ Set.toList flatVrps
    bgpSecPdusAnn = map (bgpSecToPdu Announcement) $ Set.toList bgpSec
    

-- Sort VRPs before sending them to routers
-- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis-02#section-11
-- 
sortVrps :: [Vrp] -> [Vrp] 
sortVrps = List.sortBy compareVrps 
  where
    compareVrps (Vrp asn1 p1 ml1) (Vrp asn2 p2 ml2) = 
        compare asn1 asn2 <> 
        -- Sort prefixes backwards -- it automatically means that 
        -- smaller prefixes will be in front of larger ones.
        compare (Down p1) (Down p2) <> 
        -- smaller max length should precede?
        compare ml1 ml2
    

-- toPdu :: Flags -> Vrp -> Pdu
-- toPdu flags (Vrp asn prefix maxLength) = 
--     case prefix of
--         Ipv4P v4p -> IPv4PrefixPdu flags v4p asn maxLength
--         Ipv6P v6p -> IPv6PrefixPdu flags v6p asn maxLength

vrpToPdu :: Flags -> Vrp -> Pdu
vrpToPdu flags (Vrp asn prefix maxLength) = 
    case prefix of
        Ipv4P v4p -> IPv4PrefixPdu flags v4p asn maxLength
        Ipv6P v6p -> IPv6PrefixPdu flags v6p asn maxLength

bgpSecToPdu :: Flags -> BGPSecPayload -> [Pdu]
bgpSecToPdu flags BGPSecPayload {..} = 
    let Right (DecodedBase64 spkiBytes) = decodeBase64 (unSPKI bgpSecSpki) ("WTF broken SPKI" :: Text)
    in map (\asn -> RouterKeyPdu asn flags bgpSecSki (LBS.fromStrict spkiBytes)) bgpSecAsns    
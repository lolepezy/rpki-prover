{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

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
import qualified Data.ByteString.Builder          as BB

import           Data.List.Split                  (chunksOf)

import qualified Data.Set                         as Set
import qualified Data.Vector                      as V

import           Data.Coerce
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
import           RPKI.RTR.Protocol
import           RPKI.SLURM.SlurmProcessing

import           RPKI.Parallel
import           RPKI.Time
import           RPKI.Util                        (convert, hex, decodeBase64)

import           RPKI.AppState
import           RPKI.AppTypes
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database

import           System.Timeout                   (timeout)
import           Time.Types


data PduLike = TruePdu Pdu | SerialisedPdu BS.ByteString 
    deriving (Show, Eq)

-- 
-- | Main entry point, here we start the RTR server. 
-- 
runRtrServer :: Storage s => AppContext s -> RtrConfig -> IO ()
runRtrServer appContext RtrConfig {..} = do         
    -- re-initialise `rtrState` and create a broadcast 
    -- channel to publish update for all clients
    let rtrState = appContext ^. #appState . #rtrState
    updateBroadcastChan <- atomically newBroadcastTChan 

    void $ race 
            (runSocketBusiness rtrState updateBroadcastChan)
            (listenToAppStateUpdates rtrState updateBroadcastChan)
  where    

    logger = getRtrLogger appContext

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
            logInfo logger [i|Connection from #{peer}|]
            void $ forkFinally 
                (serveConnection conn peer updateBroadcastChan rtrState) 
                (\_ -> do 
                    logInfo logger [i|Closing connection with #{peer}|]
                    close conn)
    
    -- | Block on updates on `appState` and when these update happen
    --
    --   - generate the diff, update stored diffs, increment serials, etc. in RtrState 
    --   - send 'notify PDU' to all clients using `broadcastChan`.
    --
    listenToAppStateUpdates rtrState updateBroadcastChan = do        

        let appState = appContext ^. #appState

        -- If a version is recovered from the storage after the start, 
        -- use it, otherwise wait until some complete version is generated       
        -- by a validation process 
        logInfo logger [i|RTR server: waiting for the first complete world version.|] 
        worldVersion <- atomically $ waitForAnyVersion appState                
    
        -- Do not store more than amound of VRPs in the diffs as the initial size.
        -- It's totally heuristical way of avoiding memory bloat
        rtrPayloads <- atomically $ readRtrPayloads appState
        let maxStoredDiffs = V.length (rtrPayloads ^. #uniqueVrps)
                
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
                previousVrpSize = V.length $ previousRtrPayload ^. #uniqueVrps 
                currentVrpSize  = V.length $ currentRtrPayload ^. #uniqueVrps
                previousBgpSecSize = Set.size $ previousRtrPayload ^. #bgpSec 
                currentBgpSecSize  = Set.size $ currentRtrPayload ^. #bgpSec 
                in logDebug logger $ [i|Notified about an update: #{previousVersion} -> #{newVersion}, |] <> 
                              [i|VRPs: #{previousVrpSize} -> #{currentVrpSize}, |] <>
                              [i|BGPSecs: #{previousBgpSecSize} -> #{currentBgpSecSize}.|]

            -- force evaluation of the new RTR state so that the old ones could be GC-ed.
            let !nextRtrState = if thereAreRtrUpdates
                    then updatedRtrState rtrState' newVersion rtrDiff
                    else rtrState' { lastKnownWorldVersion = newVersion }                    

            logDiff rtrDiff

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
                    writeTChan updateBroadcastChan [TruePdu notifyPdu]
                    writeTVar lastTimeNotified $ Just now
        where
          logDiff GenDiffs {..} = do 
            
            let diffText =
                    [i|VRPs: added #{Set.size $ added vrpDiff}, deleted #{Set.size $ deleted vrpDiff}, |] <>
                    [i|BGPSecs: added #{Set.size $ added bgpSecDiff}, deleted #{Set.size $ deleted bgpSecDiff}|] :: Text

            logDebug logger [i|Generated new diff, #{diffText}.|]            


    -- For every connection run 2 threads:
    --      serveLoop blocks on `recv` and accepts client's requests
    --      sendFromQueuesToClient simply sends all PDUs to the client
    -- They communicate using the outboxChan
    serveConnection connection peer updateBroadcastChan rtrState = do        
        logDebug logger [i|Waiting for the first PDU from the client #{peer}|]
        firstPdu <- recv connection 1024
        (rtrState', outboxQueue) <- 
            atomically $ (,) <$> readTVar rtrState <*> newCQueue 10

        let firstPduLazyBS = LBS.fromStrict firstPdu

        let (errorPdu, logMessage, versionedPdu) = 
                analyzePdu peer firstPduLazyBS $ bytesToVersionedPdu firstPduLazyBS        

        for_ errorPdu $ sendAll connection . pduBytesL V0
        for_ logMessage $ logError logger

        for_ versionedPdu $ \versionedPdu' -> do
            case processFirstPdu rtrState' (appContext ^. #appState) versionedPdu' firstPduLazyBS of 
                Left (errorPdu', errorMessage) -> do
                    let errorBytes = pduBytesL V0 errorPdu'
                    logError logger $ [i|Cannot respond to the first PDU from the #{peer}: #{errorMessage},|] <> 
                                      [i|error PDU: #{errorPdu'}, errorBytes = #{hex errorBytes}, length = #{pduLengthL V0 errorPdu'}|]
                    sendAll connection $ pduBytesL V0 errorPdu'

                Right (createPdus, session, warning) -> do
                    for_ warning $ logWarn logger                     
                    -- createPdus is an STM action that creates the response PDUs 
                    atomically $ writeCQueue outboxQueue =<< createPdus

                    withAsync (sendFromQueuesToClient session outboxQueue) $ \sender -> do
                        serveLoop session outboxQueue 
                            `finally` do 
                                atomically (closeCQueue outboxQueue)
                                -- Wait before returning from this functions and thus getting the sender killed
                                -- Let it first drain `outboxQueue` to the socket.
                                void $ timeout 30_000_000 $ wait sender
        where
            -- | Wait for PDUs to appear in either broadcast chan or 
            -- in this session's outbox and send them to the socket.
            sendFromQueuesToClient session outboxQueue =
                loop =<< atomically (dupTChan updateBroadcastChan)
              where
                loop stateUpdateChan = do                         
                    -- wait for queued PDUs or for state updates
                    r <- atomically $ 
                                readCQueue outboxQueue 
                            <|> (Just <$> readTChan stateUpdateChan)
                    

                    for_ r $ \pdus -> do 
                        let protocolVersion = session ^. typed @ProtocolVersion
                        let pdusToSend = 
                                filter (\case 
                                    TruePdu pdu     -> compatibleWith pdu protocolVersion
                                    SerialisedPdu _ -> True) pdus

                        for_ (chunksOf 3000 pdusToSend) $ \chunk ->                           
                            sendMany connection $ map (pduBytesL protocolVersion) chunk

                        loop stateUpdateChan
            

            -- Main loop of the client-server interaction: wait for a PDU from the client, 
            -- generate response and send the response using outboxQueue
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
                        rtrState' <- readTVar rtrState                        
                        case responseAction logger peer session rtrState' (appContext ^. #appState) pduBytes of 
                            Left (pdus, io) -> do 
                                writeCQueue outboxQueue pdus
                                pure io
                            Right (createPdus, io) -> do 
                                pdus <- createPdus
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

                bgpSec <- getBgps tx db worldVersion >>= \case 
                            Nothing   -> pure mempty
                            Just bgps -> pure $ maybe bgps (`applySlurmBgpSec` bgps) slurm
                
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
responseAction :: (Show peer, Logger logger) => 
                   logger 
                -> peer 
                -> Session 
                -> Maybe RtrState 
                -> AppState 
                -> BS.ByteString 
                -> Either ([PduLike], IO ()) (STM [PduLike], IO ())
responseAction logger peer session rtrState appState pduBytes = 
    let 
        pduBytesLazy = LBS.fromStrict pduBytes

        (errorPdu, message, versionedPdu) = 
                analyzePdu peer pduBytesLazy $ bytesToVersionedPdu pduBytesLazy
        
        logError' = maybe mempty (logError logger) message

        errorResponse = maybe mempty (: []) errorPdu
                
        response pdu = let 
            r = respondToPdu rtrState appState (toVersioned session pdu) pduBytesLazy session
            in case r of 
                Left (errorPdu', message') -> let
                    ioAction = logDebug logger [i|Parsed PDU: #{pdu}, error = #{message'}, responding with #{errorPdu}.|]
                    in Left ([errorPdu'], ioAction)                                     
                Right (pdus, warning) -> let
                    ioAction = do 
                        for_ warning $ logWarn logger
                        -- logDebug logger [i|Parsed PDU: #{pdu}, responding with (first 10) #{take 10 pdus}. PDUs.|]                            
                    in Right (pdus, ioAction)                                     

        errors = (errorResponse, logError')
    in 
        case versionedPdu of 
            Nothing                   -> Left errors
            Just (VersionedPdu pdu _) -> 
                case response pdu of 
                    Left z                 -> Left $ errors <> z
                    Right (createPdus, io) -> Right ((fst errors <>) <$> createPdus, io)


-- | Helper function to reduce repeated code.
-- 
analyzePdu :: Show peer => 
               peer 
            -> LBS.ByteString 
            -> Either PduParseError VersionedPdu 
            -> (Maybe PduLike, Maybe Text, Maybe VersionedPdu)
analyzePdu peer pduBytes = \case
        Left (ParsedNothing errorCode errorMessage) -> let
            errorPdu = ErrorPdu errorCode (Just $ convert pduBytes) (Just $ convert errorMessage)
            in (Just $ TruePdu errorPdu, 
                Just [i|PDU from the #{peer} was broken: #{errorMessage}, error PDU: #{errorPdu}|], 
                Nothing)

        Left (ParsedOnlyHeader errorCode errorMessage header@(PduHeader _ pduType)) ->                 
            let message = [i|Error parsing a PDU #{errorMessage}, parsed header #{header}.|]
            in if pduType == errorPduType
                then let 
                    errorPdu = ErrorPdu errorCode (Just $ convert pduBytes) (Just $ convert errorMessage)
                    in (Just $ TruePdu errorPdu, Just message, Nothing)                        
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
                -> AppState
                -> VersionedPdu
                -> LBS.ByteString 
                -> Either (PduLike, Text) (STM [PduLike], Session, Maybe Text)
processFirstPdu 
    rtrState 
    appState 
    versionedPdu@(VersionedPdu pdu protocolVersion) 
    pduBytes =                
    case pdu of 
        SerialQueryPdu _ _ -> let 
            session' = Session protocolVersion
            r = respondToPdu rtrState appState versionedPdu pduBytes session'
            in fmap (\(pdus, message) -> (pdus, session', message)) r

        ResetQueryPdu -> let
            session' = Session protocolVersion
            r = respondToPdu rtrState appState versionedPdu pduBytes session'
            in fmap (\(pdus, message) -> (pdus, session', message)) r

        otherPdu -> let 
            text = convert $ "First received PDU must be SerialQueryPdu or ResetQueryPdu, " <> 
                             "but received " <> show otherPdu
            in Left (TruePdu $ ErrorPdu InvalidRequest (Just $ convert pduBytes) (Just $ convert text), text)


-- | Generate PDUs that would be an appropriate response the request PDU.
-- 
respondToPdu :: Maybe RtrState
                -> AppState
                -> VersionedPdu 
                -> LBS.ByteString                 
                -> Session
                -> Either (PduLike, Text) (STM [PduLike], Maybe Text)
respondToPdu 
    rtrState    
    appState
    (VersionedPdu pdu pduProtocol)     
    pduBytes     
    (Session sessionProtocol) =
        case rtrState of 
            Nothing -> let
                text :: Text = "VRP set is empty, the RTR cache is not ready yet."
                in Left (TruePdu $ ErrorPdu NoDataAvailable (Just $ convert pduBytes) (Just $ convert text), text)
            Just rtrState'@RtrState {..} ->             
                case pdu of 
                    SerialQueryPdu sessionId clientSerial -> 
                        withProtocolVersionCheck pdu $ withSessionIdCheck currentSessionId sessionId $
                            if clientSerial == currentSerial 
                                then let 
                                    pdus = [TruePdu $ CacheResponsePdu sessionId] 
                                        <> [TruePdu $ EndOfDataPdu sessionId currentSerial defIntervals]
                                    in Right (pure pdus, Nothing)
                                else 
                                    case diffsFromSerial rtrState' clientSerial of
                                        Nothing -> 
                                            -- we don't have the data, you are too far behind
                                            Right (
                                                pure [TruePdu $ CacheResetPdu], 
                                                Just [i|No data for serial #{clientSerial}.|])
                                        Just diffs' -> let                                            
                                            pdus = [TruePdu $ CacheResponsePdu sessionId] 
                                                    <> diffPayloadPdus (squashDiffs diffs')
                                                    -- TODO Figure out how to instantiate intervals
                                                    -- Should they be configurable?                                                                     
                                                    <> [TruePdu $ EndOfDataPdu sessionId currentSerial defIntervals]
                                            in Right (pure pdus, Nothing)

                    ResetQueryPdu -> 
                        withProtocolVersionCheck pdu $ let 
                            action = do 
                                bs <- cachedPduBinary appState pduProtocol (currentCachePayloadBS pduProtocol)
                                pure $ [TruePdu $ CacheResponsePdu currentSessionId] 
                                    <> [SerialisedPdu bs]
                                    <> [TruePdu $ EndOfDataPdu currentSessionId currentSerial defIntervals]                                    
                            in Right (action, Nothing) 

                    -- TODO Refactor that stuff 
                    --  - react properly on ErrorPdu
                    --  - Log 
                    -- Dont't send back anything 
                    ErrorPdu {} -> 
                        Right (pure [], Nothing)

                    other -> let
                        text = "Unexpected PDU received from the client: " <> show other
                        in Left (TruePdu $ ErrorPdu NoDataAvailable (Just $ convert pduBytes) (Just $ convert text), convert text)
                                
    where        
        withProtocolVersionCheck _ respond = 
            if sessionProtocol == pduProtocol
                then respond
                else do            
                    let text :: Text = [i|Protocol version is not the same.|]                                
                    Left (TruePdu $ ErrorPdu UnexpectedProtocolVersion (Just $ convert pduBytes) (Just $ convert text), text)

        withSessionIdCheck currentSessionId sessionId respond =
            if currentSessionId == sessionId
                then respond
                else let 
                    text = [i|Wrong sessionId from PDU #{sessionId}, cache sessionId is #{currentSessionId}.|]
                    in Left (TruePdu $ ErrorPdu CorruptData (Just $ convert pduBytes) (Just $ convert text), text)


pduBytesL :: ProtocolVersion -> PduLike -> BS.ByteString
pduBytesL protocolVersion = \case
    TruePdu pdu      -> LBS.toStrict $ pduToBytes pdu protocolVersion
    SerialisedPdu bs -> bs

pduLengthL :: ProtocolVersion -> PduLike -> Int
pduLengthL protocolVersion = \case
    TruePdu pdu      -> fromIntegral $ pduLength pdu protocolVersion
    SerialisedPdu bs -> BS.length bs 

-- Create VRP PDUs 
diffPayloadPdus :: RtrDiffs -> [PduLike]
diffPayloadPdus GenDiffs {..} = 
    map TruePdu $ vrpWithdrawn <> vrpPdusAnn <> 
                  mconcat bgpSecWithdrawn <> mconcat bgpSecPdusAnn
  where
    vrpWithdrawn = map (vrpToPdu Withdrawal) (coerce $ Set.toList $ vrpDiff ^. #deleted)
    vrpPdusAnn   = map (vrpToPdu Announcement) $ coerce $ Set.toAscList $ vrpDiff ^. #added 
    bgpSecWithdrawn = map (bgpSecToPdu Withdrawal) $ Set.toList $ bgpSecDiff ^. #deleted
    bgpSecPdusAnn   = map (bgpSecToPdu Announcement) $ Set.toList $ bgpSecDiff ^. #added

    
currentCachePayloadBS :: ProtocolVersion -> RtrPayloads -> BS.ByteString
currentCachePayloadBS protocolVersion RtrPayloads {..} =
    LBS.toStrict 
        $ BB.toLazyByteString 
        $ mconcat 
        $ map (\pdu -> BB.lazyByteString $ pduToBytes pdu protocolVersion) 
        $ filter (`compatibleWith` protocolVersion)
        $ vrpPdusAnn <> mconcat bgpSecPdusAnn
  where    
    vrpPdusAnn    = map (vrpToPdu Announcement) $ coerce $ V.toList uniqueVrps
    bgpSecPdusAnn = map (bgpSecToPdu Announcement) $ Set.toList bgpSec
    
    
vrpToPdu :: Flags -> Vrp -> Pdu
vrpToPdu flags (Vrp asn prefix maxLength) = 
    case prefix of
        Ipv4P v4p -> IPv4PrefixPdu flags v4p asn maxLength
        Ipv6P v6p -> IPv6PrefixPdu flags v6p asn maxLength

bgpSecToPdu :: Flags -> BGPSecPayload -> [Pdu]
bgpSecToPdu flags BGPSecPayload {..} = 
    let Right (DecodedBase64 spkiBytes) = decodeBase64 (unSPKI bgpSecSpki) ("WTF broken SPKI" :: Text)
    in map (\asn -> RouterKeyPdu asn flags bgpSecSki (LBS.fromStrict spkiBytes)) bgpSecAsns    
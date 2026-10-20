{-# LANGUAGE StrictData #-}

module RPKI.RTR.RtrServer where

import           Control.Concurrent               (forkFinally, threadDelay)

import           Control.Lens                     ((^.))

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad

import           Data.Foldable                    (for_)
import           Data.Maybe                       (fromMaybe)

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Builder          as BB

import           Data.List.Split                  (chunksOf)

import qualified Data.Set                         as Set

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

import qualified RPKI.Store.Database    as DB

import           System.Timeout                   (timeout)
import           Time.Types


{- | Something that can be sent to a client: either a PDU still to be 
   serialised, or a blob already serialised by 'currentCachePayloadBS'.

   Both carry the protocol version they are framed with, so that a PDU cannot 
   be rendered with, nor a cached blob handed to, a connection that speaks a 
   different version.
-}
data PduLike = TruePdu VersionedPdu 
             | SerialisedPdu ProtocolVersion BS.ByteString 
    deriving (Show, Eq)

{- | The highest protocol version this cache supports. Errors about the first
   PDU of a connection are framed with it when the router's version cannot be
   used, i.e. it is unknown or could not be read at all.

   https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-7
-}
highestVersion :: ProtocolVersion
highestVersion = V2

-- | https://datatracker.ietf.org/doc/html/rfc8210#section-8.2
-- "The cache MUST rate-limit Serial Notifies to no more frequently than one
-- per minute."
rtrNotifyRateLimit :: Seconds
rtrNotifyRateLimit = Seconds 60

-- 
-- | Main entry point, here we start the RTR server. 
-- 
runRtrServer :: AppContext -> RtrConfig -> IO ()
runRtrServer appContext RtrConfig {..} = do         
    -- re-initialise `rtrState` and create a broadcast 
    -- channel to publish update for all clients
    let rtrState = appContext ^. #appState . #rtrState
    -- The channel is shared by all connections, which may be at different 
    -- protocol versions, so it carries the content of a Serial Notify rather 
    -- than a rendered PDU. Every connection frames its own.
    updateBroadcastChan :: TChan (RtrSessionId, SerialNumber) <- atomically newBroadcastTChan 

    void $ race 
            (runSocketBusiness rtrState updateBroadcastChan)
            (listenToAppStateUpdates rtrState updateBroadcastChan)
  where    

    logger = getRtrLogger appContext

    -- | Handling TCP conections happens here
    runSocketBusiness rtrState updateBroadcastChan = 
        withSocketsDo $ do                 
            address <- resolve (show rtrPort)
            -- Every accepted connection forks two threads and duplicates the 
            -- update broadcast channel, so the number of them has to be bounded.
            connectionCount <- newTVarIO (0 :: Int)
            bracket (open address) close (loop connectionCount)
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

        loop connectionCount sock = forever $ do
            (conn, peer) <- accept sock
            accepted <- atomically $ do 
                n <- readTVar connectionCount
                if n >= rtrMaxConnections
                    then pure False
                    else do 
                        writeTVar connectionCount $! n + 1
                        pure True
            if accepted 
                then do 
                    logInfo logger [i|Connection from #{peer}|]
                    void $ forkFinally 
                        (serveConnection conn peer updateBroadcastChan rtrState) 
                        (\_ -> do 
                            logInfo logger [i|Closing connection with #{peer}|]
                            atomically $ modifyTVar' connectionCount (\n -> n - 1)
                            close conn)
                else do 
                    logWarn logger $ 
                        [i|Rejecting RTR connection from #{peer}, |] <>
                        [i|already serving the maximum of #{rtrMaxConnections} connections.|]
                    close conn
    
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
        let maxStoredDiffs = vrpsCount (rtrPayloads ^. #uniqueVrps)
                
        logDebug logger [i|RTR started with version #{worldVersion}, maxStoredDiffs = #{maxStoredDiffs}.|] 

        let initialRtrState = newRtrState worldVersion maxStoredDiffs
        atomically $ writeTVar rtrState $ Just initialRtrState

        race_ (notifyClients $ notifyKey initialRtrState) $ forever $ do
            (rtrState', previousVersion, newVersion, currentRtrPayload) <- waitForLatestRtrPayload appContext rtrState
            previousRtrPayload <- readRtrPayload appContext previousVersion

            let rtrDiff = evalDiffs previousRtrPayload currentRtrPayload
            let thereAreRtrUpdates = not $ emptyDiffs rtrDiff

            let
                previousVrpSize = vrpsCount $ previousRtrPayload ^. #uniqueVrps
                currentVrpSize  = vrpsCount $ currentRtrPayload ^. #uniqueVrps
                previousBgpSecSize = Set.size $ previousRtrPayload ^. #bgpSec
                currentBgpSecSize  = Set.size $ currentRtrPayload ^. #bgpSec
                previousAspaSize   = Set.size $ previousRtrPayload ^. #aspas
                currentAspaSize    = Set.size $ currentRtrPayload ^. #aspas
                in logDebug logger $ [i|Notified about an update: #{previousVersion} -> #{newVersion}, |] <>
                              [i|VRPs: #{previousVrpSize} -> #{currentVrpSize}, |] <>
                              [i|BGPSecs: #{previousBgpSecSize} -> #{currentBgpSecSize}, |] <>
                              [i|ASPAs: #{previousAspaSize} -> #{currentAspaSize}.|]

            -- force evaluation of the new RTR state so that the old ones could be GC-ed.
            let !nextRtrState = if thereAreRtrUpdates
                    then updatedRtrState rtrState' newVersion rtrDiff
                    else rtrState' { lastKnownWorldVersion = newVersion }

            logDiff rtrDiff

            atomically $ writeTVar rtrState $! Just $! nextRtrState
        where
          -- The serial only changes when there's a non-empty diff, so a change 
          -- of (session, serial) is exactly "there's something to notify about".
          -- 
          -- Wait for it to differ from what was announced last, announce it 
          -- and then sleep out the rate limit. Updates that come during the sleep
          -- are not lost: after waking up the state already differs, so a single 
          -- notify for the latest serial goes out immediately.
          notifyClients lastNotified = do
            current <- atomically $ do
                key <- maybe retry (pure . notifyKey) =<< readTVar rtrState
                when (key == lastNotified) retry
                writeTChan updateBroadcastChan key
                pure key
            threadDelay $ toMicroseconds rtrNotifyRateLimit
            notifyClients current

          notifyKey s = (s ^. #currentSessionId, s ^. #currentSerial)

          logDiff GenDiffs {..} = do
            
            let diffText =
                    [i|VRPs: added #{Set.size $ added vrpDiff}, deleted #{Set.size $ deleted vrpDiff}, |] <>
                    [i|BGPSecs: added #{Set.size $ added bgpSecDiff}, deleted #{Set.size $ deleted bgpSecDiff}, |] <>
                    [i|ASPAs: added #{Set.size $ added aspaDiff}, deleted #{Set.size $ deleted aspaDiff}|] :: Text

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
                analyzePdu peer Nothing firstPduLazyBS $ bytesToVersionedPdu firstPduLazyBS

        for_ errorPdu $ sendAll connection . pduBytesL
        for_ logMessage $ logError logger

        for_ versionedPdu $ \versionedPdu' -> do
            case processFirstPdu rtrState' (appContext ^. #appState) versionedPdu' firstPduLazyBS of
                Left (errorPduL, errorMessage) -> do
                    let errorBytes = pduBytesL errorPduL
                    logError logger $ [i|Cannot respond to the first PDU from the #{peer}: #{errorMessage},|] <>
                                      [i|error PDU: #{errorPduL}, errorBytes = #{hex errorBytes}, length = #{pduLengthL errorPduL}|]
                    sendAll connection errorBytes

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
                notifyPdus (sessionId, serial) = 
                    [TruePdu $ toVersioned session $ NotifyPdu sessionId serial]

                loop stateUpdateChan = do                         
                    -- wait for queued PDUs or for state updates
                    r <- atomically $ 
                                readCQueue outboxQueue 
                            <|> (Just . notifyPdus <$> readTChan stateUpdateChan)
                    
                    for_ r $ \pdus -> do 
                        let pdusToSend = filter compatiblePduLike pdus

                        for_ (chunksOf 3000 pdusToSend) $ \chunk ->                           
                            sendMany connection $ map pduBytesL chunk

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


readRtrPayload :: AppContext -> WorldVersion -> IO RtrPayloads 
readRtrPayload AppContext {..} worldVersion = do 
    (vrps, bgpSec, aspas) <- DB.roTxT database $ \tx -> do
                slurm <- DB.getSlurm tx worldVersion
                vrps <- do
                        vrps_ <- DB.getVrps tx worldVersion
                        pure $ maybe vrps_ (`applySlurmToVrps` vrps_) slurm

                bgpSec <- DB.getBgps tx worldVersion >>= \case
                            Nothing   -> pure mempty
                            Just bgps -> pure $ maybe bgps (`applySlurmBgpSec` bgps) slurm
                
                aspas <- fromMaybe mempty <$> DB.getAspas tx worldVersion

                pure (vrps, bgpSec, aspas)

    pure $ mkRtrPayloads vrps bgpSec aspas


waitForLatestRtrPayload :: AppContext 
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
                analyzePdu peer (Just session) pduBytesLazy $ bytesToVersionedPdu pduBytesLazy

        logError' = maybe mempty (logError logger) message

        errorResponse = maybe mempty (: []) errorPdu

        response pdu = let
            r = respondToPdu rtrState appState pdu pduBytesLazy session
            in case r of 
                Left (errorPdu', message') -> let
                    ioAction = logDebug logger [i|Parsed PDU: #{pdu}, error = #{message'}, responding with #{errorPdu}.|]
                    in Left ([TruePdu $ toVersioned session errorPdu'], ioAction)                                     
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


-- | Decide what to do with a parsed (or not) PDU: the error to send back, 
-- the message to log and the PDU to respond to, each of them optional.
-- 
-- The session is Nothing for the first PDU of a connection, i.e. when the 
-- protocol version is not negotiated yet.
-- 
analyzePdu :: Show peer => 
               peer 
            -> Maybe Session
            -> LBS.ByteString 
            -> Either PduParseError VersionedPdu 
            -> (Maybe PduLike, Maybe Text, Maybe VersionedPdu)
analyzePdu peer session pduBytes = \case
        Left (ParsedNothing errorCode errorMessage) -> let
            errorPdu = newErrorPdu errorCode errorMessage
            in (Just $ framed highestVersion errorPdu, 
                Just [i|PDU from the #{peer} was broken: #{errorMessage}, error PDU: #{errorPdu}|], 
                Nothing)

        Left (UnknownProtocolVersion version pduType) -> let 
            message = [i|PDU from the #{peer} has unknown protocol version #{version}.|]
            errorPdu = case session of 
                -- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-7
                Nothing -> newErrorPdu UnsupportedProtocolVersion message
                Just _  -> newErrorPdu UnexpectedProtocolVersion message
            in (replyUnlessError pduType $ framed highestVersion errorPdu, Just message, Nothing)

        Left (ParsedOnlyHeader errorCode errorMessage header@(PduHeader version pduType)) -> let 
            message = [i|Error parsing a PDU #{errorMessage}, parsed header #{header}.|]
            errorPdu 
                | differsFromSession version = newErrorPdu UnexpectedProtocolVersion versionMessage
                | otherwise                  = newErrorPdu errorCode errorMessage
            in (replyUnlessError pduType $ framed version errorPdu, Just message, Nothing)

        Right (VersionedPdu errorPdu@ErrorPdu {} _) ->
            (Nothing, Just [i|Received an error from #{peer}: #{errorPdu}.|], Nothing)                

        -- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-7
        -- "If either party receives a PDU for a different Protocol Version once
        -- the above negotiation completes, that party MUST drop the session"
        Right (VersionedPdu pdu version) | differsFromSession version -> 
            (Just $ framed version $ newErrorPdu UnexpectedProtocolVersion versionMessage, 
             Just ([i|Received #{pdu} from #{peer} with protocol version #{version}, |] <> 
                   [i|but the session is #{session}.|]), 
             Nothing)

        Right versionedPdu -> (Nothing, Nothing, Just versionedPdu)
  where
    newErrorPdu errorCode (message :: Text) = 
        ErrorPdu errorCode (Just $ convert pduBytes) (Just message)

    -- After the negotiation everything goes back framed with the session's version, 
    -- before it with the version that suits the error. 
    framed version pdu = TruePdu $ maybe (VersionedPdu pdu version) (`toVersioned` pdu) session

    differsFromSession version = maybe False (\(Session v) -> v /= version) session

    versionMessage = "Protocol version is not the same as the one of the session."

    -- Do no send an error PDU as a response to an error PDU, it's prohibited by RFC
    -- https://datatracker.ietf.org/doc/html/rfc8210#section-5.11
    replyUnlessError pduType reply
        | pduType == errorPduType = Nothing
        | otherwise               = Just reply


-- 
-- | Process the first PDU and do the protocol version negotiation.
-- 
-- Every version up to 'highestVersion' is supported, so the session is 
-- established at the version of the router's query and everything sent back, 
-- errors included, is framed with it.
-- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-7
-- 
processFirstPdu :: Maybe RtrState 
                -> AppState
                -> VersionedPdu
                -> LBS.ByteString 
                -> Either (PduLike, Text) (STM [PduLike], Session, Maybe Text)
processFirstPdu 
    rtrState 
    appState 
    (VersionedPdu pdu protocolVersion) 
    pduBytes =                
    case pdu of 
        -- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-5.3
        SerialQueryPdu sessionId _ 
            | Just rtrState' <- rtrState
            , let currentSessionId = rtrState' ^. #currentSessionId
            , sessionId /= currentSessionId -> 
                Right (pure [out CacheResetPdu], 
                       session', 
                       Just ([i|Unknown session ID #{sessionId} in the first Serial Query, |] <> 
                             [i|cache session ID is #{currentSessionId}.|]))

        SerialQueryPdu _ _ -> respond

        ResetQueryPdu -> respond

        otherPdu -> let 
            text = convert $ "First received PDU must be SerialQueryPdu or ResetQueryPdu, " <> 
                             "but received " <> show otherPdu
            in Left (out $ ErrorPdu InvalidRequest (Just $ convert pduBytes) (Just $ convert text), text)
  where
    session' = Session protocolVersion
    out = TruePdu . toVersioned session'

    respond = case respondToPdu rtrState appState pdu pduBytes session' of 
        Left (errorPdu, message) -> Left (out errorPdu, message)
        Right (pdus, warning)    -> Right (pdus, session', warning)


-- | Generate PDUs that would be an appropriate response the request PDU.
-- 
-- The PDU must already be known to have the version of the session.
-- 
respondToPdu :: Maybe RtrState
                -> AppState
                -> Pdu 
                -> LBS.ByteString                 
                -> Session
                -> Either (Pdu, Text) (STM [PduLike], Maybe Text)
respondToPdu 
    rtrState    
    appState
    pdu
    pduBytes     
    session@(Session sessionProtocol) =
        case rtrState of 
            Nothing -> let
                text :: Text = "VRP set is empty, the RTR cache is not ready yet."
                in Left (ErrorPdu NoDataAvailable (Just $ convert pduBytes) (Just $ convert text), text)
            Just rtrState'@RtrState {..} ->             
                case pdu of 
                    SerialQueryPdu sessionId clientSerial -> 
                        withSessionIdCheck currentSessionId sessionId $
                            if clientSerial == currentSerial 
                                then let 
                                    pdus = [out $ CacheResponsePdu sessionId] 
                                        <> [out $ EndOfDataPdu sessionId currentSerial defIntervals]
                                    in Right (pure pdus, Nothing)
                                else 
                                    case diffsFromSerial rtrState' clientSerial of
                                        Nothing -> 
                                            -- we don't have the data, you are too far behind
                                            Right (
                                                pure [out CacheResetPdu], 
                                                Just [i|No data for serial #{clientSerial}.|])
                                        Just diffs' -> let                                            
                                            pdus = [out $ CacheResponsePdu sessionId] 
                                                    <> diffPayloadPdus sessionProtocol (squashDiffs diffs')
                                                    -- TODO Figure out how to instantiate intervals
                                                    -- Should they be configurable?                                                                     
                                                    <> [out $ EndOfDataPdu sessionId currentSerial defIntervals]
                                            in Right (pure pdus, Nothing)

                    ResetQueryPdu -> let
                            action = do
                                bs <- cachedPduBinary appState sessionProtocol 
                                        (currentCachePayloadBS sessionProtocol)
                                pure $ [out $ CacheResponsePdu currentSessionId] 
                                    <> [SerialisedPdu sessionProtocol bs]
                                    <> [out $ EndOfDataPdu currentSessionId currentSerial defIntervals]                                    
                            in Right (action, Nothing) 

                    -- TODO Refactor that stuff 
                    --  - react properly on ErrorPdu
                    --  - Log 
                    -- Dont't send back anything 
                    ErrorPdu {} -> 
                        Right (pure [], Nothing)

                    other -> let
                        text = "Unexpected PDU received from the client: " <> show other
                        in Left (ErrorPdu NoDataAvailable (Just $ convert pduBytes) (Just $ convert text), convert text)
                                
    where        
        -- Everything sent back to the client is framed with the version of the 
        -- session it is sent on, never with the version of the incoming PDU.
        out = TruePdu . toVersioned session

        withSessionIdCheck currentSessionId sessionId respond =
            if currentSessionId == sessionId
                then respond
                else let 
                    text = [i|Wrong sessionId from PDU #{sessionId}, cache sessionId is #{currentSessionId}.|]
                    in Left (ErrorPdu CorruptData (Just $ convert pduBytes) (Just $ convert text), text)


pduBytesL :: PduLike -> BS.ByteString
pduBytesL = \case
    TruePdu versionedPdu -> LBS.toStrict $ pduToBytes versionedPdu
    SerialisedPdu _ bs   -> bs

pduLengthL :: PduLike -> Int
pduLengthL = \case
    TruePdu versionedPdu -> fromIntegral $ pduLength versionedPdu
    SerialisedPdu _ bs   -> BS.length bs 

-- | Whether a PDU is allowed at the protocol version it is framed with.
compatiblePduLike :: PduLike -> Bool
compatiblePduLike = \case
    TruePdu (VersionedPdu pdu protocolVersion) -> compatibleWith pdu protocolVersion
    -- 'currentCachePayloadBS' only renders PDUs compatible with its version
    SerialisedPdu _ _                          -> True

-- Create VRP PDUs framed with the protocol version of the session they are for
diffPayloadPdus :: ProtocolVersion -> RtrDiffs -> [PduLike]
diffPayloadPdus protocolVersion GenDiffs {..} = 
    map (TruePdu . (`VersionedPdu` protocolVersion)) $ 
                  vrpWithdrawn <> vrpPdusAnn <> 
                  mconcat bgpSecWithdrawn <> mconcat bgpSecPdusAnn <>
                  aspaPdusAnn <> aspaWithdrawn
  where
    vrpWithdrawn = map (vrpToPdu Withdrawal) (coerce $ Set.toList $ vrpDiff ^. #deleted)
    vrpPdusAnn   = map (vrpToPdu Announcement) $ coerce $ Set.toAscList $ vrpDiff ^. #added 
    bgpSecWithdrawn = map (bgpSecToPdu Withdrawal) $ Set.toList $ bgpSecDiff ^. #deleted
    bgpSecPdusAnn   = map (bgpSecToPdu Announcement) $ Set.toList $ bgpSecDiff ^. #added

    -- An ASPA announcement replaces the previous record of the same customer AS, 
    -- and a withdrawal removes the whole record. So when the providers of a customer
    -- changed (the old record is in 'deleted', the new one is in 'added') only 
    -- the announcement must be sent, otherwise the withdrawal would remove the new record.
    -- Squashed diffs can have several deleted records of the same customer, so 
    -- withdrawals are per customer. 
    -- Announcements go first, then withdrawals, both by ascending customer AS
    -- (https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-11.2.3).
    aspaAnnounced   = Set.map customer $ aspaDiff ^. #added
    aspaPdusAnn     = map aspaToPdu $ Set.toAscList $ aspaDiff ^. #added
    aspaWithdrawn   = map (\c -> AspaPdu Withdrawal c []) 
                        $ Set.toAscList 
                        $ Set.map customer (aspaDiff ^. #deleted) `Set.difference` aspaAnnounced

    
currentCachePayloadBS :: ProtocolVersion -> RtrPayloads -> BS.ByteString
currentCachePayloadBS protocolVersion RtrPayloads {..} =
    LBS.toStrict 
        $ BB.toLazyByteString 
        $ mconcat 
        $ map (\pdu -> BB.lazyByteString $ pduToBytes $ VersionedPdu pdu protocolVersion) 
        $ filter (`compatibleWith` protocolVersion)
        $ vrpPdusAnn <> mconcat bgpSecPdusAnn <> aspaPdusAnn
  where    
    vrpPdusAnn    = map (vrpToPdu Announcement) $ mergeVrpsBy cmpPacked4Against6 uniqueVrps
    bgpSecPdusAnn = map (bgpSecToPdu Announcement) $ Set.toList bgpSec
    aspaPdusAnn   = map aspaToPdu $ Set.toAscList aspas
    
    
vrpToPdu :: Flags -> Vrp -> Pdu
vrpToPdu flags (Vrp asn prefix maxLength) = 
    case prefix of
        Ipv4P v4p -> IPv4PrefixPdu flags v4p asn maxLength
        Ipv6P v6p -> IPv6PrefixPdu flags v6p asn maxLength

bgpSecToPdu :: Flags -> BGPSecPayload -> [Pdu]
bgpSecToPdu flags BGPSecPayload {..} = 
    let Right (DecodedBase64 spkiBytes) = decodeBase64 (unSPKI bgpSecSpki) ("WTF broken SPKI" :: Text)
    in map (\asn -> RouterKeyPdu asn flags bgpSecSki (LBS.fromStrict spkiBytes)) bgpSecAsns    

-- | ASPA announcement PDU. Providers are already ordered ascending in the set.
aspaToPdu :: Aspa -> Pdu
aspaToPdu Aspa {..} = AspaPdu Announcement customer (Set.toAscList providers)

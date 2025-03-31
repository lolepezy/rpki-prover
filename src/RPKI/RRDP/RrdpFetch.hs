{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module RPKI.RRDP.RrdpFetch where

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class           (liftIO)
import           Data.Generics.Product.Typed

import           Data.Foldable
import           Data.Bifunctor                   (first)
import           Data.Text                        (Text)
import qualified Data.ByteString                  as BS
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import           Data.String.Interpolate.IsString
import           Data.Proxy
import           Data.Maybe

import           GHC.Generics

import qualified Streaming.Prelude                as S

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Metrics.System
import           RPKI.Worker
import           RPKI.Parallel
import           RPKI.Time
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.RRDP.Http
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.Validation.ObjectValidation
import           RPKI.Store.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database    as DB
import qualified RPKI.Util              as U



runRrdpFetchWorker :: AppContext s
            -> FetchConfig
            -> WorldVersion
            -> RrdpRepository
            -> ValidatorT IO (RrdpRepository, RrdpFetchStat)
runRrdpFetchWorker appContext@AppContext {..} fetchConfig worldVersion repository = do

    -- This is for humans to read in `top` or `ps`, actual parameters
    -- are passed as 'RrdpFetchParams'.
    let (URI u) = getURL repository
    let workerId = WorkerId [i|version:#{worldVersion}:rrdp-fetch:#{u}|]

    let arguments =
            [ worderIdS workerId ] <>
            rtsArguments [
                rtsN 1,
                rtsA "20m",
                rtsAL "64m",
                rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #rrdpWorkerMemoryMb) ]

    scopes <- askScopes

    workerInput <- makeWorkerInput appContext workerId
                        (RrdpFetchParams scopes repository worldVersion)
                        (Timebox $ fetchConfig ^. #rrdpTimeout)
                        (Just $ asCpuTime $ fetchConfig ^. #cpuLimit)

    wr@WorkerResult {..} <- runWorker logger workerInput arguments
    let RrdpFetchResult z = payload
    logWorkerDone logger workerId wr
    pushSystem logger $ cpuMemMetric "fetch" cpuTime clockTime maxMemory
    embedValidatorT $ pure z


-- | 
--  Update RRDP repository, actually saving all the objects in the DB.
--
-- NOTE: It will update the sessionId and serial of the repository 
-- in the same transaction it stores the data in.
-- 
updateObjectForRrdpRepository :: Storage s =>
                                AppContext s
                            -> WorldVersion
                            -> RrdpRepository
                            -> ValidatorT IO (RrdpRepository, RrdpFetchStat)
updateObjectForRrdpRepository appContext worldVersion repository =
    timedMetric (Proxy :: Proxy RrdpMetric) $
        downloadAndUpdateRRDP
            appContext
            repository
            (saveSnapshot appContext worldVersion)
            (saveDelta appContext worldVersion)

-- | 
--  Update RRDP repository, i.e. do the full cycle
--    - download notifications file, parse it
--    - decide what to do next based on it
--    - download snapshot or deltas
--    - do something appropriate with either of them
-- 
downloadAndUpdateRRDP :: AppContext s ->
                        RrdpRepository
                        -> (RrdpURL -> Notification -> BS.ByteString -> ValidatorT IO ())
                        -> (RrdpURL -> Notification -> RrdpSerial -> BS.ByteString -> ValidatorT IO ())
                        -> ValidatorT IO (RrdpRepository, RrdpFetchStat)
downloadAndUpdateRRDP
        appContext@AppContext {..}
        repo@RrdpRepository { uri = repoUri, .. }
        handleSnapshotBS                       -- ^ function to handle the snapshot bytecontent
        handleDeltaBS =                        -- ^ function to handle delta bytecontents
  do

    for_ eTag $ \et ->
        logDebug logger [i|Existing eTag for #{repoUri} is #{et}.|]

    (notificationXml, _, httpStatus, newETag) <-
            timedMetric' (Proxy :: Proxy RrdpMetric)
                (\t -> #downloadTimeMs %~ (<> t)) $
                fromTry (RrdpE . CantDownloadNotification . U.fmtEx)
                    $ downloadToBS (appContext ^. typed) (getURL repoUri) eTag

    for_ newETag $ \et ->
        logDebug logger [i|New eTag for #{repoUri} is #{et}.|]

    case httpStatus of
        HttpStatus 304 -> do
            repo' <- bumpETag newETag $ do
                usedSource RrdpNoUpdate
                pure repo
            let message = [i|Nothing to update for #{repoUri} based on ETag comparison.|]
            logDebug logger message
            pure (repo', RrdpFetchStat $ NothingToFetch message)

        _ -> do
            notification <- validatedNotification =<< hoistHere (parseNotification notificationXml)
            nextStep     <- vHoist $ rrdpNextStep repo notification

            repo' <- bumpETag newETag $
                case nextStep of
                    NothingToFetch message -> do
                        usedSource RrdpNoUpdate
                        logDebug logger [i|Nothing to update for #{repoUri}: #{message}|]
                        pure repo

                    FetchSnapshot snapshotInfo message -> do
                        usedSource $ RrdpSnapshot $ notification ^. #serial
                        logDebug logger [i|Going to use snapshot for #{repoUri}: #{message}|]
                        useSnapshot snapshotInfo notification

                    FetchDeltas sortedDeltas snapshotInfo message -> do
                            usedSource $ RrdpDelta
                                (NonEmpty.head sortedDeltas ^. typed)
                                (NonEmpty.last sortedDeltas ^. typed)

                            logDebug logger [i|Going to use deltas for #{repoUri}: #{message}|]
                            useDeltas sortedDeltas notification
                            `catchError`
                        \e -> do
                            usedSource $ RrdpSnapshot $ notification ^. #serial
                            logError logger [i|Failed to apply deltas for #{repoUri}: #{e}, will fall back to snapshot.|]
                            useSnapshot snapshotInfo notification

            pure (repo', RrdpFetchStat nextStep)
  where
    bumpETag newETag f = (\r -> r { eTag = newETag }) <$> f

    usedSource z = updateMetric @RrdpMetric @_ (#rrdpSource .~ z)
    hoistHere    = vHoist . fromEither . first RrdpE

    validatedNotification notification = do
        let repoU = unURI $ getURL repoUri
        for_ (U.getHostname repoU) $ \baseHostname -> do
            case U.getHostname $ unURI $ notification ^. #snapshotInfo . typed of
                Nothing               -> appError $ RrdpE $ BrokenSnapshotUri repoU
                Just snapshotHostname -> do
                    when (snapshotHostname /= baseHostname) $
                        appError $ RrdpE $ SnapshotUriHostname repoU snapshotHostname

            for_ (notification ^. #deltas) $ \delta -> do
                case U.getHostname $ unURI $ delta ^. typed of
                    Nothing -> appError $ RrdpE $ BrokenDeltaUri $ unURI $ delta ^. typed
                    Just deltaHostname -> do
                        when (deltaHostname /= baseHostname) $
                            appError $ RrdpE $ DeltaUriHostname repoU deltaHostname
        pure notification

    useSnapshot (SnapshotInfo uri expectedHash) notification = do
        vFocusOn LinkFocus uri $ do
            logInfo logger [i|#{uri}: downloading snapshot.|]

            (rawContent, _, httpStatus', _) <-
                timedMetric' (Proxy :: Proxy RrdpMetric)
                    (\t -> #downloadTimeMs %~ (<> t)) $ do
                    fromTryEither (RrdpE . CantDownloadSnapshot . U.fmtEx) $
                        downloadHashedBS (appContext ^. typed @Config) uri Nothing expectedHash
                            (\actualHash ->
                                Left $ RrdpE $ SnapshotHashMismatch {
                                    expectedHash = expectedHash,
                                    actualHash = actualHash
                                })
            updateMetric @RrdpMetric @_ (#lastHttpStatus .~ httpStatus')

            void $ timedMetric' (Proxy :: Proxy RrdpMetric)
                    (\t -> #saveTimeMs %~ (<> t))
                    (handleSnapshotBS repoUri notification rawContent)

            pure $ repo { rrdpMeta = rrdpMeta' }

        where
            rrdpMeta' = let
                Notification {..} = notification
                in Just $ RrdpMeta sessionId serial (RrdpIntegrity deltas)


    useDeltas sortedDeltas notification = do
        let repoURI = getURL $ repo ^. #uri
        let message = if minDeltaSerial == maxDeltaSerial
                then [i|#{repoURI}: downloading delta #{minDeltaSerial}.|]
                else [i|#{repoURI}: downloading deltas from #{minDeltaSerial} to #{maxDeltaSerial}.|]

        logInfo logger message

        -- Do not thrash the same server with too big amount of parallel 
        -- requests, it's mostly counter-productive and rude. Maybe 8 is still too much?
        let maxDeltaDownloadSimultaneously = 8

        void $ timedMetric' (Proxy :: Proxy RrdpMetric)
                (\t -> #saveTimeMs %~ (<> t)) $
                foldPipeline
                        maxDeltaDownloadSimultaneously
                        (S.each $ NonEmpty.toList sortedDeltas)
                        downloadDelta
                        (\(rawContent, serial, deltaUri) _ ->
                            inSubVScope deltaUri $
                                handleDeltaBS repoUri notification serial rawContent)
                        (mempty :: ())

        pure $ repo { rrdpMeta = rrdpMeta' }

      where
        downloadDelta (DeltaInfo uri hash serial) = do
            let deltaUri = U.convert uri
            (rawContent, _, httpStatus', _) <-
                inSubVScope deltaUri $ do
                    fromTryEither (RrdpE . CantDownloadDelta . U.fmtEx) $
                        downloadHashedBS (appContext ^. typed @Config) uri Nothing hash
                            (\actualHash ->
                                Left $ RrdpE $ DeltaHashMismatch {
                                    actualHash = actualHash,
                                    expectedHash = hash,
                                    serial = serial
                                })
            updateMetric @RrdpMetric @_ (#lastHttpStatus .~ httpStatus')
            pure (rawContent, serial, deltaUri)

        serials = NonEmpty.map (^. typed @RrdpSerial) sortedDeltas
        maxDeltaSerial = maximum serials
        minDeltaSerial = minimum serials

        rrdpMeta' = let
            Notification {..} = notification
            in Just $ RrdpMeta sessionId maxDeltaSerial (RrdpIntegrity $ toList sortedDeltas)


-- | Decides what to do next based on current state of the repository
-- | and the parsed notification file
rrdpNextStep :: RrdpRepository -> Notification -> PureValidatorT RrdpAction

rrdpNextStep RrdpRepository { rrdpMeta = Nothing } Notification{..} =
    pure $ FetchSnapshot snapshotInfo "First time seeing repository"

rrdpNextStep RrdpRepository { rrdpMeta = Just rrdpMeta } Notification {..} =

    if  | sessionId /= localSessionId ->
            pure $ FetchSnapshot snapshotInfo [i|Resetting RRDP session from #{localSessionId} to #{sessionId}|]

        | localSerial > serial -> do
            appWarn $ RrdpE $ LocalSerialBiggerThanRemote localSerial serial
            pure $ NothingToFetch [i|#{localSessionId}, local serial #{localSerial} is higher than the remote serial #{serial}.|]

        | localSerial == serial ->
            pure $ NothingToFetch [i|up-to-date, #{localSessionId}, serial #{localSerial}|]

        | otherwise ->
            case (deltas, nonConsecutiveDeltas) of
                ([], _) -> pure $ FetchSnapshot snapshotInfo
                                [i|#{localSessionId}, there is no deltas to use.|]

                (_, []) | nextSerial localSerial < deltaSerial (head sortedDeltas) ->
                            -- we are too far behind
                            pure $ FetchSnapshot snapshotInfo
                                    [i|#{localSessionId}, local serial #{localSerial} is too far behind remote #{serial}.|]

                        -- too many deltas means huge overhead -- just use snapshot, 
                        -- it's more data but less chances of getting killed by timeout
                        | length chosenDeltas > 100 ->
                            pure $ FetchSnapshot snapshotInfo
                                    [i|#{localSessionId}, there are too many deltas: #{length chosenDeltas}.|]

                        | not (null deltaIntegrityIssues) ->
                            pure $ FetchSnapshot snapshotInfo formattedIntegrityIssues

                        | otherwise ->
                            pure $ FetchDeltas (NonEmpty.fromList chosenDeltas) snapshotInfo
                                    [i|#{localSessionId}, deltas look good.|]

                (_, nc) -> do
                    appWarn $ RrdpE $ NonConsecutiveDeltaSerials nc
                    pure $ FetchSnapshot snapshotInfo
                            [i|#{localSessionId}, there are non-consecutive delta serials: #{nc}.|]

    where
        localSessionId = rrdpMeta ^. #sessionId
        localSerial    = rrdpMeta ^. #serial

        sortedSerials = map deltaSerial sortedDeltas
        sortedDeltas = List.sortOn deltaSerial deltas
        chosenDeltas = filter ((> localSerial) . deltaSerial) sortedDeltas

        nonConsecutiveDeltas = List.filter (\(s, s') -> nextSerial s /= s') $
            List.zip sortedSerials (tail sortedSerials)

        deltaIntegrityIssues =
            [ (serial_, hash, previousHash) |
                DeltaInfo _ hash serial_   <- deltas,
                DeltaInfo _ previousHash _ <- maybeToList $ Map.lookup serial_ previousDeltasBySerial,
                previousHash /= hash
            ]
          where
            previousDeltasBySerial =
                Map.fromList
                    $ map (\d -> (d ^. typed @RrdpSerial, d))
                    $ rrdpMeta ^. #integrity . #deltas

        formattedIntegrityIssues = [i|These deltas have integrity issues: #{issues}.|]
          where
            issues :: Text =
                mconcat
                $ List.intersperse "; "
                $ flip map deltaIntegrityIssues
                $ \(serial_, hash, previousHash) ->
                    [i|serial #{serial_}|] <>
                    (if previousHash /= hash
                        then [i|, used to have hash #{previousHash} and now #{hash}|]
                        else "")


deltaSerial :: DeltaInfo -> RrdpSerial
deltaSerial (DeltaInfo _ _ s) = s

nextSerial :: RrdpSerial -> RrdpSerial
nextSerial (RrdpSerial s) = RrdpSerial $ s + 1


{- 
    Snapshot case, done in parallel by two thread
        - one thread parses XML, reads base64s and pushes CPU-intensive parsing tasks into the queue 
        - another thread reads parsing tasks, waits for them and saves the results into the DB.
-}
saveSnapshot :: Storage s =>
                AppContext s
                -> WorldVersion
                -> RrdpURL
                -> Notification
                -> BS.ByteString
                -> ValidatorT IO ()
saveSnapshot
    appContext@AppContext {..}
    worldVersion repoUri notification snapshotContent = do

    -- If we are going for the snapshot we are going to need a lot of CPU
    -- time, so bump the number of CPUs to the maximum possible values    
    let maxCpuAvailable = appContext ^. typed @Config . typed @Parallelism . #cpuCount
    liftIO $ setCpuCount maxCpuAvailable
    let cpuParallelism = makeParallelism maxCpuAvailable ^. #cpuParallelism

    let snapshotUrl = notification ^. #snapshotInfo . typed @URI
    logDebug logger [i|Snapshot #{snapshotUrl} is #{BS.length snapshotContent} bytes.|]

    db <- liftIO $ readTVarIO database
    Snapshot _ sessionId serial snapshotItems <- vHoist $
        fromEither $ first RrdpE $ parseSnapshot snapshotContent

    let notificationSessionId = notification ^. typed @SessionId
    when (sessionId /= notificationSessionId) $
        appError $ RrdpE $ SnapshotSessionMismatch sessionId notificationSessionId

    let notificationSerial = notification ^. typed @RrdpSerial
    when (serial /= notificationSerial) $
        appError $ RrdpE $ SnapshotSerialMismatch serial notificationSerial

    -- Save objects in batches since
    -- 1) It's not crucial to save the whole snapshot in one transaction, 
    --    it's an idempotent operation and can be restarted 
    -- 2) It's better to not block the DB for too long if snapshot is very big.
    --    Other fetcher processes can be killed by timeout waiting on the DB
    --    rather that waiting on download
    -- 
    txFoldPipelineBatch
            cpuParallelism
            10000
            (S.mapM (newStorable db) $ S.each snapshotItems)
            (DB.rwAppTx db)
            (saveStorable db)

    DB.rwAppTx db $ \tx -> DB.updateRrdpMeta tx db (fromNotification notification) repoUri

  where

    newStorable db (SnapshotPublish uri encodedb64) =
        if supportedExtension $ U.convert uri
            then do
                a <- liftIO $ async readBlob
                pure $! Right (uri, a)
            else
                pure $! Left (RrdpE (RrdpUnsupportedObjectType (U.convert uri)), uri)
      where
        readBlob = case U.parseRpkiURL $ unURI uri of
            Left e ->
                pure $! UnparsableRpkiURL uri $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e

            Right rpkiURL -> do
                let decoded = U.decodeBase64 encodedb64 rpkiURL
                case first (\t -> RrdpE $ BadBase64 t (U.convert rpkiURL)) decoded of
                    Left e -> pure $! DecodingTrouble rpkiURL (VErr e)
                    Right (DecodedBase64 blob) ->
                        case validateSizeOfBS validationConfig blob of
                            Left e  -> pure $! DecodingTrouble rpkiURL (VErr $ ValidationE e)
                            Right _ ->
                                case urlObjectType rpkiURL of
                                    Just type_ -> do
                                        let hash = U.sha256s blob
                                        exists <- roTx db $ \tx -> DB.hashExists tx db hash
                                        if exists
                                            -- The object is already in cache. Do not parse-serialise
                                            -- anything, just skip it. We are not afraid of possible 
                                            -- race-conditions here, it's not a problem to double-insert
                                            -- an object and delete-insert race will never happen in practice
                                            -- since deletion is never concurrent with insertion.
                                            then pure $! HashExists rpkiURL hash
                                            else
                                                tryToParse rpkiURL hash blob type_
                                    Nothing ->
                                        pure $! UknownObjectType rpkiURL
          where
            tryToParse rpkiURL hash blob type_ = do
                z <- liftIO $ runValidatorT (newScopes $ unURI uri) $ vHoist $ readObjectOfType type_ blob
                (evaluate $!
                    case z of
                        (Left e, _) ->
                            ObjectParsingProblem rpkiURL (VErr e)
                                (ObjectOriginal blob) hash
                                (ObjectMeta worldVersion type_)
                        (Right ro, _) ->
                            SuccessParsed rpkiURL (toStorableObject ro)
                    ) `catch`
                    (\(e :: SomeException) ->
                        pure $! ObjectParsingProblem rpkiURL (VErr $ RrdpE $ FailedToParseSnapshotItem $ U.fmtEx e)
                                (ObjectOriginal blob) hash
                                (ObjectMeta worldVersion type_)
                    )

    saveStorable _ _ (Left (e, uri)) =
        inSubLocationScope uri $ appWarn e

    saveStorable db tx (Right (uri, a)) = do
        z <- liftIO $ waitCatch a
        case z of
            Left e  -> do
                logError logger [i|Couldn't parse object #{uri}, error #{e}, will NOTs cache the original object.|]
                inSubLocationScope uri $
                    appWarn $ RrdpE $ FailedToParseSnapshotItem $ U.fmtEx e
            Right r ->
                case r of
                    HashExists rpkiURL hash ->
                        DB.linkObjectToUrl tx db rpkiURL hash

                    UnparsableRpkiURL rpkiUrl (VWarn (VWarning e)) -> do
                        logError logger [i|Skipped object #{rpkiUrl}: #{e}|]
                        inSubLocationScope uri $ appWarn e

                    DecodingTrouble _ (VErr e) -> do
                        logError logger [i|Couldn't decode base64 for object #{uri}: #{e}|]
                        inSubLocationScope uri $ appWarn e

                    UknownObjectType rpkiUrl -> do
                        logError logger [i|Unknown object type: #{rpkiUrl}.|]
                        inSubLocationScope uri $
                            appWarn $ RrdpE $ RrdpUnsupportedObjectType $ U.convert rpkiUrl

                    ObjectParsingProblem rpkiUrl (VErr e) original hash objectMeta -> do
                        logError logger [i|Couldn't parse object #{rpkiUrl}, error #{e}, will cache the original object.|]
                        inSubLocationScope uri $ appWarn e
                        DB.saveOriginal tx db original hash objectMeta
                        DB.linkObjectToUrl tx db rpkiUrl hash
                        addedObject

                    SuccessParsed rpkiUrl so@StorableObject {..} -> do
                        DB.saveObject tx db so worldVersion
                        DB.linkObjectToUrl tx db rpkiUrl (getHash object)
                        addedObject

                    other ->
                        logDebug logger [i|Weird thing happened in `saveStorable` #{other}.|]

    validationConfig = appContext ^. typed @Config . typed @ValidationConfig


{-
    Similar to `saveSnapshot` but takes base64s from ordered list of deltas.

    NOTE: Delta application is more strict; we require complete consistency, 
    i.e. applying delta is considered failed if it tries to withdraw or replace
    a non-existent object, or add an existing one. In all these cases, we
    emit an error and fall back to downloading snapshot.
-}
saveDelta :: Storage s =>
            AppContext s
            -> WorldVersion
            -> RrdpURL
            -> Notification
            -> RrdpSerial
            -> BS.ByteString
            -> ValidatorT IO ()
saveDelta appContext worldVersion repoUri notification expectedSerial deltaContent = do
    db <- liftIO $ readTVarIO $ appContext ^. #database

    Delta _ sessionId serial deltaItems <-
        vHoist $ fromEither $ first RrdpE $ parseDelta deltaContent

    let notificationSessionId = notification ^. typed @SessionId
    when (sessionId /= notificationSessionId) $
        appError $ RrdpE $ DeltaSessionMismatch sessionId notificationSessionId

    let notificationSerial = notification ^. typed @RrdpSerial
    when (serial > notificationSerial) $
        appError $ RrdpE $ DeltaSerialTooHigh serial notificationSerial

    when (expectedSerial /= serial) $
        appError $ RrdpE $ DeltaSerialMismatch serial notificationSerial

    let savingTx f =
            DB.rwAppTx db $ \tx ->
                f tx >> DB.updateRrdpMeta tx db (fromNotification notification) repoUri

    -- Propagate exceptions from here, anything that can happen here 
    -- (storage failure, file read failure) should stop the validation and 
    -- probably stop the whole program.
    txFoldPipeline
            cpuParallelism
            (S.mapM newStorable $ S.each deltaItems)
            savingTx
            (saveStorable db)
  where

    newStorable item = do
        case item of
            DP (DeltaPublish uri hash encodedb64) ->
                processSupportedTypes uri $ do
                    a <- liftIO $ async $ readBlob uri encodedb64
                    pure $ Right $ maybe (Add uri a) (Replace uri a) hash

            DW (DeltaWithdraw uri hash) ->
                processSupportedTypes uri $ pure $ Right $ Delete uri hash
      where
        processSupportedTypes uri f =
            if supportedExtension $ U.convert uri
                then f
                else pure $ Left (RrdpE (RrdpUnsupportedObjectType (U.convert uri)), uri)

        readBlob uri encodedb64 =
            case U.parseRpkiURL $ unURI uri of
                Left e        -> pure $! UnparsableRpkiURL uri $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e
                Right rpkiURL -> do
                    case decodeBase64 encodedb64 rpkiURL of
                        Left e                     -> pure $! DecodingTrouble rpkiURL (VErr $ RrdpE e)
                        Right (DecodedBase64 blob) -> do
                            case validateSizeOfBS validationConfig blob of
                                Left e  -> pure $! DecodingTrouble rpkiURL (VErr $ ValidationE e)
                                Right _ -> do
                                    let hash = U.sha256s blob
                                    case urlObjectType rpkiURL of
                                        Just type_ -> tryToParse rpkiURL hash blob type_
                                        Nothing    -> pure $! UknownObjectType rpkiURL
          where
            tryToParse rpkiURL hash blob type_ = do
                z <- liftIO $ runValidatorT (newScopes $ unURI uri) $ vHoist $ readObjectOfType type_ blob
                (evaluate $!
                    case z of
                        (Left e, _) ->
                            ObjectParsingProblem rpkiURL (VErr e)
                                (ObjectOriginal blob) hash
                                (ObjectMeta worldVersion type_)
                        (Right ro, _) ->
                            SuccessParsed rpkiURL (toStorableObject ro)
                    ) `catch`
                    (\(e :: SomeException) ->
                        pure $! ObjectParsingProblem rpkiURL (VErr $ RrdpE $ FailedToParseSnapshotItem $ U.fmtEx e)
                                (ObjectOriginal blob) hash
                                (ObjectMeta worldVersion type_)
                    )

    saveStorable db tx r =
        case r of
            Left (e, uri)                      -> inSubLocationScope uri $ appWarn e
            Right (Add uri a)                  -> addObject db tx uri a
            Right (Replace uri a existingHash) -> replaceObject db tx uri a existingHash
            Right (Delete uri existingHash)    -> deleteObject db tx uri existingHash


    deleteObject db tx uri existingHash = do
        existsLocally <- DB.hashExists tx db existingHash
        if existsLocally
            -- Ignore withdraws and just use the time-based garbage collection
            then deletedObject
            else appError $ RrdpE $ NoObjectToWithdraw uri existingHash


    addObject db tx uri a = do
        r <- fromTry (RrdpE . FailedToParseDeltaItem . U.fmtEx) $ wait a
        case r of
            UnparsableRpkiURL rpkiUrl (VWarn (VWarning e)) -> do
                logError logger [i|Skipped object #{rpkiUrl}, error #{e} |]
                inSubLocationScope uri $ appWarn e

            DecodingTrouble rpkiUrl (VErr e) -> do
                logError logger [i|Couldn't decode base64 for object #{uri}, error #{e} |]
                inSubLocationScope (getURL rpkiUrl) $ appWarn e

            UknownObjectType rpkiUrl -> do
                logError logger [i|Unknown object type: url = #{rpkiUrl}.|]
                inSubLocationScope (getURL rpkiUrl) $
                    appWarn $ RrdpE $ RrdpUnsupportedObjectType $ U.convert rpkiUrl

            ObjectParsingProblem rpkiUrl (VErr e) original hash objectMeta -> do
                logError logger [i|Couldn't parse object #{rpkiUrl}, error #{e}, will cache the original object.|]
                inSubLocationScope (getURL rpkiUrl) $ appWarn e
                DB.saveOriginal tx db original hash objectMeta
                DB.linkObjectToUrl tx db rpkiUrl hash

            SuccessParsed rpkiUrl so@StorableObject {..} -> do
                let newHash = getHash object
                newOneIsAlreadyThere <- DB.hashExists tx db newHash
                unless newOneIsAlreadyThere $ do
                    DB.saveObject tx db so worldVersion
                    addedObject
                DB.linkObjectToUrl tx db rpkiUrl newHash

            other ->
                logDebug logger [i|Weird thing happened in `addObject` #{other}.|]

    replaceObject db tx uri a oldHash = do
        let validateOldHash = do
                oldOneIsAlreadyThere <- DB.hashExists tx db oldHash
                if oldOneIsAlreadyThere
                    then do
                        -- Ignore withdraws and just use the time-based garbage collection
                        deletedObject
                    else do
                        logError logger [i|No object #{uri} with hash #{oldHash} to replace.|]
                        inSubLocationScope uri $
                            appError $ RrdpE $ NoObjectToReplace uri oldHash

        r <- fromTry (RrdpE . FailedToParseDeltaItem . U.fmtEx) $ wait a
        case r of
            UnparsableRpkiURL rpkiUrl (VWarn (VWarning e)) -> do
                logError logger [i|Skipped object #{rpkiUrl}, error #{e} |]
                inSubLocationScope uri $ appWarn e

            DecodingTrouble rpkiUrl (VErr e) -> do
                logError logger [i|Couldn't decode base64 for object #{uri}, error #{e} |]
                inSubLocationScope (getURL rpkiUrl) $ appWarn e

            ObjectParsingProblem rpkiUrl (VErr e) original hash objectMeta -> do
                logError logger [i|Couldn't parse object #{rpkiUrl}, error #{e}, will cache the original object.|]
                inSubLocationScope (getURL rpkiUrl) $ appWarn e
                validateOldHash
                DB.saveOriginal tx db original hash objectMeta
                DB.linkObjectToUrl tx db rpkiUrl hash

            SuccessParsed rpkiUrl so@StorableObject {..} -> do
                validateOldHash
                let newHash = getHash object
                newOneIsAlreadyThere <- DB.hashExists tx db newHash
                unless newOneIsAlreadyThere $ do
                    DB.saveObject tx db so worldVersion
                    addedObject
                DB.linkObjectToUrl tx db rpkiUrl newHash

            other ->
                logDebug logger [i|Weird thing happened in `replaceObject` #{other}.|]

    logger           = appContext ^. #logger
    cpuParallelism   = appContext ^. #config . #parallelism . #cpuParallelism
    validationConfig = appContext ^. #config . #validationConfig


addedObject, deletedObject :: Monad m => ValidatorT m ()
addedObject   = updateMetric @RrdpMetric @_ (#added %~ (+1))
deletedObject = updateMetric @RrdpMetric @_ (#deleted %~ (+1))


data RrdpObjectProcessingResult =
          UnparsableRpkiURL URI VIssue
        | DecodingTrouble RpkiURL VIssue
        | HashExists RpkiURL Hash
        | UknownObjectType RpkiURL
        | ObjectParsingProblem RpkiURL VIssue ObjectOriginal Hash ObjectMeta
        | SuccessParsed RpkiURL (StorableObject RpkiObject)
    deriving stock (Show, Eq, Generic)

data DeltaOp m a = Delete URI Hash
                | Add URI (Async a)
                | Replace URI (Async a) Hash


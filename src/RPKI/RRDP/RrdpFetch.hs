{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE StrictData        #-}

module RPKI.RRDP.RrdpFetch where

import           Control.Concurrent.STM           (readTVarIO)
import           Control.Exception.Lifted         (finally)
import           Control.Lens                     ((.~), (%~), (&), (^.))
import           Control.Monad.Except
import           Data.Generics.Product.Typed

import           Data.Bifunctor                   (first)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import           Data.String.Interpolate.IsString
import           Data.Proxy

import           GHC.Generics

import qualified Streaming.Prelude                as S
import           System.Mem                       (performGC)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Process
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.RRDP.Http
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.Validation.ObjectValidation
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database              (rwAppTx)
import qualified RPKI.Store.Database              as DB
import qualified RPKI.Store.Repository            as RS
import           RPKI.Time
import qualified RPKI.Util                        as U



runRrdpFetch :: AppContext s 
            -> WorldVersion
            -> RrdpRepository             
            -> ValidatorT IO RrdpRepository
runRrdpFetch appContext@AppContext {..} worldVersion repository = do

    -- Do not allow fetch process to have more than 4 CPUs to avoid memory bloat.
    let maxCpuPerFetch = 4    
    let adjustedParallelism = (appContext ^. typed @Config)
            & typed @Parallelism . #cpuCount
            %~ (min maxCpuPerFetch)

    -- this is for humans to read in `top` or `ps`                        
    let workerId = "rrdp-fetch:" <> unpack (unURI $ getURL $ repository ^. #uri)

    let arguments = 
            [ workerId ] <>
            rtsArguments [ rtsN 1, rtsA "20m", rtsAL "64m", rtsMaxMemory "1G" ]

    vp <- askEnv
    (((z, vs), stderr), elapsed) <- 
                timedMS $ runWorker 
                        appContext
                        adjustedParallelism 
                        (RrdpFetchParams vp repository)
                        worldVersion
                        arguments                        
    embedState vs
    case z of 
        Left e  -> appError e
        Right r -> do 
            let formattedWorkerLog =             
                    [i|Worker #{workerId}> done, took #{elapsed}ms, 
<worker-log> 
#{(decodeUtf8 $ LBS.toStrict stderr)}</worker-log>|]                
            logDebugM logger formattedWorkerLog                
            pure r


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
                            -> ValidatorT IO RrdpRepository
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
                        -> ValidatorT IO RrdpRepository
downloadAndUpdateRRDP 
        appContext@AppContext {..}
        repo@(RrdpRepository repoUri _ _)      
        handleSnapshotBS                       -- ^ function to handle the snapshot bytecontent
        handleDeltaBS =                        -- ^ function to handle delta bytecontents
  do        
    ((notificationXml, _, _), notificationDownloadTime) <- 
                            fromTry (RrdpE . CantDownloadNotification . U.fmtEx) 
                                $ timedMS 
                                $ downloadToBS (appContext ^. typed) (getURL repoUri)         
    bumpDownloadTime notificationDownloadTime
    notification         <- hoistHere $ parseNotification notificationXml
    nextStep             <- vHoist $ rrdpNextStep repo notification

    case nextStep of
        NothingToDo message -> do 
            used RrdpNoUpdate
            logDebugM logger [i|Nothing to update for #{repoUri}: #{message}|]
            pure repo

        UseSnapshot snapshotInfo message -> do 
            used RrdpSnapshot
            logDebugM logger [i|Going to use snapshot for #{repoUri}: #{message}|]
            useSnapshot snapshotInfo notification

        UseDeltas sortedDeltas snapshotInfo message -> 
                (do 
                    used RrdpDelta
                    logDebugM logger [i|Going to use deltas for #{repoUri}: #{message}|]
                    useDeltas sortedDeltas notification)
                    `catchError` 
                \e -> do         
                    -- NOTE At the moment we ignore the fact that some objects are wrongfully added by 
                    -- some of the deltas
                    logErrorM logger [i|Failed to apply deltas for #{repoUri}: #{e}, will fall back to snapshot.|]
                    used RrdpSnapshot
                    useSnapshot snapshotInfo notification            
  where
    
    used z       = updateMetric @RrdpMetric @_ (& #rrdpSource .~ z)    
    
    hoistHere    = vHoist . fromEither . first RrdpE        
    ioBottleneck = appContext ^. typed @AppBottleneck . #ioBottleneck        

    bumpDownloadTime t = updateMetric @RrdpMetric @_ (& #downloadTimeMs %~ (<> TimeMs t))

    useSnapshot (SnapshotInfo uri hash) notification = 
        inSubVPath (U.convert uri) $ do
            logInfoM logger [i|#{uri}: downloading snapshot.|]
            
            ((rawContent, _, httpStatus'), downloadedIn) <- timedMS $ 
                fromTryEither (RrdpE . CantDownloadSnapshot . U.fmtEx) $ 
                    downloadHashedBS (appContext ^. typed @Config) uri hash                                    
                        (\actualHash -> 
                            Left $ RrdpE $ SnapshotHashMismatch { 
                                expectedHash = hash,
                                actualHash = actualHash                                            
                            })                

            bumpDownloadTime downloadedIn
            updateMetric @RrdpMetric @_ (& #lastHttpStatus .~ httpStatus') 

            (_, savedIn) <- timedMS $ handleSnapshotBS repoUri notification rawContent  
            updateMetric @RrdpMetric @_ (& #saveTimeMs .~ TimeMs savedIn)          

            pure $ repo { rrdpMeta = rrdpMeta' }

        where
            rrdpMeta' = Just (notification ^. #sessionId, notification ^. #serial)                    
    

    useDeltas sortedDeltas notification = do
        let repoURI = getURL $ repo ^. #uri
        let message = if minSerial == maxSerial 
                then [i|#{repoURI}: downloading delta #{minSerial}.|]
                else [i|#{repoURI}: downloading deltas from #{minSerial} to #{maxSerial}.|]
        
        logInfoM logger message

        -- Try to deallocate all the bytestrings created by mmaps right after they are used, 
        -- otherwise they will hold too much files open.
        downloadAndSave `finally` liftIO performGC        

      where

        downloadAndSave = do
            -- Do not thrash the same server with too big amount of parallel 
            -- requests, it's mostly counter-productive and rude. Maybe 8 is still too much?
            localRepoBottleneck <- liftIO $ newBottleneckIO 8            
            (_, savedIn) <- timedMS $ foldPipeline
                                (localRepoBottleneck <> ioBottleneck)
                                (S.each sortedDeltas)
                                downloadDelta
                                (\(rawContent, serial, deltaUri) _ -> 
                                    inSubVPath deltaUri $ 
                                        handleDeltaBS repoUri notification serial rawContent)
                                (mempty :: ())
            bumpDownloadTime savedIn                
            updateMetric @RrdpMetric @_ (& #saveTimeMs .~ TimeMs savedIn)          

            pure $ repo { rrdpMeta = rrdpMeta' }

        downloadDelta (DeltaInfo uri hash serial) = do
            let deltaUri = U.convert uri 
            (rawContent, _, httpStatus') <- 
                inSubVPath deltaUri $ do
                    fromTryEither (RrdpE . CantDownloadDelta . U.fmtEx) $ 
                        downloadHashedBS (appContext ^. typed @Config) uri hash
                            (\actualHash -> 
                                Left $ RrdpE $ DeltaHashMismatch {
                                    actualHash = actualHash,
                                    expectedHash = hash,
                                    serial = serial
                                })
            updateMetric @RrdpMetric @_ (& #lastHttpStatus .~ httpStatus') 
            pure (rawContent, serial, deltaUri)

        serials = map (^. typed @RrdpSerial) sortedDeltas
        maxSerial = List.maximum serials
        minSerial = List.minimum serials

        rrdpMeta' = Just (notification ^. typed @SessionId, maxSerial)            


data Step
  = UseSnapshot SnapshotInfo Text
  | UseDeltas
      { sortedDeltas :: [DeltaInfo]
      , snapshotInfo :: SnapshotInfo
      , message :: Text
      }
  | NothingToDo Text
  deriving (Show, Eq, Ord, Generic)



-- | Decides what to do next based on current state of the repository
-- | and the parsed notification file
rrdpNextStep :: RrdpRepository -> Notification -> PureValidatorT Step

rrdpNextStep (RrdpRepository _ Nothing _) Notification{..} = 
    pure $ UseSnapshot snapshotInfo "Unknown repository"

rrdpNextStep (RrdpRepository _ (Just (repoSessionId, repoSerial)) _) Notification{..} =

    if  | sessionId /= repoSessionId -> 
            pure $ UseSnapshot snapshotInfo [i|Resetting RRDP session from #{repoSessionId} to #{sessionId}|]

        | repoSerial > serial -> do 
            appWarn $ RrdpE $ LocalSerialBiggerThanRemote repoSerial serial
            pure $ NothingToDo [i|Local serial #{repoSerial} is lower than the remote serial #{serial}.|]

        | repoSerial == serial -> 
            pure $ NothingToDo "Up-to-date."
    
        | otherwise ->
            case (deltas, nonConsecutiveDeltas) of
                ([], _) -> pure $ UseSnapshot snapshotInfo "There is no deltas to use."

                (_, []) | nextSerial repoSerial < deltaSerial (head sortedDeltas) ->
                            -- we are too far behind
                            pure $ UseSnapshot snapshotInfo "Local serial is too far behind."

                        -- too many deltas means huge overhead -- just use snapshot, 
                        -- it's more data but less chances of getting killed by timeout
                        | length chosenDeltas > 100 ->
                            pure $ UseSnapshot snapshotInfo [i|There are too many deltas: #{length chosenDeltas}.|]                        

                        | otherwise ->
                            pure $ UseDeltas chosenDeltas snapshotInfo "Deltas look good."

                (_, nc) -> appError $ RrdpE $ NonConsecutiveDeltaSerials nc
                
            where
                sortedSerials = map deltaSerial sortedDeltas
                sortedDeltas = List.sortOn deltaSerial deltas
                chosenDeltas = filter ((> repoSerial) . deltaSerial) sortedDeltas

                nonConsecutiveDeltas = List.filter (\(s, s') -> nextSerial s /= s') $
                    List.zip sortedSerials (tail sortedSerials)


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
saveSnapshot appContext worldVersion repoUri notification snapshotContent = do              

    -- If we are going for the snapshot we are going to need a lot of CPU
    -- time, so bump the number of CPUs to the maximum possible values    
    let maxCpuAvailable = appContext ^. typed @Config . typed @Parallelism . #cpuCount
    liftIO $ setCpuCount maxCpuAvailable
    let cpuParallelism = makeParallelism maxCpuAvailable ^. #cpuParallelism

    db <- liftIO $ readTVarIO $ appContext ^. #database
    let objectStore     = db ^. #objectStore
    let repositoryStore = db ^. #repositoryStore   
    (Snapshot _ sessionId serial snapshotItems) <- vHoist $         
        fromEither $ first RrdpE $ parseSnapshot snapshotContent

    let notificationSessionId = notification ^. typed @SessionId
    when (sessionId /= notificationSessionId) $ 
        appError $ RrdpE $ SnapshotSessionMismatch sessionId notificationSessionId

    let notificationSerial = notification ^. typed @RrdpSerial
    when (serial /= notificationSerial) $ 
        appError $ RrdpE $ SnapshotSerialMismatch serial notificationSerial

    let savingTx sessionId serial f = 
            rwAppTx objectStore $ \tx -> 
                f tx >> RS.updateRrdpMeta tx repositoryStore (sessionId, serial) repoUri 

    void $ txFoldPipeline 
                cpuParallelism
                (S.mapM (newStorable objectStore) $ S.each snapshotItems)
                (savingTx sessionId serial)
                (saveStorable objectStore)
                (mempty :: ())
    where        

    newStorable objectStore (SnapshotPublish uri encodedb64) =             
        if supportedExtension $ U.convert uri 
            then do 
                task <- readBlob `strictTask` bottleneck
                pure $ Right (uri, task)
            else
                pure $ Left (RrdpE (UnsupportedObjectType (U.convert uri)), uri)
        where 
        readBlob = case U.parseRpkiURL $ unURI uri of
            Left e -> 
                pure $! UnparsableRpkiURL uri $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e

            Right rpkiURL ->
                case first RrdpE $ U.decodeBase64 encodedb64 rpkiURL of
                    Left e -> pure $! DecodingTrouble rpkiURL (VErr e)
                    Right (DecodedBase64 decoded) -> do                             
                        case validateSizeOfBS validationConfig decoded of 
                            Left e  -> pure $! DecodingTrouble rpkiURL (VErr $ ValidationE e)
                            Right _ -> 
                                liftIO $ roTx objectStore $ \tx -> do     
                                    let hash = U.sha256s decoded  
                                    exists <- DB.hashExists tx objectStore hash
                                    pure $! if exists 
                                        -- The object is already in cache. Do not parse-serialise
                                        -- anything, just skip it. We are not afraid of possible 
                                        -- race-conditions here, it's not a problem to double-insert
                                        -- an object and delete-insert race will never happen in practice
                                        -- since deletion is never concurrent with insertion.
                                        then HashExists rpkiURL hash
                                        else
                                            case first ParseE $ readObject rpkiURL decoded of 
                                                Left e   -> ObjectParsingProblem rpkiURL (VErr e)
                                                Right ro -> Success rpkiURL (toStorableObject ro)
                                    
    saveStorable _ _ (Left (e, uri)) _ = 
        inSubVPath (unURI uri) $ appWarn e             

    saveStorable objectStore tx (Right (uri, a)) _ =           
        waitTask a >>= \case     
            HashExists rpkiURL hash ->
                DB.linkObjectToUrl tx objectStore rpkiURL hash
            UnparsableRpkiURL uri (VWarn (VWarning e)) -> do                    
                logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                inSubVPath (unURI uri) $ appWarn e 
            DecodingTrouble rpkiUrl (VErr e) -> do
                logErrorM logger [i|Couldn't decode base64 for object #{uri}, error #{e} |]
                inSubVPath (unURI $ getURL rpkiUrl) $ appError e                 
            ObjectParsingProblem rpkiUrl (VErr e) -> do                    
                logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                inSubVPath (unURI $ getURL rpkiUrl) $ appError e                 
            Success rpkiUrl so@StorableObject {..} -> do 
                DB.putObject tx objectStore so worldVersion                    
                DB.linkObjectToUrl tx objectStore rpkiUrl (getHash object)
                addedObject                     
            other -> 
                logDebugM logger [i|Weird thing happened in `saveStorable` #{other}.|]                                     

    logger         = appContext ^. typed @AppLogger           
    
    bottleneck     = appContext ^. typed @AppBottleneck . #cpuBottleneck    
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
saveDelta appContext worldVersion repoUri notification currentSerial deltaContent = do                
    db <- liftIO $ readTVarIO $ appContext ^. #database
    let objectStore     = db ^. #objectStore
    let repositoryStore = db ^. #repositoryStore   

    Delta _ sessionId serial deltaItems <- 
        vHoist $ fromEither $ first RrdpE $ parseDelta deltaContent    

    let notificationSessionId = notification ^. typed @SessionId
    when (sessionId /= notificationSessionId) $ 
        appError $ RrdpE $ DeltaSessionMismatch sessionId notificationSessionId

    let notificationSerial = notification ^. typed @RrdpSerial
    when (serial > notificationSerial) $ 
        appError $ RrdpE $ DeltaSerialTooHigh serial notificationSerial

    when (currentSerial /= serial) $
        appError $ RrdpE $ DeltaSerialMismatch serial notificationSerial
    
    let savingTx sessionId serial f = 
            rwAppTx objectStore $ \tx -> 
                f tx >> RS.updateRrdpMeta tx repositoryStore (sessionId, serial) repoUri 

    -- Propagate exceptions from here, anything that can happen here 
    -- (storage failure, file read failure) should stop the validation and 
    -- probably stop the whole program.
    txFoldPipeline 
            cpuParallelism
            (S.mapM newStorable $ S.each deltaItems)
            (savingTx sessionId serial)
            (saveStorable objectStore)
            (mempty :: ())
    where        

    newStorable item = do 
        case item of
            DP (DeltaPublish uri hash encodedb64) -> 
                processSupportedTypes uri $ do 
                    task <- readBlob uri encodedb64 `pureTask` bottleneck
                    pure $ Right $ maybe (Add uri task) (Replace uri task) hash
                    
            DW (DeltaWithdraw uri hash) -> 
                processSupportedTypes uri $                     
                    pure $ Right $ Delete uri hash                    
        where
        processSupportedTypes uri f = 
            if supportedExtension $ U.convert uri 
                then f
                else    
                    pure $ Left (RrdpE (UnsupportedObjectType (U.convert uri)), uri)

        readBlob uri encodedb64 = 
            case U.parseRpkiURL $ unURI uri of
                Left e        -> UnparsableRpkiURL uri $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e
                Right rpkiURL -> do 
                    case decodeBase64 encodedb64 rpkiURL of
                        Left e -> DecodingTrouble rpkiURL (VErr $ RrdpE e)
                        Right (DecodedBase64 decoded) -> do 
                            case validateSizeOfBS validationConfig decoded of 
                                Left e  -> ObjectParsingProblem rpkiURL (VErr $ ValidationE e)
                                Right _ ->                                 
                                    case readObject rpkiURL decoded of 
                                        Left e   -> ObjectParsingProblem rpkiURL (VErr $ ParseE e)
                                        Right ro -> Success rpkiURL (toStorableObject ro)                     

    saveStorable objectStore tx r _ = 
        case r of 
        Left (e, uri)                         -> inSubVPath (unURI uri) $ appWarn e             
        Right (Add uri task)                  -> addObject objectStore tx uri task 
        Right (Replace uri task existingHash) -> replaceObject objectStore tx uri task existingHash
        Right (Delete uri existingHash)       -> deleteObject objectStore tx uri existingHash                                        
    

    deleteObject objectStore tx uri existingHash = do 
        existsLocally <- DB.hashExists tx objectStore existingHash
        if existsLocally
            -- Ignore withdraws and just use the time-based garbage collection
            then deletedObject
            else appError $ RrdpE $ NoObjectToWithdraw uri existingHash
        

    addObject objectStore tx uri a =
        waitTask a >>= \case
            UnparsableRpkiURL uri (VWarn (VWarning e)) -> do
                logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                inSubVPath (unURI uri) $ appWarn e 
            DecodingTrouble rpkiUrl (VErr e) -> do
                logErrorM logger [i|Couldn't decode base64 for object #{uri}, error #{e} |]
                inSubVPath (unURI $ getURL rpkiUrl) $ appError e                                     
            ObjectParsingProblem rpkiUrl (VErr e) -> do
                logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                inSubVPath (unURI $ getURL rpkiUrl) $ appError e 
            Success rpkiUrl so@StorableObject {..} -> do 
                let hash' = getHash object
                alreadyThere <- DB.hashExists tx objectStore hash'
                if alreadyThere 
                    then 
                        DB.linkObjectToUrl tx objectStore rpkiUrl hash'
                    else do                                    
                        DB.putObject tx objectStore so worldVersion                      
                        DB.linkObjectToUrl tx objectStore rpkiUrl hash'
                        addedObject
            other -> 
                logDebugM logger [i|Weird thing happened in `addObject` #{other}.|]

    replaceObject objectStore tx uri a oldHash = do            
        waitTask a >>= \case
            UnparsableRpkiURL uri (VWarn (VWarning e)) -> do
                logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                inSubVPath (unURI uri) $ appWarn e 
            DecodingTrouble rpkiUrl (VErr e) -> do
                logErrorM logger [i|Couldn't decode base64 for object #{uri}, error #{e} |]
                inSubVPath (unURI $ getURL rpkiUrl) $ appError e                                           
            ObjectParsingProblem rpkiUrl (VErr e) -> do
                logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                inSubVPath (unURI $ getURL rpkiUrl) $ appError e 
            Success rpkiUrl so@StorableObject {..} -> do 
                oldOneIsAlreadyThere <- DB.hashExists tx objectStore oldHash                           
                if oldOneIsAlreadyThere 
                    then do 
                        -- Ignore withdraws and just use the time-based garbage collection
                        deletedObject
                    else do 
                        logErrorM logger [i|No object #{uri} with hash #{oldHash} to replace.|]
                        inSubVPath (unURI uri) $ 
                            appError $ RrdpE $ NoObjectToReplace uri oldHash

                let hash' = getHash object
                newOneIsAlreadyThere <- DB.hashExists tx objectStore hash'
                if newOneIsAlreadyThere
                    then 
                        DB.linkObjectToUrl tx objectStore rpkiUrl hash'
                    else do                            
                        DB.putObject tx objectStore so worldVersion
                        DB.linkObjectToUrl tx objectStore rpkiUrl hash'
                        addedObject

            other -> 
                logDebugM logger [i|Weird thing happened in `replaceObject` #{other}.|]
                
                                                                                

    logger           = appContext ^. typed @AppLogger           
    cpuParallelism   = appContext ^. typed @Config . typed @Parallelism . #cpuParallelism
    bottleneck       = appContext ^. typed @AppBottleneck . #cpuBottleneck                      
    validationConfig = appContext ^. typed @Config . typed @ValidationConfig


addedObject, deletedObject :: Monad m => ValidatorT m ()
addedObject   = updateMetric @RrdpMetric @_ (& #added %~ (+1))
deletedObject = updateMetric @RrdpMetric @_ (& #deleted %~ (+1))


data ObjectProcessingResult =           
          UnparsableRpkiURL URI VProblem
        | DecodingTrouble RpkiURL VProblem
        | HashExists RpkiURL Hash
        | ObjectParsingProblem RpkiURL VProblem
        | Success RpkiURL (StorableObject RpkiObject)
    deriving Show

data DeltaOp m a = Delete URI Hash 
                | Add URI (Task m a) 
                | Replace URI (Task m a) Hash



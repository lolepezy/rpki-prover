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
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import           Data.String.Interpolate.IsString
import           Data.Proxy

import           GHC.Generics

import qualified Streaming.Prelude                as S
import           System.Mem                       (performGC)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.RRDP.Http
import RPKI.RRDP.Parse
    ( decodeBase64, parseDelta, parseNotification, parseSnapshot )
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database              (rwAppTx)
import qualified RPKI.Store.Database              as DB
import qualified RPKI.Store.Repository            as RS
import           RPKI.Time
import qualified RPKI.Util                        as U



-- | 
--  Update RRDP repository, i.e. do the full cycle
--    - download notifications file, parse it
--    - decide what to do next based on it
--    - download snapshot or deltas
--    - do something appropriate with either of them
-- 
downloadAndUpdateRRDP :: AppContext s ->
                        RrdpRepository 
                        -> (RrdpURL -> Notification -> LBS.ByteString -> ValidatorT IO ()) 
                        -> (RrdpURL -> Notification -> Serial -> LBS.ByteString -> ValidatorT IO ()) 
                        -> ValidatorT IO RrdpRepository
downloadAndUpdateRRDP 
        appContext@AppContext {..}
        repo@(RrdpRepository repoUri _ _)      
        handleSnapshotBS                       -- ^ function to handle the snapshot bytecontent
        handleDeltaBS =                        -- ^ function to handle delta bytecontents
  do        
    ((notificationXml, _, httpStatus), notificationDownloadTime) <- 
                            fromTry (RrdpE . CantDownloadNotification . U.fmtEx) 
                                $ timedMS 
                                $ downloadToBS (appContext ^. typed) (getURL repoUri)         
    bumpDownloadTime notificationDownloadTime
    notification         <- hoistHere $ parseNotification notificationXml
    nextStep             <- vHoist $ rrdpNextStep repo notification

    case nextStep of
        NothingToDo -> do 
            used RrdpNoUpdate
            pure repo

        UseSnapshot snapshotInfo -> do 
            used RrdpSnapshot
            useSnapshot snapshotInfo notification

        UseDeltas sortedDeltas snapshotInfo -> 
                (used RrdpDelta >> useDeltas sortedDeltas notification)
                    `catchError` 
                \e -> do         
                    -- NOTE At the moment we ignore the fact that some objects are wrongfully added by 
                    -- some of the deltas
                    logErrorM logger [i|Failed to apply deltas for #{repoUri}: #{e}, will fall back to snapshot.|]
                    appWarn e
                    used RrdpSnapshot
                    useSnapshot snapshotInfo notification            
  where
    
    used z       = updateMetric @RrdpMetric @_ (& #rrdpSource .~ z)    
    
    hoistHere    = vHoist . fromEither . first RrdpE        
    ioBottleneck = appContext ^. typed @AppBottleneck . #ioBottleneck        

    bumpDownloadTime t = updateMetric @RrdpMetric @_ (& #downloadTimeMs %~ (<> TimeMs t))

    useSnapshot (SnapshotInfo uri hash) notification = 
        inSubVPath (U.convert uri) $ do
            logDebugM logger [i|#{uri}: downloading snapshot.|]
            (r, downloadedIn, savedIn) <- downloadAndSave            
            pure r
        where
            downloadAndSave = do
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

                pure (repo { rrdpMeta = rrdpMeta' }, downloadedIn, savedIn)

            rrdpMeta' = Just (notification ^. #sessionId, notification ^. #serial)                    
    

    useDeltas sortedDeltas notification = do
        let repoURI = getURL $ repo ^. #uri
        let message = if minSerial == maxSerial 
                then [i|#{repoURI}: downloading delta #{minSerial}.|]
                else [i|#{repoURI}: downloading deltas from #{minSerial} to #{maxSerial}.|]
        
        logDebugM logger message

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

                pure $! repo { rrdpMeta = rrdpMeta' }

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

            serials = map (^. typed @Serial) sortedDeltas
            maxSerial = List.maximum serials
            minSerial = List.minimum serials

            rrdpMeta' = Just (notification ^. typed @SessionId, maxSerial)            


data Step
  = UseSnapshot SnapshotInfo
  | UseDeltas
      { sortedDeltas :: [DeltaInfo]
      , snapshotInfo :: SnapshotInfo
      }
  | NothingToDo
  deriving (Show, Eq, Ord, Generic)



-- | Decides what to do next based on current state of the repository
-- | and the parsed notification file
rrdpNextStep :: RrdpRepository -> Notification -> PureValidatorT Step
rrdpNextStep (RrdpRepository _ Nothing _) Notification{..} = 
    pure $ UseSnapshot snapshotInfo
rrdpNextStep (RrdpRepository _ (Just (repoSessionId, repoSerial)) _) Notification{..} =
    if  | sessionId /= repoSessionId -> pure $ UseSnapshot snapshotInfo
        | repoSerial > serial        -> do 
            appWarn $ RrdpE $ LocalSerialBiggerThanRemote repoSerial serial
            pure NothingToDo
        | repoSerial == serial       -> pure NothingToDo
        | otherwise ->
            case (deltas, nonConsecutiveDeltas) of
                ([], _) -> pure $ UseSnapshot snapshotInfo
                (_, []) | nextSerial repoSerial < head (map deltaSerial sortedDeltas) ->
                            -- we are too far behind
                            pure $ UseSnapshot snapshotInfo
                        | otherwise ->
                            pure $ UseDeltas chosenDeltas snapshotInfo
                (_, nc) -> appError $ RrdpE $ NonConsecutiveDeltaSerials nc
            where
                sortedSerials = map deltaSerial sortedDeltas
                sortedDeltas = List.sortOn deltaSerial deltas
                chosenDeltas = filter ((> repoSerial) . deltaSerial) sortedDeltas

                nonConsecutiveDeltas = List.filter (\(s, s') -> nextSerial s /= s') $
                    List.zip sortedSerials (tail sortedSerials)


deltaSerial :: DeltaInfo -> Serial
deltaSerial (DeltaInfo _ _ s) = s

nextSerial :: Serial -> Serial
nextSerial (Serial s) = Serial $ s + 1


-- | 
--  Update RRDP repository, actually saving all the objects in the DB.
--
-- NOTE: It will update the sessionId and serial of the repository 
-- in the same transaction it stores the data in.
-- 
updateObjectForRrdpRepository :: Storage s => 
                                AppContext s 
                            -> RrdpRepository 
                            -> ValidatorT IO RrdpRepository
updateObjectForRrdpRepository appContext repository = do
    let repoURI = getURL $ repository ^. #uri
    timedMetric (Proxy :: Proxy RrdpMetric) $ 
        downloadAndUpdateRRDP 
            appContext 
            repository 
            (saveSnapshot appContext)  
            (saveDelta appContext)                           


{- 
    Snapshot case, done in parallel by two thread
        - one thread parses XML, reads base64s and pushes CPU-intensive parsing tasks into the queue 
        - another thread read parsing tasks, waits for them and saves the results into the DB.
-} 
saveSnapshot :: Storage s => 
                AppContext s 
                -> RrdpURL
                -> Notification 
                -> LBS.ByteString 
                -> ValidatorT IO ()
saveSnapshot appContext repoUri notification snapshotContent = do      
    -- TODO Bad idea if we are lagging behind, put WorldVersion in the reader?
    worldVersion <- liftIO $ getWorldVerionIO $ appContext ^. typed @AppState
    doSaveObjects worldVersion 
  where
      -- 
    doSaveObjects worldVersion = do 
        db <- liftIO $ readTVarIO $ appContext ^. #database
        let objectStore     = db ^. #objectStore
        let repositoryStore = db ^. #repositoryStore   
        (Snapshot _ sessionId serial snapshotItems) <- vHoist $ 
            fromEither $ first RrdpE $ parseSnapshot snapshotContent

        let notificationSessionId = notification ^. typed @SessionId
        when (sessionId /= notificationSessionId) $ 
            appError $ RrdpE $ SnapshotSessionMismatch sessionId notificationSessionId

        let notificationSerial = notification ^. typed @Serial
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
                        pure $! Right $ SError $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e

                    Right rpkiURL ->
                        case first RrdpE $ decodeBase64 encodedb64 rpkiURL of
                            Left e -> pure $! Right $ SError $ VErr e
                            Right (DecodedBase64 decoded) ->
                                liftIO $ roTx objectStore $ \tx -> do     
                                    let hash = U.sha256s decoded  
                                    exists <- DB.hashExists tx objectStore hash
                                    pure $! if exists 
                                    -- The object is already in cache. Do not parse-serialise
                                    -- anything, just skip it. We are not afraid of possible 
                                    -- race-conditions here, it's not a problem to double-insert
                                    -- an object and delete-insert race will never happen in practice.
                                        then Left (rpkiURL, hash)
                                        else
                                            case first ParseE $ readObject rpkiURL decoded of 
                                                Left e   -> Right $! SError $ VErr e
                                                Right ro -> Right $! SObject $ toStorableObject ro rpkiURL
                                        
        saveStorable objectStore _ (Left (e, uri)) _ = 
            inSubVPath (unURI uri) $ appWarn e             

        saveStorable objectStore tx (Right (uri, a)) _ =           
            waitTask a >>= \case     
                Left (rpkiURL, hash) ->
                    DB.linkObjectToUrl tx objectStore rpkiURL hash worldVersion
                Right (SError (VWarn (VWarning e))) -> do                    
                    logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                    inSubVPath (unURI uri) $ appWarn e 
                Right (SError (VErr e)) -> do                    
                    logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                    inSubVPath (unURI uri) $ appError e                 
                Right (SObject so@StorableObject {..}) -> do 
                    DB.putObject tx objectStore so worldVersion                    
                    DB.linkObjectToUrl tx objectStore tag (getHash object) worldVersion
                    addedObject                    

    logger         = appContext ^. typed @AppLogger           
    cpuParallelism = appContext ^. typed @Config . typed @Parallelism . #cpuParallelism
    bottleneck     = appContext ^. typed @AppBottleneck . #cpuBottleneck    


{-
    Similar to `saveSnapshot` but takes base64s from ordered list of deltas.

    NOTE: Delta application is more strict; we require complete consistency, 
    i.e. applying delta is considered failed if it tries to withdraw or replace
    a non-existent object, or place an existent one. In all these cases, we
    fall back to downloading the snapshot.
-}
saveDelta :: Storage s => 
            AppContext s 
            -> RrdpURL 
            -> Notification 
            -> Serial             
            -> LBS.ByteString 
            -> ValidatorT IO ()
saveDelta appContext repoUri notification currentSerial deltaContent = do        
    worldVersion <- liftIO $ getWorldVerionIO $ appContext ^. typed @AppState
    doSaveObjects worldVersion
  where
    doSaveObjects worldVersion = do
        db <- liftIO $ readTVarIO $ appContext ^. #database
        let objectStore     = db ^. #objectStore
        let repositoryStore = db ^. #repositoryStore   

        Delta _ sessionId serial deltaItems <- 
            vHoist $ fromEither $ first RrdpE $ parseDelta deltaContent    

        let notificationSessionId = notification ^. typed @SessionId
        when (sessionId /= notificationSessionId) $ 
            appError $ RrdpE $ DeltaSessionMismatch sessionId notificationSessionId

        let notificationSerial = notification ^. typed @Serial
        when (serial > notificationSerial) $ 
            appError $ RrdpE $ DeltaSerialTooHigh serial notificationSerial

        when (currentSerial /= serial) $
            appError $ RrdpE $ DeltaSerialMismatch serial notificationSerial
        
        let savingTx sessionId serial f = 
                rwAppTx objectStore $ \tx -> 
                    f tx >> RS.updateRrdpMeta tx repositoryStore (sessionId, serial) repoUri 

        -- Propagate exceptions from here, anything that can happen here 
        -- (storage failure, mmap failure) should stop the validation and 
        -- probably stop the whole program.
        txFoldPipeline 
                cpuParallelism
                (S.mapM newStorable $ S.each deltaItems)
                (savingTx sessionId serial)
                (saveStorable objectStore)
                (mempty :: ())
      where        
        newStorable (DP (DeltaPublish uri hash encodedb64)) =
            if supportedExtension $ U.convert uri 
                then do 
                    task <- readBlob `pureTask` bottleneck
                    pure $ Right $ maybe (Add uri task) (Replace uri task) hash
                else 
                    pure $ Left (RrdpE (UnsupportedObjectType (U.convert uri)), uri)
            where 
                readBlob = case U.parseRpkiURL $ unURI uri of
                    Left e        -> SError $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e
                    Right rpkiURL -> parseAndProcess rpkiURL encodedb64

        newStorable (DW (DeltaWithdraw uri hash)) = 
            pure $ Right $ Delete uri hash
        
        saveStorable objectStore _ (Left (e, uri)) _ = 
            inSubVPath (unURI uri) $ appWarn e             

        saveStorable objectStore tx (Right op) _ =
            case op of
                Delete uri existingHash -> do
                    -- Ignore withdraws and just use the time-based garbage collection
                    existsLocally <- DB.hashExists tx objectStore existingHash
                    if existsLocally
                        then deletedObject
                        else appError $ RrdpE $ NoObjectToWithdraw uri existingHash
                        
                Add uri async'             -> addObject objectStore tx uri async'                
                Replace uri async' oldHash -> replaceObject objectStore tx uri async' oldHash                    
        
        addObject objectStore tx uri a =
            waitTask a >>= \case
                SError (VWarn (VWarning e)) -> do                    
                    logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                    inSubVPath (unURI uri) $ appWarn e 
                SError (VErr e) -> do                    
                    logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                    inSubVPath (unURI uri) $ appError e 
                SObject so@StorableObject {..} -> do
                    let hash' = getHash object
                    alreadyThere <- DB.hashExists tx objectStore hash'
                    if alreadyThere 
                        then 
                            DB.linkObjectToUrl tx objectStore tag hash' worldVersion
                        else do                                    
                            DB.putObject tx objectStore so worldVersion                      
                            DB.linkObjectToUrl tx objectStore tag hash' worldVersion
                            addedObject

        replaceObject objectStore tx uri a oldHash = do            
            waitTask a >>= \case
                SError (VWarn (VWarning e)) -> do                    
                    logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                    inSubVPath (unURI uri) $ appWarn e 
                SError (VErr e) -> do                    
                    logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                    inSubVPath (unURI uri) $ appError e 
                SObject so@StorableObject {..} -> do        
                    oldOneIsAlreadyThere <- DB.hashExists tx objectStore oldHash                           
                    if oldOneIsAlreadyThere 
                        then do 
                            -- Ignore withdraws and just use the time-based garbage collection
                            deletedObject
                        else do 
                            logWarnM logger [i|No object #{uri} with hash #{oldHash} to replace.|]
                            inSubVPath (unURI uri) $ 
                                appError $ RrdpE $ NoObjectToReplace uri oldHash

                    let hash' = getHash object
                    newOneIsAlreadyThere <- DB.hashExists tx objectStore hash'
                    if newOneIsAlreadyThere
                        then 
                            DB.linkObjectToUrl tx objectStore tag hash' worldVersion
                        else do                            
                            DB.putObject tx objectStore so worldVersion
                            DB.linkObjectToUrl tx objectStore tag hash' worldVersion
                            addedObject
                                                                                  

    logger          = appContext ^. typed @AppLogger           
    cpuParallelism  = appContext ^. typed @Config . typed @Parallelism . #cpuParallelism
    bottleneck      = appContext ^. typed @AppBottleneck . #cpuBottleneck                      


addedObject, deletedObject :: Monad m => ValidatorT m ()
addedObject   = updateMetric @RrdpMetric @_ (& #added %~ (+1))
deletedObject = updateMetric @RrdpMetric @_ (& #deleted %~ (+1))


parseAndProcess :: RpkiURL -> EncodedBase64 -> StorableUnit RpkiObject VProblem RpkiURL 
parseAndProcess u b64 =     
    case parsed of
        Left e   -> SError $ VErr e
        Right ro -> SObject $! toStorableObject ro u
    where
        parsed = do
            DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
            first ParseE $ readObject u b    


data DeltaOp m a = Delete URI Hash 
                | Add URI (Task m a) 
                | Replace URI (Task m a) Hash
data DeltaOp1 a = Delete1 URI Hash 
                | Add1 URI a
                | Replace1 URI a Hash

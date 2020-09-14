{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}


module RPKI.RRDP.RrdpFetch where

import           Control.Exception.Lifted         (finally)
import           Control.Lens                     ((^.))
import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Data.Generics.Product.Typed

import           Data.Bifunctor                   (first)
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.RRDP.Http
import           RPKI.RRDP.HttpContext
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database              (roAppTx, rwAppTx)
import qualified RPKI.Store.Database              as DB
import qualified RPKI.Store.Repository            as RS
import           RPKI.Time
import qualified RPKI.Util                        as U
import           RPKI.Version

import           Data.IORef.Lifted

import qualified Streaming.Prelude                as S

import           System.Mem                       (performGC)



-- | 
--  Update RRDP repository, i.e. do the full cycle
--    - download notifications file, parse it
--    - decide what to do next based on it
--    - download snapshot or deltas
--    - do something appropriate with either of them
-- 
downloadAndUpdateRRDP :: WithVContext vc => 
                        AppContext s ->
                        HttpContext ->
                        RrdpRepository ->                 
                        (RrdpURL -> Notification -> LBS.ByteString -> ValidatorT vc IO ()) ->
                        (RrdpURL -> Notification -> Serial -> LBS.ByteString -> ValidatorT vc IO ()) ->
                        ValidatorT vc IO RrdpRepository
downloadAndUpdateRRDP 
        appContext
        httpContext
        repo@(RrdpRepository repoUri _ _)      
        handleSnapshotBS                       -- ^ function to handle the snapshot bytecontent
        handleDeltaBS =                        -- ^ function to handle delta bytecontents
    do
    (notificationXml, _) <- fromTry (RrdpE . CantDownloadNotification . U.fmtEx) $ 
                                downloadToLazyBS httpContext rrdpConf (getURL repoUri)     
    notification         <- hoistHere $ parseNotification notificationXml
    nextStep             <- vHoist $ rrdpNextStep repo notification

    case nextStep of
        NothingToDo                         -> pure repo
        UseSnapshot snapshotInfo            -> useSnapshot snapshotInfo notification            
        UseDeltas sortedDeltas snapshotInfo -> 
                useDeltas sortedDeltas notification
                    `catchError` 
                \e -> do         
                    -- NOTE At the moment we ignore the fact that some objects are wrongfully added
                    logErrorM logger [i|Failed to apply deltas for #{repoUri}: #{e}, will fall back to snapshot.|]
                    appWarn e
                    useSnapshot snapshotInfo notification            
    where        
        hoistHere = vHoist . fromEither . first RrdpE
                
        rrdpConf = appContext ^. typed @Config . typed @RrdpConf
        logger   = appContext ^. typed @AppLogger
        ioBottleneck = appContext ^. typed @AppBottleneck . #ioBottleneck

        useSnapshot (SnapshotInfo uri hash) notification = 
            forChild (U.convert uri) $ do       
                logDebugM logger [i|#{uri}: downloading snapshot.|]
                (r, downloadedIn, savedIn) <- downloadAndSave
                logDebugM logger [i|#{uri}: downloaded in #{downloadedIn}ms and saved snapshot in #{savedIn}ms.|]                        
                pure r
            where                     
                downloadAndSave = do
                    ((rawContent, _), downloadedIn) <- timedMS $ 
                            fromTryEither (RrdpE . CantDownloadSnapshot . U.fmtEx) $ 
                                    downloadHashedLazyBS httpContext rrdpConf uri hash                                    
                                        (\actualHash -> Left $ RrdpE $ SnapshotHashMismatch hash actualHash)
                    (_, savedIn) <- timedMS $ handleSnapshotBS repoUri notification rawContent            
                    pure (repo { rrdpMeta = rrdpMeta' }, downloadedIn, savedIn)   

                rrdpMeta' = Just (notification ^. #sessionId, notification ^. #serial)

        useDeltas sortedDeltas notification = do
            let repoURI = getURL $ repo ^. #uri
            logDebugM logger [i|#{repoURI}: downloading deltas from #{minSerial} to #{maxSerial}.|]

            -- Try to deallocate all the bytestrings created by mmaps right after they are used, 
            -- otherwise they will hold too much files open.
            (r, elapsed) <- timedMS $ downloadAndSave `finally` liftIO performGC

            logDebugM logger [i|#{repoURI}: downloaded and saved deltas, took #{elapsed}ms.|]                        
            pure r
            where
                downloadAndSave = do
                    -- TODO Do not thrash the same server with too big amount of parallel 
                    -- requests, it's mostly counter-productive and rude. Maybe 8 is still too much.         
                    localRepoBottleneck <- liftIO $ newBottleneckIO 8            
                    void $ foldPipeline
                                (localRepoBottleneck <> ioBottleneck)
                                (S.each sortedDeltas)
                                downloadDelta
                                (\(rawContent, serial) _ -> handleDeltaBS repoUri notification serial rawContent)
                                (mempty :: ())
            
                    pure $ repo { rrdpMeta = rrdpMeta' }

                downloadDelta (DeltaInfo uri hash serial) = do
                    (rawContent, _) <- fromTryEither (RrdpE . CantDownloadDelta . U.fmtEx) $ 
                                            downloadHashedLazyBS httpContext rrdpConf uri hash
                                                (\actualHash -> Left $ RrdpE $ DeltaHashMismatch hash actualHash serial)
                    pure (rawContent, serial)

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
rrdpNextStep :: RrdpRepository -> Notification -> PureValidatorT env Step
rrdpNextStep (RrdpRepository _ Nothing _) Notification{..} = 
    pure $ UseSnapshot snapshotInfo
rrdpNextStep (RrdpRepository _ (Just (repoSessionId, repoSerial)) _) Notification{..} =
    if  | sessionId /= repoSessionId -> pure $ UseSnapshot snapshotInfo
        | repoSerial > serial        -> do 
            appWarn $ RrdpE $ LocalSerialBiggerThanRemote repoSerial serial
            pure $ UseSnapshot snapshotInfo
        | repoSerial == serial       -> pure NothingToDo
        | otherwise ->
            case (deltas, nonConsecutive) of
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

                nonConsecutive = List.filter (\(s, s') -> nextSerial s /= s') $
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
                                AppContext s ->
                                RrdpRepository ->
                                ValidatorT vc IO RrdpRepository
updateObjectForRrdpRepository appContext@AppContext {..} repository = do        
        stats <- liftIO newRrdpStat
        r <- downloadAndUpdateRRDP 
                appContext 
                httpContext
                repository 
                (saveSnapshot appContext stats)  
                (saveDelta appContext stats)          
        RrdpStat {..} <- liftIO $ completeRrdpStat stats
        let repoURI = getURL $ repository ^. #uri
        logDebugM logger [i|Downloaded #{repoURI}, added #{added} objects, ignored removals of #{removed}.|]
        pure r


{- Snapshot case, done in parallel by two thread
    - one thread parses XML, reads base64s and pushes CPU-intensive parsing tasks into the queue 
    - another thread read parsing tasks, waits for them and saves the results into the DB.
-} 
saveSnapshot :: Storage s => 
                AppContext s 
                -> RrdpStatWork 
                -> RrdpURL
                -> Notification 
                -> LBS.ByteString 
                -> ValidatorT vc IO ()
saveSnapshot appContext rrdpStats repoUri notification snapshotContent = do      
    worldVersion <- liftIO $ getWorldVerion $ appContext ^. typed @Versions
    doSaveObjects worldVersion 
  where
    doSaveObjects worldVersion = do
        -- TODO check that the session_id and serial are the same as in the notification file
        (Snapshot _ sessionId serial snapshotItems) <- vHoist $ 
            fromEither $ first RrdpE $ parseSnapshot snapshotContent

        let notificationSessionId = notification ^. typed @SessionId
        when (sessionId /= notificationSessionId) $ 
            appError $ RrdpE $ SnapshotSessionMismatch sessionId notificationSessionId

        let notificationSerial = notification ^. typed @Serial
        when (serial /= notificationSerial) $ 
            appError $ RrdpE $ SnapshotSerialMismatch serial notificationSerial

        -- split into writing transactions of 10000 elements to make them always finite 
        -- and independent from the size of the snapshot.
        void $ txFoldPipeline 
                    cpuParallelism
                    (S.mapM newStorable $ S.each snapshotItems)
                    (savingTx sessionId serial)
                    saveStorable
                    (mempty :: ())
      where
        savingTx sessionId serial f = 
            rwAppTx objectStore $ \tx -> do
                f tx
                RS.updateRrdpMeta tx repositoryStore (sessionId, serial) repoUri 

        newStorable (SnapshotPublish uri encodedb64) =             
            if supportedExtension $ U.convert uri 
                then do 
                    task <- readBlob `pureTask` bottleneck
                    pure $ Right (uri, task)
                else
                    pure $ Left (RrdpE UnsupportedObjectType, uri)
            where 
                readBlob = case U.parseRpkiURL $ unURI uri of
                    Left e        -> SError $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e
                    Right rpkiURL -> parseAndProcess rpkiURL encodedb64
                                        
        saveStorable _ (Left (e, uri)) _             = forChild (unURI uri) $ appWarn e             

        saveStorable tx (Right (uri, a)) _ = do            
            waitTask a >>= \case                        
                SError (VWarn (VWarning e)) -> do                    
                    logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                    forChild (unURI uri) $ appWarn e 
                SError (VErr e) -> do                    
                    logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                    forChild (unURI uri) $ appError e 
                SObject so -> do 
                    DB.putObject tx objectStore so worldVersion
                    addedOne rrdpStats                    

    logger         = appContext ^. typed @AppLogger           
    cpuParallelism = appContext ^. typed @Config . typed @Parallelism . #cpuParallelism
    bottleneck     = appContext ^. typed @AppBottleneck . #cpuBottleneck
    database        = appContext ^. #database
    objectStore     = database ^. #objectStore
    repositoryStore = database ^. #repositoryStore


{-
    Similar to `saveSnapshot` but takes base64s from ordered list of deltas.

    NOTE: Delta application is more strict; we require complete consistency, 
    i.e. applying delta is considered failed if it tries to withdraw or replace
    a non-existent object, or place an existent one. In all these cases, we
    fall back to downloading the snapshot.
-}
saveDelta :: Storage s => 
            AppContext s 
            -> RrdpStatWork
            -> RrdpURL 
            -> Notification 
            -> Serial             
            -> LBS.ByteString 
            -> ValidatorT conf IO ()
saveDelta appContext rrdpStats repoUri notification currentSerial deltaContent = do        
    worldVersion  <- liftIO $ getWorldVerion $ appContext ^. typed @Versions
    doSaveObjects worldVersion
  where
    doSaveObjects worldVersion = do
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

        let deltaItemS = S.each deltaItems

        -- Propagate exceptions from here, anything that can happen here 
        -- (storage failure, mmap failure) should stop the validation and 
        -- probably stop the whole process.
        txFoldPipeline 
                cpuParallelism
                (S.mapM newStorable deltaItemS)
                (savingTx sessionId serial)
                saveStorable
                (mempty :: ())                        
      where        
        savingTx sessionId serial f = 
            rwAppTx database $ \tx -> do
                f tx
                RS.updateRrdpMeta tx repositoryStore (sessionId, serial) repoUri 

        newStorable (DP (DeltaPublish uri hash encodedb64)) =
            if supportedExtension $ U.convert uri 
                then do 
                    task <- readBlob `pureTask` bottleneck
                    pure $ Right $ maybe (Add uri task) (Replace uri task) hash
                else 
                    pure $ Left (RrdpE UnsupportedObjectType, uri)
            where 
                readBlob = case U.parseRpkiURL $ unURI uri of
                    Left e        -> SError $ VWarn $ VWarning $ RrdpE $ BadURL $ U.convert e
                    Right rpkiURL -> parseAndProcess rpkiURL encodedb64

        newStorable (DW (DeltaWithdraw uri hash)) = 
            pure $ Right $ Delete uri hash
        
        saveStorable _ (Left (e, uri)) _             = forChild (unURI uri) $ appWarn e             

        saveStorable tx (Right op) _ =
            case op of
                Delete uri existingHash -> do
                    -- Ignore withdraws and just use the time-based garbage collection
                    existsLocally <- DB.hashExists tx objectStore existingHash
                    unless existsLocally $
                        appError $ RrdpE $ NoObjectToWithdraw uri existingHash
                        
                Add uri async'             -> addObject tx uri async'                
                Replace uri async' oldHash -> replaceObject tx uri async' oldHash                    
        
        addObject tx uri a =
            waitTask a >>= \case
                SError (VWarn (VWarning e)) -> do                    
                    logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                    forChild (unURI uri) $ appWarn e 
                SError (VErr e) -> do                    
                    logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                    forChild (unURI uri) $ appError e 
                SObject so@(StorableObject ro _) -> liftIO $ do
                    alreadyThere <- DB.hashExists tx objectStore (getHash ro)
                    unless alreadyThere $ do
                        DB.putObject tx objectStore so worldVersion                      
                        addedOne rrdpStats

        replaceObject tx uri a oldHash = do            
            waitTask a >>= \case
                SError (VWarn (VWarning e)) -> do                    
                    logErrorM logger [i|Skipped object #{uri}, error #{e} |]
                    forChild (unURI uri) $ appWarn e 
                SError (VErr e) -> do                    
                    logErrorM logger [i|Couldn't parse object #{uri}, error #{e} |]
                    forChild (unURI uri) $ appError e 
                SObject so@(StorableObject ro _) -> do        
                    oldOneIsAlreadyThere <- DB.hashExists tx objectStore oldHash                           
                    if oldOneIsAlreadyThere 
                        then do 
                            -- Ignore withdraws and just use the time-based garbage collection
                            liftIO $ removedOne rrdpStats
                        else do 
                            logWarnM logger [i|No object #{uri} with hash #{oldHash} to replace.|]
                            forChild (unURI uri) $ 
                                appError (RrdpE $ NoObjectToReplace uri oldHash) 

                    newOneIsAlreadyThere <- DB.hashExists tx objectStore (getHash ro)
                    unless newOneIsAlreadyThere $ do                            
                        DB.putObject tx objectStore so worldVersion
                        addedOne rrdpStats                                                                                                 


    logger          = appContext ^. typed @AppLogger           
    cpuParallelism  = appContext ^. typed @Config . typed @Parallelism . #cpuParallelism
    bottleneck      = appContext ^. typed @AppBottleneck . #cpuBottleneck                      
    database        = appContext ^. #database
    objectStore     = database ^. #objectStore
    repositoryStore = database ^. #repositoryStore


parseAndProcess :: RpkiURL -> EncodedBase64 -> StorableUnit RpkiObject VProblem
parseAndProcess u b64 =     
    case parsed of
        Left e   -> SError $ VErr e
        Right ro -> SObject $! toStorableObject ro                    
    where
        parsed = do
            DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
            first ParseE $ readObject u b    


data DeltaOp m a = Delete !URI !Hash 
                | Add !URI !(Task m a) 
                | Replace !URI !(Task m a) !Hash

data RrdpStat = RrdpStat {
    added   :: !Int,
    removed :: !Int
}

data RrdpStatWork = RrdpStatWork {
    added   :: IORef Int,
    removed :: IORef Int
}

completeRrdpStat :: RrdpStatWork -> IO RrdpStat
completeRrdpStat RrdpStatWork {..} = 
    RrdpStat <$> readIORef added <*> readIORef removed

newRrdpStat :: IO RrdpStatWork
newRrdpStat = RrdpStatWork <$> newIORef 0 <*> newIORef 0

addedOne :: MonadIO m => RrdpStatWork -> m ()
addedOne RrdpStatWork {..} = U.increment added

removedOne :: MonadIO m => RrdpStatWork -> m ()
removedOne RrdpStatWork {..} = U.increment removed
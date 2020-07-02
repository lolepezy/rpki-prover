{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}


module RPKI.RRDP.Update where

import           Control.Exception.Lifted
import           Control.Lens                     ((^.))
import           Control.Monad.Except
import           Data.Generics.Labels
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.Bifunctor                   (first)
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text

import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.AppContext
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.RRDP.Http
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database              as Stores
import           RPKI.Time
import qualified RPKI.Util                        as U

import           Data.IORef.Lifted

import qualified Streaming.Prelude as S
import Data.Kind (Type)
import Control.Monad.Identity (Identity)



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
                (LBS.ByteString   -> ValidatorT vc IO Validations) ->
                (LBS.ByteString -> ValidatorT vc IO Validations) ->
                ValidatorT vc IO (RrdpRepository, Validations)
downloadAndUpdateRRDP 
        appContext
        httpContext
        repo@(RrdpRepository repoUri _ _)      
        handleSnapshotBS                       -- ^ function to handle the snapshot bytecontent
        handleDeltaBS =                        -- ^ function to handle all deltas bytecontents
    do
    (notificationXml, _) <- fromEitherM $ downloadToLazyBS httpContext
                                rrdpConf repoUri (RrdpE . CantDownloadNotification . show)    
    notification         <- hoistHere $ parseNotification notificationXml
    nextStep             <- hoistHere $ rrdpNextStep repo notification

    case nextStep of
        NothingToDo                         -> pure (repo, mempty)
        UseSnapshot snapshotInfo            -> useSnapshot snapshotInfo notification            
        UseDeltas sortedDeltas snapshotInfo -> 
                useDeltas sortedDeltas notification
                    `catchError` 
                \e -> do         
                    -- NOTE At tyhe moment we ignore the fact that some objects are wrongfully added
                    logErrorM logger [i|Failed to apply deltas for #{repoUri}: #{e}, will fall back to snapshot.|]
                    appWarn e
                    useSnapshot snapshotInfo notification            
    where        
        hoistHere = vHoist . fromEither . first RrdpE
                
        rrdpConf = appContext ^. typed @Config . typed
        logger   = appContext ^. typed @AppLogger
        ioBottleneck = appContext ^. typed @AppBottleneck . #ioBottleneck

        useSnapshot (SnapshotInfo uri hash) notification = do       
            logDebugM logger [i|#{uri}: downloading snapshot.|]
            (r, elapsed) <- timedMS downloadAndSave
            logDebugM logger [i|#{uri}: downloaded and saved snapshot, took #{elapsed}ms.|]                        
            pure r
            where                     
                downloadAndSave = do
                    (rawContent, _) <- fromEitherM $ downloadHashedLazyBS httpContext rrdpConf uri hash
                                        (RrdpE . CantDownloadSnapshot . show)                 
                                        (\actualHash -> Left $ RrdpE $ SnapshotHashMismatch hash actualHash)
                    validations <- handleSnapshotBS rawContent            
                    pure (repo { rrdpMeta = rrdpMeta' }, validations)       

                rrdpMeta' = Just (notification ^. #sessionId, notification ^. #serial)

        useDeltas sortedDeltas notification = do
            let repoURI = repo ^. typed @URI
            logDebugM logger [i|#{repoURI}: downloading deltas from #{minSerial} to #{maxSerial}.|]
            (r, elapsed) <- timedMS downloadAndSave
            logDebugM logger [i|#{repoURI}: downloaded and saved deltas, took #{elapsed}ms.|]                        
            pure r
            where
                downloadAndSave = do
                    -- TODO Do not thrash the same server with too big amount of parallel 
                    -- requests, it's mostly counter-productive and rude. Maybe 8 is still too much.         
                    localRepoBottleneck <- liftIO $ newBottleneckIO 8            
                    validations <- foldPipeline
                                        (localRepoBottleneck <> ioBottleneck)
                                        (S.each sortedDeltas)
                                        downloadDelta
                                        (\bs validations -> (validations <>) <$> handleDeltaBS bs)
                                        (mempty :: Validations)

                    pure (repo { rrdpMeta = rrdpMeta' }, validations)                    

                downloadDelta (DeltaInfo uri hash serial) = do
                    (rawContent, _) <- fromEitherM $ downloadHashedLazyBS httpContext rrdpConf uri hash
                                            (RrdpE . CantDownloadDelta . show)
                                            (\actualHash -> Left $ RrdpE $ DeltaHashMismatch hash actualHash serial)
                    pure rawContent

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
rrdpNextStep :: RrdpRepository -> Notification -> Either RrdpError Step
rrdpNextStep (RrdpRepository _ Nothing _) Notification{..} = 
    Right $ UseSnapshot snapshotInfo
rrdpNextStep (RrdpRepository _ (Just (repoSessionId, repoSerial)) _) Notification{..} =
    if  | sessionId /= repoSessionId -> Right $ UseSnapshot snapshotInfo
        | repoSerial > serial  -> Left $ LocalSerialBiggerThanRemote repoSerial serial
        | repoSerial == serial -> Right NothingToDo
        | otherwise ->
            case (deltas, nonConsecutive) of
                ([], _) -> Right $ UseSnapshot snapshotInfo
                (_, []) | nextSerial repoSerial < head (map deltaSerial sortedDeltas) ->
                            -- we are too far behind
                            Right $ UseSnapshot snapshotInfo
                        | otherwise ->
                            Right $ UseDeltas chosenDeltas snapshotInfo
                (_, nc) -> Left $ NonConsecutiveDeltaSerials nc
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
updateObjectForRrdpRepository :: Storage s => 
                                AppContext s ->
                                RrdpRepository ->
                                ValidatorT vc IO (RrdpRepository, Validations)
updateObjectForRrdpRepository appContext@AppContext{..} repository =
    withHttp $ \httpContext -> do        
        stats <- liftIO newRrdpStat
        (r, v) <- downloadAndUpdateRRDP 
                appContext 
                httpContext
                repository 
                (saveSnapshot appContext stats)  
                (saveDelta appContext stats)          
        RrdpStat {..} <- liftIO $ completeRrdpStat stats
        logDebugM logger [i|Downloaded #{unURI $ repository ^. typed}, added #{added} objects, removed #{removed}.|]
        pure (r, v)


{- Snapshot case, done in parallel by two thread
    - one thread parses XML, reads base64s and pushes CPU-intensive parsing tasks into the queue 
    - another thread read parsing tasks, waits for them and saves the results into the DB.
-} 
saveSnapshot :: Storage s => 
                AppContext s -> 
                RrdpStatWork ->
                LBS.ByteString -> 
                ValidatorT vc IO Validations
saveSnapshot appContext rrdpStats snapshotContent = do           
    (Snapshot _ _ _ snapshotItems) <- vHoist $ 
        fromEither $ first RrdpE $ parseSnapshot snapshotContent

    -- split into writing transactions of 10000 elements to make them always finite 
    -- and independent from the size of the snapshot.
    fromTry 
        (StorageE . StorageError . U.fmtEx)    
        (txFoldPipelineChunked 
            cpuParallelism
            (S.mapM newStorable $ S.each snapshotItems)
            (rwTx objectStore)
            10000            
            saveStorable   
            (mempty :: Validations))        
    where
        newStorable (SnapshotPublish uri encodedb64) = do
            task <- (parseAndProcess uri encodedb64) `pureTask` bottleneck
            pure (uri, task)
                                
        saveStorable tx (uri, a) validations = 
            waitTask a >>= \case                        
                SError e   -> do
                    logError_ logger [i|Couldn't parse object #{uri}, error #{e} |]
                    pure $ validations <> mError (vContext uri) e
                SObject so -> do 
                    Stores.putObject tx objectStore so
                    addedOne rrdpStats
                    pure validations

        logger         = appContext ^. typed @AppLogger           
        cpuParallelism = appContext ^. typed @Config . typed @Parallelism . #cpuParallelism
        bottleneck     = appContext ^. typed @AppBottleneck . #cpuBottleneck
        objectStore    = appContext ^. #database . #objectStore


{-
    The same as snapshots but takes base64s from ordered 
    list of deltas.
-}
saveDelta :: Storage s => 
            AppContext s -> 
            RrdpStatWork ->
            LBS.ByteString -> 
            ValidatorT conf IO Validations
saveDelta appContext rrdpStats deltaContent = do        
    delta <- vHoist $ fromEither $ first RrdpE $ parseDelta deltaContent    

    let deltaItemS = S.each $ delta ^. typed @[DeltaItem]
    fromTry (StorageE . StorageError . U.fmtEx) 
        (txFoldPipeline 
            cpuParallelism
            (S.mapM newStorable deltaItemS)
            (rwTx objectStore) 
            saveStorable 
            (mempty :: Validations))    
    where        
        newStorable (DP (DeltaPublish uri hash encodedb64)) = do 
            task <- (parseAndProcess uri encodedb64) `pureTask` bottleneck
            pure $ maybe (Add uri task) (Replace uri task) hash

        newStorable (DW (DeltaWithdraw _ hash)) = 
            pure $ Delete hash

        saveStorable tx op validations =
            case op of
                Delete hash                -> do
                    Stores.deleteObject tx objectStore hash
                    pure validations
                Add uri async'             -> addObject tx uri async' validations
                Replace uri async' oldHash -> replaceObject tx uri async' oldHash validations                    
        
        addObject tx uri a validations =
            waitTask a >>= \case
                SError e -> do
                    logError_ logger [i|Couldn't parse object #{uri}, error #{e} |]
                    pure $ validations <> mError (vContext uri) e
                SObject so@(StorableObject ro _) -> do
                    alreadyThere <- Stores.hashExists tx objectStore (getHash ro)
                    unless alreadyThere $ do                                    
                        Stores.putObject tx objectStore so                        
                        addedOne rrdpStats
                    pure validations

        replaceObject tx uri a oldHash validations = 
            waitTask a >>= \case
                SError e -> do
                    logError_ logger [i|Couldn't parse object #{uri}, error #{e} |]
                    pure $ validations <> mError (vContext uri) e
                SObject so@(StorableObject ro _) -> do        
                    oldOneIsAlreadyThere <- Stores.hashExists tx objectStore oldHash                           
                    if oldOneIsAlreadyThere 
                        then do 
                            Stores.deleteObject tx objectStore oldHash
                            removedOne rrdpStats
                            let newHash = getHash ro
                            newOneIsAlreadyThere <- Stores.hashExists tx objectStore newHash
                            if newOneIsAlreadyThere
                                then do
                                    logWarn_ logger [i|There's an existing object with hash: #{newHash}|]
                                    pure $ validations <> mWarning (vContext uri) 
                                        (VWarning $ RrdpE $ ObjectExistsWhenReplacing uri oldHash)
                                else do                                             
                                    Stores.putObject tx objectStore so
                                    addedOne rrdpStats
                                    pure validations                                            
                        else do 
                            logWarn_ logger [i|No object #{uri} with hash #{oldHash} to replace.|]
                            pure $ validations <> mWarning (vContext uri) 
                                (VWarning $ RrdpE $ NoObjectToReplace uri oldHash) 

        logger         = appContext ^. typed @AppLogger           
        cpuParallelism = appContext ^. typed @Config . typed @Parallelism . #cpuParallelism
        bottleneck     = appContext ^. typed @AppBottleneck . #cpuBottleneck                      
        objectStore    = appContext ^. #database . #objectStore


parseAndProcess :: URI -> EncodedBase64 -> StorableUnit RpkiObject AppError
parseAndProcess (URI u) b64 =     
    case parsed of
        Left e   -> SError e
        Right ro -> SObject $! toStorableObject ro                    
    where
        parsed = do
            DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
            first ParseE $ readObject (Text.unpack u) b    


data DeltaOp m a = Delete !Hash 
                | Add !URI !(Task m a) 
                | Replace !URI !(Task m a) !Hash


data RrdpStat = RrdpStat {
    added :: Int,
    removed ::  Int
}

data RrdpStatWork = RrdpStatWork {
    added :: IORef Int,
    removed :: IORef Int
}

completeRrdpStat :: RrdpStatWork -> IO RrdpStat
completeRrdpStat RrdpStatWork {..} = 
    RrdpStat <$> readIORef added <*> readIORef removed

newRrdpStat :: IO RrdpStatWork
newRrdpStat = RrdpStatWork <$> newIORef 0 <*> newIORef 0

addedOne :: RrdpStatWork -> IO ()
addedOne RrdpStatWork {..} = increment added

removedOne :: RrdpStatWork -> IO ()
removedOne RrdpStatWork {..} = increment removed

increment :: (MonadIO m, Num a) => IORef a -> m ()
increment counter = liftIO $ atomicModifyIORef' counter $ \c -> (c + 1, ())
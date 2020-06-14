{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.RRDP.Update where

import           Control.Lens                     ((^.))
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed
import           Control.Monad.Except

import           Data.Bifunctor                   (first)
import qualified Data.List                        as List
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text

import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Execution
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.RRDP.Http
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database              as Stores
import           RPKI.Time
import qualified RPKI.Util                        as U

import           Data.IORef.Lifted


-- | 
--  Update RRDP repository, i.e. do the full cycle
--    - download notifications file, parse it
--    - decide what to do next based on it
--    - download snapshot or deltas
--    - do something appropriate with either of them
-- 
downloadAndUpdateRRDP :: WithVContext vc => 
                AppContext s ->
                RrdpRepository ->                 
                (Snapshot -> ValidatorT vc IO Validations) ->
                ([Delta]  -> ValidatorT vc IO Validations) ->
                ValidatorT vc IO (RrdpRepository, Validations)
downloadAndUpdateRRDP 
        AppContext{..} 
        repo@(RrdpRepository repoUri _ _)    
        handleSnapshot                       -- ^ function to handle the snapshot 
        handleDeltas =                       -- ^ function to handle all deltas
    do
    (notificationXml, _) <- fromEitherM $ downloadToLazyBS 
                                rrdpConf repoUri (RrdpE . CantDownloadNotification . show)    
    notification         <- hoistHere $ parseNotification notificationXml
    nextStep             <- hoistHere $ rrdpNextStep repo notification

    case nextStep of
        NothingToDo                         -> pure (repo, mempty)
        UseSnapshot snapshotInfo            -> useSnapshot snapshotInfo
        UseDeltas sortedDeltas snapshotInfo -> 
                useDeltas sortedDeltas notification
                    `catchError` 
                \e -> appWarn e >> useSnapshot snapshotInfo
    where
        hoistHere = vHoist . fromEither . first RrdpE
                
        Config {..} = config

        useSnapshot (SnapshotInfo uri hash) = do 
            logDebugM logger [i|Downloading snapshot: #{unURI uri} |]        
            (rawContent, _) <- fromEitherM $ downloadHashedLazyBS rrdpConf uri hash
                                (RrdpE . CantDownloadSnapshot . show)                 
                                (\actualHash -> Left $ RrdpE $ SnapshotHashMismatch hash actualHash)
            snapshot    <- hoistHere $ parseSnapshot rawContent
            validations <- handleSnapshot snapshot
            pure (repoFromSnapshot snapshot, validations)            

        useDeltas sortedDeltas notification = do
            -- TODO Do not thrash the same server with too big amount of parallel 
            -- requests, it's mostly counter-productive and rude
            
            deltas       <- parallelTasksN 16 sortedDeltas downloadDelta            

            -- TODO Optimise it, don't creat all the deltas at once, 
            -- do parsing inside of the             
            parsedDeltas <- forM deltas (hoistHere . parseDelta)

            validations  <- handleDeltas parsedDeltas
            pure (repoFromDeltas parsedDeltas notification, validations)
            where
                downloadDelta (DeltaInfo uri hash serial) = do
                    fst <$> (fromEitherM $ downloadHashedLazyBS rrdpConf uri hash
                                        (RrdpE . CantDownloadDelta . show)                         
                                        (\actualHash -> Left $ RrdpE $ DeltaHashMismatch hash actualHash serial))

        repoFromSnapshot :: Snapshot -> RrdpRepository
        repoFromSnapshot (Snapshot _ sid s _) = repo { rrdpMeta = Just (sid, s) }

        repoFromDeltas :: [Delta] -> Notification -> RrdpRepository
        repoFromDeltas ds notification = repo { rrdpMeta = Just (newSessionId, newSerial) }
            where
                newSessionId = sessionId notification
                newSerial = List.maximum $ map (\(Delta _ _ s _) -> s) ds        


data Step = UseSnapshot SnapshotInfo
        | UseDeltas { 
            sortedDeltas :: [DeltaInfo], 
            snapshotInfo :: SnapshotInfo 
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
                                Stores.RpkiObjectStore s ->
                                ValidatorT vc IO (RrdpRepository, Validations)
updateObjectForRrdpRepository appContext@AppContext{..} repository@RrdpRepository { uri = uri' } objectStore = do
    added   <- newIORef (0 :: Int)
    removed <- newIORef (0 :: Int)        
    repo <- updateRepo' (cpuBottleneck appThreads) added removed
    a <- readIORef added        
    r <- readIORef removed
    logInfoM logger [i|Added #{a} objects, removed #{r} for repository #{unURI uri'}|]
    pure repo
    where
        updateRepo' bottleneck added removed =     
            downloadAndUpdateRRDP appContext repository saveSnapshot saveDeltas
            where
                cpuParallelism = config ^. typed @Parallelism . field @"cpuParallelism"

                {- Snapshot case, done in parallel by two thread
                    - one thread to parse XML, reads base64s and pushes 
                      CPU-intensive parsing tasks into the queue
                    - one thread to save objects, read asyncs, waits for them
                      and save the results into the DB.
                -} 
                saveSnapshot (Snapshot _ _ _ snapshotItems) = do                                        
                    (r, elapsed) <- timedMS $ fromTry 
                        (StorageE . StorageError . U.fmtEx)                
                        (txConsumeFold 
                            cpuParallelism
                            snapshotItems 
                            storableToChan 
                            (rwTx objectStore) 
                            chanToStorage   
                            (mempty :: Validations))                        
                    logDebugM logger [i|Saved snapshot for the repository: #{repository}, took #{elapsed}ms |]
                    pure r
                    where
                        storableToChan (SnapshotPublish uri encodedb64) = do
                            -- logDebugM logger [i|rrdp uri = #{uri}|]               
                            task <- (pure $! parseAndProcess uri encodedb64) `strictTask` bottleneck
                            -- a <- AsyncL.async $ pure $! parseAndProcess uri encodedb64
                            pure (uri, task)
                                                
                        chanToStorage tx (uri, a) validations = 
                            waitTask a >>= \case                        
                                SError e   -> do
                                    logError_ logger [i|Couldn't parse object #{uri}, error #{e} |]
                                    pure $ validations <> mError (vContext uri) e
                                SObject so -> do 
                                    Stores.putObject tx objectStore so
                                    increment added
                                    pure validations

                {-
                    The same as snapshots but takes base64s from ordered 
                    list of deltas.
                -}
                saveDeltas :: [Delta] -> ValidatorT conf IO Validations
                saveDeltas deltas = do
                    let serials = map (^. typed @Serial) deltas
                    logDebugM logger [i|Saving deltas #{serials} for the repository: #{repository} |]
                    fromTry (StorageE . StorageError . U.fmtEx) 
                        (txConsumeFold 
                            cpuParallelism
                            deltaItems 
                            storableToChan 
                            (rwTx objectStore) 
                            chanToStorage 
                            (mempty :: Validations))
                    where
                        deltaItems :: [DeltaItem] = concatMap (\(Delta _ _ _ dis) -> dis) deltas                        

                        storableToChan (DP (DeltaPublish uri hash encodedb64)) = do 
                            task <- (pure $! parseAndProcess uri encodedb64) `strictTask` bottleneck
                            pure $ maybe (Add uri task) (Replace uri task) hash

                        storableToChan (DW (DeltaWithdraw _ hash)) = 
                            pure $ Delete hash

                        chanToStorage tx op validations =
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
                                        increment added
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
                                            increment removed
                                            let newHash = getHash ro
                                            newOneIsAlreadyThere <- Stores.hashExists tx objectStore newHash
                                            if newOneIsAlreadyThere
                                                then do
                                                    logWarn_ logger [i|There's an existing object with hash: #{newHash}|]
                                                    pure $ validations <> mWarning (vContext uri) 
                                                        (VWarning $ RrdpE $ ObjectExistsWhenReplacing uri oldHash)
                                                else do                                             
                                                    Stores.putObject tx objectStore so
                                                    increment added
                                                    pure validations                                            
                                        else do 
                                            logWarn_ logger [i|No object with hash #{oldHash} nothing to replace|]
                                            pure $ validations <> mWarning (vContext uri) 
                                                (VWarning $ RrdpE $ NoObjectToReplace uri oldHash)                                            
                                                                                    
        parseAndProcess (URI u) b64 =     
            case parsed of
                Left e   -> SError e
                Right ro -> SObject $! toStorableObject ro                    
            where
                parsed = do
                    DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
                    first ParseE $ readObject (Text.unpack u) b                

        increment added = void $ atomicModifyIORef' added $ \c -> (c + 1, ())


data DeltaOp m a = Delete !Hash 
                | Add !URI !(Task m a) 
                | Replace !URI !(Task m a) !Hash


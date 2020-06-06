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

import           Data.Bifunctor                   (first, second)
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.List                        as List
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import qualified Network.Wreq                     as WR

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
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database              as Stores
import           RPKI.Time
import qualified RPKI.Util                        as U

import qualified Data.ByteString.Streaming        as Q
import           Data.ByteString.Streaming.HTTP

import qualified Crypto.Hash.SHA256               as S256

import           System.IO                        (Handle, hClose)
import           System.IO.Posix.MMap.Lazy        (unsafeMMapFile)
import           System.IO.Temp                   (withTempFile)

import           Control.Exception.Lifted
import           Data.IORef.Lifted



{- 
    TODO 
    1) Replace IO with some reasonable Monad (MonadIO + MonadMask/MonadUnliftIO/MonadBaseControl).
    2) Maybe return bracketed IO actions instead of exectuting them.
-}
updateRrdpRepo :: WithVContext vc => 
                AppContext s ->
                RrdpRepository ->                 
                (Snapshot -> ValidatorT vc IO Validations) ->
                ([Delta]  -> ValidatorT vc IO Validations) ->
                ValidatorT vc IO (RrdpRepository, Validations)
updateRrdpRepo AppContext{..} repo@(RrdpRepository repoUri _ _) handleSnapshot handleDeltas = do

    notificationXml <- fromEitherM $ fetch repoUri (RrdpE . CantDownloadNotification . show)    
    notification    <- hoistHere $ parseNotification notificationXml
    nextStep        <- hoistHere $ rrdpNextStep repo notification

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
            rawContent <- fromEitherM $ downloadHashedContent rrdpConf uri hash
                                (RrdpE . CantDownloadSnapshot . show)                 
                                (\actualHash -> Left $ RrdpE $ SnapshotHashMismatch hash actualHash)
            snapshot    <- hoistHere $ parseSnapshot rawContent
            validations <- handleSnapshot snapshot
            pure (repoFromSnapshot snapshot, validations)            

        useDeltas sortedDeltas notification = do            
            rawContents <- parallelTasks (ioThreads appThreads) sortedDeltas downloadDelta
            deltas      <- forM rawContents $ \case
                                Left e                -> appError e
                                Right (_, rawContent) -> hoistHere $ parseDelta rawContent
            validations <- handleDeltas deltas
            pure (repoFromDeltas deltas notification, validations)
            where
                downloadDelta di@(DeltaInfo uri hash serial) = do
                    rawContent <- downloadHashedContent rrdpConf uri hash
                        (RrdpE . CantDownloadDelta . show)                         
                        (\actualHash -> Left $ RrdpE $ DeltaHashMismatch hash actualHash serial)                    
                    pure $! (di,) <$> rawContent

        repoFromSnapshot :: Snapshot -> RrdpRepository
        repoFromSnapshot (Snapshot _ sid s _) = repo { rrdpMeta = Just (sid, s) }

        repoFromDeltas :: [Delta] -> Notification -> RrdpRepository
        repoFromDeltas ds notification = repo { rrdpMeta = Just (newSessionId, newSerial) }
            where
                newSessionId = sessionId notification
                newSerial = List.maximum $ map (\(Delta _ _ s _) -> s) ds        


-- | Download HTTP content to a temporary file, compare the hash and return 
-- lazy ByteString content mapped onto a temporary file. Snapshots (and probably 
-- some deltas) can be big enough so that we don't want to keep them in memory.
--
-- NOTE: The file will be deleted from the temporary directory by withSystemTempFile, 
-- but the descriptor taken by mmap will stay until the byte string it GC-ed, so it's 
-- safe to use them after returning from this function.
downloadHashedContent :: (MonadIO m) => 
                        RrdpConf ->
                        URI -> 
                        Hash -> 
                        (SomeException -> e) ->
                        (Hash -> Either e LBS.ByteString) ->
                        m (Either e LBS.ByteString)
downloadHashedContent RrdpConf {..} uri@(URI u) hash cantDownload hashMishmatch = liftIO $ do
    -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
    -- to minimize the heap. Snapshots can be pretty big, so we don't want 
    -- a spike in heap usage.
    let tmpFileName = U.convert $ U.normalizeUri u
    withTempFile tmpRoot tmpFileName $ \name fd -> 
        streamHttpToFileAndHash uri cantDownload fd >>= \case        
            Left e -> pure (Left e)
            Right actualHash 
                | actualHash /= hash -> 
                    pure $! hashMishmatch actualHash
                | otherwise -> do
                    hClose fd
                    content <- unsafeMMapFile name
                    pure (Right content)


-- | Download HTTP stream into a file while calculating its hash at the same time
streamHttpToFileAndHash :: MonadIO m =>
                            URI -> 
                            (SomeException -> err) -> 
                            Handle -> 
                            m (Either err Hash)
streamHttpToFileAndHash (URI uri) errorMapping destinationHandle = 
    liftIO $ first errorMapping <$> try go    
    where
        go = do
            req  <- parseRequest $ Text.unpack uri
            tls  <- newManager tlsManagerSettings 
            hash <- newIORef S256.init
            withHTTP req tls $ \resp -> 
                Q.hPut destinationHandle $ 
                    Q.chunkMapM (\chunk -> 
                        modifyIORef' hash (`S256.update` chunk) >> pure chunk) $ 
                    responseBody resp
            h' <- readIORef hash        
            pure $! U.mkHash $ S256.finalize h'


-- | Download HTTP stream into memory bytestring
fetch :: MonadIO m => URI -> (SomeException -> e) -> m (Either e LBS.ByteString)
fetch (URI uri) err = liftIO $ do
    r <- try (WR.get $ Text.unpack uri)
    pure $! first err $ second (^. WR.responseBody) r            


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


updateObjectForRrdpRepository :: Storage s => 
                                AppContext s ->
                                RrdpRepository ->
                                Stores.RpkiObjectStore s ->
                                ValidatorT vc IO (RrdpRepository, Validations)
updateObjectForRrdpRepository appContext@AppContext{..} repository@RrdpRepository { uri = uri' } objectStore = do
    added   <- newIORef (0 :: Int)
    removed <- newIORef (0 :: Int)        
    (repo, elapsed) <- timedMS $ updateRepo' (cpuThreads appThreads) added removed
    a <- readIORef added        
    r <- readIORef removed
    logInfoM logger [i|Added #{a} objects, removed #{r} for repository #{unURI uri'}, took #{elapsed}ms|]
    pure repo
    where
        updateRepo' threads added removed =     
            updateRrdpRepo appContext repository saveSnapshot saveDelta
            where
                cpuParallelism = config ^. typed @Parallelism . field @"cpuParallelism"

                saveSnapshot (Snapshot _ _ _ snapshotItems) = do                    
                    logDebugM logger [i|Saving snapshot for the repository: #{repository} |]
                    fromTry 
                        (StorageE . StorageError . U.fmtEx)                
                        (txConsumeFold 
                            cpuParallelism
                            snapshotItems 
                            storableToChan 
                            (rwTx objectStore) 
                            chanToStorage   
                            (mempty :: Validations))                        
                    where
                        storableToChan (SnapshotPublish uri encodedb64) = do
                            -- logDebugM logger [i|rrdp uri = #{uri}|]               
                            task <- (pure $! parseAndProcess uri encodedb64) `submitStrict` threads
                            -- a <- AsyncL.async $ pure $! parseAndProcess uri encodedb64
                            pure (uri, task)
                                                
                        chanToStorage tx (uri, a) validations = 
                            waitTask a >>= \case                        
                                SError e   -> do
                                    logError_ logger [i|Couldn't parse object #{uri}, error #{e} |]
                                    pure $ validations <> mError (vContext uri) e
                                SObject so -> do 
                                    Stores.putObject tx objectStore so
                                    void $ atomicModifyIORef' added $ \c -> (c + 1, ())
                                    pure validations

                saveDelta :: [Delta] -> ValidatorT conf IO Validations
                saveDelta deltas =            
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

                        -- storableToChan :: DeltaItem -> IO (DeltaOp (StorableUnit RpkiObject AppError))
                        storableToChan (DP (DeltaPublish uri hash encodedb64)) = do 
                            -- a <- AsyncL.async $ pure $! parseAndProcess uri encodedb64
                            task <- (pure $! parseAndProcess uri encodedb64) `submitStrict` threads
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
                                        void $ atomicModifyIORef' added $ \c -> (c + 1, ())                                    
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
                                            void $ atomicModifyIORef' removed $ \c -> (c + 1, ())
                                            let newHash = getHash ro
                                            newOneIsAlreadyThere <- Stores.hashExists tx objectStore newHash
                                            if newOneIsAlreadyThere
                                                then do
                                                    logWarn_ logger [i|There's an existing object with hash: #{newHash}|]
                                                    pure $ validations <> mWarning (vContext uri) 
                                                        (VWarning $ RrdpE $ ObjectExistsWhenReplacing uri oldHash)
                                                else do                                             
                                                    Stores.putObject tx objectStore so
                                                    void $ atomicModifyIORef' added $ \c -> (c + 1, ())
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
                    

data DeltaOp m a = Delete !Hash 
                | Add !URI !(Task m a) 
                | Replace !URI !(Task m a) !Hash


{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module RPKI.RRDP.Update where

import           Control.Exception
import           Control.Lens                   ((^.))
import           Control.Monad
import           Control.Monad.Reader

import           Data.Bifunctor                 (first, second)
import qualified Data.ByteString.Lazy           as BL
import           Data.IORef
import           Data.String.Interpolate
import qualified Data.List                      as L
import qualified Data.Text                      as T
import qualified Network.Wreq                   as WR

import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Storable
import           RPKI.Store.Stores
import qualified RPKI.Util                      as U
import           RPKI.Parallel

import qualified Data.ByteString.Streaming      as Q
import           Data.ByteString.Streaming.HTTP


import qualified Crypto.Hash.SHA256             as S256

import           System.IO                      (Handle, hClose)
import           System.IO.Posix.MMap.Lazy      (unsafeMMapFile)
import           System.IO.Temp                 (withSystemTempFile)

import qualified UnliftIO.Async                 as Unlift


{- 
    TODO 
    1) Replace IO with some reasonable Monad (MonadIO + MonadMask/MonadUnliftIO/MonadBaseControl).
    2) Maybe return bracketed IO actions instead of exectuting them.
-}
updateRrdpRepo :: AppContext ->
                RrdpRepository ->                 
                (Snapshot -> IO (Maybe AppError)) ->
                (Delta -> IO (Maybe AppError)) ->
                IO (Either AppError (RrdpRepository, Maybe AppError))
updateRrdpRepo AppContext{..} repo@(RrdpRepository repoUri _) handleSnapshot handleDelta = do
    notificationXml <- download repoUri (RrdpE . CantDownloadNotification . show)
    bindRight (notificationXml >>= (first RrdpE . parseNotification)) $ \notification -> 
        bindRight (first RrdpE $ rrdpNextStep repo notification) $ \case
            NothingToDo            -> pure $ Right (repo, Nothing)
            UseSnapshot snapshot   -> fmap (, Nothing) <$> useSnapshot snapshot                            
            UseDeltas sortedDeltas -> useDeltas sortedDeltas notification
    where
        bindRight e f = either (pure . Left) f e 
        
        useSnapshot (SnapshotInfo uri hash) =            
            downloadAndParse uri hash
                (RrdpE . CantDownloadSnapshot . show)                 
                (\realHash -> Left $ RrdpE $ SnapshotHashMismatch hash realHash)
                (\content -> do
                    let snapshot = first RrdpE $ parseSnapshot content
                    bindRight snapshot $ \s ->
                        maybe (Right $ repoFromSnapshot s) Left <$> handleSnapshot s)


        -- TODO Rollback to snapshot processing in case of an error
        useDeltas sortedDeltas notification = do
            deltas <- parallel (parallelism config) processDelta sortedDeltas            
            foldM foldDeltas ([], Nothing) deltas >>= \case 
                (ds, Nothing) -> pure $ Right (repoFromDeltas ds notification, Nothing)
                ([], Just e)  -> pure $ Left e
                (ds, Just e)  -> pure $ Right (repoFromDeltas ds notification, Just e)
            where
                foldDeltas (valids, Just e)   _         = pure (valids, Just e)
                foldDeltas (valids, Nothing) (Left e')  = pure (valids, Just e')
                foldDeltas (valids, Nothing) (Right d) =
                    handleDelta d >>= \case                 
                        Nothing -> pure (d : valids, Nothing)
                        Just e  -> pure (valids, Just e)

                processDelta (DeltaInfo uri hash serial) = 
                    downloadAndParse uri hash
                        (RrdpE . CantDownloadDelta . show)                         
                        (\realHash -> Left $ RrdpE $ DeltaHashMismatch hash realHash serial)
                        (\content -> pure $ first RrdpE $ parseDelta content)

        useDeltas' :: [DeltaInfo]
                    -> Notification
                    -> IO (Either AppError (RrdpRepository, Maybe AppError))
        useDeltas' sortedDeltas notification = do
            deltas <- parallel (parallelism config) processDelta sortedDeltas            
            foldM foldDeltas ([], Nothing) deltas >>= \case 
                (ds, Nothing) -> pure $ Right (repoFromDeltas ds notification, Nothing)
                ([], Just e)  -> pure $ Left e
                (ds, Just e)  -> pure $ Right (repoFromDeltas ds notification, Just e)
            where
                foldDeltas (valids, Just e)   _         = pure (valids, Just e)
                foldDeltas (valids, Nothing) (Left e')  = pure (valids, Just e')
                foldDeltas (valids, Nothing) (Right d) =
                    handleDelta d >>= \case                 
                        Nothing -> pure (d : valids, Nothing)
                        Just e  -> pure (valids, Just e)

                processDelta (DeltaInfo uri hash serial) = 
                    downloadAndParse uri hash
                        (RrdpE . CantDownloadDelta . show)                         
                        (\realHash -> Left $ RrdpE $ DeltaHashMismatch hash realHash serial)
                        (\content -> pure $ first RrdpE $ parseDelta content)

        downloadAndParse :: URI
                    -> Hash        
                    -> (SomeException -> AppError)
                    -> (Hash -> Either AppError a)
                    -> (BL.ByteString -> IO (Either AppError a))
                    -> IO (Either AppError a)
        downloadAndParse uri@(URI u) hash cantDownload hashMishmatch consumeContent = do
            -- Download xml file to a temporary file and MMAP it to a lazy bytestring 
            -- to minimize the heap. Snapshots can be pretty big, so we don't want 
            -- a spike in heap usage.
            let tmpFileName = U.convert $ U.normalizeUri u
            logInfo_ logger [i|tmpFileName = #{tmpFileName}.|]
            withSystemTempFile tmpFileName $ \name fd -> do                    
                realHash <- streamHttpToFile uri cantDownload fd
                bindRight realHash $ \realHash' ->
                    if realHash' /= hash                                
                        then pure $ hashMishmatch realHash'
                        else do
                            hClose fd
                            content <- unsafeMMapFile name
                            consumeContent content

        repoFromSnapshot :: Snapshot -> RrdpRepository
        repoFromSnapshot (Snapshot _ sid s _) = RrdpRepository repoUri $ Just (sid, s)

        repoFromDeltas :: [Delta] -> Notification -> RrdpRepository
        repoFromDeltas ds notification = RrdpRepository repoUri $ Just (newSessionId, newSerial)
            where
                newSessionId = sessionId notification
                newSerial = L.maximum $ map (\(Delta _ _ s _) -> s) ds        

-- | Download HTTP stream into memory bytestring
download :: MonadIO m => URI -> (SomeException -> e) -> m (Either e BL.ByteString)
download (URI uri) err = liftIO $ do
    r <- try (WR.get $ T.unpack uri)
    pure $ first err $ second (^. WR.responseBody) r


-- | Download HTTP stream into a file while calculating its hash at the same time
streamHttpToFile :: MonadIO m =>
                    URI -> 
                    (SomeException -> err) -> 
                    Handle -> 
                    m (Either err Hash)
streamHttpToFile (URI uri) err destinationHandle = 
    liftIO $ first err <$> try go    
    where
        go = do
            req  <- parseRequest $ T.unpack uri
            tls  <- newManager tlsManagerSettings 
            hash <- newIORef S256.init
            withHTTP req tls $ \resp -> 
                Q.hPut destinationHandle $ 
                    Q.chunkMapM (\chunk -> 
                        modifyIORef' hash (`S256.update` chunk) >> pure chunk) $ 
                    responseBody resp
            h' <- readIORef hash        
            pure $ Hash $ S256.finalize h'


data Step = UseSnapshot SnapshotInfo
        | UseDeltas { sortedDeltas :: [DeltaInfo] }
        | NothingToDo
    deriving (Show, Eq, Ord, Generic)

-- | Decides what to do next based on current state of the repository
-- | and the parsed notification file
rrdpNextStep :: RrdpRepository -> Notification -> Either RrdpError Step
rrdpNextStep (RrdpRepository _ Nothing) Notification{..} = 
    Right $ UseSnapshot snapshotInfo
rrdpNextStep (RrdpRepository _ (Just (repoSessionId, repoSerial))) Notification{..} =
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
                            Right $ UseDeltas chosenDeltas
                (_, nc) -> Left $ NonConsecutiveDeltaSerials nc
            where
                sortedSerials = map deltaSerial sortedDeltas
                sortedDeltas = L.sortOn deltaSerial deltas
                chosenDeltas = filter ((> repoSerial) . deltaSerial) sortedDeltas

                nonConsecutive = L.filter (\(s, s') -> nextSerial s /= s') $
                    L.zip sortedSerials (tail sortedSerials)


deltaSerial :: DeltaInfo -> Serial
deltaSerial (DeltaInfo _ _ s) = s

nextSerial :: Serial -> Serial
nextSerial (Serial s) = Serial $ s + 1

-- TODO Add warnings and errors to the specific VContext
updateObjectForRrdpRepository :: Storage s => 
                AppContext ->
                RrdpRepository ->
                RpkiObjectStore s ->
                ValidatorT conf IO (RrdpRepository, Maybe AppError)
updateObjectForRrdpRepository appContext@AppContext{..} repository objectStore = 
    fromIOEither $ updateRrdpRepo appContext repository saveSnapshot saveDelta
    where
        rwTx_ = rwTx objectStore        

        saveSnapshot (Snapshot _ _ _ snapshotItems) = do
            logInfo_ logger [i|Using snapshot for the repository: #{repository} |]
            either Just (const Nothing) . 
                first (StorageE . StorageError . U.fmtEx) <$> 
                    try (txFunnel (parallelism config) snapshotItems 
                        storableToChan rwTx_ chanToStorage)
            where
                storableToChan (SnapshotPublish u encodedb64) = do
                    a <- Unlift.async $ pure $! asStorable u encodedb64
                    pure (u, a)
                
                chanToStorage tx (u, a) = Unlift.wait a >>= \case                        
                    SError e   -> logError_ logger [i|Couldn't parse object #{u}, error #{e} |]
                    SObject so -> putObject tx objectStore so
            
        saveDelta (Delta _ _ _ deltaItems) = 
            either Just (const Nothing) . 
                first (StorageE . StorageError . U.fmtEx) <$> 
                    try (txFunnel (parallelism config) deltaItems 
                        storableToChan rwTx_ chanToStorage)
            where
                storableToChan (DP (DeltaPublish u h encodedb64)) = do 
                    a <- Unlift.async $ pure $! asStorable u encodedb64
                    pure $ Right (u, h, a)

                storableToChan (DW (DeltaWithdraw u h)) = pure $ Left (u, h)       

                chanToStorage tx = \case
                    Left (_, h)           -> deleteObject tx objectStore h
                    Right (u, Nothing, a) -> 
                        Unlift.wait a >>= \case
                            SError e -> logError_ logger [i|Couldn't parse object #{u}, error #{e} |]
                            SObject so@(StorableObject ro _) -> do
                                let h = getHash ro
                                getByHash tx objectStore h >>= \case
                                    Nothing -> putObject tx objectStore so
                                    (Just _ :: Maybe RpkiObject) ->
                                        -- TODO Add location
                                        logWarn_ logger [i|There's an existing object with hash #{h} |]
                    Right (u, Just oldHash, a) -> 
                        Unlift.wait a >>= \case
                            SError e -> logError_ logger [i|Couldn't parse object #{u}, error #{e} |]
                            SObject so@(StorableObject ro _) -> do                                    
                                getByHash tx objectStore oldHash >>= \case 
                                    Nothing -> 
                                        logWarn_ logger [i|No object with hash #{oldHash} nothing to replace|]
                                    (Just _ :: Maybe RpkiObject) -> do 
                                        deleteObject tx objectStore oldHash
                                        let newHash = getHash ro
                                        getByHash tx objectStore newHash >>= \case 
                                            Nothing -> putObject tx objectStore so
                                            (Just _ :: Maybe RpkiObject)  -> 
                                                -- TODO Add location
                                                logWarn_ logger [i|There's an existing object with hash: #{newHash}|]

        asStorable (URI u) b64 = case parsed of
            Left e   -> SError e
            Right ro -> SObject $ toStorableObject ro
            where
                parsed = do
                    DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
                    first ParseE $ readObject (T.unpack u) b
                    



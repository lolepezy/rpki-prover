{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.RRDP.Update where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Control.Lens               ((^.))

import Control.DeepSeq (($!!))


import           Data.Bifunctor             (first, second)
import qualified Data.ByteString.Lazy       as BL
import Data.IORef
import Data.Has
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Store.Base.Storage
import           RPKI.RRDP.Parse
import           RPKI.Parse.Parse
import           RPKI.RRDP.Types
import           RPKI.Logging
import qualified RPKI.Util                  as U

import qualified Data.ByteString.Streaming as Q
import Data.ByteString.Streaming.HTTP

import qualified Crypto.Hash.SHA256      as S256

import System.IO.Temp (withSystemTempFile)
import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import System.IO (Handle, hClose)

import qualified UnliftIO.Async as Unlift

{- 
    TODO 
    1) Replace IO with some reasonable Monad (MonadIO + MonadMask/MonadUnliftIO/MonadBaseControl).
    2) Maybe return bracketed IO actions instead of exectuting them.
-}
updateRrdpRepo :: RrdpRepository -> 
                (Snapshot -> IO (Maybe SomeError)) ->
                (Delta -> IO (Maybe SomeError)) ->
                IO (Either SomeError (RrdpRepository, Maybe e))
updateRrdpRepo repo@(RrdpRepository repoUri _) handleSnapshot handleDelta = do
    notificationXml <- download repoUri (RrdpE . CantDownloadNotification . show)
    bindRight (notificationXml >>= (first RrdpE . parseNotification)) $ \notification -> 
        bindRight (first RrdpE $ rrdpNextStep repo notification) $ \case
            NothingToDo            -> pure $ Right (repo, Nothing)
            UseSnapshot snapshot   -> fmap (, Nothing) <$> useSnapshot snapshot                            
            UseDeltas sortedDeltas -> useDeltas sortedDeltas notification
    where
        bindRight e f = either (pure . Left) f e 
        
        useSnapshot (SnapshotInfo uri@(URI u) hash) = do
            let tmpFileName = U.convert $ U.normalizeUri u
            -- Download snapshot to a temporary file and MMAP it to a lazy bytestring 
            -- to minimize the heap. Snapshots can be pretty big, so we don't want 
            -- a spike in heap usage
            withSystemTempFile tmpFileName $ \name fd -> do
                realHash <- downloadToFile uri (RrdpE . CantDownloadSnapshot . show) fd
                bindRight realHash $ \realHash' ->
                    if realHash' /= hash
                        then pure $ Left $ RrdpE $ SnapshotHashMismatch hash realHash'
                        else do
                            -- File has to be closed before it can be opened again by mmap
                            hClose fd
                            snapshot <- first RrdpE . parseSnapshot <$> unsafeMMapFile name
                            bindRight snapshot $ \s ->
                                maybe (Right $ repoFromSnapshot s) Left <$> handleSnapshot s

        useDeltas sortedDeltas notification = do
            deltas <- U.parallel 10 processDelta sortedDeltas            
            foldM foldDeltas' ([], Nothing) deltas >>= \case 
                (ds, Nothing) -> pure $ Right (repoFromDeltas ds notification, Nothing)
                (_, Just e)   -> pure $ Left e
            where
                foldDeltas' (valids, Just e)   _         = pure (valids, Just e)
                foldDeltas' (valids, Nothing) (Left e')  = pure (valids, Just e')
                foldDeltas' (valids, Nothing) (Right d) =
                    handleDelta d >>= \case 
                        Nothing -> pure (d : valids, Nothing)
                        Just e  -> pure (valids, Just e)

                processDelta (DeltaInfo uri hash serial) = do                     
                    deltaXml <- download uri (RrdpE . CantDownloadDelta . show)                        
                    bindRight deltaXml $ \dXml ->
                        let realHash = U.sha256 dXml
                        in pure $ if realHash /= hash
                            then Left $ RrdpE $ DeltaHashMismatch hash realHash serial
                            else let !d = first RrdpE $ parseDelta dXml in d

        repoFromSnapshot :: Snapshot -> RrdpRepository
        repoFromSnapshot (Snapshot _ sid s _) = RrdpRepository repoUri $ Just (sid, s)

        repoFromDeltas :: [Delta] -> Notification -> RrdpRepository
        repoFromDeltas ds notification = RrdpRepository repoUri $ Just (newSessionId, newSerial)
            where
                newSessionId = sessionId notification
                newSerial = L.maximum $ map (\(Delta _ _ s _) -> s) ds        


download :: MonadIO m => URI -> (SomeException -> e) -> m (Either e BL.ByteString)
download (URI uri) err = liftIO $ do
    r <- try (WR.get $ T.unpack uri)
    pure $ first err $ second (^. WR.responseBody) r


downloadToFile :: MonadIO m =>
                URI -> 
                (SomeException -> err) -> 
                Handle -> 
                m (Either err Hash)
downloadToFile (URI uri) err destinationHandle = 
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

-- Decide what to do next based on current state of the repository
-- and the parsed notification file
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
                (_, []) | next repoSerial < head (map getSerial sortedDeltas) ->
                           -- we are too far behind
                           Right $ UseSnapshot snapshotInfo
                        | otherwise ->
                           Right $ UseDeltas chosenDeltas
                (_, nc) -> Left $ NonConsecutiveDeltaSerials nc
            where
                sortedSerials = map getSerial sortedDeltas
                sortedDeltas = L.sortOn getSerial deltas
                chosenDeltas = filter ((> repoSerial) . getSerial) sortedDeltas

                nonConsecutive = L.filter (\(s, s') -> next s /= s') $
                    L.zip sortedSerials (tail sortedSerials)


getSerial :: DeltaInfo -> Serial
getSerial (DeltaInfo _ _ s) = s

next :: Serial -> Serial
next (Serial s) = Serial $ s + 1


processRrdp :: (Has AppLogger conf, Storage s) =>                 
                RrdpRepository ->
                s ->
                ReaderT conf IO (Either SomeError (RrdpRepository, Maybe RrdpError))
processRrdp repository storage = do
    logger :: AppLogger   <- asks getter 
    lift $ updateRrdpRepo repository (saveSnapshot logger) (saveDelta logger)
    where        
        saveSnapshot :: AppLogger -> Snapshot -> IO (Maybe SomeError)
        saveSnapshot logger (Snapshot _ _ _ ps) = do
            logInfo_ logger $ U.convert $ "Using snapshot for the repository: " <> show repository
            either Just (const Nothing) . 
                first (StorageE . StorageError . U.fmtEx) <$> 
                    try (U.txFunnel 20 ps objectAsyncs (readWriteTx storage) writeObject)
            where
                objectAsyncs (SnapshotPublish u encodedb64) =
                    Unlift.async $ pure $!! (u, mkObject u encodedb64)
                
                writeObject tx a = Unlift.wait a >>= \case                        
                    (u, Left e)   -> logError_ logger $ U.convert $ "Couldn't parse object: " <> show e <> ", u = " <> show u
                    (u, Right st) -> storeObj tx storage st            

        saveDelta :: AppLogger -> Delta -> IO (Maybe SomeError)
        saveDelta logger (Delta _ _ _ ds) = 
            either Just (const Nothing) . 
                first (StorageE . StorageError . U.fmtEx) <$> 
                    try (U.txFunnel 20 ds objectAsyncs (readWriteTx storage) writeObject)
            where
                objectAsyncs (DP (DeltaPublish u h encodedb64)) = do 
                    a <- Unlift.async $ pure $!! mkObject u encodedb64
                    pure $ Right (u, h, a)

                objectAsyncs (DW (DeltaWithdraw u h)) = pure $ Left (u, h)       

                writeObject tx = \case
                    Left (u, h)           -> delete tx storage (h, u)
                    Right (u, Nothing, a) -> 
                        Unlift.wait a >>= \case
                            Left e -> logError_ logger $ U.convert ("Couldn't parse object: " <> show e)
                            Right (h, st) ->
                                getByHash tx storage h >>= \case 
                                    Nothing -> storeObj tx storage (h, st)
                                    Just _ ->
                                        -- TODO Add location
                                        logWarn_ logger $ U.convert ("There's an existing object with hash: " <> show h)
                    Right (u, Just h, a) -> 
                        Unlift.wait a >>= \case
                            Left e -> logError_ logger $ U.convert ("Couldn't parse object: " <> show e)
                            Right (h', st) ->
                                getByHash tx storage h >>= \case 
                                    Nothing -> 
                                        logWarn_ logger $ U.convert ("No object with hash : " <> show h <> ", nothing to replace")
                                    Just _ -> do 
                                        delete tx storage (h, u)
                                        getByHash tx storage h' >>= \case 
                                            Nothing -> storeObj tx storage (h, st)
                                            Just _  -> 
                                                -- TODO Add location
                                                logWarn_ logger $ U.convert ("There's an existing object with hash: " <> show h)

        mkObject (URI u) b64 = do
            DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
            ro <- first ParseE $ readObject (T.unpack u) b    
            pure (getHash ro, toStorable ro)

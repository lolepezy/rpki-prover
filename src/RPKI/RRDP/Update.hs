{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.RRDP.Update where

import           Control.Exception
import           Control.Monad
import           Control.Lens               ((^.))

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import Control.Monad.Catch hiding (try)

import           Data.Bifunctor             (first, second)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.DList                 as DL
import Data.IORef
import Data.Char (isAlpha)
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           GHC.Generics

import           RPKI.Domain
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import qualified RPKI.Util                  as U

--
import Streaming
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Streaming as Q
import Data.ByteString.Streaming.HTTP

import qualified Crypto.Hash.SHA256      as S256

import System.IO.Temp (withSystemTempFile)
import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import System.IO (Handle)

withHash :: Q.ByteString IO a -> Q.ByteString IO a
withHash x = x

streamHttpResponse :: URI -> 
                      (Q.ByteString IO () -> IO (Either err a)) -> 
                      (SomeException -> err) -> 
                      ExceptT err IO (a, Hash)
streamHttpResponse (URI uri) handler err = 
    lift (try go) >>= \case
        Left (e :: SomeException) -> throwE $ err e
        Right r                   -> except r
    where 
        go = do
            req     <- parseRequest $ T.unpack uri
            tls     <- newManager tlsManagerSettings 
            hashRef <- newIORef S256.init
            s <- withHTTP req tls $ \resp -> 
                handler $ 
                    Q.fromChunks $ 
                    S.mapM (\b -> modifyIORef hashRef (`S256.update` b) >> pure b) $ 
                    Q.toChunks $ responseBody resp
            h <- readIORef hashRef
            pure $ (,Hash $ S256.finalize h) <$> s
                    

streamHttpResponse0 :: URI -> 
                    (Q.ByteString IO () -> IO a) -> 
                    (SomeException -> err) -> 
                    ExceptT err IO a
streamHttpResponse0 (URI uri) handler err = 
  lift (try go) >>= \case
      Left (e :: SomeException) -> throwE $ err e
      Right r                   -> pure r
  where 
      go = do
          req     <- parseRequest $ T.unpack uri
          tls     <- newManager tlsManagerSettings 
          withHTTP req tls $ \resp -> handler $ responseBody resp



tryDownload :: MonadIO m => URI -> (SomeException -> err) -> ExceptT err m BL.ByteString
tryDownload (URI uri) err = do
    t <- liftIO $ try (WR.get $ T.unpack uri)
    case t of
        Left (e :: SomeException) -> throwE $ err e
        Right r                   -> pure $ r ^. WR.responseBody


-- TODO Replace RrdpGeneralProblem with something more concrete
-- TODO Add logging
makeUpdates :: Repository 'Rrdp -> IO (Either Problem UpdateResult)
makeUpdates repo@(RrdpRepo repoUri _ _) = runExceptT $ do
    notificationXml <- tryDownload repoUri (Fatal . CantDownloadNotification . show)
    notification    <- except $ first Fatal $ parseNotification notificationXml
    nextStep        <- except $ first Fatal $ rrdpNextStep repo notification
    case nextStep of
        NothingToDo -> lift $ pure (repo, DontSave)
        UseSnapshot (SnapshotInfo snapshotUri hash) -> do
            snapshotXml                  <- tryDownload snapshotUri (Fatal . CantDownloadSnapshot . show)
            snapshot@(Snapshot _ _ _ ps) <- withExceptT Fatal $ processSnapshot snapshotXml hash
            lift $ pure (repoFromSnapshot snapshot, SaveSnapshot ps)
        UseDeltas sortedDeltasToBeApplied -> do
            deltas <- lift $ 
                forM sortedDeltasToBeApplied $ \(DeltaInfo uri hash serial) ->
                    runExceptT $ do
                        deltaXml <- tryDownload uri (CantDownloadDelta . show)
                        processDelta deltaXml hash serial
            let saveDeltas = SaveDeltas . map (\(Delta _ _ _ items) -> SaveDelta items)
            let newRepo ds = repoFromDeltas ds notification
            case deltasTillFirstError deltas of
                (Nothing, validDeltas) -> lift $ pure (newRepo validDeltas, saveDeltas validDeltas)
                (Just e,  validDeltas) -> throwE $ Partial e (newRepo validDeltas, saveDeltas validDeltas)
    where
        deltasTillFirstError :: [Either RrdpError Delta] -> (Maybe RrdpError, [Delta])
        deltasTillFirstError deltas = second DL.toList $ go deltas DL.empty
            where
                go [] accumulated = (Nothing, accumulated)
                go (Left e :   _) accumulated = (Just e, accumulated)
                go (Right d : ds) accumulated = go ds (accumulated `DL.snoc` d)

        repoFromDeltas :: [Delta] -> Notification -> Repository 'Rrdp
        repoFromDeltas ds notification = RrdpRepo repoUri newSessionId newSerial
            where
                newSessionId = sessionId notification
                newSerial = L.maximum $ map (\(Delta _ _ s _) -> s) ds

        repoFromSnapshot :: Snapshot -> Repository 'Rrdp
        repoFromSnapshot (Snapshot _ sid s _) = RrdpRepo repoUri sid s


processSnapshot :: Monad m => BL.ByteString -> Hash -> ExceptT RrdpError m Snapshot
processSnapshot snapshotXml hash = do
    let realHash = U.sha256 snapshotXml
    when (realHash /= hash) $ throwE $ SnapshotHashMismatch hash realHash
    except $ parseSnapshot snapshotXml

processDelta :: Monad m => BL.ByteString -> Hash -> Serial -> ExceptT RrdpError m Delta
processDelta deltaXml hash serial = do
    let realHash = U.sha256 deltaXml
    when (realHash /= hash) $ throwE $ DeltaHashMismatch hash realHash serial
    except $ parseDelta deltaXml


streamUpdates :: (MonadIO m, MonadMask m) => 
                Repository 'Rrdp -> 
                (Snapshot -> ExceptT RrdpError m ()) ->
                (Delta -> ExceptT RrdpError m ()) ->
                m (Either Problem (Repository 'Rrdp))
streamUpdates repo@(RrdpRepo repoUri _ _) handleSnapshot handleDelta = runExceptT $ do
    notificationXml <- tryDownload repoUri (Fatal . CantDownloadNotification . show)
    notification    <- except $ first Fatal $ parseNotification notificationXml
    nextStep        <- except $ first Fatal $ rrdpNextStep repo notification    
    case nextStep of
        NothingToDo            -> pure repo
        UseSnapshot snapshot   -> useSnapshot snapshot
        UseDeltas sortedDeltas -> useDeltas sortedDeltas notification
    where        
        useSnapshot (SnapshotInfo uri@(URI u) hash) = do
            let tmpFileName = U.convert $ normalize u
            -- Download snapshot to a temporary file and MMAP it to a lazy bytestring 
            -- to minimize the heap usage. Snapshots can be pretty big, so we don't want 
            -- a spike in heap usage
            withSystemTempFile tmpFileName $ \name fd -> do
                realHash <- downloadToFile uri (Fatal . CantDownloadSnapshot . show) fd
                when (realHash /= hash) $ throwE $ Fatal $ SnapshotHashMismatch hash realHash
                xml <- liftIO $ unsafeMMapFile name
                snapshot <- except $ first Fatal $ parseSnapshot xml
                withExceptT Fatal $ handleSnapshot snapshot            
                pure $ repoFromSnapshot snapshot
        
        useDeltas sortedDeltasToApply notification = do
            deltas <- liftIO $ forM sortedDeltasToApply $ \(DeltaInfo uri hash serial) -> do
                    x :: Either RrdpError (Delta, Hash) <- runExceptT $ streamHttpResponse uri
                        (\s -> parseDelta <$> Q.toLazy_ s)
                        (CantDownloadDelta . show)       
                    pure $ case x of 
                        Left e                   -> Left e
                        Right (d, h) | h /= hash -> Left $ DeltaHashMismatch hash h serial
                                     | otherwise -> Right d
            let saveDeltas = SaveDeltas . map (\(Delta _ _ _ items) -> SaveDelta items)
            let newRepo ds = repoFromDeltas ds notification
            case deltasTillFirstError deltas of
                (Nothing, validDeltas) -> pure $ newRepo validDeltas
                (Just e,  validDeltas) -> throwE $ Partial e (newRepo validDeltas, saveDeltas validDeltas) 

        repoFromSnapshot :: Snapshot -> Repository 'Rrdp
        repoFromSnapshot (Snapshot _ sid s _) = RrdpRepo repoUri sid s

        repoFromDeltas :: [Delta] -> Notification -> Repository 'Rrdp
        repoFromDeltas ds notification = RrdpRepo repoUri newSessionId newSerial
            where
                newSessionId = sessionId notification
                newSerial = L.maximum $ map (\(Delta _ _ s _) -> s) ds

        deltasTillFirstError :: [Either RrdpError Delta] -> (Maybe RrdpError, [Delta])
        deltasTillFirstError deltas = second DL.toList $ go deltas DL.empty
            where
                go [] accumulated = (Nothing, accumulated)
                go (Left e :   _) accumulated = (Just e, accumulated)
                go (Right d : ds) accumulated = go ds (accumulated `DL.snoc` d)


downloadToFile :: MonadIO m =>
                URI -> 
                (SomeException -> err) -> 
                Handle -> 
                ExceptT err m Hash
downloadToFile (URI uri) err destinationHandle = 
    liftIO (try go) >>= \case
        Left (e :: SomeException) -> throwE $ err e
        Right r                   -> pure r
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

            
normalize :: T.Text -> T.Text
normalize = T.map (\c -> if isAlpha c then c else '_') 


type UpdateResult = (Repository 'Rrdp, ObjectUpdates)

data Problem = Fatal RrdpError | Partial RrdpError UpdateResult
    deriving (Show, Eq, Ord, Generic)

newtype SaveDelta = SaveDelta [DeltaItem]
    deriving (Show, Eq, Ord, Generic)

data Step = UseSnapshot SnapshotInfo
          | UseDeltas { sortedDeltas :: [DeltaInfo] }
          | NothingToDo
    deriving (Show, Eq, Ord, Generic)

data ObjectUpdates = SaveSnapshot [SnapshotPublish]
                   | SaveDeltas [SaveDelta]
                   | DontSave
    deriving (Show, Eq, Ord, Generic)


-- Decide what to do next based on current state of the repository
-- and the parsed notification file
rrdpNextStep :: Repository 'Rrdp -> Notification -> Either RrdpError Step
rrdpNextStep (RrdpRepo _ repoSessionId repoSerial) Notification{..} =
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


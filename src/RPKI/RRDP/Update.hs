{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module RPKI.RRDP.Update where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

import           Control.Lens               ((^.))

import           Control.Monad.Primitive    (PrimMonad (..), stToPrim)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except

import           Data.Bifunctor             (first, second)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (chr, isSpace)
import qualified Data.List                  as L
import qualified Data.DList                 as DL
import qualified Data.Map                   as M
import           Data.String.Utils          (strip)
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           Data.Hex                   (unhex)
import           Data.Word

import           Data.IORef
import           Text.Read                  (readMaybe)

import           Xeno.SAX                   as X

import           RPKI.Domain
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types
import qualified RPKI.Util                  as U


tryDownload :: URI -> (SomeException -> err) -> ExceptT err IO BL.ByteString
tryDownload (URI uri) err = do
    t <- lift $ try (WR.get $ T.unpack uri)
    case t of
        Left (e :: SomeException) -> throwE $ err e
        Right r                   -> pure $ r ^. WR.responseBody


-- TODO Replace RrdpGeneralProblem with something more concrete
-- TODO Add logging
-- TODO Add actual saving to the storage
updateLocalState :: Repository 'Rrdp -> IO (Either Problem UpdateResult)
updateLocalState repo@(RrdpRepo uri repoSessionId repoSerial) = runExceptT $ do
    notificationXml <- tryDownload uri (Fatal . CantDownloadNotification . show)
    notification    <- except $ first Fatal $ parseNotification notificationXml
    nextStep        <- except $ first Fatal $ rrdpWorkflow repo notification
    processNext nextStep
    where
        processNext :: Step -> ExceptT Problem IO UpdateResult
        processNext NothingToDo = lift $ pure (repo, DontSave)
        processNext (ProcessSnapshot (SnapshotInfo uri hash)) = do
            snapshotXml <- tryDownload uri (Fatal . CantDownloadSnapshot . show)
            (repo,) <$> SaveSnapshot 
                    <$> (\(Snapshot _ _ _ ps) -> ps) 
                    <$> (withExceptT Fatal $ processSnapshot snapshotXml hash)
        processNext (ProcessDeltas sortedDeltasToApply) = do
            deltas <- lift $ processDeltas sortedDeltasToApply                
            case extract deltas of
                (Nothing, validDeltas) -> lift $ pure (
                    repoFromDeltas validDeltas, 
                    SaveDeltas $ map (\(Delta _ _ _ items) -> SaveDelta items) validDeltas)
                (Just e, validDeltas)  -> throwE $ Partial e (
                    repoFromDeltas validDeltas, 
                    SaveDeltas $ map (\(Delta _ _ _ items) -> SaveDelta items) validDeltas)

        extract :: [Either RrdpError Delta] -> (Maybe RrdpError, [Delta])
        extract deltas = second DL.toList $ go deltas DL.empty
            where
                go [] accumulated = (Nothing, accumulated)    
                go (Left e : ds)  accumulated = (Just e, accumulated) 
                go (Right d : ds) accumulated = go ds (accumulated `DL.snoc` d)

        repoFromDeltas :: [Delta] -> Repository 'Rrdp
        repoFromDeltas ds = RrdpRepo uri repoSessionId newSerial
            where newSerial = L.maximum $ map (\(Delta _ _ s _) -> s) ds


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

processDeltas :: [DeltaInfo] -> IO [Either RrdpError Delta]
processDeltas sortedDeltasToApply = 
    forM sortedDeltasToApply $ \(DeltaInfo uri hash serial) -> 
            runExceptT $ do
                deltaXml <- tryDownload uri (CantDownloadDelta . show)
                processDelta deltaXml hash serial


type UpdateResult = (Repository 'Rrdp, ObjectUpdates)

data Problem = Fatal RrdpError | Partial RrdpError UpdateResult

data SaveDelta = SaveDelta [DeltaItem]

data Step = ProcessSnapshot SnapshotInfo
          | ProcessDeltas { sortedDeltas :: [DeltaInfo] }
          | NothingToDo

data ObjectUpdates = SaveSnapshot [SnapshotPublish]
                   | SaveDeltas [SaveDelta]
                   | DontSave


-- Decide what to do next based on current state of the repository
-- and the parsed notification file
rrdpWorkflow :: Repository 'Rrdp -> Notification -> Either RrdpError Step
rrdpWorkflow (RrdpRepo _ repoSessionId repoSerial) Notification{..} =
    if  | sessionId /= repoSessionId -> Right $ ProcessSnapshot snapshotInfo
        | repoSerial > serial  -> Left RrdpGeneralProblem -- TODO weird shit happened
        | repoSerial == serial -> Right NothingToDo
        | otherwise ->
            case (deltas, nonConsecutive) of
                ([], _) -> Right $ ProcessSnapshot snapshotInfo
                (_, []) | repoSerial < (head $ map getSerial sortedDeltas) ->
                           -- we are too far behind
                           Right $ ProcessSnapshot snapshotInfo
                       | otherwise ->
                           Right $ ProcessDeltas chosenDeltas
                (_, nc) -> Right $ ProcessDeltas chosenDeltas
            where
                sortedDeltas = L.sortOn getSerial deltas
                chosenDeltas = filter ((> repoSerial) . getSerial) sortedDeltas

                nonConsecutive = L.filter (\(p, n) -> getSerial n /= next(getSerial p)) $
                    L.zip sortedDeltas (tail sortedDeltas)

getSerial :: DeltaInfo -> Serial
getSerial (DeltaInfo _ _ s) = s 

next (Serial s) = Serial $ s + 1
 
-- parMap :: [a] -> Int -> (a -> b) -> IO [b]
-- parMap as poolSize f = go 0 as []
--     where
--         go [] r = r
--         go (a : as) =
--         queue a q        = (q |>) <$> async (f a)
--         deque (b :<| q') = wait b

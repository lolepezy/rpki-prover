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
import qualified Data.DList                 as DL
import qualified Data.List                  as L
import qualified Data.Map                   as M
import           Data.String.Utils          (strip)
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           Data.Hex                   (unhex)
import           Data.Word

import           GHC.Generics

import           Data.IORef
import           Text.Read                  (readMaybe)

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
makeUpdates :: Repository 'Rrdp -> IO (Either Problem UpdateResult)
makeUpdates repo@(RrdpRepo uri repoSessionId repoSerial) = runExceptT $ do
    notificationXml <- tryDownload uri (Fatal . CantDownloadNotification . show)
    notification    <- except $ first Fatal $ parseNotification notificationXml
    nextStep        <- except $ first Fatal $ rrdpNextStep repo notification
    case nextStep of
        NothingToDo -> lift $ pure (repo, DontSave)
        UseSnapshot (SnapshotInfo uri hash) -> do
            snapshotXml                  <- tryDownload uri (Fatal . CantDownloadSnapshot . show)
            snapshot@(Snapshot _ _ _ ps) <- withExceptT Fatal $ processSnapshot snapshotXml hash
            lift $ pure (repoFromSnapshot snapshot, SaveSnapshot ps)
        UseDeltas sortedDeltasToApply -> do
            deltas <- lift $ forM sortedDeltasToApply $ \(DeltaInfo uri hash serial) ->
                                runExceptT $ do
                                    deltaXml <- tryDownload uri (CantDownloadDelta . show)
                                    processDelta deltaXml hash serial
            let saveDeltas ds = SaveDeltas $ map (\(Delta _ _ _ items) -> SaveDelta items) ds
            let newRepo ds = repoFromDeltas ds notification
            case extract deltas of
                (Nothing, validDeltas) -> lift $ pure (newRepo validDeltas, saveDeltas validDeltas)
                (Just e, validDeltas)  -> throwE $ Partial e (newRepo validDeltas, saveDeltas validDeltas)
    where
        extract :: [Either RrdpError Delta] -> (Maybe RrdpError, [Delta])
        extract deltas = second DL.toList $ go deltas DL.empty
            where
                go [] accumulated = (Nothing, accumulated)
                go (Left e : ds)  accumulated = (Just e, accumulated)
                go (Right d : ds) accumulated = go ds (accumulated `DL.snoc` d)

        repoFromDeltas :: [Delta] -> Notification -> Repository 'Rrdp
        repoFromDeltas ds notification = RrdpRepo uri newSessionId newSerial
            where
                newSessionId = sessionId notification
                newSerial = L.maximum $ map (\(Delta _ _ s _) -> s) ds

        repoFromSnapshot :: Snapshot -> Repository 'Rrdp
        repoFromSnapshot (Snapshot _ sid s _) = RrdpRepo uri sid s


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


type UpdateResult = (Repository 'Rrdp, ObjectUpdates)

data Problem = Fatal RrdpError | Partial RrdpError UpdateResult
    deriving (Show, Eq, Ord, Generic)

data SaveDelta = SaveDelta [DeltaItem]
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
                (_, []) | next repoSerial < (head $ map getSerial sortedDeltas) ->
                           -- we are too far behind
                           Right $ UseSnapshot snapshotInfo
                        | otherwise ->
                           Right $ UseDeltas chosenDeltas
                (_, nc) -> Left $ NonConsecutiveDeltaSerials nc
            where
                sortedSerials = map getSerial sortedDeltas
                sortedDeltas = L.sortOn getSerial deltas
                chosenDeltas = filter ((> repoSerial) . getSerial) sortedDeltas

                nonConsecutive = L.filter (\(p, n) -> n /= next p) $
                    L.zip sortedSerials (tail sortedSerials)

getSerial :: DeltaInfo -> Serial
getSerial (DeltaInfo _ _ s) = s

next (Serial s) = Serial $ s + 1


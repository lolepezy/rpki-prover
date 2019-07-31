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
import qualified Data.ByteString.Lazy       as BL
import qualified Data.DList                 as DL
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           GHC.Generics

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


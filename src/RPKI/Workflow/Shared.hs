{-# LANGUAGE StrictData #-}

{- | The state the workflow threads share with each other: what is running,
     what has been discovered and what still needs validating.
-}
module RPKI.Workflow.Shared where

import           Control.Concurrent.STM
import           Control.Exception               (finally)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Lens
import           GHC.Generics

import           Data.Foldable                   (for_)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Fetch.Fetch
import           RPKI.Logging
import           RPKI.Metrics.Prometheus
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Workflow.Task


data WorkflowShared = WorkflowShared { 
        -- Currently running tasks, it is needed to keep track which 
        -- tasks can run parallel to each other and avoid race conditions.
        runningTasks :: Tasks,

        -- It's just handy to avoid passing this one as a parameter the whole time
        prometheusMetrics :: PrometheusMetrics,

        -- Looping fetcher threads
        fetchers :: Fetchers,

        -- Looping fetcher threads
        lastFqdnFetch :: TVar (Map FQDN Instant),

        -- TAs that need to be revalidated because repositories 
        -- associated with these TAs have been fetched.
        tasToValidate :: TVar (Set TaName),

        -- Earliest expiration time for any object for a TA
        earliestToExpire :: TVar (Map TaName EarliestToExpire),

        -- Moments at which TAs are to be revalidated, e.g. when a new 
        -- manifest becomes current. A TA is here for as long as there's 
        -- a thread waiting for the first of its moments.
        scheduledRevalidations :: TVar (Map TaName (Set Instant)),

        tals :: [TAL]
    }
    deriving stock (Generic)


withWorkflowShared :: AppContext s
                    -> PrometheusMetrics 
                    -> [TAL]
                    -> (WorkflowShared -> IO b) 
                    -> IO b
withWorkflowShared AppContext {..} prometheusMetrics tals f = do
    shared <- liftIO $ atomically $ do 
        runningTasks     <- newRunningTasks
        fetchers         <- newFetchers config (appState ^. #fetcheables)
        tasToValidate    <- newTVar mempty
        lastFqdnFetch    <- newTVar mempty
        earliestToExpire <- newTVar mempty
        scheduledRevalidations <- newTVar mempty
        pure WorkflowShared {..}

    f shared `finally` liftIO (stopAllFetchers (shared ^. #fetchers))


-- | Ask the validation loop to (re)validate these TAs on its next round. 
-- This is the one signal the whole triggered-validation design turns on: 
-- fetches that found updates, TA certificates that changed and objects 
-- that are about to expire all end up here.
requestRevalidation :: MonadIO m => TVar (Set TaName) -> Set TaName -> m ()
requestRevalidation tasToValidate tas = 
    liftIO $ atomically $ modifyTVar' tasToValidate (<> tas)


-- | Ask for the TAs to be revalidated at the given moment.
--
-- There's one waiting thread per TA at most, it's there for as long as the
-- TA is in `scheduledRevalidations`. It waits for the first of the TA's 
-- moments, and wakes up earlier when an earlier one is added. A later moment 
-- is kept as well: revalidating at the first one doesn't do for it, e.g. a 
-- manifest that is current only later is not used before that.
scheduleRevalidationAt :: AppLogger 
                       -> TVar (Map TaName (Set Instant)) 
                       -> TVar (Set TaName) 
                       -> Set TaName -> Instant -> IO ()
scheduleRevalidationAt logger scheduledRevalidations tasToValidate tas moment = do
    withoutThread <- atomically $ do
        scheduled <- readTVar scheduledRevalidations
        writeTVar scheduledRevalidations $
            Map.unionWith (<>) scheduled $ Map.fromSet (const $ Set.singleton moment) tas
        pure $ Set.toList $ tas `Set.difference` Map.keysSet scheduled

    logInfo logger [i|Will revalidate #{Set.toList tas} at #{moment}.|]
    for_ withoutThread $ \taName ->
        forkLogged logger [i|Exception in the scheduled revalidation of #{taName}|] $
            waitAndRevalidate taName
  where
    waitAndRevalidate taName = do
        firstMoment <- atomically $ firstMomentOf taName <$> readTVar scheduledRevalidations
        for_ firstMoment $ \first_ -> do
            Now now <- thisInstant
            timeIsUp <- registerDelay $ microsecondsBetween now first_
            -- Until it's time, or until there's an earlier moment
            atomically $ do
                up      <- readTVar timeIsUp
                earlier <- (/= Just first_) . firstMomentOf taName <$> readTVar scheduledRevalidations
                unless (up || earlier) retry

            Now now' <- thisInstant
            more <- atomically $ do
                scheduled <- readTVar scheduledRevalidations
                let (due, later) = Set.partition (<= now') $ Map.findWithDefault mempty taName scheduled
                unless (Set.null due) $
                    modifyTVar' tasToValidate (Set.insert taName)
                if Set.null later
                    then do
                        writeTVar scheduledRevalidations $ Map.delete taName scheduled
                        pure False
                    else do
                        writeTVar scheduledRevalidations $ Map.insert taName later scheduled
                        pure True

            when more $ waitAndRevalidate taName

    firstMomentOf taName = Set.lookupMin <=< Map.lookup taName

    -- Rounded up, so that it's not woken up before the moment
    microsecondsBetween (Instant earlier) (Instant later) =
        fromIntegral $ max 0 $ (later - earlier + 999) `div` 1000

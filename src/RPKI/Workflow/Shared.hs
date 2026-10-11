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

        -- Earliest expiration time for any object for a TA, as the
        -- last validation of the TA found it (see `revalidateOnExpiry`)
        earliestToExpire :: TVar (Map TaName EarliestToExpire),

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
        pure WorkflowShared {..}

    f shared `finally` liftIO (stopAllFetchers (shared ^. #fetchers))


-- | Ask the validation loop to (re)validate these TAs on its next round. 
-- This is the one signal the whole triggered-validation design turns on: 
-- fetches that found updates, TA certificates that changed and objects 
-- that are about to expire all end up here.
requestRevalidation :: MonadIO m => TVar (Set TaName) -> Set TaName -> m ()
requestRevalidation tasToValidate tas =
    liftIO $ atomically $ modifyTVar' tasToValidate (<> tas)


-- | Revalidate a TA right after its earliest expiration time, i.e. right after
-- the first of its objects expires.
--
-- Every validation of a TA replaces its expiration time, and since in practice
-- it keeps receding into the future as objects are re-issued, most of these
-- moments never come. So there's one thread for all TAs: it waits for the
-- first moment and starts over whenever any of them changes. A moment is
-- dropped once it has come, the validation it triggers sets the next one.
revalidateOnExpiry :: AppLogger
                   -> TVar (Map TaName EarliestToExpire)
                   -> TVar (Set TaName)
                   -> IO ()
revalidateOnExpiry logger earliestToExpire tasToValidate = forever $ do
    Now now <- thisInstant
    (expired, waiting) <- atomically $ do
        (expired, waiting) <- Map.partition (hasExpiredBy now) <$> readTVar earliestToExpire
        writeTVar earliestToExpire waiting
        pure (expired, waiting)

    for_ (Map.toList expired) $ \(taName, EarliestToExpire expiresAt) ->
        logDebug logger [i|The first object of #{taName} expired at #{expiresAt}, will revalidate it.|]
    requestRevalidation tasToValidate $ Map.keysSet expired

    -- Until the first of the moments, or until the moments change. It doesn't
    -- sleep for longer than an hour, the clock can jump or the machine sleep.
    let untilFirst = minimum $ maxWait : [ microsecondsBetween now t | EarliestToExpire t <- Map.elems waiting ]
    timeIsUp <- registerDelay untilFirst
    atomically $ do
        up      <- readTVar timeIsUp
        changed <- (/= waiting) <$> readTVar earliestToExpire
        unless (up || changed) retry
  where
    hasExpiredBy now (EarliestToExpire expiresAt) = expiresAt < now

    maxWait = 3600 * 1_000_000

    -- Rounded up, so that it's not woken up before the moment
    microsecondsBetween (Instant earlier) (Instant later) =
        fromIntegral $ max 0 $ (later - earlier + 999) `div` 1000

{-# LANGUAGE StrictData #-}

{- | The state the workflow threads share with each other: what is running,
     what has been discovered and what still needs validating.
-}
module RPKI.Workflow.Shared where

import           Control.Concurrent.STM
import           Control.Exception               (finally)
import           Control.Monad.IO.Class
import           Control.Lens
import           GHC.Generics

import           Data.Map.Strict                 (Map)
import           Data.Set                        (Set)

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Fetch.Fetch
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

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
import           Data.Hourglass                  (Seconds)
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

        -- When each TA is to be revalidated at the latest (see `revalidateWhenDue`)
        nextValidation :: TVar (Map TaName Due),

        -- How many CAs the latest validation of each TA found that nothing
        -- had been fetched for, with publication points still to be tried
        pendingCas :: TVar (Map TaName Int),

        tals :: [TAL]
    }
    deriving stock (Generic)


withWorkflowShared :: AppContext
                    -> PrometheusMetrics 
                    -> [TAL]
                    -> (WorkflowShared -> IO b) 
                    -> IO b
withWorkflowShared AppContext {..} prometheusMetrics tals f = do
    Now now <- thisInstant
    shared <- liftIO $ atomically $ do 
        runningTasks   <- newRunningTasks
        fetchers       <- newFetchers config (appState ^. #fetcheables)
        tasToValidate  <- newTVar mempty
        lastFqdnFetch  <- newTVar mempty
        -- Also the TAs that won't be validated until something asks for it, 
        -- e.g. the ones without a TA certificate yet
        nextValidation <- newTVar $ Map.fromList 
            [ (getTaName tal, Periodic $ momentAfter now $ config ^. #validationConfig . #revalidationInterval) 
            | tal <- tals ]
        pendingCas     <- newTVar mempty
        pure WorkflowShared {..}

    f shared `finally` liftIO (stopAllFetchers (shared ^. #fetchers))


-- | Ask the validation loop to (re)validate these TAs on its next round. 
-- This is the one signal the whole triggered-validation design turns on: 
-- fetches that found updates, TA certificates that changed and objects 
-- that expire or become valid all end up here.
requestRevalidation :: MonadIO m => TVar (Set TaName) -> Set TaName -> m ()
requestRevalidation tasToValidate tas =
    liftIO $ atomically $ modifyTVar' tasToValidate (<> tas)


-- | When a TA is to be revalidated at the latest, unless something asks for 
-- it before: right after the validity of one of its objects changes, i.e. 
-- right after the first of them expires or becomes valid, or once it hasn't
-- been validated for revalidationInterval. 
--
-- The latter is in case something that should have asked for a revalidation 
-- didn't, so that the payloads of a TA are never older than that. 
data Due = ValidityChange Instant
         | Periodic Instant
    deriving stock (Show, Eq, Ord, Generic)

dueAt :: Due -> Instant
dueAt = \case
    ValidityChange t -> t
    Periodic t       -> t

-- | When a TA that was validated at the moment is due next.
dueAfterValidation :: Seconds -> Instant -> EarliestValidityChange -> Due
dueAfterValidation interval validatedAt (EarliestValidityChange change)
    | change < periodic = ValidityChange change
    | otherwise         = Periodic periodic
  where
    periodic = momentAfter validatedAt interval


-- | Revalidate every TA when it's due.
--
-- Every validation of a TA replaces its moment, and since in practice it keeps
-- receding into the future, as objects are re-issued and as the TA is validated
-- because of fetches, most of these moments never come. So there's one thread
-- for all TAs: it waits for the first moment and starts over whenever any of
-- them changes. A TA whose moment has come is due again an interval later,
-- in case its validation fails. A validation that doesn't replaces that.
revalidateWhenDue :: AppLogger
                  -> Seconds
                  -> TVar (Map TaName Due)
                  -> TVar (Set TaName)
                  -> IO ()
revalidateWhenDue logger interval nextValidation tasToValidate = forever $ do
    Now now <- thisInstant
    (due, waiting) <- atomically $ do
        (due, notYet) <- Map.partition (hasComeBy now) <$> readTVar nextValidation
        let waiting = notYet <> fmap (const $ Periodic $ momentAfter now interval) due
        writeTVar nextValidation waiting
        pure (due, waiting)

    for_ (Map.toList due) $ \case
        (taName, ValidityChange moment) ->
            logDebug logger [i|An object of #{taName} expired or became valid at #{moment}, will revalidate it.|]
        (taName, Periodic moment) ->
            logDebug logger [i|#{taName} was due for its periodic revalidation at #{moment}, will revalidate it.|]
    requestRevalidation tasToValidate $ Map.keysSet due

    -- Until the first of the moments, or until the moments change. It doesn't
    -- sleep for longer than an hour, the clock can jump or the machine sleep.
    let untilFirst = minimum $ maxWait : [ microsecondsBetween now (dueAt d) | d <- Map.elems waiting ]
    timeIsUp <- registerDelay untilFirst
    atomically $ do
        up    <- readTVar timeIsUp
        moved <- (/= waiting) <$> readTVar nextValidation
        unless (up || moved) retry
  where
    hasComeBy now due = dueAt due < now

    maxWait = 3600 * 1_000_000

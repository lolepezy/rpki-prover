{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Workflow where

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad

import           Control.Lens                     ((^.), (%~), (&))
import           Data.Generics.Product.Typed

import           Data.Int                         (Int64)
import qualified Data.Set                         as Set

import           Data.Hourglass
import           Data.String.Interpolate.IsString

import Prometheus

import           RPKI.AppState
import           RPKI.CommonTypes
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.TopDown

import           RPKI.AppContext
import           RPKI.Metrics
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time

import           RPKI.Store.Base.LMDB
import           RPKI.Store.AppStorage

import           System.Exit
import Data.Maybe (fromMaybe)
import RPKI.Store.Repository (getPublicationPoints)

type AppEnv = AppContext LmdbStorage


runWorkflow :: (Storage s, MaintainableStorage s) => 
                AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do
    -- Use a command queue to avoid fully concurrent operations, i.e. cleanup 
    -- opearations and such should not run at the same time with validation 
    -- (not only for consistency reasons, but we want to avoid locking the 
    -- DB for long time by the cleanup process).
    globalQueue <- newCQueueIO 10

    -- Run RTR server thread when rtrConfig is present in the AppConfig.  
    -- If not needed it will the an noop.  
    rtrServer <- initRtrIfNeeded

    -- Fill in the current state if it's not too old.
    -- It is useful in case of restarts.        
    loadStoredAppState appContext

    -- Initialise prometheus metrics here
    prometheusMetrics <- createPrometheusMetrics

    -- Run threads that periodicallly generate tasks and put them 
    -- to the queue and one thread that executes the tasks.
    mapConcurrently_ (\f -> f globalQueue) [ 
            taskExecutor,
            generateNewWorldVersion prometheusMetrics, 
            generatePeriodicTask 10_000_000 cacheCleanupInterval cacheGC,
            generatePeriodicTask 30_000_000 cacheCleanupInterval cleanOldVersions,
            -- generatePeriodicTask (24 * 60 * 60 * 1_000_000) storageDefragmentInterval defragment,
            -- generatePeriodicTask 40_000_000 revalidationInterval defragment,
            rtrServer   
        ]
    where
        storageDefragmentInterval = config ^. #storageDefragmentInterval
        cacheCleanupInterval = config ^. #cacheCleanupInterval
        oldVersionsLifetime  = config ^. #oldVersionsLifetime
        cacheLifeTime        = config ^. #cacheLifeTime
        revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
        rtrConfig            = config ^. #rtrConfig

        validateTaTask prometheusMetrics globalQueue worldVersion = 
            atomically $ writeCQueue globalQueue (validateTAs prometheusMetrics worldVersion)             

        -- periodically update world version and generate command 
        -- to re-validate all TAs
        generateNewWorldVersion prometheusMetrics globalQueue = periodically revalidationInterval $ do 
            oldWorldVersion <- getWorldVerionIO appState
            newWorldVersion <- updateWorldVerion appState
            logDebug_ logger [i|Generated new world version, #{oldWorldVersion} ==> #{newWorldVersion}.|]
            validateTaTask prometheusMetrics globalQueue newWorldVersion

        generatePeriodicTask delay interval whatToDo globalQueue = do
            threadDelay delay
            periodically interval $ do
                worldVersion <- getWorldVerionIO appState
                atomically $ writeCQueue globalQueue (whatToDo worldVersion)

        taskExecutor globalQueue = do
            logDebug_ logger [i|Starting task executor.|]
            forever $ do 
                atomically (readCQueue globalQueue) >>= fromMaybe (pure ())

        validateTAs prometheusMetrics worldVersion = do
            logInfo_ logger [i|Validating all TAs, world version #{worldVersion} |]
            executeOrDie
                (processTALs tals)
                (\tdResult@TopDownResult{..} elapsed -> do 
                    uniqueVrps <- saveTopDownResult tdResult                                
                    logInfoM logger [i|Validated all TAs, got #{length uniqueVrps} VRPs, took #{elapsed}ms|])
            where 
                processTALs tals = do 
                    storedPubPoints   <- roTx database $ \tx -> getPublicationPoints tx (repositoryStore database)
                    repositoryContext <- newTVarIO $ newRepositoryContext storedPubPoints

                    rs <- forConcurrently tals $ \tal -> do 
                        (r@TopDownResult{..}, elapsed) <- timedMS $ 
                                validateTA appContext tal worldVersion repositoryContext 
                        logInfo_ logger [i|Validated #{getTaName tal}, got #{length vrps} VRPs, took #{elapsed}ms|]
                        pure r

                    let metrics = addTotalValidationMetric $ mconcat rs
                    let appMetrics = metrics ^. typed @ValidationState . typed @AppMetric
                    updatePrometheus appMetrics prometheusMetrics
                    pure $! metrics

                saveTopDownResult TopDownResult {..} = 
                    rwTx database $ \tx -> do
                        let uniqueVrps = Set.fromList vrps
                        putValidations tx (validationsStore database) worldVersion (tdValidations ^. typed)
                        putMetrics tx (metricStore database) worldVersion (tdValidations ^. typed)
                        putVrps tx database (Set.toList uniqueVrps) worldVersion
                        completeWorldVersion tx database worldVersion
                        atomically $ do 
                            completeCurrentVersion appState                                    
                            writeTVar (appState ^. #currentVrps) uniqueVrps
                        pure uniqueVrps

        cacheGC worldVersion = do
            let now = versionToMoment worldVersion
            executeOrDie 
                (cleanObjectCache database $ versionIsOld now cacheLifeTime)
                (\(deleted, kept) elapsed -> 
                    logInfo_ logger [i|Done with cache GC, deleted #{deleted} objects, kept #{kept}, took #{elapsed}ms|])

        cleanOldVersions worldVersion = do
            let now = versionToMoment worldVersion
            executeOrDie 
                (deleteOldVersions database $ versionIsOld now oldVersionsLifetime)
                (\deleted elapsed -> 
                    logInfo_ logger [i|Done with deleting older versions, deleted #{deleted} versions, took #{elapsed}ms|])

        -- defragment worldVersion = do
        --     (_, elapsed) <- timedMS $ runMaintenance appContext 
        --     logInfo_ logger [i|Done with defragmenting the storage, version #{worldVersion}, took #{elapsed}ms|]

        executeOrDie :: IO a -> (a -> Int64 -> IO ()) -> IO ()
        executeOrDie f onRight = 
            exec `catches` [
                    Handler $ \(AppException seriousProblem) ->
                        die [i|Something really bad happened #{seriousProblem}, exiting.|],
                    Handler $ \(_ :: AsyncCancelled) ->
                        die [i|Interrupted with Ctrl-C, exiting.|],                        
                    Handler $ \(weirdShit :: SomeException) ->
                        logError_ logger [i|Something weird happened #{weirdShit}, exiting.|]
                ] 
            where
                exec = do 
                    (r, elapsed) <- timedMS f
                    onRight r elapsed

        initRtrIfNeeded = 
            case rtrConfig of 
                Nothing -> do 
                    pure $ \_ -> pure ()
                Just rtrConfig' -> 
                    pure $ \_ -> runRtrServer appContext rtrConfig' 


-- | Load the state corresponding to the last completed version.
-- 
loadStoredAppState :: Storage s => AppContext s -> IO ()
loadStoredAppState AppContext {..} = do     
    Now now' <- thisInstant    
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
    roTx database $ \tx -> 
        getLastCompletedVersion database tx >>= \case 
            Nothing          -> pure ()

            Just lastVersion             
                | versionIsOld now' revalidationInterval lastVersion ->                     
                    logInfo_ logger [i|Last cached version #{lastVersion} is too old to be used.|]

                | otherwise -> do 
                    (vrps, elapsed) <- timedMS $ do             
                        -- TODO It takes 350ms, which is pretty strange, profile it.           
                        !vrps <- getVrps tx database lastVersion
                        atomically $ do
                            completeCurrentVersion appState                            
                            writeTVar (appState ^. #currentVrps) (Set.fromList vrps)                        
                        pure vrps
                    logInfo_ logger $ [i|Last cached version #{lastVersion} used to initialise |] <> 
                                      [i|current state (#{length vrps} VRPs), took #{elapsed}ms.|]
                

-- 
versionIsOld :: Instant -> Seconds -> WorldVersion -> Bool
versionIsOld now period (WorldVersion nanos) =
    let validatedAt = fromNanoseconds nanos
    in not $ closeEnoughMoments validatedAt now period

-- | Execute an IO action every N seconds
-- 
periodically :: Seconds -> IO () -> IO ()
periodically (Seconds interval) action =
    forever $ do 
        Now start <- thisInstant        
        action
        Now end <- thisInstant
        let executionTimeNs = toNanoseconds end - toNanoseconds start
        when (executionTimeNs < nanosPerSecond * interval) $ do        
            let timeToWaitNs = nanosPerSecond * interval - executionTimeNs                        
            when (timeToWaitNs > 0) $ 
                threadDelay $ (fromIntegral timeToWaitNs) `div` 1000         

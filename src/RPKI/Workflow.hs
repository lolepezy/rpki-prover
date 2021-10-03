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

import           System.Exit

import           RPKI.AppState
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
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
import           RPKI.Util (ifJust)


type AppLmdbEnv = AppContext LmdbStorage


runWorkflow :: (Storage s, MaintainableStorage s) => 
                AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do
    -- Use a command queue to avoid fully concurrent operations, i.e. cleanup 
    -- operations and such should not run at the same time with validation 
    -- (not only for consistency reasons, but we also want to avoid locking 
    -- the DB for long time by a cleanup process).
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
            generatePeriodicTask (12 * 60 * 60 * 1_000_000) storageCompactionInterval compact,
            rtrServer   
        ]
    where
        storageCompactionInterval = config ^. #storageCompactionInterval
        cacheCleanupInterval = config ^. #cacheCleanupInterval
        oldVersionsLifetime  = config ^. #oldVersionsLifetime
        cacheLifeTime        = config ^. #cacheLifeTime
        revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
        rtrConfig            = config ^. #rtrConfig

        -- periodically update world version and generate command 
        -- to re-validate all TAs
        generateNewWorldVersion prometheusMetrics globalQueue = do            
            periodically revalidationInterval $ do 
                oldWorldVersion <- getWorldVerionIO appState
                newWorldVersion <- updateWorldVerion appState
                logDebug_ logger [i|Generated new world version, #{oldWorldVersion} ==> #{newWorldVersion}.|]                
                atomically $ do 
                    writeCQueue globalQueue (validateAllTAs prometheusMetrics newWorldVersion)                    
                    pure Repeat

            atomically $ closeCQueue globalQueue

        generatePeriodicTask delay interval whatToDo globalQueue = do
            threadDelay delay
            periodically interval $ do
                worldVersion <- getWorldVerionIO appState
                atomically $ do 
                    closed <- isClosedCQueue globalQueue
                    unless closed $ 
                        writeCQueue globalQueue (whatToDo worldVersion)
                    pure $ if closed 
                            then Done
                            else Repeat

        taskExecutor globalQueue = go 
          where
            go = do 
                z <- atomically (readCQueue globalQueue)
                ifJust z $ \task -> task >> go
        
        validateAllTAs prometheusMetrics worldVersion = do
            logInfo_ logger [i|Validating all TAs, world version #{worldVersion} |]
            database' <- readTVarIO database 
            executeOrDie
                (processTALs database' tals)
                (\vrps elapsed ->                    
                    logInfoM logger [i|Validated all TAs, got #{Set.size vrps} VRPs, took #{elapsed}ms|])
            where 
                processTALs database' tals = do
                    TopDownResult {..} <- addTotalValidationMetric . mconcat <$> 
                                validateMutlipleTAs appContext worldVersion tals                        

                    updatePrometheus (topDownValidations ^. typed) prometheusMetrics                    
                    
                    rwTx database' $ \tx -> do     
                        let vrpsFromAllTAs = allVrps vrps                                      
                        putValidations tx (validationsStore database') worldVersion (topDownValidations ^. typed)
                        putMetrics tx (metricStore database') worldVersion (topDownValidations ^. typed)
                        putVrps tx database' vrps worldVersion
                        completeWorldVersion tx database' worldVersion
                        atomically $ completeCurrentVersion appState vrps
                        pure vrpsFromAllTAs

        cacheGC worldVersion = do
            let now = versionToMoment worldVersion
            database' <- readTVarIO database 
            executeOrDie 
                (cleanObjectCache database' $ versionIsOld now cacheLifeTime)
                (\CleanUpResult {..} elapsed -> 
                    logInfo_ logger $ [i|Cleanup: deleted #{deletedObjects} objects, kept #{keptObjects}, |] <> 
                                      [i|deleted #{deletedURLs} dangling URLs, took #{elapsed}ms|])

        cleanOldVersions worldVersion = do
            let now = versionToMoment worldVersion
            database' <- readTVarIO database 
            executeOrDie 
                (deleteOldVersions database' $ versionIsOld now oldVersionsLifetime)
                (\deleted elapsed -> 
                    logInfo_ logger [i|Done with deleting older versions, deleted #{deleted} versions, took #{elapsed}ms|])

        compact worldVersion = do
            (_, elapsed) <- timedMS $ runMaintenance appContext 
            logInfo_ logger [i|Done with compacting the storage, version #{worldVersion}, took #{elapsed}ms|]

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
    database' <- readTVarIO database 
    roTx database' $ \tx -> 
        getLastCompletedVersion database' tx >>= \case 
            Nothing          -> pure ()

            Just lastVersion             
                | versionIsOld now' revalidationInterval lastVersion ->                     
                    logInfo_ logger [i|Last cached version #{lastVersion} is too old to be used, will re-run validation.|]

                | otherwise -> do 
                    (vrps, elapsed) <- timedMS $ do             
                        -- TODO It takes 350ms, which is pretty strange, profile it.           
                        !vrps <- getVrps tx database' lastVersion
                        --
                        ifJust vrps $ atomically . completeCurrentVersion appState
                        pure vrps
                    ifJust vrps $ \v -> 
                        logInfo_ logger $ [i|Last cached version #{lastVersion} used to initialise |] <> 
                                          [i|current state (#{vrpCount v} VRPs), took #{elapsed}ms.|]
                

-- 
versionIsOld :: Instant -> Seconds -> WorldVersion -> Bool
versionIsOld now period (WorldVersion nanos) =
    let validatedAt = fromNanoseconds nanos
    in not $ closeEnoughMoments validatedAt now period


-- | Execute an IO action every N seconds
periodically :: Seconds -> IO NextStep -> IO ()
periodically (Seconds interval) action = go 
  where
    go = do 
        Now start <- thisInstant        
        nextStep <- action
        Now end <- thisInstant
        let executionTimeNs = toNanoseconds end - toNanoseconds start
        when (executionTimeNs < nanosPerSecond * interval) $ do        
            let timeToWaitNs = nanosPerSecond * interval - executionTimeNs                        
            when (timeToWaitNs > 0) $ 
                threadDelay $ fromIntegral timeToWaitNs `div` 1000         
        case nextStep of 
            Repeat -> go 
            Done   -> pure ()        

data NextStep = Repeat | Done
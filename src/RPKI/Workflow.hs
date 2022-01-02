{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Workflow where

import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad

import           Control.Lens                     ((^.))
import           Data.Generics.Product.Typed

import           Data.Foldable (for_)

import           Data.Int                         (Int64)

import           Data.Hourglass
import           Data.String.Interpolate.IsString

import           System.Exit
import           System.Directory
import           System.FilePath                  ((</>))

import           RPKI.AppState
import           RPKI.AppMonad
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

import           RPKI.Store.AppStorage


runWorkflow :: (Storage s, MaintainableStorage s) => 
                AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do

    -- Use a command queue to avoid fully concurrent operations, i.e. cleanup 
    -- operations and such should not run at the same time with validation 
    -- (not only for consistency reasons, but we also want to avoid locking 
    -- the DB for long time by a cleanup process).
    globalQueue <- newCQueueIO 10    

    -- Fill in the current state if it's not too old.
    -- It is useful in case of restarts.        
    void $ loadStoredAppState appContext

    -- Run RTR server thread when rtrConfig is present in the AppConfig.  
    -- If not needed it will the an noop.  
    rtrServer <- initRtrIfNeeded

    -- Initialise prometheus metrics here
    prometheusMetrics <- createPrometheusMetrics

    -- Run threads that periodicallly generate tasks and one thread that 
    -- executes the tasks. Tasks are put into the queue after having been 
    -- generated. `taskExecutor` picks them up from the queue and executes.
    mapConcurrently_ (\f -> f globalQueue) [ 
            taskExecutor,
            generateRevalidationTask prometheusMetrics, 
            generatePeriodicTask 10_000_000 cacheCleanupInterval cacheGC,
            generatePeriodicTask 30_000_000 cacheCleanupInterval cleanOldVersions,
            generatePeriodicTask (12 * 60 * 60 * 1_000_000) storageCompactionInterval compact,
            -- generatePeriodicTask (1 * 1_000_000) storageCompactionInterval compact,
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
        generateRevalidationTask prometheusMetrics globalQueue = do            
            periodically revalidationInterval (do                                 
                    writeQ globalQueue (validateAllTAs prometheusMetrics)                    
                    pure Repeat)
                `finally`
                atomically (closeCQueue globalQueue)

        taskExecutor globalQueue = go 
          where
            go = do 
                z <- atomically (readCQueue globalQueue)
                for_ z $ \task -> updateWorldVersion >>= task >> go
        
        validateAllTAs prometheusMetrics worldVersion = do
            logInfo_ logger [i|Validating all TAs, world version #{worldVersion} |]
            database' <- readTVarIO database 
            executeOrDie
                (processTALs database' `finally` cleanupAfterValidation)
                (\(vrps, slurmedVrps) elapsed ->                    
                    logInfoM logger $
                        [i|Validated all TAs, got #{estimateVrpCount vrps} VRPs, |] <> 
                        [i|#{estimateVrpCount slurmedVrps} SLURM-ed VRPs, took #{elapsed}ms|])
            where 
                cleanupAfterValidation = do 
                    let tmpDir = config ^. #tmpDirectory
                    logDebugM logger [i|Cleaning up temporary directory #{tmpDir}.|]                    
                    listDirectory tmpDir >>= mapM_ (removePathForcibly . (tmpDir </>))
                    
                processTALs database' = do
                    TopDownResult {..} <- addUniqueVRPCount . mconcat <$> 
                                validateMutlipleTAs appContext worldVersion tals                        

                    updatePrometheus (topDownValidations ^. typed) prometheusMetrics                                    

                    -- Apply SLURM if it is set in the appState
                    (slurmValidations, maybeSlurm) <- 
                        case appState ^. #readSlurm of
                            Nothing       -> pure (mempty, Nothing)
                            Just readFunc -> do 
                                logInfoM logger [i|Re-reading and re-validating SLURM files.|]
                                (z, vs) <- runValidatorT (newValidatorPath "read-slurm") readFunc
                                case z of 
                                    Left e -> do 
                                        logErrorM logger [i|Failed to read apply SLURM files: #{e}|]
                                        pure (vs, Nothing)
                                    Right slurm -> 
                                        pure (vs, Just slurm)

                    -- Save all the results into LMDB
                    let updatedValidation = slurmValidations <> topDownValidations ^. typed
                    rwTx database' $ \tx -> do
                        putValidations tx database' worldVersion (updatedValidation ^. typed)
                        putMetrics tx database' worldVersion (topDownValidations ^. typed)                        
                        putVrps tx database' vrps worldVersion
                        for_ maybeSlurm $ putSlurm tx database' worldVersion
                        completeWorldVersion tx database' worldVersion
                        slurmedVrps <- atomically $ do 
                            setCurrentVersion appState worldVersion
                            completeVersion appState worldVersion vrps maybeSlurm
                        pure (vrps, slurmedVrps)

        cacheGC worldVersion = do            
            database' <- readTVarIO database 
            executeOrDie 
                (cleanObjectCache database' $ versionIsOld (versionToMoment worldVersion) cacheLifeTime)
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


        -- Write an action to the global queue with given interval.
        generatePeriodicTask delay interval action globalQueue = do
            threadDelay delay
            periodically interval $ do                 
                closed <- atomically $ isClosedCQueue globalQueue
                unless closed $ writeQ globalQueue action
                pure $ if closed 
                        then Done
                        else Repeat

        updateWorldVersion = do 
            newVersion <- newWorldVersion      
            existing <- getWorldVerionIO appState          
            logDebug_ logger $ case existing of
                Nothing -> 
                    [i|Generated first world version #{newVersion}.|]                
                Just oldWorldVersion ->
                    [i|Generated new world version, #{oldWorldVersion} ==> #{newVersion}.|]
            pure newVersion


        writeQ globalQueue z = do 
            isFull <- atomically $ ifFullCQueue globalQueue
            when isFull $ logInfoM logger $
                                [i|Task queue is full. Normally that means that revalidation interval |] <>
                                [i|is too short for the time validation takes. It is recommended to restart |] <>
                                [i|with a higher re-validation interval value.|]                                    
            atomically $ writeCQueue globalQueue z                        


-- | Load the state corresponding to the last completed version.
-- 
loadStoredAppState :: Storage s => AppContext s -> IO (Maybe WorldVersion)
loadStoredAppState AppContext {..} = do     
    Now now' <- thisInstant    
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
    database' <- readTVarIO database 
    roTx database' $ \tx -> 
        getLastCompletedVersion database' tx >>= \case 
            Nothing          -> pure Nothing

            Just lastVersion             
                | versionIsOld now' revalidationInterval lastVersion -> do
                    logInfo_ logger [i|Last cached version #{lastVersion} is too old to be used, will re-run validation.|]
                    pure Nothing

                | otherwise -> do                     
                    (vrps, elapsed) <- timedMS $ do             
                        -- TODO It takes 350-400ms, which is pretty strange, profile it.           
                        !vrps  <- getVrps tx database' lastVersion
                        !slurm <- slurmForVersion tx database' lastVersion
                        --
                        for_ vrps $ \vrps' -> void $ 
                                atomically $ do 
                                    setCurrentVersion appState lastVersion
                                    completeVersion appState lastVersion vrps' slurm
                        pure vrps
                    for_ vrps $ \v -> 
                        logInfo_ logger $ [i|Last cached version #{lastVersion} used to initialise |] <> 
                                        [i|current state (#{estimateVrpCount v} VRPs), took #{elapsed}ms.|]
                    pure $ Just lastVersion             
                

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
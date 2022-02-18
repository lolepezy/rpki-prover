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
import           Data.Maybe (mapMaybe)

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
import           RPKI.Metrics.Prometheus
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
    prometheusMetrics <- createPrometheusMetrics config

    -- This is a flag to determine if datbase compaction may be required at all.
    deletedSomeData <- newTVarIO False

    -- Run threads that 
    --  - generate re-validation jobs        
    --  - periodicallly generate jobs and put them into 
    --    the queue based on the last execution time
    --  - take jobs from the queue and execute them
    -- 
    mapConcurrently_ (\f -> f globalQueue) [
            jobExecutor,
            pollJobs deletedSomeData,
            generateRevalidationJob prometheusMetrics,            
            rtrServer
        ]
    where
        -- periodically update world version and generate command 
        -- to re-validate all TAs
        generateRevalidationJob prometheusMetrics globalQueue =
            periodically (config ^. typed @ValidationConfig . #revalidationInterval) 
                    (do
                        writeQ globalQueue (validateAllTAs prometheusMetrics)
                        pure Repeat)
                `finally`
                    atomically (closeCQueue globalQueue)        

        -- 60 seconds resolution is good enough for all the practical purposes here.
        pollJobs deletedSomeData globalQueue = periodically 60 $ do
            
            let availableJobs = [
                        ("gcJob",               config ^. #cacheCleanupInterval, cacheGC deletedSomeData),
                        ("cleanOldVersionsJob", config ^. #storageCompactionInterval, compact deletedSomeData),
                        ("compactionJob",       config ^. #cacheCleanupInterval, cleanOldVersions deletedSomeData)
                    ]

            Now now <- thisInstant
            jobs <- do
                db <- readTVarIO database
                roTx db $ \tx -> allJobs tx db

            let toBeExecuted = flip mapMaybe availableJobs $
                        \(job, interval, action) ->
                            case filter ((==job) . fst) jobs of
                                [] -> Just (job, action)
                                [(_, lastExecuted)]
                                    | lastExecuted > now ||
                                        closeEnoughMoments lastExecuted now interval
                                        -> Nothing
                                    | otherwise
                                        -> Just (job, action)
                                _ -> Nothing

            unless (null toBeExecuted) $
                logDebug_ logger [i|Jobs to be executed: #{map fst toBeExecuted}.|]
            for_ toBeExecuted $ \(jobName, jobF) -> do
                closed <- atomically $ isClosedCQueue globalQueue
                unless closed $
                    writeQ globalQueue $ \worldVersion' -> do
                        logDebug_ logger [i|Executing job #{jobName}.|]
                        jobF worldVersion' `finally` (do
                            Now endTime <- thisInstant
                            -- re-read `db` since it could have been changed by the time the
                            -- job is finished (after compaction, for example)                            
                            db <- readTVarIO database
                            rwTx db $ \tx -> setJobTime tx db jobName endTime)

            closed <- atomically $ isClosedCQueue globalQueue
            pure $ if closed then Done else Repeat

        jobExecutor globalQueue = go
          where
            go = do
                z <- atomically (readCQueue globalQueue)
                for_ z $ \job -> updateWorldVersion >>= job >> go

        validateAllTAs prometheusMetrics worldVersion = do
            logInfo_ logger [i|Validating all TAs, world version #{worldVersion} |]
            database' <- readTVarIO database
            executeOrDie
                (processTALs database' `finally` cleanupAfterValidation)
                (\(vrps, slurmedVrps) elapsed ->
                    logInfoM logger $
                        [i|Validated all TAs, got #{estimateVrpCount vrps} VRPs (probably not unique), |] <>
                        [i|#{estimateVrpCount slurmedVrps} SLURM-ed VRPs, took #{elapsed}ms|])
            where
                cleanupAfterValidation = do
                    let tmpDir = config ^. #tmpDirectory
                    logDebugM logger [i|Cleaning up temporary directory #{tmpDir}.|]
                    listDirectory tmpDir >>= mapM_ (removePathForcibly . (tmpDir </>))

                processTALs database' = do
                    TopDownResult {..} <- addUniqueVRPCount . mconcat <$>
                                validateMutlipleTAs appContext worldVersion tals

                    updatePrometheus (topDownValidations ^. typed) prometheusMetrics worldVersion

                    -- Apply SLURM if it is set in the appState
                    (slurmValidations, maybeSlurm) <-
                        case appState ^. #readSlurm of
                            Nothing       -> pure (mempty, Nothing)
                            Just readFunc -> do
                                logInfoM logger [i|Re-reading and re-validating SLURM files.|]
                                (z, vs) <- runValidatorT (newScopes "read-slurm") readFunc
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

        cacheGC deletedSomeData worldVersion = do
            database' <- readTVarIO database
            executeOrDie
                (cleanObjectCache database' $ versionIsOld (versionToMoment worldVersion) (config ^. #cacheLifeTime))
                (\CleanUpResult {..} elapsed -> do 
                    when (deletedObjects > 0) $ 
                        atomically $ writeTVar deletedSomeData True
                    logInfo_ logger $ [i|Cleanup: deleted #{deletedObjects} objects, kept #{keptObjects}, |] <>
                                      [i|deleted #{deletedURLs} dangling URLs, took #{elapsed}ms.|])

        cleanOldVersions deletedSomeData worldVersion = do
            let now = versionToMoment worldVersion
            database' <- readTVarIO database
            executeOrDie
                (deleteOldVersions database' $ versionIsOld now (config ^. #oldVersionsLifetime))
                (\deleted elapsed -> do 
                    when (deleted > 0) $ 
                        atomically $ writeTVar deletedSomeData True
                    logInfo_ logger [i|Done with deleting older versions, deleted #{deleted} versions, took #{elapsed}ms.|])

        compact deletedSomeData worldVersion = do
            -- Some heuristics first to see if it's obvisouly too early to run compaction 
            deletedSomething <- readTVarIO deletedSomeData
            if deletedSomething then do 
                (_, elapsed) <- timedMS $ runMaintenance appContext
                logInfo_ logger [i|Done with compacting the storage, version #{worldVersion}, took #{elapsed}ms.|]
            else 
                logDebug_ logger [i|Nothing has been deleted from the storage, will not run compaction.|]

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
            case config ^. #rtrConfig of
                Nothing -> do
                    pure $ \_ -> pure ()
                Just rtrConfig' ->
                    pure $ \_ -> runRtrServer appContext rtrConfig'


        -- Write an action to the global queue with given interval.
        -- generatePeriodicJob delay interval action globalQueue = do
        --     threadDelay delay
        --     periodically interval $ do                 
        --         closed <- atomically $ isClosedCQueue globalQueue
        --         unless closed $ writeQ globalQueue action
        --         pure $ if closed 
        --                 then Done
        --                 else Repeat

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
                                [i|Job queue is full. Normally that means that revalidation interval |] <>
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
periodically interval action = go
  where
    go = do
        Now start <- thisInstant
        nextStep <- action
        Now end <- thisInstant
        delayFromTo start end interval
        case nextStep of
            Repeat -> go
            Done   -> pure ()

data NextStep = Repeat | Done

delayFromTo :: Instant -> Instant -> Seconds -> IO ()
delayFromTo start end (Seconds interval) = do
    let executionTimeNs = toNanoseconds end - toNanoseconds start
    when (executionTimeNs < nanosPerSecond * interval) $ do
        let timeToWaitNs = nanosPerSecond * interval - executionTimeNs
        when (timeToWaitNs > 0) $
            threadDelay $ fromIntegral timeToWaitNs `div` 1000           
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

import           Control.Lens
import           Data.Generics.Product.Typed
import           GHC.Generics (Generic)

import           Data.List
import           Data.Foldable (for_)
import           Data.Int                         (Int64)
import qualified Data.Text                        as Text
import qualified Data.Map.Strict as Map
import           Data.Maybe                       (fromMaybe)

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
import           RPKI.Messages
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Metrics.System
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.Validation.TopDown

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Worker

import           RPKI.Store.AppStorage
import           RPKI.SLURM.Types

runWorkflow :: (Storage s, MaintainableStorage s) =>
                AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do

    -- Use a small command queue to avoid fully concurrent operations, i.e. cleanup 
    -- operations and such should not run at the same time with validation 
    -- (not only for consistency reasons, but we also want to avoid locking 
    -- the DB for long time by a cleanup process).
    globalQueue <- newCQueueIO 10

    -- Fill in the current appState if it's not too old.
    -- It is useful in case of restarts. 
    void $ loadStoredAppState appContext

    -- Run RTR server thread when rtrConfig is present in the AppConfig.  
    -- If not needed it will the an noop.  
    let rtrServer = initRtrIfNeeded

    -- Initialise prometheus metrics here
    prometheusMetrics <- createPrometheusMetrics config

    -- Some shared state between the threads for simplicity.
    sharedBetweenJobs <- newTVarIO newShared

    -- Threads that will run periodic jobs and persist timestaps of running them
    -- for consistent scheduling.
    periodicJobs <- periodicJobExecutors sharedBetweenJobs

    let threads = [ 
                -- thread that takes jobs from the queue and executes them sequentially
                jobExecutor,
                -- thread that generate re-validation jobs
                generateRevalidationJob prometheusMetrics,
                -- RTR server job (or a noop)  
                const rtrServer
            ]

    mapConcurrently_ (\f -> f globalQueue) $ threads <> periodicJobs
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

        periodicJobExecutors sharedBetweenJobs = do 
            let availableJobs = [
                    -- These initial delay numbers are pretty arbitrary and chosen based 
                    -- on reasonable order of the jobs.
                    ("gcJob",               10_000_000, config ^. #cacheCleanupInterval, cacheGC sharedBetweenJobs),
                    ("cleanOldPayloadsJob", 20_000_000, config ^. #cacheCleanupInterval, cleanOldPayloads sharedBetweenJobs),
                    ("compactionJob",       30_000_000, config ^. #storageCompactionInterval, compact sharedBetweenJobs),
                    ("rsyncCleanupJob",     60_000_000, config ^. #rsyncCleanupInterval, rsyncCleanup)
                    ]  
            
            persistedJobs <- do
                db <- readTVarIO database
                roTx db $ \tx -> Map.fromList <$> allJobs tx db

            Now now <- thisInstant                   
            
            pure $ 
                flip map availableJobs $ 
                    \(job, defInitialDelay, interval, action) -> 
                        let (initialDelay, jobRun) =  
                                case Map.lookup job persistedJobs of 
                                    Nothing           -> (defInitialDelay, FirstRun)
                                    Just lastExecuted -> (fromIntegral $ leftToWait lastExecuted now interval, RanBefore)
                        
                            jobAction worldVersion = do                                 
                                    action worldVersion jobRun
                                `finally` (do
                                    Now endTime <- thisInstant
                                    -- re-read `db` since it could have been changed by the time the
                                    -- job is finished (after compaction, in particular)                            
                                    db <- readTVarIO database
                                    rwTx db $ \tx -> setJobCompletionTime tx db job endTime)

                        in \queue -> do 
                            let delaySeconds = initialDelay `div` 1_000_000
                            let delayText :: Text.Text = if initialDelay <= 0 
                                    then [i|for ASAP execution (it is #{-delaySeconds}s due)|] 
                                    else [i|with initial delay #{delaySeconds}s|] 
                            logDebug logger [i|Scheduling job '#{job}' #{delayText} and interval #{interval}.|] 
                            generatePeriodicJob initialDelay interval jobAction queue                        
    
        jobExecutor globalQueue = go
          where
            go = do
                z <- atomically (readCQueue globalQueue)
                for_ z $ \job -> updateWorldVersion >>= job >> go

        validateAllTAs prometheusMetrics worldVersion = do
            logInfo logger [i|Validating all TAs, world version #{worldVersion} |]
            db <- readTVarIO database
            executeOrDie
                (processTALs db `finally` cleanupAfterValidation)
                (\(rtrPayloads, slurmedPayloads) elapsed -> do 
                    let vrps = rtrPayloads ^. #vrps
                    let slurmedVrps = slurmedPayloads ^. #vrps
                    logInfo logger $
                        [i|Validated all TAs, got #{estimateVrpCount vrps} VRPs (probably not unique), |] <>
                        [i|#{estimateVrpCount slurmedVrps} SLURM-ed VRPs, took #{elapsed}ms|])
            where
                processTALs db = do
                    (z, workerVS) <- runProcessTALsWorker worldVersion                    
                    case z of 
                        Left e -> do 
                            logError logger [i|Validator process failed: #{e}.|]
                            rwTx db $ \tx -> do
                                saveValidations tx db worldVersion (workerVS ^. typed)
                                saveMetrics tx db worldVersion (workerVS ^. typed)
                                completeWorldVersion tx db worldVersion                            
                            updatePrometheus (workerVS ^. typed) prometheusMetrics worldVersion
                            pure (mempty, mempty)            
                        Right WorkerResult {..} -> do                              
                            let ValidationResult vs maybeSlurm = payload
                            
                            pushSystem logger $ cpuMemMetric "validation" cpuTime maxMemory
                        
                            let topDownState = workerVS <> vs
                            logDebug logger [i|Validation result: 
#{formatValidations (topDownState ^. typed)}.|]
                            updatePrometheus (topDownState ^. typed) prometheusMetrics worldVersion                        
                            
                            reReadAndUpdatePayloads maybeSlurm
                  where
                    reReadAndUpdatePayloads maybeSlurm = do 
                        roTx db (\tx -> getRtrPayloads tx db worldVersion) >>= \case                         
                            Nothing -> do 
                                logError logger [i|Something weird happened, could not re-read VRPs.|]
                                pure (mempty, mempty)
                            Just rtrPayloads -> do                 
                                slurmedPayloads <- atomically $ completeVersion appState worldVersion rtrPayloads maybeSlurm
                                pure (rtrPayloads, slurmedPayloads)                        

                cleanupAfterValidation = do
                    -- Cleanup tmp directory, if some fetchers died abruptly 
                    -- there may be leftover files.
                    let tmpDir = config ^. #tmpDirectory
                    logDebug logger [i|Cleaning up temporary directory #{tmpDir}.|]
                    listDirectory tmpDir >>= mapM_ (removePathForcibly . (tmpDir </>))
                    
                    -- Cleanup reader table of LMDB cache, it may get littered by 
                    -- dead processes, unfinished/killed transaction, etc.
                    -- All these stale transactions are essentially bugs, but it's 
                    -- easier to just clean them up rather then prevent all 
                    -- possible leakages (even if it is possible).
                    cleaned <- cleanUpStaleTx appContext
                    logDebug logger [i|Cleaned #{cleaned} stale readers from LMDB cache.|]                                

        -- Delete objects in the store that were read by top-down validation 
        -- longer than `cacheLifeTime` hours ago.
        cacheGC sharedBetweenJobs worldVersion _ = do            
            executeOrDie
                cleanupUntochedObjects
                (\z elapsed -> 
                    case z of 
                        Left message -> logError logger message
                        Right CleanUpResult {..} -> do 
                            when (deletedObjects > 0) $ 
                                atomically $ modifyTVar' sharedBetweenJobs (#deletedAnythingFromDb .~ True)
                            logInfo logger $ [i|Cleanup: deleted #{deletedObjects} objects, kept #{keptObjects}, |] <>
                                            [i|deleted #{deletedURLs} dangling URLs, took #{elapsed}ms.|])
          where
            cleanupUntochedObjects = do                 
                (z, _) <- runCleapUpWorker worldVersion      
                case z of 
                    Left e                  -> pure $ Left [i|Cache cleanup process failed: #{e}.|]
                    Right WorkerResult {..} -> do 
                        pushSystem logger $ cpuMemMetric "cache-clean-up" cpuTime maxMemory
                        pure $ Right payload                                       

        -- Delete oldest world versions and all the data related to them.
        cleanOldPayloads sharedBetweenJobs worldVersion _ = do
            let now = versionToMoment worldVersion
            db <- readTVarIO database
            executeOrDie
                (deleteOldPayloads db $ versionIsOld now (config ^. #oldVersionsLifetime))
                (\deleted elapsed -> do 
                    when (deleted > 0) $ do
                        atomically $ modifyTVar' sharedBetweenJobs (#deletedAnythingFromDb .~ True)                        
                        logInfo logger [i|Done with deleting older versions, deleted #{deleted} versions, took #{elapsed}ms.|])

        -- Do the custom LMDB compaction
        compact sharedBetweenJobs worldVersion _ = do
            -- Some heuristics first to see if it's obvisouly too early to run compaction:
            -- if we have never deleted anything, there's no fragmentation, so no compaction needed.
            SharedBetweenJobs {..} <- readTVarIO sharedBetweenJobs
            if deletedAnythingFromDb then do 
                (_, elapsed) <- timedMS $ runMaintenance appContext
                logInfo logger [i|Done with compacting the storage, version #{worldVersion}, took #{elapsed}ms.|]
            else 
                logDebug logger [i|Nothing has been deleted from the storage, compaction is not needed.|]

        -- Delete local rsync mirror
        rsyncCleanup _ jobRun =
            executeOrDie
                (case jobRun of 
                    FirstRun -> 
                        -- Do not actually do anything at the very first run.
                        -- Statistically the first run would mean that the application 
                        -- just was installed and started working and it's not very likely 
                        -- that there's already a lot of garbage in the rsync mirror directory.                        
                        pure ()
                    RanBefore -> do
                        let rsyncDir = config ^. #rsyncConf . #rsyncRoot
                        logDebug logger [i|Deleting rsync mirrors in #{rsyncDir}.|]
                        listDirectory rsyncDir >>= mapM_ (removePathForcibly . (rsyncDir </>))
                )
                (\_ elapsed -> 
                    case jobRun of 
                        FirstRun  -> pure ()  
                        RanBefore ->                    
                            logInfo logger [i|Done cleaning up rsync, took #{elapsed}ms.|])

        executeOrDie :: IO a -> (a -> TimeMs -> IO ()) -> IO ()
        executeOrDie f onRight =
            exec `catches` [
                    Handler $ \(AppException seriousProblem) ->
                        die [i|Something really bad happened #{seriousProblem}, exiting.|],
                    Handler $ \(_ :: AsyncCancelled) ->
                        die [i|Interrupted with Ctrl-C, exiting.|],
                    Handler $ \(weirdShit :: SomeException) ->
                        logError logger [i|Something weird happened #{weirdShit}, exiting.|]
                ]
            where
                exec = do
                    (r, elapsed) <- timedMS f
                    onRight r elapsed        

        -- Write an action to the global queue with given interval.
        generatePeriodicJob delay interval action globalQueue = do
            -- Delay may be 0 or negative (the job is long overdue), 
            -- don't wait for anything then
            when (delay > 0) $
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
            logDebug logger $ case existing of
                Nothing ->
                    [i|Generated first world version #{newVersion}.|]
                Just oldWorldVersion ->
                    [i|Generated new world version, #{oldWorldVersion} ==> #{newVersion}.|]
            pure newVersion


        writeQ globalQueue z = do
            isFull <- atomically $ ifFullCQueue globalQueue
            when isFull $ logInfo logger $
                                [i|Job queue is full. Normally that means that revalidation interval |] <>
                                [i|is too short for the time validation takes. It is recommended to restart |] <>
                                [i|with a higher re-validation interval value.|]
            atomically $ writeCQueue globalQueue z

        initRtrIfNeeded = 
            for_ (config ^. #rtrConfig) $ runRtrServer appContext


        runProcessTALsWorker worldVersion = do 
            let talsStr = Text.intercalate "," $ sort $ map (unTaName . getTaName) tals
            let workerId = WorkerId $ "validation:" <> talsStr

            let maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount

            let arguments = 
                    [ worderIdS workerId ] <>
                    rtsArguments [ 
                        rtsN maxCpuAvailable, 
                        rtsA "24m", 
                        rtsAL "128m", 
                        rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #validationWorkerMemoryMb) ]
            
            runValidatorT 
                    (newScopes "validator") $ 
                        runWorker logger config workerId 
                            (ValidationParams worldVersion tals)                        
                            (Timebox $ config ^. typed @ValidationConfig . #topDownTimeout)
                            arguments                                        

        runCleapUpWorker worldVersion = do             
            let workerId = WorkerId "cache-clean-up"
            
            let arguments = 
                    [ worderIdS workerId ] <>
                    rtsArguments [ 
                        rtsN 2, 
                        rtsA "24m", 
                        rtsAL "64m", 
                        rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #cleanupWorkerMemoryMb) ]
            
            runValidatorT 
                    (newScopes "cache-clean-up") $ 
                        runWorker logger config workerId 
                            (CacheCleanupParams worldVersion)
                            (Timebox 300)
                            arguments                                        

-- To be called by the validation worker process
runValidation :: Storage s =>
                AppContext s
            -> WorldVersion
            -> [TAL]
            -> IO (ValidationState, Maybe Slurm)
runValidation appContext@AppContext {..} worldVersion tals = do       
    db <- readTVarIO database

    TopDownResult {..} <- addUniqueVRPCount . mconcat <$>
                validateMutlipleTAs appContext worldVersion tals

    -- Apply SLURM if it is set in the appState
    (slurmValidations, maybeSlurm) <-
        case appState ^. #readSlurm of
            Nothing       -> pure (mempty, Nothing)
            Just readFunc -> do
                logInfo logger [i|Re-reading and re-validating SLURM files.|]
                (z, vs) <- runValidatorT (newScopes "read-slurm") readFunc
                case z of
                    Left e -> do
                        logError logger [i|Failed to read SLURM files: #{e}|]
                        pure (vs, Nothing)
                    Right slurm ->
                        pure (vs, Just slurm)

    -- Save all the results into LMDB
    let updatedValidation = slurmValidations <> topDownValidations ^. typed
    (_, elapsed) <- timedMS $ rwTx db $ \tx -> do
        saveValidations tx db worldVersion (updatedValidation ^. typed)
        saveMetrics tx db worldVersion (topDownValidations ^. typed)
        saveVrps tx db (payloads ^. #vrps) worldVersion
        saveAspas tx db (payloads ^. #aspas) worldVersion
        saveGbrs tx db (payloads ^. #gbrs) worldVersion
        saveBgps tx db (payloads ^. #bgpCerts) worldVersion
        for_ maybeSlurm $ saveSlurm tx db worldVersion
        completeWorldVersion tx db worldVersion

    logDebug logger [i|Saved payloads for the version #{worldVersion} in #{elapsed}ms.|]    
    threadDelay 30_000_000

    pure (updatedValidation, maybeSlurm)


-- To be called from the cache cleanup worker
-- 
runCacheCleanup :: Storage s =>
                AppContext s
                -> WorldVersion                
                -> IO CleanUpResult
runCacheCleanup AppContext {..} worldVersion = do        
    db <- readTVarIO database
    -- Use the latest completed validation moment as a cutting point.
    -- This is to prevent cleaning up objects if they were untouched 
    -- because prover wasn't running for too long.
    cutOffVersion <- roTx db $ \tx -> 
        fromMaybe worldVersion <$> getLastCompletedVersion db tx

    deleteStaleContent db (versionIsOld (versionToMoment cutOffVersion) (config ^. #cacheLifeTime))


-- | Load the state corresponding to the last completed version.
-- 
loadStoredAppState :: Storage s => AppContext s -> IO (Maybe WorldVersion)
loadStoredAppState AppContext {..} = do
    Now now' <- thisInstant
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
    db <- readTVarIO database
    roTx db $ \tx ->
        getLastCompletedVersion db tx >>= \case
            Nothing          -> pure Nothing

            Just lastVersion
                | versionIsOld now' revalidationInterval lastVersion -> do
                    logInfo logger [i|Last cached version #{lastVersion} is too old to be used, will re-run validation.|]
                    pure Nothing

                | otherwise -> do
                    (payloads, elapsed) <- timedMS $ do                                            
                        slurm    <- slurmForVersion tx db lastVersion
                        payloads <- getRtrPayloads tx db lastVersion                        
                        for_ payloads $ \payloads' -> 
                            void $ atomically $ completeVersion appState lastVersion payloads' slurm
                        pure payloads
                    for_ payloads $ \p -> do 
                        let vrps = p ^. #vrps
                        logInfo logger $ [i|Last cached version #{lastVersion} used to initialise |] <>
                                         [i|current state (#{estimateVrpCount vrps} VRPs), took #{elapsed}ms.|]
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
        let pause = leftToWait start end interval
        when (pause > 0) $
            threadDelay $ fromIntegral pause
        case nextStep of
            Repeat -> go
            Done   -> pure ()

leftToWait :: Instant -> Instant -> Seconds -> Int64
leftToWait start end (Seconds interval) = let
    executionTimeNs = toNanoseconds end - toNanoseconds start
    timeToWaitNs = nanosPerSecond * interval - executionTimeNs
    in timeToWaitNs `div` 1000               


data NextStep = Repeat | Done

data SharedBetweenJobs = SharedBetweenJobs { 
        deletedAnythingFromDb :: Bool
    }
    deriving (Show, Eq, Ord, Generic)

data JobRun = FirstRun | RanBefore
    deriving (Show, Eq, Ord, Generic)  

newShared :: SharedBetweenJobs
newShared = SharedBetweenJobs False

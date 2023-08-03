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
import           Data.List.NonEmpty              (fromList)
import           Data.Foldable                   (for_)
import qualified Data.Text                       as Text
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import qualified Deque.Strict                    as Deq
import           Data.Maybe                      (fromMaybe)
import           Data.Int                        (Int64)
import           Data.Hourglass

import           Data.String.Interpolate.IsString
import           System.Exit
import           System.Directory
import           System.FilePath                  ((</>))

import           UnliftIO.Async                   (pooledForConcurrently)

import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Messages
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Fetch
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


-- A job run can be the first one or not and 
-- sometimes we need this information.
data JobRun = FirstRun | RanBefore
    deriving (Show, Eq, Ord, Generic)  

-- Some jobs are dangerous to run with anything else
newtype JobLock = JobLock (TVar Bool)

newJobLock :: STM JobLock
newJobLock = JobLock <$> newTVar False

exclusiveJob :: JobLock -> IO a -> IO a
exclusiveJob (JobLock t) f = 
    bracket lock unlock (const f)
  where 
    lock     = atomically $ readTVar t >>= (`when` retry)
    unlock _ = atomically $ writeTVar t False

data WorkflowShared = WorkflowShared { 
        -- Indicates if anything was ever deleted from the DB,
        -- it is needed for cleanup and compaction procedures.
        deletedAnythingFromDb :: TVar Bool,

        -- Use a small command queue to avoid fully concurrent operations, i.e. cleanup 
        -- operations and such should not run at the same time with validation 
        -- (not only for consistency reasons, but we also want to avoid locking 
        -- the DB for long time by a cleanup process).        
        globalQueue :: ClosableQueue (WorldVersion -> IO ()),

        -- The same type of the separate queue for the async fetcher.
        asyncFetchQueue :: ClosableQueue (WorldVersion -> IO ()),

        -- This lock is to be used by compaction and async-fetch jobs,
        -- since they should never run at the same time.
        exclusiveLock :: JobLock
    }
    deriving (Generic)

newWorkflowShared :: STM WorkflowShared
newWorkflowShared = WorkflowShared 
            <$> newTVar False 
            <*> newCQueue 10 
            <*> newCQueue 2 
            <*> newJobLock


-- The main entry point for the whole validator workflow. Runs multiple threads, 
-- running validation, RTR server, cleanups, cache maintenance and async fetches.
-- 
runWorkflow :: (Storage s, MaintainableStorage s) =>
                AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do    

    -- Some shared state between the threads for simplicity.
    workflowShared <- atomically newWorkflowShared

    -- Initialise prometheus metrics here
    prometheusMetrics <- createPrometheusMetrics config

    -- Threads that will run periodic jobs and persist timestaps of running them
    -- for consistent scheduling.
    periodicJobs <- periodicJobStarters workflowShared

    -- Fill in the current appState if it's not too old.
    -- It is useful in case of restarts. 
    void $ loadStoredAppState appContext

    let threads = [ 
                -- thread that takes jobs from the queue and executes them sequentially
                jobExecutor,
                -- thread that generates re-validation jobs
                generateRevalidationJob prometheusMetrics,                                
                -- Run RTR server thread when rtrConfig is present in the AppConfig.      
                const initRtrIfNeeded            
            ]

    mapConcurrently_ (\f -> f workflowShared) $ threads <> periodicJobs
    where

        -- The main loop of the validator: periodically 
        -- generate a command to re-validate all TAs.        
        generateRevalidationJob prometheusMetrics WorkflowShared {..} =
            periodically (config ^. typed @ValidationConfig . #revalidationInterval) 
                    (do                        
                        writeQ globalQueue $ \_ -> do 
                            -- Validate all TAs top-down
                            validateAllTAs prometheusMetrics =<< createWorldVersion
                            -- After every validation run async repository fetcher   
                            -- for slow/unstable/timed out repositories                       
                            runAsyncFetcherIfNeeded
                        pure Repeat)
                `finally`
                    atomically (closeCQueue globalQueue)        
          where
            runAsyncFetcherIfNeeded = 
                for_ (config ^. #validationConfig . #asyncFetchConfig) $ \_ -> do                     
                    writeQIfPossible asyncFetchQueue $ \worldVersion -> 
                        -- aquire the same lock as the compaction job to not 
                        -- allow async fetches during LMDB compaction
                        exclusiveJob exclusiveLock $ do 
                            logDebug logger [i|Running async fetch job.|] 
                            validations <- runFetches appContext
                            db <- readTVarIO database
                            rwTx db $ \tx -> do
                                saveValidations tx db worldVersion (validations ^. typed)
                                saveMetrics tx db worldVersion (validations ^. typed)
                                asyncFetchWorldVersion tx db worldVersion                  
                            logDebug logger [i|Finished async fetch job.|]

        periodicJobStarters workflowShared = do 
            let availableJobs = [
                    -- These initial delay numbers are pretty arbitrary and chosen based 
                    -- on reasonable order of the jobs.
                    ("gcJob",               10_000_000, config ^. #cacheCleanupInterval, cacheGC workflowShared),
                    ("cleanOldPayloadsJob", 20_000_000, config ^. #cacheCleanupInterval, cleanOldPayloads workflowShared),
                    ("compactionJob",       30_000_000, config ^. #storageCompactionInterval, compact workflowShared),
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
                        
                            jobWrapper worldVersion = do                                 
                                    action worldVersion jobRun
                                `finally` (do
                                    Now endTime <- thisInstant
                                    -- re-read `db` since it could have been changed by the time the
                                    -- job is finished (after compaction, in particular)                            
                                    db <- readTVarIO database
                                    rwTx db $ \tx -> setJobCompletionTime tx db job endTime)

                        in \WorkflowShared {..} -> do 
                            let delaySeconds = initialDelay `div` 1_000_000
                            let delayText :: Text.Text = if initialDelay <= 0 
                                    then [i|for ASAP execution (it is #{-delaySeconds}s due)|] 
                                    else [i|with initial delay #{delaySeconds}s|] 
                            logDebug logger [i|Scheduling job '#{job}' #{delayText} and interval #{interval}.|] 
                            generatePeriodicJob initialDelay interval jobWrapper globalQueue
    
        jobExecutor WorkflowShared {..} = 
            void $ concurrently (go globalQueue) (go asyncFetchQueue)
          where
            go queue = do
                z <- atomically $ readCQueue queue
                for_ z $ \job -> do 
                    createWorldVersion >>= job
                    go queue

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
                    ((z, workerVS), workerId) <- runProcessTALsWorker worldVersion                    
                    case z of 
                        Left e -> do 
                            logError logger [i|Validator process failed: #{e}.|]
                            rwTx db $ \tx -> do
                                saveValidations tx db worldVersion (workerVS ^. typed)
                                saveMetrics tx db worldVersion (workerVS ^. typed)
                                completeWorldVersion tx db worldVersion                            
                            updatePrometheus (workerVS ^. typed) prometheusMetrics worldVersion
                            pure (mempty, mempty)            
                        Right wr@WorkerResult {..} -> do                              
                            let ValidationResult vs maybeSlurm = payload
                            
                            logWorkerDone logger workerId wr
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
                    when (cleaned > 0) $ 
                        logDebug logger [i|Cleaned #{cleaned} stale readers from LMDB cache.|]                                

        -- Delete objects in the store that were read by top-down validation 
        -- longer than `cacheLifeTime` hours ago.
        cacheGC WorkflowShared {..} worldVersion _ = do            
            executeOrDie
                cleanupUntochedObjects
                (\z elapsed -> 
                    case z of 
                        Left message -> logError logger message
                        Right CleanUpResult {..} -> do 
                            when (deletedObjects > 0) $ 
                                atomically $ writeTVar deletedAnythingFromDb True
                            logInfo logger $ [i|Cleanup: deleted #{deletedObjects} objects, kept #{keptObjects}, |] <>
                                            [i|deleted #{deletedURLs} dangling URLs, took #{elapsed}ms.|])
          where
            cleanupUntochedObjects = do                 
                ((z, _), workerId) <- runCleapUpWorker worldVersion      
                case z of 
                    Left e                    -> pure $ Left [i|Cache cleanup process failed: #{e}.|]
                    Right wr@WorkerResult {..} -> do 
                        logWorkerDone logger workerId wr
                        pushSystem logger $ cpuMemMetric "cache-clean-up" cpuTime maxMemory
                        pure $ Right payload                                       

        -- Delete oldest payloads, e.g. VRPs, ASPAs, validation results, etc.
        cleanOldPayloads WorkflowShared {..} worldVersion _ = do
            let now = versionToMoment worldVersion
            db <- readTVarIO database
            executeOrDie
                (deleteOldPayloads db $ versionIsOld now (config ^. #oldVersionsLifetime))
                (\deleted elapsed -> do 
                    when (deleted > 0) $ do
                        atomically $ writeTVar deletedAnythingFromDb True
                        logInfo logger [i|Done with deleting older versions, deleted #{deleted} versions, took #{elapsed}ms.|])

        -- Do the LMDB compaction
        compact WorkflowShared {..} worldVersion _ = exclusiveJob exclusiveLock $ do
            -- Some heuristics first to see if it's obvisouly too early to run compaction:
            -- if we have never deleted anything, there's no fragmentation, so no compaction needed.
            deletedAnything <- readTVarIO deletedAnythingFromDb
            if deletedAnything then do 
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
        generatePeriodicJob delay interval action queue = do
            -- Delay may be 0 or negative (the job is long overdue), 
            -- don't wait for anything then
            when (delay > 0) $
                threadDelay delay
            periodically interval $ do                 
                closed <- atomically $ isClosedCQueue queue
                unless closed $ writeQ queue action
                pure $ if closed 
                        then Done
                        else Repeat

        createWorldVersion = do
            newVersion <- newWorldVersion
            existing <- getWorldVerionIO appState
            logDebug logger $ case existing of
                Nothing ->
                    [i|Generated first world version #{newVersion}.|]
                Just oldWorldVersion ->
                    [i|Generated new world version, #{oldWorldVersion} ==> #{newVersion}.|]
            pure newVersion


        writeQ queue z = do
            isFull <- atomically $ ifFullCQueue queue
            when isFull $ logInfo logger $
                                [i|Job queue is full. Normally that means that revalidation interval |] <>
                                [i|is too short for the time validation takes. It is recommended to restart |] <>
                                [i|with a higher re-validation interval value.|]
            atomically $ writeCQueue queue z

        writeQIfPossible queue z = atomically $ do
            isFull <- ifFullCQueue queue
            unless isFull $ writeCQueue queue z

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
            
            r <- runValidatorT 
                    (newScopes "validator") $ 
                        runWorker logger config workerId 
                            (ValidationParams worldVersion tals)                        
                            (Timebox $ config ^. typed @ValidationConfig . #topDownTimeout)
                            arguments                                        
            pure (r, workerId)

        runCleapUpWorker worldVersion = do             
            let workerId = WorkerId "cache-clean-up"
            
            let arguments = 
                    [ worderIdS workerId ] <>
                    rtsArguments [ 
                        rtsN 2, 
                        rtsA "24m", 
                        rtsAL "64m", 
                        rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #cleanupWorkerMemoryMb) ]
            
            r <- runValidatorT             
                    (newScopes "cache-clean-up") $ 
                        runWorker logger config workerId 
                            (CacheCleanupParams worldVersion)
                            (Timebox 300)
                            arguments                                                                    
            pure (r, workerId)                            

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
        saveVrps tx db (payloads ^. typed) worldVersion
        saveAspas tx db (payloads ^. typed) worldVersion
        saveGbrs tx db (payloads ^. typed) worldVersion
        saveBgps tx db (payloads ^. typed) worldVersion
        for_ maybeSlurm $ saveSlurm tx db worldVersion
        completeWorldVersion tx db worldVersion

    logDebug logger [i|Saved payloads for the version #{worldVersion} in #{elapsed}ms.|]    

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
        fromMaybe worldVersion <$> getLastValidationVersion db tx

    deleteStaleContent db (versionIsOld (versionToMoment cutOffVersion) (config ^. #cacheLifeTime))


-- | Load the state corresponding to the last completed validation version.
-- 
loadStoredAppState :: Storage s => AppContext s -> IO (Maybe WorldVersion)
loadStoredAppState AppContext {..} = do
    Now now' <- thisInstant
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
    db <- readTVarIO database
    roTx db $ \tx ->
        getLastValidationVersion db tx >>= \case
            Nothing  -> pure Nothing

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


{- 
    Run periodic fetches for slow repositories asychronously to the validation process.

    - Periodically check if there're repositories that don't have speed `Quick`
    - Try to refetch these repositories
    - 
-}
runFetches :: Storage s => AppContext s -> IO ValidationState
runFetches appContext@AppContext {..} = do         
    withRepositoriesProcessing adjustedConfig $ \repositoryProcessing -> do

        pps <- readTVarIO $ repositoryProcessing ^. #publicationPoints
        -- We only care about the repositories that are mentioned 
        -- in the last validation.
        let problematicRepositories = filter (\(u, _) -> 
                u `Set.member` (pps ^. #slowRequested)) $ 
                findSpeedProblems pps

        let reposText = Text.intercalate ", " $ map (toText . fst) problematicRepositories
        unless (null problematicRepositories) $ 
            logDebug logger [i|Will try to asynchronously fetch these repositories: #{reposText}|]

        void $ pooledForConcurrently problematicRepositories $ \(url, repository) -> do 

            -- TODO This fiddling is weird and probably redundant
            let pp = case repository of 
                        RsyncR r -> RsyncPP $ r ^. #repoPP
                        RrdpR r  -> RrdpPP r

            let ppAccess = PublicationPointAccess $ fromList [pp]            
            worldVersion <- newWorldVersion
            void $ runValidatorT (newScopes' RepositoryFocus url) $ 
                    fetchPPWithFallback adjustedConfig repositoryProcessing worldVersion ppAccess
            
        validationStateOfFetches repositoryProcessing   
  where
    -- Replace rsync and rrdp timeouts with their async counterparts, 
    -- since fetcher is not aware if it works in sync or async context.
    adjustedConfig = appContext 
        & #config . #rsyncConf %~ (\rc -> rc & #rsyncTimeout .~ rc ^. #asyncRsyncTimeout)
        & #config . #rrdpConf %~ (\rc -> rc & #rrdpTimeout .~ rc ^. #asyncRrdpTimeout)



data Task = ValidationTask
        | CleanupCacheTask
        | LmdbCompactTask
        | CleanupTask
        | AsyncFetchTask

data ParallelQueue = ParallelQueue {
    queue :: TVar (Deq.Deque (Task, IO ()))
}

canRunAtTheSameTime :: Task -> Task -> Bool
canRunAtTheSameTime t1 t2 = compatible t1 t2 || compatible t2 t1
  where
    compatible ValidationTask AsyncFetchTask = True
    compatible ValidationTask AsyncFetchTask = True



data NextStep = Repeat | Done

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


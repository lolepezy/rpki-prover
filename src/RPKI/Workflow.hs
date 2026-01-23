{-# LANGUAGE StrictData           #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Workflow (
    runValidatorWorkflow,
    runValidation,
    runCacheCleanup
) where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.IO.Class

import           Control.Lens hiding (indices, Indexable)
import           Data.Generics.Product.Typed
import           GHC.Generics

import qualified Data.ByteString.Lazy            as LBS

import           Data.Foldable                   (for_)
import qualified Data.Text                       as Text
import qualified Data.List                       as List
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import qualified Data.Map.Monoidal.Strict        as MonoidalMap
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import qualified Data.Vector                     as V
import           Data.Maybe                      (fromMaybe, catMaybes, isJust)
import           Data.Int                        (Int64)
import           Data.Hourglass
import           Data.Time.Clock                 (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Data.IxSet.Typed                as IxSet
import qualified Data.Hashable                   as Hashable

import           Data.String.Interpolate.IsString
import           Numeric.Natural
import           System.Exit
import           System.Directory
import           System.FilePath                  ((</>))
import           System.Posix.Signals

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
import           RPKI.Http.Types
import           RPKI.Http.Dto
import qualified RPKI.Store.Database               as DB
import           RPKI.Validation.TopDown

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.Store.AppStorage
import           RPKI.RRDP.Types
import           RPKI.TAL
import           RPKI.Parallel
import           RPKI.Util                     
import           RPKI.Time
import           RPKI.Worker
import           RPKI.SLURM.Types
import           UnliftIO (pooledForConcurrentlyN)

{- 
    Fully asynchronous execution.

    - Validations are scheduled periodically for all TAs.

    - Validation creates a list of repositories mentioned in the certificates.

    - Every newly discovered repository is added to the fetching machinery

    - After every fetch a validation can be triggered iff
        * there are "significant" updates in the fetch (not MFTs and CRLs only)
        * validation happened not less than N seconds ago
    
    - Fetches must always happen atomically, including snapshot fetches

    - 
-}

-- A job run can be the first one or not and 
-- sometimes we need this information.
data JobRun = FirstRun | RanBefore
    deriving stock (Show, Eq, Ord, Generic)  

data WorkflowShared = WorkflowShared { 
        -- Indicates if anything was ever deleted from the DB
        -- since the start of the server. It helps to avoid 
        -- unnecessary compaction procedures: no deletions 
        -- means no compaction is reqired.
        deletedAnythingFromDb :: TVar Bool,

        -- Currently running tasks, it is needed to keep track which 
        -- tasks can run parallel to each other and avoid race conditions.
        runningTasks :: Tasks,

        -- It's just handy to avoid passing this one as a parameter the whole time
        prometheusMetrics :: PrometheusMetrics,

        -- Looping fetcher threads
        fetchers :: Fetchers,

        -- TAs that need to be revalidated because repositories 
        -- associated with these TAs have been fetched.
        tasToValidate :: TVar (Set TaName),

        -- Earliest expiration time for any object for a TA
        earliestToExpire :: TVar (Map TaName EarliestToExpire),

        tals :: [TAL]
    }
    deriving stock (Generic)


withWorkflowShared :: (MonadBaseControl IO m, MonadIO m, Storage s) 
                    => AppContext s
                    -> PrometheusMetrics 
                    -> [TAL]
                    -> (WorkflowShared -> m b) 
                    -> m b
withWorkflowShared AppContext {..} prometheusMetrics tals f = do
    shared <- liftIO $ atomically $ do 
        deletedAnythingFromDb <- newTVar False
        runningTasks          <- newRunningTasks
        fetchers <- do 
                -- We want to share the fetcheables that are already defined in the appState
                -- to have it gloabally available (in particular available to the REST API)
                let fetcheables = appState ^. #fetcheables
                runningFetchers    <- newTVar mempty
                firstFinishedFetchBy <- newTVar mempty
                uriByTa            <- newTVar mempty
                untrustedFetchSemaphore <- newSemaphore (fromIntegral $ config ^. #parallelism . #fetchParallelism)
                trustedFetchSemaphore   <- newSemaphore (fromIntegral $ config ^. #parallelism . #fetchParallelism)                            
                rsyncPerHostSemaphores  <- newTVar mempty                
                pure $ Fetchers {..}                        

        tasToValidate <- newTVar mempty
        earliestToExpire <- newTVar mempty
        pure WorkflowShared {..}

    f shared `finally`
        liftIO (mask_ $ do
            fs <- atomically $ Map.elems <$> readTVar (shared ^. #fetchers . #runningFetchers)
            for_ fs $ \thread -> Conc.throwTo thread AsyncCancelled)


-- Different types of periodic tasks that may run 
data Task =
    -- top-down validation and sync fetches
    ValidationTask

    -- delete old objects and old versions
    | CacheCleanupTask    

    | LmdbCompactTask

    -- cleanup files in tmp, stale LMDB reader transactions, run-away child processes, etc.
    | LeftoversCleanupTask

    -- async fetches of slow repositories
    | FetchTask

    -- Delete local rsync mirror once in a long while
    | RsyncCleanupTask
    deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)


data Scheduling = Scheduling {        
        initialDelay :: Int,
        interval     :: Seconds,
        taskDef      :: (Task, WorldVersion -> JobRun -> IO ()),
        persistent   :: Bool        
    }
    deriving stock (Generic)

newtype Tasks = Tasks { 
        running :: TVar (Map Task Natural)
    }
    deriving stock (Generic)

newRunningTasks :: STM Tasks
newRunningTasks = Tasks <$> newTVar mempty


-- The main entry point for the whole validator workflow. Runs multiple threads, 
-- running validation, RTR server, cleanups, cache maintenance and async fetches.
-- 
runValidatorWorkflow :: (Storage s, MaintainableStorage s) => AppContext s -> [TAL] -> IO ()
runValidatorWorkflow appContext@AppContext {..} tals = do    
    case config ^. #proverRunMode of     
        ServerMode   -> selfRecoveryLoop 0
        OneOffMode _ -> 
            -- Don't try to automatically resolve DB locking issues in the one-off mode
            runAll appContext tals
                `catches` handlers (die "Database problem: read-write transaction has timed out, exiting.")
  where
    maxRecoveryAttempts = 10 :: Int

    selfRecoveryLoop recoveryAttempts = do
        let txTimeoutHandler = 
                if recoveryAttempts >= maxRecoveryAttempts
                then die [i|Database problem: read-write transaction has timed out #{recoveryAttempts + 1} times, giving up.|]
                else tryToFixStuckDb

        race_ (runAll appContext tals) monitorDbState
            `catches` 
            handlers txTimeoutHandler

        selfRecoveryLoop (recoveryAttempts + 1)

    monitorDbState = forever $ do 
        -- We can get a signal from a worker process that the DB is stuck 
        atomically $ waitForStuckDb appState
        throwIO TxTimeout
    
    tryToFixStuckDb =
        join $ atomically $ do             
            let systemState = appState ^. #systemState
            modifyTVar' systemState (#dbState .~ DbTryingToFix)
            pure $ do 
                logInfo logger "Database read-write transaction has timed out, restarting all workers and reopening storage."
                killAllWorkers appContext                                
                logInfo logger "Killed all worker processes."
                reopenStorage appContext 
                logInfo logger "Database re-opened after deleting the lock file."
                atomically $ modifyTVar' systemState (#dbState .~ DbOperational)
    
    handlers txTimeoutAction = [
            Handler $ \(AppException seriousProblem) ->
                die [i|Something really bad happened: #{seriousProblem}, exiting.|],
            Handler $ \(_ :: TxTimeout) -> 
                txTimeoutAction,
            Handler $ \(_ :: AsyncCancelled) -> 
                die [i|Interrupted with Ctrl-C, exiting.|]            
        ]


runAll :: (Storage s, MaintainableStorage s) =>
                         AppContext s -> [TAL] -> IO ()
runAll appContext@AppContext {..} tals = do    
    void $ concurrently (
            -- Fill in the current appState if it's not too old.
            -- It is useful in case of restarts.             
            loadStoredAppState appContext)
        (do 
            prometheusMetrics <- createPrometheusMetrics config

            withWorkflowShared appContext prometheusMetrics tals $ \workflowShared ->           
                case config ^. #proverRunMode of     
                    ServerMode -> 
                        void $ concurrently                    
                            (concurrently
                                (runScheduledTasks workflowShared)
                                (revalidate workflowShared))
                            runRtrIfConfigured            

                    OneOffMode _ -> 
                        void $ revalidate workflowShared                                  
        )
  where
    allTaNames = map getTaName tals
    
    revalidate workflowShared = do 
        canValidateAgain <- newTVarIO True
        race_ 
            (triggeredValidationLoop canValidateAgain FirstRun)
            periodicallyRevalidateAllTAs

      where
        periodicallyRevalidateAllTAs = do 
            let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
            forever $ do       
                threadDelay $ toMicroseconds revalidationInterval
                atomically $ writeTVar
                    (workflowShared ^. #tasToValidate) (Set.fromList allTaNames)

        triggeredValidationLoop canValidateAgain run = do 
            talsToValidate <- waitForTasToValidate            
            void $ do 
                worldVersion <- newWorldVersion            
                validateTAs workflowShared worldVersion talsToValidate

                case config ^. #proverRunMode of     
                    ServerMode -> scheduleNextAndLoop
                    OneOffMode vrpOutputFile -> do                        
                        canStop <- hasValidatedEverythingForEveryTA workflowShared
                        if canStop then 
                            outputVrps vrpOutputFile
                        else
                            scheduleNextAndLoop
          where
            scheduleNextAndLoop = do 
                void $ forkFinally 
                    (do              
                        -- If this thread leaks, it's not a biggy, it will exit pretty soon           
                        Conc.threadDelay $ toMicroseconds $ config ^. #validationConfig . #minimalRevalidationInterval
                        atomically $ writeTVar canValidateAgain True)
                    (logException logger "Exception in revalidation delay thread")
                triggeredValidationLoop canValidateAgain RanBefore           

            waitForTasToValidate = atomically $ do
                (`unless` retry) =<< readTVar canValidateAgain                
                case run of
                    FirstRun -> reset >> pure tals
                    RanBefore -> do 
                        tas <- readTVar (workflowShared ^. #tasToValidate)
                        when (Set.null tas) retry
                        reset
                        pure $ filter (\tal -> getTaName tal `Set.member` tas) tals                           
              where
                reset = do 
                    writeTVar (workflowShared ^. #tasToValidate) mempty
                    writeTVar canValidateAgain False
                    

    outputVrps vrpOutputFile = do 
        vrps <- roTxT database $ \tx db -> 
                DB.getLatestVersion tx db >>= \case 
                    Nothing            -> pure Nothing
                    Just latestVersion -> Just <$> DB.getVrps tx db latestVersion
        case vrps of
            Nothing -> do
                logWarn logger [i|Don't have any VRPs, exiting.|]                
            Just vrps' ->                 
                LBS.writeFile vrpOutputFile $ unRawCSV $ vrpDtosToCSV $ toVrpDtos vrps'

    hasValidatedEverythingForEveryTA WorkflowShared { fetchers = Fetchers {..}} = do 
        -- check if for every TA the last validation time is later than 
        -- all of the first fetching dates for the repositories
        versions  <- roTxT database DB.getLatestVersions
        fetchedBy <- readTVarIO firstFinishedFetchBy                
        urisByTA  <- readTVarIO uriByTa
        
        let allValidationsAreLaterThanFetches = 
                all (\(ta, validatedBy) -> 
                    case [ Map.lookup uri fetchedBy | uri <- IxSet.indexKeys $ IxSet.getEQ ta urisByTA ] of 
                        [] -> False
                        z  -> let 
                            (fetchedAtLeastOnce, notFetched) = List.partition isJust z
                            in case notFetched of 
                                [] -> all (validatedBy >) $ catMaybes fetchedAtLeastOnce
                                _  -> False
                    ) $ perTA versions

        pure allValidationsAreLaterThanFetches


    schedules workflowShared = [            
            Scheduling {                                 
                initialDelay = 600 * 1_000_000,
                interval = config ^. #cacheCleanupInterval,
                taskDef = (CacheCleanupTask, cacheCleanup workflowShared),
                persistent = True                
            },       
            Scheduling {                 
                initialDelay = 900 * 1_000_000,
                interval = config ^. #storageCompactionInterval,
                taskDef = (LmdbCompactTask, compact workflowShared),
                persistent = True
            },
            Scheduling {             
                initialDelay = 1200 * 1_000_000,
                interval = config ^. #rsyncCleanupInterval,
                taskDef = (RsyncCleanupTask, rsyncCleanup),
                persistent = True
            },
            let interval = config ^. typed @ValidationConfig . #revalidationInterval
            in Scheduling {                 
                initialDelay = toMicroseconds interval `div` 2,                
                taskDef = (LeftoversCleanupTask, \_ _ -> cleanupLeftovers),
                persistent = False,
                ..
            }
        ]              

    -- For each schedule 
    --   * run a thread that would try to run the task periodically 
    --   * run tasks using `runConcurrentlyIfPossible` to make sure 
    --     there is no data races between different tasks
    runScheduledTasks workflowShared = do                
        persistedJobs <- roTxT database $ \tx db -> Map.fromList <$> DB.allJobs tx db

        Now now <- thisInstant
        forConcurrently (schedules workflowShared) $ \Scheduling { taskDef = (task, action), ..} -> do                        
            let name = fmtGen task
            let (delay, jobRun0) =                  
                    if persistent
                    then case Map.lookup name persistedJobs of 
                        Nothing -> 
                            (initialDelay, FirstRun)
                        Just lastExecuted -> 
                            (fromIntegral $ leftToWaitMicros (Earlier lastExecuted) (Later now) interval, RanBefore)
                    else (initialDelay, FirstRun)

            let delayInSeconds = delay `div` 1_000_000
            let delayText :: Text.Text = 
                    case () of 
                      _ | delay == 0 -> [i|for ASAP execution|] 
                        | delay < 0  -> [i|for ASAP execution (it is #{-delayInSeconds}s due)|] 
                        | otherwise  -> [i|with initial delay #{delayInSeconds}s|]                     
            logDebug logger [i|Scheduling task '#{name}' #{delayText} and interval #{interval}.|] 

            when (delay > 0) $
                threadDelay delay

            let actualAction jobRun = do
                    logDebug logger [i|Running task '#{name}'.|]
                    worldVersion <- newWorldVersion
                    action worldVersion jobRun 
                        `finally` (do  
                            when persistent $ do                       
                                Now endTime <- thisInstant
                                -- re-read `db` since it could have been changed by the time the
                                -- job is finished (after compaction, in particular)                                                                
                                rwTxT database $ \tx db' -> DB.setJobCompletionTime tx db' name endTime
                            updateMainResourcesStat
                            logDebug logger [i|Done with task '#{name}'.|])    

            periodically interval jobRun0 $ \jobRun -> do                 
                runConcurrentlyIfPossible logger task (workflowShared ^. #runningTasks) (actualAction jobRun) 
                pure RanBefore

    updateMainResourcesStat = do 
        (cpuTime, maxMemory) <- processStat
        SystemInfo {..} <- readTVarIO $ appState ^. #system
        Now now <- thisInstant
        let clockTime = durationMs startUpTime now
        pushSystem logger $ cpuMemMetric "root" cpuTime clockTime maxMemory        

    validateTAs workflowShared worldVersion talsToValidate = do  
        let taNames = map getTaName talsToValidate
        logInfo logger [i|Validating TAs #{taNames}, world version #{worldVersion} |]
        
        ((rtrPayloads, slurmedPayloads), elapsed) <- timedMS processTALs            
        let vrps = rtrPayloads ^. #vrps
        let slurmedVrps = slurmedPayloads ^. #vrps
        logInfo logger $
            [i|Validated TAs #{taNames}, got #{estimateVrpCount vrps} VRPs (probably not unique), |] <>
            [i|#{estimateVrpCount slurmedVrps} SLURM-ed VRPs, took #{elapsed}ms|]
      where
        processTALs = do
            ((z, workerVS), workerId) <- runValidationWorker worldVersion talsToValidate            
            let reportError message = do 
                    logError logger message
                    rwTxT database $ \tx db -> do
                        DB.saveValidationVersion tx db worldVersion allTaNames mempty workerVS
                    updatePrometheus (workerVS ^. typed) (workflowShared ^. #prometheusMetrics) worldVersion
                    pure (mempty, mempty)

            case z of 
                Left e -> 
                    reportError [i|Validator process failed: #{e}.|]                    

                Right wr@WorkerResult {..} -> do     
                    case payload of 
                        Left (ErrorResult message) ->
                            reportError [i|Validator process failed: #{message}.|]

                        Right (ValidationResult vs discovered maybeSlurm) -> do                                             
                            adjustFetchers appContext (fmap fst discovered) workflowShared
                            scheduleRevalidationOnExpiry appContext (fmap snd discovered) workflowShared
                            
                            logWorkerDone logger workerId wr
                            pushSystem logger $ cpuMemMetric "validation" cpuTime clockTime maxMemory
                        
                            let topDownState = workerVS <> vs
                            logDebug logger [i|Validation result: 
#{formatValidations (topDownState ^. typed)}.|]
                            updatePrometheus (topDownState ^. typed) (workflowShared ^. #prometheusMetrics) worldVersion                        
                            
                            (!q, elapsed) <- timedMS $ reReadAndUpdatePayloads maybeSlurm
                            logDebug logger [i|Re-read payloads, took #{elapsed}ms.|]
                            pure q
          where
            reReadAndUpdatePayloads maybeSlurm = do 
                roTxT database (\tx db -> DB.getRtrPayloads tx db worldVersion) >>= \case                         
                    Nothing -> do 
                        logError logger [i|Something weird happened, could not re-read VRPs.|]
                        pure (mempty, mempty)
                    Just rtrPayloads -> atomically $ do                                                    
                        slurmedPayloads <- completeVersion appState worldVersion rtrPayloads maybeSlurm 
                        when (config ^. #withValidityApi) $
                            updatePrefixIndex appState slurmedPayloads
                        pure (rtrPayloads, slurmedPayloads)
                          
    -- Delete objects in the store that were read by top-down validation 
    -- longer than `shortLivedCacheLifeTime` hours ago.
    cacheCleanup workflowShared worldVersion _ = do
        (r, elapsed) <- timedMS cleanupOldObjects
        case r of 
            Left message -> logError logger message
            Right DB.CleanUpResult {..} -> do 
                when (deletedObjects > 0) $ do
                    atomically $ writeTVar (workflowShared ^. #deletedAnythingFromDb) True
                let perType :: String = if mempty /= deletedPerType 
                    then [i|in particular #{Map.toList deletedPerType}, |] 
                    else ""
                logInfo logger $ [i|Cleanup: deleted #{deletedObjects} objects, #{perType}kept #{keptObjects}, |] <>
                                 [i|deleted #{deletedURLs} dangling URLs, #{deletedVersions} old versions, took #{elapsed}ms.|]
      where
        cleanupOldObjects = do                 
            ((z, _), workerId) <- runCleanUpWorker worldVersion      
            case z of 
                Left e -> pure $ Left [i|Cache cleanup process failed: #{e}.|]
                Right wr@WorkerResult {..} -> do 
                    case payload of 
                        Left (ErrorResult message) ->
                            pure $ Left [i|Cache cleanup process failed: #{message}.|]
                        Right r -> do
                            logWorkerDone logger workerId wr
                            pushSystem logger $ cpuMemMetric "cache-clean-up" cpuTime clockTime maxMemory
                            pure $ Right r

    -- Do LMDB compaction
    compact workflowShared worldVersion _ = do
        -- Some heuristics first to see if it's obvisouly too early to run compaction:
        -- if we have never deleted anything, there's no fragmentation, so no compaction needed.
        deletedAnything <- readTVarIO $ workflowShared ^. #deletedAnythingFromDb
        if deletedAnything then do
            (_, elapsed) <- timedMS $ runMaintenance appContext
            logInfo logger [i|Done with compacting the storage, version #{worldVersion}, took #{elapsed}ms.|]
        else 
            logDebug logger [i|Nothing has been deleted from the storage, compaction is not needed.|]

    -- Delete temporary files and LMDB stale reader transactions
    cleanupLeftovers = do
        -- Cleanup tmp directory, if some fetchers died abruptly 
        -- there may be leftover files.        
        let tmpDir = configValue $ config ^. #tmpDirectory
        logDebug logger [i|Cleaning up temporary directory #{tmpDir}.|]
        now <- getCurrentTime
        files <- listDirectory tmpDir

        -- Temporary RRDP files cannot meaningfully live longer than that
        let Seconds (fromIntegral -> maxTimeout :: NominalDiffTime) = 
                10 + config ^. #rrdpConf . #rrdpTimeout

        forM_ files $ \file -> 
            ignoreSync $ do 
                let fullPath = tmpDir </> file
                ageInSeconds <- diffUTCTime now <$> getModificationTime fullPath            
                when (ageInSeconds > maxTimeout) $
                    removePathForcibly fullPath                

        -- Cleanup reader table of LMDB cache, it may get littered by 
        -- dead processes, unfinished/killed transaction, etc.
        -- All these stale transactions are essentially bugs, but it's 
        -- easier to just clean them up rather than prevent all 
        -- possible leakages (even if it were possible to prevent them).
        cleaned <- cleanUpStaleTx appContext
        when (cleaned > 0) $ 
            logDebug logger [i|Cleaned #{cleaned} stale readers from LMDB cache.|]
        
        -- Kill all orphan workers (including rsync client processes) that may still
        -- be running and refusing to die. Sometimes an rsync process can leak and 
        -- linger, kill the expired ones
        killWorkers appContext =<< removeExpiredWorkers appState                    

    -- Delete local rsync mirror. The assumption here is that over time there
    -- be a lot of local copies of rsync repositories that are so old that 
    -- the next time they are updated, most of the new repository will be downloaded 
    -- anyway. Since most of the time RRDP is up, rsync updates are rare, so local 
    -- data is mostly stale and just takes disk space.
    rsyncCleanup _ jobRun =
        case jobRun of 
            -- Do not actually do anything at the very first run.
            -- Statistically the first run would mean that the application 
            -- just was installed and started working and it's not very likely 
            -- that there's already a lot of garbage in the rsync mirror directory.                                    
            FirstRun  -> pure ()  
            RanBefore -> do 
                deleteRsyncMirrors `catch` \(SomeException e) -> 
                    logError logger [i|Failed to clean up rsync mirrors: #{e}.|]
      where
        deleteRsyncMirrors = do
            (_, elapsed) <- timedMS $ do                             
                let rsyncDir = configValue $ config ^. #rsyncConf . #rsyncRoot
                logDebug logger [i|Deleting rsync mirrors in #{rsyncDir}.|]
                listDirectory rsyncDir >>= mapM_ (removePathForcibly . (rsyncDir </>))
            logInfo logger [i|Done cleaning up rsync, took #{elapsed}ms.|]

    runRtrIfConfigured = 
        for_ (config ^. #rtrConfig) $ runRtrServer appContext


    -- Workers for functionality running in separate processes.
    --     
    runValidationWorker worldVersion talsToValidate = do 
        let talsStr = Text.intercalate "," $ List.sort $ map (unTaName . getTaName) talsToValidate                    
            workerId = WorkerId [i|version:#{worldVersion}:validation:#{talsStr}|]

            maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount

            -- TODO make it a runtime thing, config?
            -- let profilingFlags = [ "-p", "-hT", "-l" ]
            profilingFlags = [ ]

            arguments = 
                [ workerIdStr workerId ] <>
                rtsArguments ( 
                    profilingFlags <> [ 
                        rtsN maxCpuAvailable, 
                        rtsA "24m", 
                        rtsAL "128m", 
                        rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #validationWorkerMemoryMb) 
                    ])
        
        r <- runValidatorT 
                (newScopes "validator") $ do 
                    let timeout = config ^. typed @ValidationConfig . #topDownTimeout
                    workerInput <- makeWorkerInput appContext workerId
                                    ValidationParams {..}
                                    (Timebox timeout)
                                    Nothing
                    workerInfo <- newWorkerInfo (GenericWorker "validation") timeout (convert $ workerIdStr workerId)
                    runWorker logger workerInput arguments workerInfo

        pure (r, workerId)

    runCleanUpWorker worldVersion = do             
        let workerId = WorkerId [i|version:#{worldVersion}:cache-clean-up|]
        
        let arguments = 
                [ workerIdStr workerId ] <>
                rtsArguments [ 
                    rtsN 2, 
                    rtsA "24m", 
                    rtsAL "64m", 
                    rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #cleanupWorkerMemoryMb) ]
        
        r <- runValidatorT             
                (newScopes "cache-clean-up") $ do
                    let timeout = 300
                    workerInput <- makeWorkerInput appContext workerId
                                        (CacheCleanupParams worldVersion)
                                        (Timebox timeout)
                                        Nothing
                    
                    workerInfo <- newWorkerInfo (GenericWorker "cache-clean-up") timeout (convert $ workerIdStr workerId)
                    runWorker logger workerInput arguments workerInfo
        pure (r, workerId)                            

-- To be called by the validation worker process
runValidation :: Storage s =>
                AppContext s
            -> WorldVersion
            -> [TAL]
            -> [TaName]
            -> IO (ValidationState, Map TaName (Fetcheables, EarliestToExpire), Maybe Slurm)
runValidation appContext@AppContext {..} worldVersion talsToValidate allTaNames = do           

    results <- validateMutlipleTAs appContext worldVersion talsToValidate
        
    -- Apply SLURM if it is set in the appState
    (slurmValidations, maybeSlurm) <- reReadSlurm        

    -- Save all the results into LMDB    
    ((deleted, updatedValidation), elapsed) <- timedMS $ rwTxT database $ \tx db -> do              
                            
        let results' = addVersionPerTA results
        updatedValidation <- addUniqueVrpCountsToMetrics tx db results' slurmValidations                

        let resultsToSave = toPerTA 
                $ map (\(ta, r) -> (ta, (r ^. typed, r ^. typed))) 
                $ Map.toList results'

        DB.saveValidationVersion tx db worldVersion 
            allTaNames resultsToSave updatedValidation                            
     
        for_ maybeSlurm $ DB.saveSlurm tx db worldVersion        
 
        -- We want to keep not more than certain number of latest versions in the DB,
        -- so after adding one, check if the oldest one(s) should be deleted.
        deleted <- DB.deleteOldestVersionsIfNeeded tx db (config ^. #versionNumberToKeep)

        let validations = updatedValidation <> snd (allTAs resultsToSave)

        handleValidations tx db (validations ^. typed)        

        pure (deleted, validations)

    let deletedStr = case deleted of
            [] -> "none"
            _  -> show deleted
    logDebug logger [i|Saved payloads for the version #{worldVersion}, deleted #{deletedStr} oldest version(s) in #{elapsed}ms.|]

    pure (updatedValidation, 
        Map.map (\r -> (r ^. #discoveredRepositories, r ^. #earliestNotValidAfter)) results, 
        maybeSlurm)

  where

    reReadSlurm =
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
    
    addUniqueVrpCountsToMetrics tx db results slurmValidations = do 

        previousVersion <- DB.previousVersion tx db worldVersion        

        vrps <- forM allTaNames $ \taName -> do 
            case Map.lookup taName results of 
                Just p  -> pure (taName, toVrps $ p ^. typed)
                Nothing -> case previousVersion of 
                        Nothing -> pure (taName, mempty) 
                        Just pv -> (taName, ) <$> DB.getVrpsForTA tx db pv taName    
   
        pure $ addUniqueVRPCount (toPerTA vrps) slurmValidations
      where
        -- TODO This is very expensive and _probably_ there's a faster 
        -- way to do it then Set.fromList
        addUniqueVRPCount vrps !vs = let
                vrpCountLens = typed @Metrics . #vrpCounts
                totalUnique = Count (fromIntegral $ uniqueVrpCount vrps)        
                perTaUnique = fmap (Count . fromIntegral . Set.size . Set.fromList . V.toList . unVrps) (unPerTA vrps)   
            in vs & vrpCountLens . #totalUnique .~ totalUnique                
                  & vrpCountLens . #perTaUnique .~ perTaUnique

    addVersionPerTA :: Map TaName TopDownResult -> Map TaName TopDownResult
    addVersionPerTA = 
        fmap $ #topDownValidations . #topDownMetric . #validationMetrics 
                %~ fmap (#validatedBy .~ ValidatedBy worldVersion)         

    -- Here we do anything that needs to be done in case of specific 
    -- fetch/validation issues are present    
    handleValidations tx db validations = do
        forceSnapshotForReferencialIssues tx db validations
        -- other processings if needed

    -- https://github.com/lolepezy/rpki-prover/issues/249
    -- This is to handle referential integrity issues, i.e. manifests referring to 
    -- objects that are not found in the cache. That might be caused by
    -- either a bug in the code, e.g. we GC-ed the referred object too early 
    -- or a problem in the repository. In either case we force snapshot fetch 
    -- to recover repository integrity. This is hacky and should be reconsidered 
    -- in the future, but it works well for now.
    forceSnapshotForReferencialIssues tx db (Validations validations) = do
        Now now <- thisInstant
        for_ (Map.toList repositoriesWithManifestIntegrityIssues) $ \(rrdpUrl, issues) -> do 
            DB.updateRrdpMetaM tx db rrdpUrl $ \case 
                Nothing   -> pure Nothing
                Just meta -> do 
                    let enforcedSnapshot = meta & #enforcement ?~ 
                             NextTimeFetchSnapshot now [i|Manifest integrity issues: #{issues}|]       
                    case meta ^. #enforcement of    
                        Nothing -> do
                            logInfo logger [i|Repository #{rrdpUrl} has integrity issues #{issues}, will force it to re-fetch snapshot.|]
                            pure $ Just enforcedSnapshot

                        -- Don't update the enforcement if it's already set to fetch the snapshot
                        Just n@(NextTimeFetchSnapshot _ _) -> do 
                            logDebug logger [i|Repository #{rrdpUrl} has integrity issues, not changing #{n}.|]
                            pure $ Just meta

                        Just (ForcedSnaphotAt processedAt)
                            -- If the last forced fetch was less than N hours ago, don't do it again
                            | closeEnoughMoments (Earlier processedAt) (Later now)
                                (config ^. #validationConfig . #rrdpForcedSnapshotMinInterval) -> 
                                    pure $ Just meta

                            | otherwise -> do 
                                logInfo logger 
                                    [i|Repository #{rrdpUrl} has integrity issues #{issues}, last forced snapshot fetch was at #{processedAt}, will force it again.|]
                                pure $ Just enforcedSnapshot
      where 
        repositoriesWithManifestIntegrityIssues = 
            Map.fromListWith (<>) [ 
                (relevantRepo, relevantIssues) | 
                    (scope, issues) <- Map.toList validations,                    
                    let relevantIssues = filter manifestIntegrityError (Set.toList issues),
                    not (null relevantIssues),
                    relevantRepo <- mostNarrowPPScope scope
                ]        
          where
            manifestIntegrityError = \case                
                VErr (ValidationE e)             -> isRefentialIntegrityError e
                VWarn (VWarning (ValidationE e)) -> isRefentialIntegrityError e                    
                _                                -> False
              where
                isRefentialIntegrityError = \case
                    MftFallback (ValidationE e) _    -> isRefentialIntegrityError e
                    ManifestEntryDoesn'tExist _ _    -> True
                    NoCRLExists _ _                  -> True                
                    ManifestEntryHasWrongFileType {} -> True                
                    ReferentialIntegrityError _      -> True                
                    _                                -> False

            mostNarrowPPScope (Scope s) = 
                take 1 [ url | PPFocus (RrdpU url) <- NonEmpty.toList s ]


-- | Adjust running fetchers to the latest discovered repositories
-- Updates fetcheables with new ones, creates fetchers for new URLs,
-- and stops fetchers that are no longer needed.
adjustFetchers :: Storage s => AppContext s -> Map TaName Fetcheables -> WorkflowShared -> IO ()
adjustFetchers appContext@AppContext {..} discoveredFetcheables workflowShared@WorkflowShared { fetchers = Fetchers {..} } = do
    (currentFetchers, toStop, toStart) <- atomically $ do            

        -- All the URLs that were discovered by the recent validations of all TAs
        relevantUrls :: Set RpkiURL <- do 
                uriByTa_ <- updateUriPerTa discoveredFetcheables <$> readTVar uriByTa
                writeTVar uriByTa uriByTa_
                pure $ Set.fromList $ IxSet.indexKeys uriByTa_
            
        -- This basically means that we filter all the fetcheables 
        -- (new or current) by being amongst the relevant URLs. 
        modifyTVar' fetcheables $ \currentFetcheables -> 
                Fetcheables $ MonoidalMap.filterWithKey 
                    (\url _ -> url `Set.member` relevantUrls) $ 
                    unFetcheables $ currentFetcheables <> mconcat (Map.elems discoveredFetcheables)

        running <- readTVar runningFetchers
        let runningFetcherUrls = Map.keysSet running

        pure (running, 
              Set.difference runningFetcherUrls relevantUrls,
              Set.difference relevantUrls runningFetcherUrls)        

    -- logDebug logger [i|Adjusting fetchers: toStop = #{toStop}, toStart = #{toStart}, currentFetchers = #{Map.keys currentFetchers}|]

    mask_ $ do
        -- Stop and remove fetchers for URLs that are no longer needed    
        for_ (Set.toList toStop) $ \url ->
            for_ (Map.lookup url currentFetchers) $ \thread -> do
                Conc.throwTo thread AsyncCancelled

        threads <- forM (Set.toList toStart) $ \url ->
            (url, ) <$> forkFinally 
                            (newFetcher appContext workflowShared url)
                            (logException logger [i|Exception in fetcher thread for #{url}|])

        atomically $ do
            modifyTVar' runningFetchers $ \r -> do 
                let addedNewAsyncs = foldr (uncurry Map.insert) r threads
                foldr Map.delete addedNewAsyncs $ Set.toList toStop
                        
-- | Create a new fetcher for the given URL and run it.
newFetcher :: Storage s => AppContext s -> WorkflowShared -> RpkiURL -> IO ()
newFetcher appContext@AppContext {..} WorkflowShared { fetchers = fetchers@Fetchers {..}, ..} url = do
    ignoreSync $ go `finally` dropFetcher fetchers url    
  where
    go = case config ^. #proverRunMode of         
        OneOffMode _ -> void fetchOnce
        ServerMode   -> do 
            Now start <- thisInstant        
            pauseIfNeeded start
            fetchLoop
      where    
        fetchLoop = do 
            Now start <- thisInstant
            fetchOnce >>= \case        
                Nothing -> do                
                    logInfo logger [i|Fetcher for #{url} is not needed and will be deleted.|]
                Just interval -> do 
                    logDebug logger [i|Fetcher for #{url} finished, next fetch in #{interval}.|]
                    Now end <- thisInstant
                    let pause = leftToWaitMicros (Earlier start) (Later end) interval
                    when (pause > 0) $
                        threadDelay $ fromIntegral pause

                    fetchLoop 

        pauseIfNeeded now = do 
            f <- fetchableForUrl 
            for_ f $ \_ -> do 
                r <- roTxT database (\tx db -> DB.getRepository tx db url)
                for_ r $ \repository -> do          
                    let status = getMeta repository ^. #status
                    let lastFetchMoment = 
                            case status of
                                FetchedAt t -> Just t
                                FailedAt t  -> Just t
                                _           -> Nothing
                    
                    for_ lastFetchMoment $ \lastFetch -> do 
                        worldVersion <- newWorldVersion
                        let interval = refreshInterval (newFetchConfig config) repository worldVersion status Nothing 0
                        let pause = leftToWaitMicros (Earlier lastFetch) (Later now) interval                                        
                        when (pause > 0) $ do  
                            let pauseSeconds = pause `div` 1_000_000
                            logDebug logger $ 
                                [i|Fetcher for #{url} finished at #{lastFetch}, now is #{now}, |] <> 
                                [i|interval is #{interval}, first fetch will be paused by #{pauseSeconds}s.|]
                            threadDelay $ fromIntegral pause

    fetchOnce = do 
        fetchableForUrl >>= \case        
            Nothing -> 
                pure Nothing

            Just _ -> do 
                let fetchConfig = newFetchConfig config
                worldVersion <- newWorldVersion
                repository <- fromMaybe (newRepository url) <$> 
                                roTxT database (\tx db -> DB.getRepository tx db url) 

                ((r, validations), duration) <-                 
                        withFetchLimits fetchConfig repository $ timedMS $ 
                            runValidatorT (newScopes' RepositoryFocus url) $ do                                 
                                runConcurrentlyIfPossible logger FetchTask runningTasks 
                                    $ fetchRepository appContext fetchConfig worldVersion repository

                rememberFirstFetchBy worldVersion
                updatePrometheusForRepository url duration prometheusMetrics

                -- TODO Use durationMs, it is the only time metric for failed and killed fetches 
                case r of
                    Right (repository', stats) -> do                         
                        let (updateRepo, interval) = updateRepository fetchConfig
                                repository' worldVersion (FetchedAt (versionToInstant worldVersion)) stats duration

                        saveFetchOutcome updateRepo validations                        
                        triggerTaRevalidationIf $ hasUpdates validations                                                         

                        pure $ Just interval

                    Left _ -> do
                        let newStatus = FailedAt $ versionToInstant worldVersion
                        let (updatedRepo, interval) = updateRepository fetchConfig repository worldVersion newStatus Nothing duration
                        saveFetchOutcome updatedRepo validations

                        fetchableForUrl >>= \case
                            Nothing -> 
                                -- this whole fetcheable is gone
                                pure Nothing
                            Just fallbacks -> do  
                                anyUpdates <- fetchFallbacks fetchConfig worldVersion fallbacks                                                            
                                triggerTaRevalidationIf anyUpdates
                                if anyUpdates 
                                    then do                                        
                                        pure $ Just $ 
                                                case [ () | RsyncU _ <- Set.toList fallbacks ] of                                                     
                                                    -- Not implemented yet, in reality it should never happen, 
                                                    -- fallbacks can only be rsync in forseable future
                                                    [] -> interval
                                                    -- fallbacks managed to get through and it was rsync (duh), 
                                                    -- so it should be a normal rsync interval then                                                    
                                                    _  -> max interval (config ^. #validationConfig . #rsyncRepositoryRefreshInterval)
                                    else 
                                        -- nothing responded, so just go with the normal exponential backoff thing
                                        pure $ Just interval                                        
                                
      where
        fetchFallbacks fetchConfig worldVersion fallbacks = do 
            -- TODO Make it a bit smarter based on the overal number and overall load
            let maxThreads = 32
            repositories <- pooledForConcurrentlyN maxThreads (Set.toList fallbacks) $ \fallbackUrl -> do 

                repository <- fromMaybe (newRepository fallbackUrl) <$> 
                                roTxT database (\tx db -> DB.getRepository tx db fallbackUrl)
                                
                ((r, validations), duration) <- 
                        withFetchLimits fetchConfig repository 
                            $ runConcurrentlyIfPossible logger FetchTask runningTasks                                 
                                $ timedMS
                                $ runValidatorT (newScopes' RepositoryFocus fallbackUrl) 
                                    $ fetchRepository appContext fetchConfig worldVersion repository                

                updatePrometheusForRepository fallbackUrl duration prometheusMetrics
                let repo = case r of
                        Right (repository', _noRrdpStats) -> 
                            -- realistically at this time the only fallback repositories are rsync, so 
                            -- there's no RrdpFetchStat ever
                            updateMeta' repository' (#status .~ FetchedAt (versionToInstant worldVersion))
                        Left _ -> do
                            updateMeta' repository (#status .~ FailedAt (versionToInstant worldVersion))
            
                pure (repo, validations)            

            rwTxT database $ \tx db -> do
                DB.saveRepositories tx db (map fst repositories)
                DB.saveRepositoryValidationStates tx db repositories

            pure $ any (hasUpdates . snd) repositories
        

    fetchableForUrl = do 
        Fetcheables fs <- readTVarIO fetcheables
        pure $ MonoidalMap.lookup url fs


    updateRepository fetchConfig repo worldVersion newStatus stats duration = (updated, interval)
      where
        interval = refreshInterval fetchConfig repo worldVersion newStatus stats duration
        updated = updateMeta' repo 
            (\meta -> meta 
                & #status .~ newStatus 
                & #refreshInterval ?~ interval)

    refreshInterval fetchConfig repository worldVersion newStatus rrdpStats duration = 
        case newStatus of                 
            FailedAt _ -> exponentialBackoff currentInterval
            _ ->       
                case rrdpStats of 
                    Nothing                  -> defaultInterval
                    Just RrdpFetchStat {..} -> 
                        case action of 
                            NothingToFetch _ -> increaseInterval currentInterval 
                            FetchDeltas {..} 
                                | moreThanOne sortedDeltas -> decreaseInterval currentInterval
                                | otherwise                -> currentInterval
                            FetchSnapshot _ _       -> currentInterval
                            ForcedFetchSnapshot _ _ -> currentInterval
      where                                    
        currentInterval = 
            fromMaybe defaultInterval (getMeta repository ^. #refreshInterval)

        exponentialBackoff (Seconds s) = min 
            (Seconds $ s + s `div` 2 + 2 * kindaRandomness) 
            ((fetchConfig ^. #maxFailedBackoffInterval) + 3 * Seconds kindaRandomness)

        moreThanOne = ( > 1) . length . NonEmpty.take 2

        defaultInterval = case repository of 
            RrdpR _  -> config ^. #validationConfig . #rrdpRepositoryRefreshInterval
            RsyncR _ -> config ^. #validationConfig . #rsyncRepositoryRefreshInterval

        increaseInterval (Seconds s) = trimInterval $ Seconds $ s + s `div` 5 + kindaRandomness
        decreaseInterval (Seconds s) = trimInterval $ Seconds $ s - s `div` 3 - kindaRandomness

        minInterval = 
            case repository of                
                -- it's signifantly cheaper to use E-Tag and If_No-Mobified-Since, 
                -- so the interval can be smaller
                RrdpR (RrdpRepository { eTag = Just _ }) -> fetchConfig ^. #minFetchInterval
                _                                        -> 2 * fetchConfig ^. #minFetchInterval

        trimInterval interval = 
            max minInterval 
                (min ((fetchConfig ^. #maxFetchInterval) + Seconds kindaRandomness) interval)  

        -- Pseudorandom stuff is added to spread repositories over time more of less 
        -- uniformly and avoid having peaks of activity. It's just something looking 
        -- relatively random without IO. This one will return a number from 0 to 19.
        kindaRandomness = let 
            h :: Integer = fromIntegral $ Hashable.hash url
            w :: Integer = fromIntegral $ let (WorldVersion w_) = worldVersion in Hashable.hash w_
            d :: Integer = fromIntegral $ unTimeMs duration
            r = (w `mod` 83 + h `mod` 77 + d `mod` 37) `mod` 20
            in fromIntegral r :: Int64

    saveFetchOutcome r validations =
        rwTxT database $ \tx db -> do
            DB.saveRepositories tx db [r]
            DB.saveRepositoryValidationStates tx db [(r, validations)]

    
    withFetchLimits :: FetchConfig -> Repository -> IO a -> IO a
    withFetchLimits fetchConfig repository f = do
        case repository of 
            RrdpR _                   -> rrdpFetch
            RsyncR (getRsyncURL -> r) -> rsyncFetch r
      where
        timeToWait = fetchConfig ^. #fetchLaunchWaitDuration

        semaphoreToUse = 
            case getMeta repository ^. #status of  
                -- TODO Add logic "if succeeded more than N times"
                FetchedAt _ -> fetchers ^. #trustedFetchSemaphore
                _           -> fetchers ^. #untrustedFetchSemaphore

        rrdpFetch = withSemaphoreOrTimeout semaphoreToUse timeToWait f

        rsyncFetch (RsyncURL host _) = do 
            -- Some hosts limit the number of connections, so we need to limit
            -- the number of concurrent rsync fetches per host.            
            rsyncHostSempahore <- atomically $ do        
                    rsyncS <- readTVar rsyncPerHostSemaphores
                    case Map.lookup host rsyncS of 
                        Nothing -> do 
                            s <- newSemaphore $ config ^. #rsyncConf . #rsyncPerHostLimit
                            writeTVar rsyncPerHostSemaphores $ Map.insert host s rsyncS
                            pure s
                        Just s -> 
                            pure s            
            
            withSemaphore rsyncHostSempahore 
                $ withSemaphoreOrTimeout semaphoreToUse timeToWait f
          

    hasUpdates validations = let 
            metrics = validations ^. #topDownMetric
            rrdps = MonoidalMap.elems $ unMetricMap $ metrics ^. #rrdpMetrics
            rsyncs = MonoidalMap.elems $ unMetricMap $ metrics ^. #rsyncMetrics                
        in any (\m -> rrdpRepoHasSignificantUpdates (m ^. typed)) rrdps ||
           any (\m -> rsyncRepoHasSignificantUpdates (m ^. typed)) rsyncs

    triggerTaRevalidationIf condition = atomically $ do 
        case config ^. #proverRunMode of         
            OneOffMode _ -> trigger
            ServerMode   -> when condition trigger                
      where
        trigger = do 
            relevantTas <- Set.fromList . IxSet.indexKeys . IxSet.getEQ url <$> readTVar uriByTa
            modifyTVar' tasToValidate $ (<>) relevantTas            
    
    rememberFirstFetchBy version = atomically $ do 
        fff <- readTVar firstFinishedFetchBy
        when (Map.notMember url fff) $ 
            writeTVar firstFinishedFetchBy $ Map.insert url version fff                   

-- Keep track of the earliest expiration time for each TA (i.e. the earlist time when some object of the TA will expire).
-- Reschedule revalidation of the TA at the moment right after its earliest expiration time. Since this expiration time
-- in practice keeps receding to the future as new objects are added, the revalidation is most likely not needed at all, 
-- that's why we double-check it once again before revalidation.
scheduleRevalidationOnExpiry :: Storage s => AppContext s -> Map TaName EarliestToExpire -> WorkflowShared -> IO ()
scheduleRevalidationOnExpiry AppContext {..} expirationTimes WorkflowShared {..} = do
    Now now <- thisInstant

    -- Filter out TAs for which expiration time hasn't changed
    onlyUpdatedExpirations <- 
        fmap catMaybes $ atomically $ do         
            forM (Map.toList expirationTimes) $ \(taName, expiration) -> do 
                let updateIt = do 
                        modifyTVar' earliestToExpire $ Map.insert taName expiration
                        pure $ Just (taName, expiration)

                m <- readTVar earliestToExpire
                case Map.lookup taName m of
                    Nothing -> updateIt
                    Just e
                        | e == expiration -> pure Nothing
                        | otherwise       -> updateIt

    for_ onlyUpdatedExpirations $ \(taName, expiration@(EarliestToExpire expiresAt)) -> do
        let timeToWait = instantDiff (Earlier now) (Later expiresAt)
        let expiresSoonEnough = timeToWait < config ^. #validationConfig . #revalidationInterval
        when (now < expiresAt && expiration /= mempty && expiresSoonEnough) $ do
            logDebug logger [i|The first object for #{taName} will expire at #{expiresAt}, will schedule re-validation right after.|]
            void $ forkFinally
                    (do
                        threadDelay $ toMicroseconds timeToWait
                        let triggerRevalidation = atomically $ modifyTVar' tasToValidate $ Set.insert taName
                        join $ atomically $ do 
                            e <- readTVar earliestToExpire
                            pure $ case Map.lookup taName e of 
                                Just t 
                                    -- expiration time changed since the trigger was scheduled, 
                                    -- so don't do anything, there're a later trigger for this TA
                                    | t > expiration -> do
                                        logDebug logger [i|Will cancel the re-validation for #{taName} scheduled after expiration at #{expiresAt}, new expiration time is #{t}.|]
                                        pure ()
                                    | otherwise      -> do 
                                        logDebug logger [i|Will not cancel re-validation for #{taName} scheduled after expiration at #{expiresAt}, new expiration time is #{t}.|]
                                        triggerRevalidation
                                Nothing              -> triggerRevalidation
                    )                        
                    (const $ pure ())


-- To be called from the cache cleanup worker
-- 
runCacheCleanup :: Storage s =>
                AppContext s
                -> WorldVersion                
                -> IO DB.CleanUpResult
runCacheCleanup AppContext {..} worldVersion = do        
    db <- readTVarIO database
    -- Use the latest completed validation moment as a cutting point.
    -- This is to prevent cleaning up objects actual object if they were 
    -- untouched because prover was stopped for a long period.
    cutOffVersion <- roTx db $ \tx -> 
        fromMaybe worldVersion <$> DB.getLatestVersion tx db
    
    let cutOffMoment = versionToInstant cutOffVersion
        tooOldLongLived  = versionIsOld cutOffMoment (config ^. #longLivedCacheLifeTime)
        tooOldShortLived = versionIsOld cutOffMoment (config ^. #shortLivedCacheLifeTime)

    DB.deleteStaleContent db DB.DeletionCriteria {
            versionIsTooOld  = tooOldLongLived,
            objectIsTooOld = \version type_ -> 
                case type_ of 
                    -- Most of the object churn happens because of the manifest and CRL updates, 
                    -- so they should be removed from the cache sooner than more long-lived objects
                    MFT -> tooOldShortLived version
                    CRL -> tooOldShortLived version
                    _   -> tooOldLongLived version
        }

-- | Load the state corresponding to the last completed validation version.
-- 
loadStoredAppState :: Storage s => AppContext s -> IO (Maybe WorldVersion)
loadStoredAppState AppContext {..} = do
    Now now' <- thisInstant
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval    
    roTxT database $ \tx db ->
        DB.getLatestVersion tx db >>= \case
            Nothing  -> pure Nothing

            Just lastVersion
                | versionIsOld now' revalidationInterval lastVersion -> do
                    logInfo logger [i|Last cached version #{lastVersion} is too old to be used, will re-run validation.|]
                    pure Nothing

                | otherwise -> do
                    (payloads, elapsed) <- timedMS $ do                                            
                        slurm    <- DB.getSlurm tx db lastVersion
                        payloads <- DB.getRtrPayloads tx db lastVersion                        
                        for_ payloads $ \payloads' -> do 
                            slurmedPayloads <- atomically $ completeVersion appState lastVersion payloads' slurm                            
                            when (config ^. #withValidityApi) $                                
                                -- do it in a separate thread to speed up the startup
                                void $ forkFinally 
                                        (atomically $ updatePrefixIndex appState slurmedPayloads) 
                                        (logException logger [i|Exception in updating prefix index for #{lastVersion}|])
                        pure payloads
                    for_ payloads $ \p -> do 
                        let vrps = p ^. #vrps
                        logInfo logger $ [i|Last cached version #{lastVersion} used to initialise |] <>
                                         [i|current state (#{estimateVrpCount vrps} VRPs), took #{elapsed}ms.|]
                    pure $ Just lastVersion


canRunInParallel :: Task -> Task -> Bool
canRunInParallel t1 t2 = 
    t2 `elem` canRunWith t1 || t1 `elem` canRunWith t2
  where    
    canRunWith = \case 
        ValidationTask       -> allExcept [LmdbCompactTask]

        -- two different fetches can run in parallel, it's fine    
        FetchTask            -> [ValidationTask, FetchTask, CacheCleanupTask]

        CacheCleanupTask     -> [ValidationTask, FetchTask, RsyncCleanupTask]    
        RsyncCleanupTask     -> allExcept [FetchTask]
        LeftoversCleanupTask -> allExcept [LmdbCompactTask]
    
        -- this one can only run alone
        LmdbCompactTask     -> []
  
    allExcept tasks = filter (not . (`elem` tasks)) [minBound..maxBound]
        
    
runConcurrentlyIfPossible :: (MonadIO m, MonadBaseControl IO m) 
                        => AppLogger -> Task -> Tasks -> m a -> m a
runConcurrentlyIfPossible logger taskType Tasks {..} action = do 
    {- 
        Theoretically, exclusive maintenance tasks can starve indefinitely and 
        never get picked up because of fechers running all the time.
        But in practice that is very unlikely to happen, so we'll gamble for now.
    -}    
    let canRunWith runningTasks =             
            all (canRunInParallel taskType . fst) $ Map.toList runningTasks

    (runningTasks, canRun) <- liftIO $ atomically $ do 
                runningTasks <- readTVar running
                pure (Map.toList runningTasks, canRunWith runningTasks)

    unless canRun $ 
        logDebug logger [i|Task #{taskType} cannot run concurrently with #{runningTasks} and has to wait.|]        

    join $ liftIO $ atomically $ do 
        runningTasks_ <- readTVar running
        if canRunWith runningTasks_
            then do 
                writeTVar running $ Map.insertWith (+) taskType 1 runningTasks_
                pure $ action
                        `finally` 
                        liftIO (atomically (modifyTVar' running $ Map.alter (>>= 
                                    (\count -> if count > 1 then Just (count - 1) else Nothing)
                                ) taskType))
            else retry


versionIsOld :: Instant -> Seconds -> WorldVersion -> Bool
versionIsOld now period version =
    let validatedAt = versionToInstant version
    in not $ closeEnoughMoments (Earlier validatedAt) (Later now) period


periodically :: Seconds -> a -> (a -> IO a) -> IO a
periodically interval a0 action = do         
    go a0
  where
    go a = do
        Now start <- thisInstant        
        a' <- action a
        Now end <- thisInstant
        let pause = leftToWaitMicros (Earlier start) (Later end) interval
        when (pause > 0) $
            threadDelay $ fromIntegral pause
        
        go a'        


leftToWaitMicros :: Earlier -> Later -> Seconds -> Int64
leftToWaitMicros (Earlier earlier) (Later later) (Seconds interval) = 
    timeToWaitNs `div` 1000
  where
    executionTimeNs = toNanoseconds later - toNanoseconds earlier
    timeToWaitNs = nanosPerSecond * interval - executionTimeNs    

logException :: MonadIO m => AppLogger -> Text.Text -> Either SomeException a -> m ()
logException logger logText result = 
    case result of
        Left ex -> logDebug logger [i|logException: #{logText}: #{ex}|]
        Right _ -> pure ()

ignoreSync :: MonadBaseControl IO m => m () -> m ()
ignoreSync f =     
    catch f $ \(e :: SomeException) -> 
        case fromException e of
            Just (_ :: SomeAsyncException) -> throwIO e
            Nothing -> pure ()


killAllWorkers :: AppContext s -> IO ()
killAllWorkers appContext@AppContext {..} = do
    killWorkers appContext =<< removeAllRunningWorkers appState    

killWorkers :: AppContext s -> [WorkerInfo] -> IO ()
killWorkers AppContext {..} workers = do
    forConcurrently_ workers $ \WorkerInfo {..} -> do 
        r <- try $ do
                signalProcess killProcess workerPid
                logInfo logger [i|Killed worker process with PID #{workerPid}, #{cli}, it expired at #{endOfLife}.|]
        logException logger [i|Exception in worker process killer thread|] r

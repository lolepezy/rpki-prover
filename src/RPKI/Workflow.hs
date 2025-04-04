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
import           GHC.Generics

import qualified Data.ByteString.Lazy            as LBS

import qualified Data.List.NonEmpty              as NE
import           Data.Foldable                   (for_, toList)
import qualified Data.Text                       as Text
import qualified Data.List                       as List
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import           Data.Maybe                      (fromMaybe, catMaybes)
import           Data.Int                        (Int64)
import           Data.Hourglass

import           Data.String.Interpolate.IsString
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
import           RPKI.RRDP.Types
import           RPKI.Logging
import           RPKI.Metrics.System
import qualified RPKI.Store.Database               as DB
import           RPKI.Validation.TopDown

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.Store.AppStorage
import           RPKI.TAL
import           RPKI.Util                     
import           RPKI.Time
import           RPKI.Worker
import           RPKI.SLURM.Types
import           RPKI.Http.Dto
import           RPKI.Http.Types

-- A job run can be the first one or not and 
-- sometimes we need this information.
data JobRun = FirstRun | RanBefore
    deriving stock (Show, Eq, Ord, Generic)  

data WorkflowShared = WorkflowShared { 
        -- Indicates if anything was ever deleted from the DB
        -- since the start of the server. It helps to avoid 
        -- unnecessary cleanup and compaction procedures.
        deletedAnythingFromDb :: TVar Bool,

        -- Currently running tasks, it is needed to keep track which 
        -- tasks can run parallel to each other and avoid race conditions.
        runningTasks :: RunningTasks,

        prometheusMetrics :: PrometheusMetrics,

        -- Async fetch is a special case, we want to know if it's 
        -- running to avoid launching it until the previous run is done.
        asyncFetchIsRunning :: TVar Bool
    }
    deriving stock (Generic)

newWorkflowShared :: PrometheusMetrics -> STM WorkflowShared
newWorkflowShared prometheusMetrics = WorkflowShared 
            <$> newTVar False             
            <*> newRunningTasks
            <*> pure prometheusMetrics
            <*> newTVar False

-- Different types of periodic tasks that may run 
data TaskType = 
        -- top-down validation and sync fetches
        ValidationTask

        -- delete old objects and old versions
        | CacheCleanupTask    

        | LmdbCompactTask

        -- cleanup files in tmp and stale LMDB reader transactions
        | LeftoversCleanupTask

        -- async fetches of slow repositories
        | AsyncFetchTask

        -- Delete local rsync mirror once in a long while
        | RsyncCleanupTask
        deriving stock (Show, Eq, Ord, Bounded, Enum, Generic)


data Task = Task TaskType (IO ())
    deriving (Generic)

data Scheduling = Scheduling {        
        initialDelay :: Int,
        interval     :: Seconds,
        taskDef      :: (TaskType, WorldVersion -> JobRun -> IO ()),
        persistent   :: Bool        
    }
    deriving stock (Generic)

-- The main entry point for the whole validator workflow. Runs multiple threads, 
-- running validation, RTR server, cleanups, cache maintenance and async fetches.
-- 
runWorkflow :: (Storage s, MaintainableStorage s) =>
                AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do    
    void $ concurrently (
            -- Fill in the current appState if it's not too old.
            -- It is useful in case of restarts.             
            loadStoredAppState appContext)
        (do 
            prometheusMetrics <- createPrometheusMetrics config

            -- Shared state between the threads for simplicity.
            workflowShared <- atomically $ newWorkflowShared prometheusMetrics    

            case config ^. #proverRunMode of     
                OneOffMode vrpOutputFile -> oneOffRun workflowShared vrpOutputFile
                ServerMode ->             
                    void $ concurrently
                            -- Run the main scheduler and RTR server if RTR is configured    
                            (runScheduledTasks workflowShared)
                            runRtrIfConfigured            
        )
  where
    oneOffRun workflowShared vrpOutputFile = do 
        worldVersion <- createWorldVersion
        void $ validateTAs workflowShared worldVersion FirstRun
        vrps <- roTxT database $ \tx db -> 
                DB.getLastValidationVersion db tx >>= \case 
                    Nothing            -> pure Nothing
                    Just latestVersion -> DB.getVrps tx db latestVersion
        case vrps of 
            Nothing -> logWarn logger [i|Don't have any VRPs.|]
            _       -> LBS.writeFile vrpOutputFile $ unRawCSV $ vrpDtosToCSV $ toVrpDtos vrps

    schedules workflowShared = [
            Scheduling {                 
                initialDelay = 0,
                interval = config ^. typed @ValidationConfig . #revalidationInterval,
                taskDef = (ValidationTask, validateTAs workflowShared),
                persistent = False
            },
            let interval = config ^. #cacheCleanupInterval
            in Scheduling { 
                -- do it half of the interval from now, it will be reasonable "on average"
                initialDelay = toMicroseconds interval `div` 2,                
                taskDef = (CacheCleanupTask, cacheCleanup workflowShared),
                persistent = True,
                ..
            },        
            Scheduling {                 
                initialDelay = 1200_000_000,
                interval = config ^. #storageCompactionInterval,
                taskDef = (LmdbCompactTask, compact workflowShared),
                persistent = True
            },
            Scheduling {             
                initialDelay = 1200_000_000,
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
    runScheduledTasks workflowShared@WorkflowShared {..} = do                
        persistedJobs <- roTxT database $ \tx db -> Map.fromList <$> DB.allJobs tx db        

        Now now <- thisInstant
        forConcurrently (schedules workflowShared) $ \Scheduling {..} -> do                        
            let name = fmtGen $ fst taskDef
            let (delay, jobRun0) =                  
                    if persistent
                    then case Map.lookup name persistedJobs of 
                        Nothing -> 
                            (initialDelay, FirstRun)
                        Just lastExecuted -> 
                            (fromIntegral $ leftToWait lastExecuted now interval, RanBefore)
                    else (initialDelay, FirstRun)            

            let delayInSeconds = initialDelay `div` 1_000_000
            let delayText :: Text.Text = 
                    case () of 
                      _ | delay == 0 -> [i|for ASAP execution|] 
                        | delay < 0  -> [i|for ASAP execution (it is #{-delayInSeconds}s due)|] 
                        | otherwise  -> [i|with initial delay #{delayInSeconds}s|]                     
            logDebug logger [i|Scheduling task '#{name}' #{delayText} and interval #{interval}.|] 

            when (delay > 0) $
                threadDelay delay

            let makeTask jobRun = do 
                    Task (fst taskDef) $ do                         
                        logDebug logger [i|Running task '#{name}'.|]
                        worldVersion <- createWorldVersion
                        let action = snd taskDef
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
                runConcurrentlyIfPossible logger (makeTask jobRun) runningTasks                    
                pure RanBefore

    updateMainResourcesStat = do 
        (cpuTime, maxMemory) <- processStat
        SystemInfo {..} <- readTVarIO $ appState ^. #system
        Now now <- thisInstant
        let clockTime = durationMs startUpTime now
        pushSystem logger $ cpuMemMetric "root" cpuTime clockTime maxMemory        

    validateTAs workflowShared@WorkflowShared {..} worldVersion _ = do
        doValidateTAs workflowShared worldVersion 
        `finally`
        (case config ^. #proverRunMode of 
                ServerMode    -> runAsyncFetcherIfNeeded
                OneOffMode {} -> pure ())
      where
        runAsyncFetcherIfNeeded = 
            case config ^. #validationConfig . #fetchMethod of             
                -- no async fetch
                SyncOnly     -> pure ()
                SyncAndAsync -> 
                    join $ atomically $ do 
                        readTVar asyncFetchIsRunning >>= \case                
                            False -> pure $ 
                                void $ forkFinally (do                                
                                        runConcurrentlyIfPossible logger asyncFetchTask runningTasks)
                                        (const $ atomically $ writeTVar asyncFetchIsRunning False)
                            True -> 
                                pure $ pure ()
          where 
            asyncFetchTask = Task AsyncFetchTask $ do 
                atomically (writeTVar asyncFetchIsRunning True)                
                fetchVersion <- createWorldVersion     
                logInfo logger [i|Running asynchronous fetch #{fetchVersion}.|]            
                (_, TimeMs elapsed) <- timedMS $ do 
                    validations <- runAsyncFetches appContext fetchVersion
                    updatePrometheus (validations ^. typed) prometheusMetrics worldVersion
                    rwTxT database $ \tx db -> do
                        DB.saveValidations tx db fetchVersion (validations ^. typed)
                        DB.saveMetrics tx db fetchVersion (validations ^. typed)
                        DB.asyncFetchWorldVersion tx db fetchVersion                                      
                logInfo logger [i|Finished asynchronous fetch #{fetchVersion} in #{elapsed `div` 1000}s.|]


    doValidateTAs WorkflowShared {..} worldVersion = do
        logInfo logger [i|Validating all TAs, world version #{worldVersion} |]
        executeOrDie
            processTALs
            (\(rtrPayloads, slurmedPayloads) elapsed -> do 
                let vrps = rtrPayloads ^. #vrps
                let slurmedVrps = slurmedPayloads ^. #vrps
                logInfo logger $
                    [i|Validated all TAs, got #{estimateVrpCount vrps} VRPs (probably not unique), |] <>
                    [i|#{estimateVrpCount slurmedVrps} SLURM-ed VRPs, took #{elapsed}ms|])
      where
        processTALs = do
            ((z, workerVS), workerId) <- runValidationWorker worldVersion                    
            case z of 
                Left e -> do 
                    logError logger [i|Validator process failed: #{e}.|]
                    rwTxT database $ \tx db -> do
                        DB.saveValidations tx db worldVersion (workerVS ^. typed)
                        DB.saveMetrics tx db worldVersion (workerVS ^. typed)
                        DB.completeValidationWorldVersion tx db worldVersion                            
                    updatePrometheus (workerVS ^. typed) prometheusMetrics worldVersion
                    pure (mempty, mempty)            
                Right wr@WorkerResult {..} -> do                              
                    let ValidationResult vs maybeSlurm = payload
                    
                    logWorkerDone logger workerId wr
                    pushSystem logger $ cpuMemMetric "validation" cpuTime clockTime maxMemory
                
                    let topDownState = workerVS <> vs
                    logDebug logger [i|Validation result: 
#{formatValidations (topDownState ^. typed)}.|]
                    updatePrometheus (topDownState ^. typed) prometheusMetrics worldVersion                        
                    
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
    -- longer than `cacheLifeTime` hours ago.
    cacheCleanup WorkflowShared {..} worldVersion _ = do            
        executeOrDie
            cleanupUntochedObjects
            (\z elapsed -> 
                case z of 
                    Left message -> logError logger message
                    Right DB.CleanUpResult {..} -> do 
                        when (deletedObjects > 0) $ do
                            atomically $ writeTVar deletedAnythingFromDb True
                        logInfo logger $ [i|Cleanup: deleted #{deletedObjects} objects, kept #{keptObjects}, |] <>
                                         [i|deleted #{deletedURLs} dangling URLs, #{deletedVersions} old versions, took #{elapsed}ms.|])
      where
        cleanupUntochedObjects = do                 
            ((z, _), workerId) <- runCleanUpWorker worldVersion      
            case z of 
                Left e                    -> pure $ Left [i|Cache cleanup process failed: #{e}.|]
                Right wr@WorkerResult {..} -> do 
                    logWorkerDone logger workerId wr
                    pushSystem logger $ cpuMemMetric "cache-clean-up" cpuTime clockTime maxMemory
                    pure $ Right payload                                       

    -- Do LMDB compaction
    compact WorkflowShared {..} worldVersion _ = do
        -- Some heuristics first to see if it's obvisouly too early to run compaction:
        -- if we have never deleted anything, there's no fragmentation, so no compaction needed.
        deletedAnything <- readTVarIO deletedAnythingFromDb
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
        listDirectory tmpDir >>= mapM_ (removePathForcibly . (tmpDir </>))
        
        -- Cleanup reader table of LMDB cache, it may get littered by 
        -- dead processes, unfinished/killed transaction, etc.
        -- All these stale transactions are essentially bugs, but it's 
        -- easier to just clean them up rather then prevent all 
        -- possible leakages (even if it were possible).
        cleaned <- cleanUpStaleTx appContext
        when (cleaned > 0) $ 
            logDebug logger [i|Cleaned #{cleaned} stale readers from LMDB cache.|]
        
        -- Kill all orphan rsync processes that are still running and refusing to die
        -- Sometimes and rsync process can leak and linger, kill the expired ones
        removeExpiredRsyncProcesses appState >>= \processes ->
            forM_ processes $ \(pid, WorkerInfo {..}) ->  
                -- Run it in a separate thread, if sending the signal fails
                -- the thread gets killed without no impact on anything else 
                -- ever. If it's successful, we'll log a message about it
                forkIO $ (do
                    signalProcess killProcess pid
                    logInfo logger [i|Killed rsync client process with PID #{pid}, #{cli}, it expired at #{endOfLife}.|])
                    `catch` 
                    (\(_ :: SomeException) -> pure ())            

    -- Delete local rsync mirror. The assumption here is that over time there
    -- be a lot of local copies of rsync repositories that are so old that 
    -- the next time they are updated, most of the new repository will be downloaded 
    -- anyway. Since most of the time RRDP is up, rsync updates are rare, so local 
    -- data is stale and just takes disk space.
    rsyncCleanup _ jobRun =
        case jobRun of 
            -- Do not actually do anything at the very first run.
            -- Statistically the first run would mean that the application 
            -- just was installed and started working and it's not very likely 
            -- that there's already a lot of garbage in the rsync mirror directory.                                    
            FirstRun  -> pure ()  
            RanBefore -> 
                executeOrDie
                    (do
                        let rsyncDir = configValue $ config ^. #rsyncConf . #rsyncRoot
                        logDebug logger [i|Deleting rsync mirrors in #{rsyncDir}.|]
                        listDirectory rsyncDir >>= mapM_ (removePathForcibly . (rsyncDir </>))
                    )
                    (\_ elapsed -> logInfo logger [i|Done cleaning up rsync, took #{elapsed}ms.|])

    -- Give up and die as soon as something serious happends. 
    -- If disk data is corrupted or we run out of disk or something 
    -- like that, it doesn't make sense to keep running.
    -- 
    executeOrDie :: IO a -> (a -> TimeMs -> IO ()) -> IO ()
    executeOrDie f onRight =
        exec `catches` [
                Handler $ \(AppException seriousProblem) ->
                    die [i|Something really bad happened: #{seriousProblem}, exiting.|],
                Handler $ \(_ :: AsyncCancelled) ->
                    die [i|Interrupted with Ctrl-C, exiting.|],
                Handler $ \(weirdShit :: SomeException) ->
                    die [i|Something really bad and also unknown happened: #{weirdShit}, exiting.|]
            ]
        where
            exec = do
                (r, elapsed) <- timedMS f
                onRight r elapsed        

    createWorldVersion = do
        newVersion      <- newWorldVersion        
        existingVersion <- getWorldVerionIO appState
        logDebug logger $ 
            case existingVersion of
                Nothing ->
                    [i|Generated new world version #{newVersion}.|]
                Just oldWorldVersion ->
                    [i|Generated new world version, #{oldWorldVersion} ==> #{newVersion}.|]
        pure newVersion

    runRtrIfConfigured = 
        for_ (config ^. #rtrConfig) $ runRtrServer appContext


    -- Workers for functionality running in separate processes.
    --     
    runValidationWorker worldVersion = do 
        let talsStr = Text.intercalate "," $ List.sort $ map (unTaName . getTaName) tals                    
            workerId = WorkerId [i|version:#{worldVersion}:validation:#{talsStr}|]

            maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount

            -- TODO make it a runtime thing, config?
            -- let profilingFlags = [ "-p", "-hT", "-l" ]
            profilingFlags = [ ]

            arguments = 
                [ worderIdS workerId ] <>
                rtsArguments ( 
                    profilingFlags <> [ 
                        rtsN maxCpuAvailable, 
                        rtsA "24m", 
                        rtsAL "128m", 
                        rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #validationWorkerMemoryMb) 
                    ])
        
        r <- runValidatorT 
                (newScopes "validator") $ do 
                    workerInput <- 
                        makeWorkerInput appContext workerId
                            (ValidationParams worldVersion tals)                        
                            (Timebox $ config ^. typed @ValidationConfig . #topDownTimeout)
                            Nothing
                    runWorker logger workerInput arguments

        pure (r, workerId)

    runCleanUpWorker worldVersion = do             
        let workerId = WorkerId [i|version:#{worldVersion}:cache-clean-up|]
        
        let arguments = 
                [ worderIdS workerId ] <>
                rtsArguments [ 
                    rtsN 2, 
                    rtsA "24m", 
                    rtsAL "64m", 
                    rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #cleanupWorkerMemoryMb) ]
        
        r <- runValidatorT             
                (newScopes "cache-clean-up") $ do 
                    workerInput <- makeWorkerInput appContext workerId
                                        (CacheCleanupParams worldVersion)
                                        (Timebox 300)
                                        Nothing
                    runWorker logger workerInput arguments                                                                   
        pure (r, workerId)                            

-- To be called by the validation worker process
runValidation :: Storage s =>
                AppContext s
            -> WorldVersion
            -> [TAL]
            -> IO (ValidationState, Maybe Slurm)
runValidation appContext@AppContext {..} worldVersion tals = do           

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
    (deleted, elapsed) <- timedMS $ rwTxT database $ \tx db -> do        
        DB.saveMetrics tx db worldVersion (topDownValidations ^. typed)
        DB.saveValidations tx db worldVersion (updatedValidation ^. typed)
        DB.saveRoas tx db roas worldVersion
        DB.saveSpls tx db (payloads ^. typed) worldVersion
        DB.saveAspas tx db (payloads ^. typed) worldVersion
        DB.saveGbrs tx db (payloads ^. typed) worldVersion
        DB.saveBgps tx db (payloads ^. typed) worldVersion        
        for_ maybeSlurm $ DB.saveSlurm tx db worldVersion
        DB.completeValidationWorldVersion tx db worldVersion

        -- We want to keep not more than certain number of latest versions in the DB,
        -- so after adding one, check if the oldest one(s) should be deleted.
        r <- DB.deleteOldestVersionsIfNeeded tx db (config ^. #versionNumberToKeep)

        handleValidations tx db (topDownValidations ^. typed)        

        pure r

    logDebug logger [i|Saved payloads for the version #{worldVersion}, deleted #{deleted} oldest versions(s) in #{elapsed}ms.|]

    pure (updatedValidation, maybeSlurm)
  where
    -- Here we do anything that needs to be done in case of specific 
    -- fetch/validation issues are present    
    handleValidations tx db (Validations validations) = do 
        Now now <- thisInstant
        for_ (Map.toList repositoriesWithManifestIntegrityIssues) $ \(rrdpUrl, issues) -> do
            logInfo logger [i|Repository #{rrdpUrl} has manifest integrity issues #{issues}, will force it to re-fetch snapshot.|]
            
            DB.updateRrdpMetaM tx db rrdpUrl $ \case 
                Nothing   -> pure Nothing
                Just meta -> do 
                    let enforcement = Just $ NextTimeFetchSnapshot [i|Manifest integrity issues: #{issues}|]
                    case meta ^. #enforcement of    
                        Nothing -> 
                            pure $ Just $ meta { enforcement = enforcement }

                        -- Don't update the enforcement if it's already set to fetch the snapshot
                        Just (NextTimeFetchSnapshot _) -> 
                                pure $ Just meta

                        Just (ForcedSnaphotAt processedAt)
                                -- If the last forced fetch was less than N hours ago, don't do it again
                            | closeEnoughMoments processedAt now 
                                (config ^. #validationConfig . #rrdpForcedSnapshotMinInterval) -> 
                                    pure $ Just meta

                            | otherwise -> do 
                                logInfo logger [i|Last time snapshot was forced at #{processedAt} will force it again.|]
                                pure $ Just $ meta { enforcement = enforcement }
      where 
        repositoriesWithManifestIntegrityIssues = 
            Map.fromListWith (<>) [ 
                (relevantRepo, relevantIssues) | 
                    (scope, issues) <- Map.toList validations,
                    relevantRepo    <- mostNarrowPPScope scope,
                    let relevantIssues = filter manifestIntegrityError (Set.toList issues),
                    not (null relevantIssues)
                ]        
          where
            manifestIntegrityError = \case
                VErr (ValidationE e) -> case e of 
                    ManifestEntryDoesn'tExist _ _       -> True
                    NoCRLExists _ _                     -> True                
                    ManifestEntryHasWrongFileType _ _ _ -> True                
                    ReferentialIntegrityError _         -> True                
                    _                                   -> False
                _                                       -> False

            mostNarrowPPScope (Scope s) = 
                take 1 [ url | PPFocus (RrdpU url) <- NE.toList s ]


-- To be called from the cache cleanup worker
-- 
runCacheCleanup :: Storage s =>
                AppContext s
                -> WorldVersion                
                -> IO DB.CleanUpResult
runCacheCleanup AppContext {..} worldVersion = do        
    db <- readTVarIO database
    -- Use the latest completed validation moment as a cutting point.
    -- This is to prevent cleaning up objects if they were untouched 
    -- because prover wasn't running for too long.
    cutOffVersion <- roTx db $ \tx -> 
        fromMaybe worldVersion <$> DB.getLastValidationVersion db tx

    DB.deleteStaleContent db (versionIsOld (versionToMoment cutOffVersion) (config ^. #cacheLifeTime))


-- | Load the state corresponding to the last completed validation version.
-- 
loadStoredAppState :: Storage s => AppContext s -> IO (Maybe WorldVersion)
loadStoredAppState AppContext {..} = do
    Now now' <- thisInstant
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval    
    roTxT database $ \tx db ->
        DB.getLastValidationVersion db tx >>= \case
            Nothing  -> pure Nothing

            Just lastVersion
                | versionIsOld now' revalidationInterval lastVersion -> do
                    logInfo logger [i|Last cached version #{lastVersion} is too old to be used, will re-run validation.|]
                    pure Nothing

                | otherwise -> do
                    (payloads, elapsed) <- timedMS $ do                                            
                        slurm    <- DB.slurmForVersion tx db lastVersion
                        payloads <- DB.getRtrPayloads tx db lastVersion                        
                        for_ payloads $ \payloads' -> do 
                            slurmedPayloads <- atomically $ completeVersion appState lastVersion payloads' slurm                            
                            when (config ^. #withValidityApi) $                                
                                -- do it in a separate thread to speed up the startup
                                void $ forkIO $ atomically $ updatePrefixIndex appState slurmedPayloads
                        pure payloads
                    for_ payloads $ \p -> do 
                        let vrps = p ^. #vrps
                        logInfo logger $ [i|Last cached version #{lastVersion} used to initialise |] <>
                                         [i|current state (#{estimateVrpCount vrps} VRPs), took #{elapsed}ms.|]
                    pure $ Just lastVersion


{- 
    Run periodic fetches for slow repositories asychronously to the validation process.

    - Periodically check if there're repositories that don't have fetchType `ForSyncFetch`
    - Try to refetch these repositories
    - Repositories that were successful and fast enought get back `ForSyncFetch` fetch type.
-}
runAsyncFetches :: Storage s => AppContext s -> WorldVersion -> IO ValidationState
runAsyncFetches appContext@AppContext {..} worldVersion = do             
    withRepositoriesProcessing appContext $ \repositoryProcessing -> do

        pps <- readPublicationPoints repositoryProcessing      
        -- We only care about the repositories that are maarked for asynchronous fetch 
        let asyncRepos = Map.fromList $ findRepositoriesForAsyncFetch pps        

        -- And mentioned on some certificates during the last validation.        
        let problematicPpas = let 
                toPpa []   = Nothing
                toPpa urls = fmap PublicationPointAccess 
                                $ NE.nonEmpty 
                                $ map repositoryToPP
                                $ catMaybes
                                $ map (\u -> Map.lookup u asyncRepos) urls                 
                
                -- Also use only the ones filtered by config options 
                in catMaybes $ map (filterPPAccess config) $ 
                   catMaybes $ map toPpa $ toList $ pps ^. #usedForAsync

        unless (null problematicPpas) $ do                                     
            let ppaToText (PublicationPointAccess ppas) = 
                    Text.intercalate " -> " $ map (fmtGen . getRpkiURL) $ NE.toList ppas
            let reposText = Text.intercalate ", " $ map ppaToText problematicPpas
            let totalRepoCount = repositoryCount pps 
            logInfo logger [i|Asynchronous fetch, version #{worldVersion}, #{length problematicPpas} out of #{totalRepoCount} repositories: #{reposText}|]

        void $ forConcurrently (sortPpas asyncRepos problematicPpas) $ \ppAccess -> do                         
            let url = getRpkiURL $ NE.head $ unPublicationPointAccess ppAccess
            void $ runValidatorT (newScopes' RepositoryFocus url) $ 
                    fetchWithFallback appContext repositoryProcessing 
                        worldVersion (asyncFetchConfig config) ppAccess
                    
        validationStateOfFetches repositoryProcessing   
  where    
    -- Sort them by the last fetch time, the fastest first. 
    -- If a repository was never fetched, it goes to the end of the list
    sortPpas asyncRepos ppas = map fst 
            $ List.sortOn promisingFirst
            $ map withRepo ppas    
      where
        promisingFirst (_, repo) = fmap duration repo

        withRepo ppa = let 
                primary = NE.head $ unPublicationPointAccess ppa
            in (ppa, Map.lookup (getRpkiURL primary) asyncRepos)

        duration r = 
            fromMaybe (TimeMs 1000_000_000) $ (getMeta r) ^. #lastFetchDuration

    repositoryToPP = \case    
        RsyncR r -> RsyncPP $ r ^. #repoPP
        RrdpR r  -> RrdpPP r


canRunInParallel :: TaskType -> TaskType -> Bool
canRunInParallel t1 t2 = 
    t2 `elem` canRunWith t1 || t1 `elem` canRunWith t2
  where    
    canRunWith ValidationTask        = [AsyncFetchTask]    
    canRunWith AsyncFetchTask        = [ValidationTask]
    canRunWith RsyncCleanupTask      = allExcept [ValidationTask, AsyncFetchTask]
    canRunWith _                     = []
  
    allExcept tasks = filter (not . (`elem` tasks)) [minBound..maxBound]
        
    
newtype RunningTasks = RunningTasks { 
        running :: TVar (Set.Set TaskType)
    }

newRunningTasks :: STM RunningTasks
newRunningTasks = RunningTasks <$> newTVar mempty

runConcurrentlyIfPossible :: AppLogger -> Task -> RunningTasks -> IO ()
runConcurrentlyIfPossible logger (Task taskType action) RunningTasks {..} = do 
    (runningTasks, canRun) <- atomically $ do 
                runningTasks <- readTVar running
                let canRun = Set.null $ Set.filter (not . canRunInParallel taskType) runningTasks
                pure (Set.toList runningTasks, canRun)

    unless canRun $ 
        logDebug logger [i|Task #{taskType} cannot run concurrently with #{runningTasks} and has to wait.|]        

    join $ atomically $ do 
        r <- readTVar running
        if Set.null $ Set.filter (not . canRunInParallel taskType) r
            then do 
                writeTVar running $ Set.insert taskType r
                pure $ action
                        `finally` 
                        atomically (modifyTVar' running $ Set.filter (/= taskType))
            else retry


-- 
versionIsOld :: Instant -> Seconds -> WorldVersion -> Bool
versionIsOld now period (WorldVersion nanos) =
    let validatedAt = fromNanoseconds nanos
    in not $ closeEnoughMoments validatedAt now period


periodically :: Seconds -> a -> (a -> IO a) -> IO a
periodically interval a0 action = do         
    go a0
  where
    go a = do
        Now start <- thisInstant        
        a' <- action a
        Now end <- thisInstant
        let pause = leftToWait start end interval
        when (pause > 0) $
            threadDelay $ fromIntegral pause
        
        go a'        


leftToWait :: Instant -> Instant -> Seconds -> Int64
leftToWait start end (Seconds interval) = let
    executionTimeNs = toNanoseconds end - toNanoseconds start
    timeToWaitNs = nanosPerSecond * interval - executionTimeNs
    in timeToWaitNs `div` 1000               

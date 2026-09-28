{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Workflow (
    runValidatorWorkflow,
    runValidation,
    runCacheCleanup,
    rollBackLongTransactions
) where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad

import           Control.Lens hiding (indices, Indexable)
import           Data.Generics.Product.Typed

import qualified Data.ByteString.Lazy            as LBS

import           Data.Foldable                   (for_)
import qualified Data.Text                       as Text
import qualified Data.List.NonEmpty              as NonEmpty
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set
import           Data.Maybe                      (fromMaybe, catMaybes, isJust)
import           Data.Hourglass
import           Data.Time.Clock                 (NominalDiffTime, diffUTCTime, getCurrentTime)
import qualified Data.IxSet.Typed                as IxSet

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
import           RPKI.Fetch.Fetch
import           RPKI.Logging
import           RPKI.Metrics.Process
import           RPKI.Metrics.System
import           RPKI.Http.Types
import           RPKI.Http.Dto
import qualified RPKI.Store.Database               as DB
import qualified RPKI.Store.SQLite                 as SQLite
import           RPKI.Validation.TopDown

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.AppStorage
import           RPKI.RRDP.Types
import           RPKI.TAL
import           RPKI.Util                     
import           RPKI.Time
import           RPKI.Worker
import           RPKI.Workflow.Fetcher
import           RPKI.Workflow.Shared
import           RPKI.Workflow.Task
import           RPKI.SLURM.Types
import qualified UnliftIO.Exception              as UIO

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

-- The main entry point for the whole validator workflow. Runs multiple threads, 
-- running validation, RTR server, cleanups, cache maintenance and async fetches.
-- 
runValidatorWorkflow :: MaintainableStorage s => AppContext s -> [TAL] -> IO ()
runValidatorWorkflow appContext@AppContext {..} tals = do    
    DB.rwTxT database $ \tx ->
        DB.setActiveTAs tx (map getTaName tals)
        
    runAll appContext tals
        `catches` [
            Handler $ \(AppException seriousProblem) ->
                die [i|Something really bad happened: #{seriousProblem}, exiting.|],
            Handler $ \(_ :: AsyncCancelled) -> 
                die [i|Interrupted with Ctrl-C, exiting.|]            
        ]


runAll :: MaintainableStorage s =>
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
                        mapConcurrently_ id [
                            runScheduledTasks workflowShared,
                            revalidate workflowShared,
                            runRtrIfConfigured
                        ]                        

                    OneOffMode _ -> 
                        -- Scheduled jobs don't run in the one-off mode, so the things 
                        -- they are responsible for have to happen here: TA certificates 
                        -- have to be refreshed, otherwise there would be nothing to 
                        -- validate at all, and the WAL has to be checkpointed, otherwise 
                        -- it grows to the size of everything the workers write.
                        race_ checkpointPeriodically $ do 
                            worldVersion <- newWorldVersion
                            void $ fetchTaCertificates workflowShared worldVersion FirstRun
                            void $ revalidate workflowShared
        )
  where
    allTaNames = map getTaName tals

    checkpointPeriodically = forever $ do 
        threadDelay $ toMicroseconds $ config ^. typed @StorageConfig . #walCheckpointInterval
        checkpointDatabase appContext

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
                -- If this thread leaks, it's not a biggy, it will exit pretty soon           
                forkLogged logger "Exception in revalidation delay thread" $ do              
                    Conc.threadDelay $ toMicroseconds $ config ^. #validationConfig . #minimalRevalidationInterval
                    atomically $ writeTVar canValidateAgain True
                triggeredValidationLoop canValidateAgain RanBefore           

            waitForTasToValidate = do 
                readyForFirstRun <- 
                    case (run, config ^. #proverRunMode) of 
                        (RanBefore, _) -> pure []

                        -- TA certificates have just been refreshed in the one-off mode,
                        -- so there's nothing to wait for, whatever is missing by now 
                        -- is going to be reported as an error by the validation.
                        (FirstRun, OneOffMode _) -> pure tals

                        -- On the very first run go ahead with the TAs that already have their 
                        -- certificate in the cache. There's nothing to validate for the rest of 
                        -- them (and, on a cold cache, for any of them) until the TA certificate 
                        -- job has downloaded the certificates, and it triggers the validation 
                        -- itself as soon as it has.                        
                        (FirstRun, ServerMode)   -> do 
                            cached <- DB.roTxT database $ \tx ->
                                Set.fromList . map (getTaName . (^. #tal)) <$> DB.getTAs tx
                            pure $ filter ((`Set.member` cached) . getTaName) tals

                atomically $ do
                    (`unless` retry) =<< readTVar canValidateAgain                
                    case readyForFirstRun of
                        _ : _ -> reset >> pure readyForFirstRun
                        []    -> do 
                            tas <- readTVar (workflowShared ^. #tasToValidate)
                            when (Set.null tas) retry
                            reset
                            pure $ filter (\tal -> getTaName tal `Set.member` tas) tals                           
              where
                reset = do 
                    writeTVar (workflowShared ^. #tasToValidate) mempty
                    writeTVar canValidateAgain False
                    

    outputVrps vrpOutputFile = do 
        vrps <- DB.roTxT database $ \tx ->
                DB.getLatestVersion tx >>= \case
                    Nothing            -> pure Nothing
                    Just latestVersion -> Just <$> DB.getVrps tx latestVersion
        case vrps of
            Nothing -> do
                logWarn logger [i|Don't have any VRPs, exiting.|]                
            Just vrps' ->                 
                LBS.writeFile vrpOutputFile $ unRawCSV $ vrpDtosToCSV $ toVrpDtos vrps'

    hasValidatedEverythingForEveryTA WorkflowShared { fetchers = Fetchers {..}} = do 
        -- check if for every TA the last validation time is later than 
        -- all of the first fetching dates for the repositories
        versions  <- DB.roTxT database DB.getLatestVersions
        fetchedBy <- readTVarIO firstFinishedFetchBy                
        urisByTA  <- readTVarIO uriByTa
        
        -- Every repository of the TA must have been fetched at least once, and 
        -- the TA must have been validated after all of those first fetches.
        pure $ all (\(ta, validatedBy) -> 
                let urls = IxSet.indexKeys $ IxSet.getEQ ta urisByTA
                in not (null urls) && 
                   all (\url -> maybe False (validatedBy >) $ Map.lookup url fetchedBy) urls
            ) $ perTA versions


    schedules workflowShared = [            
            Scheduling {                                 
                task         = CacheCleanupTask,
                initialDelay = 600 * 1_000_000,
                interval     = config ^. #cacheCleanupInterval,
                persistent   = True,
                action       = cacheCleanup workflowShared
            },
            Scheduling {             
                task         = RsyncCleanupTask,
                initialDelay = 1200 * 1_000_000,
                interval     = config ^. #rsyncCleanupInterval,
                persistent   = True,
                action       = rsyncCleanup
            },
            let interval = config ^. typed @ValidationConfig . #revalidationInterval
            in Scheduling {                 
                task         = LeftoversCleanupTask,
                initialDelay = toMicroseconds interval `div` 2,                
                persistent   = False,
                action       = \_ _ -> cleanupLeftovers,
                interval
            },
            Scheduling {
                task         = TaCertificateTask,
                initialDelay = 0,
                interval     = config ^. typed @ValidationConfig . #taCertificateRefreshInterval,
                persistent   = False,
                action       = fetchTaCertificates workflowShared
            },
            let interval = config ^. typed @StorageConfig . #walCheckpointInterval
            in Scheduling {
                task         = WalCheckpointTask,
                initialDelay = toMicroseconds interval,
                persistent   = False,
                action       = \_ _ -> checkpointDatabase appContext,
                interval
            }
        ]              

    -- For each schedule 
    --   * run a thread that would try to run the task periodically 
    --   * run tasks using `runConcurrentlyIfPossible` to make sure 
    --     there is no data races between different tasks
    runScheduledTasks workflowShared = do                
        persistedJobs <- DB.roTxT database $ \tx -> Map.fromList <$> DB.allJobs tx

        Now now <- thisInstant
        forConcurrently_ (schedules workflowShared) $ \Scheduling {..} -> do                        
            let name = fmtGen task
            -- A persistent task remembers when it last completed, so after a 
            -- restart it waits out the rest of its interval instead of the 
            -- initial delay, and it is not a first run any more.
            let (delay, firstRun) = 
                    case guard persistent >> Map.lookup name persistedJobs of 
                        Nothing           -> (initialDelay, FirstRun)
                        Just lastExecuted -> 
                            (fromIntegral $ leftToWaitMicros (Earlier lastExecuted) (Later now) interval, RanBefore)

            let delayInSeconds = delay `div` 1_000_000
            let delayText :: Text.Text 
                delayText | delay == 0 = [i|for ASAP execution|] 
                          | delay < 0  = [i|for ASAP execution (it is #{-delayInSeconds}s due)|] 
                          | otherwise  = [i|with initial delay #{delayInSeconds}s|]
            logDebug logger [i|Scheduling task '#{name}' #{delayText} and interval #{interval}.|] 

            when (delay > 0) $
                threadDelay delay

            periodically interval firstRun $ \jobRun -> 
                runConcurrentlyIfPossible logger task (workflowShared ^. #runningTasks) $ do 
                    logDebug logger [i|Running task '#{name}'.|]
                    worldVersion <- newWorldVersion
                    action worldVersion jobRun 
                        `finally` (do  
                            when persistent $ do                       
                                Now endTime <- thisInstant
                                -- re-read `db` since it could have been changed by the time the
                                -- job is finished (after compaction, in particular)                                                                
                                DB.rwTxT database $ \tx -> DB.setJobCompletionTime tx name endTime
                            updateMainResourcesStat
                            logDebug logger [i|Done with task '#{name}'.|])    

    updateMainResourcesStat = do
        stats <- processStat
        SystemInfo {..} <- readTVarIO $ appState ^. #system
        Now now <- thisInstant
        let clockTime = durationMs startUpTime now
        pushSystem logger $ resourceUsageMetric "root" clockTime stats

    validateTAs workflowShared worldVersion talsToValidate = do  
        let taNames = map getTaName talsToValidate
        logInfo logger [i|Validating TAs #{taNames}, world version #{worldVersion} |]
        
        ((rtrPayloads, slurmedPayloads), elapsed) <- timedMS processTALs            
        -- The ones that came before there was any version to add them to
        saveSystemProblems appContext
        let vrps = rtrPayloads ^. #vrps
        let slurmedVrps = slurmedPayloads ^. #vrps
        logInfo logger $
            [i|Validated TAs #{taNames}, got #{estimateVrpCount vrps} VRPs (probably not unique), |] <>
            [i|#{estimateVrpCount slurmedVrps} SLURM-ed VRPs, took #{elapsed}ms|]
      where
        processTALs = do
            (z, workerVS) <- runValidationWorker worldVersion talsToValidate

            -- Workers that exceeded their limits are not about any TA, 
            -- so they go to the common validations of the version.
            limitsVS <- atomically $ (\problems -> mempty & typed .~ problems) 
                                        <$> workerLimitProblems appState

            let reportError message = do 
                    logError logger message
                    let commonVS = workerVS <> limitsVS
                    DB.rwTxT database $ \tx -> do
                        DB.saveValidationVersion tx worldVersion mempty commonVS
                    updatePrometheus (commonVS ^. typed) (workflowShared ^. #prometheusMetrics) worldVersion
                    pure (mempty, mempty)

            case z of 
                Left e -> 
                    reportError [i|Validator process failed: #{e}.|]                    

                Right (ValidationResult vs discovered) -> do
                    adjustFetchers appContext (fmap fst discovered) workflowShared
                    scheduleRevalidationOnExpiry appContext (fmap snd discovered) workflowShared

                    -- The worker has saved the version, SLURM is read and
                    -- stored for it here since the worker doesn't read files.
                    -- Workers' limit problems are added to it here as well.
                    (slurmVS, maybeSlurm) <- reReadSlurm appContext
                    let commonVS = slurmVS <> limitsVS
                    when (isJust maybeSlurm || commonVS /= mempty) $
                        DB.rwTxT database $ \tx -> do
                            for_ maybeSlurm $ DB.saveSlurm tx worldVersion
                            DB.addCommonValidations tx worldVersion commonVS

                    let topDownState = workerVS <> vs <> commonVS
                    logDebug logger [i|Validation result: 
#{formatValidations (topDownState ^. typed)}.|]
                    updatePrometheus (topDownState ^. typed) (workflowShared ^. #prometheusMetrics) worldVersion                        
                    
                    (!q, elapsed) <- timedMS $ reReadAndUpdatePayloads maybeSlurm
                    logDebug logger [i|Re-read payloads, took #{elapsed}ms.|]
                    pure q
          where
            reReadAndUpdatePayloads maybeSlurm = do 
                DB.roTxT database (\tx -> DB.getRtrPayloads tx worldVersion) >>= \case
                    Nothing -> do 
                        logError logger [i|Something weird happened, could not re-read VRPs.|]
                        pure (mempty, mempty)
                    Just rtrPayloads -> atomically $ do                                                    
                        slurmedPayloads <- completeVersion appState worldVersion rtrPayloads maybeSlurm 
                        when (config ^. #withValidityApi) $
                            updatePrefixIndex appState slurmedPayloads
                        pure (rtrPayloads, slurmedPayloads)
                          

    -- Only TAs whose certificate actually changed (a first-ever download, or a
    -- genuinely new certificate) need to be revalidated. Most refreshes just
    -- reconfirm the certificate already in the cache, and used to trigger a
    -- full revalidation of every TA anyway -- wasted CPU, since nothing about
    -- the TA changed.
    fetchTaCertificates workflowShared worldVersion _ = do
        changedTaNames <- fmap catMaybes $ forConcurrently tals $ \tal -> do
            let taName = getTaName tal
            (r, elapsed) <- timedMS $ refreshTaCertificate appContext tal worldVersion
            case r of 
                Left e -> do
                    logError logger [i|Failed to download and validate TA certificate for #{taName}: #{e}.|]
                    pure Nothing

                Right changed -> do 
                    logDebug logger $ 
                        [i|Downloaded and validated TA certificate for #{taName}, |] <> 
                        [i|changed = #{changed}, took #{elapsed}ms.|]
                    pure $ if changed then Just taName else Nothing

        requestRevalidation (workflowShared ^. #tasToValidate) $ Set.fromList changedTaNames

    -- Delete objects in the store that were read by top-down validation 
    -- longer than `shortLivedCacheLifeTime` hours ago.
    cacheCleanup _ worldVersion _ = do
        (r, elapsed) <- timedMS cleanupOldObjects
        case r of 
            Left message -> logError logger message
            Right DB.CleanUpResult {..} -> do
                let perType :: String = if mempty /= deletedPerType
                    then [i|in particular #{Map.toList deletedPerType}, |]
                    else ""
                logInfo logger $ [i|Cleanup: deleted #{deletedObjects} objects, #{perType}kept #{keptObjects}, |] <>
                                 [i|deleted #{deletedObjectUrls} stale object-URL links, |] <>
                                 [i|deleted #{deletedURLs} dangling URLs, #{deletedVersions} old versions, |] <>
                                 [i|deleted #{deletedErikPartitions} orphaned Erik partitions, took #{elapsed}ms.|]
      where
        cleanupOldObjects = do                 
            (z, _) <- runCleanUpWorker worldVersion
            case z of 
                Left e  -> pure $ Left [i|Cache cleanup process failed: #{e}.|]
                Right r -> pure $ Right r

    -- Delete temporary files and any stale storage-backend state
    cleanupLeftovers = do
        -- Cleanup tmp directory, if some fetchers died abruptly 
        -- there may be leftover files.        
        let tmpDir = configValue $ config ^. #tmpDirectory
        logDebug logger [i|Cleaning up temporary directory #{tmpDir}.|]
        now <- getCurrentTime
        files <- listDirectory tmpDir

        -- Temporary RRDP files cannot meaningfully live longer than that
        let Seconds (fromIntegral -> maxTimeout :: NominalDiffTime) =
                10 + config ^. typed @SystemConfig . #rrdpWorkerLimits . #workerTimeout

        -- Do not touch "erik" subdirectory, it has it's own cleanup mechanism
        forM_ (filter (/= "erik") files) $ \file ->
            ignoreSync $ do 
                let fullPath = tmpDir </> file
                ageInSeconds <- diffUTCTime now <$> getModificationTime fullPath            
                when (ageInSeconds > maxTimeout) $
                    removePathForcibly fullPath                

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
    runValidationWorker worldVersion talsToValidate =
        runValidatorIO (newScopes "validator") $
            runWorker appContext ValidationParams {..} Nothing

    runCleanUpWorker worldVersion = 
        runValidatorIO (newScopes "cache-clean-up") $ do
            CacheCleanupResult r <- runWorker appContext (CacheCleanupParams worldVersion) Nothing
            pure r


{- | The main process's side of transaction timeouts. A worker with a transaction 
     running for too long just exits ('dieOfLongTransactions'); the main process 
     rolls the transaction back instead, whoever started it gets 
     'SQLite.TransactionTimedOut'. It is also reported as a problem of the latest 
     version, to be seen in the UI and not only in the log.
-}
rollBackLongTransactions :: AppContext s -> IO ()
rollBackLongTransactions appContext@AppContext {..} = do
    db <- readTVarIO database
    SQLite.watchTransactions (DB.unDB db) timeouts $ 
        \stuck@SQLite.StuckTx { kind, runningFor, limit } -> do
            let message = [i|A #{SQLite.txKindName kind} transaction of the main process has been running |] <> 
                          [i|for #{runningFor}, the limit is #{limit}, rolling it back.|]
            logError logger message
            SQLite.abortTransaction stuck
            reportSystemProblem appContext $ StorageE $ StorageError message
  where
    timeouts = let t = config ^. #storageConfig . #txTimeout in SQLite.TxTimeouts t t

-- | Add a problem of the main process, one that isn't about any TA, to the 
-- common validations of the latest version, where the UI shows it. If there 
-- is no version yet, it waits in 'systemProblems' for the next validation.
reportSystemProblem :: AppContext s -> AppError -> IO ()
reportSystemProblem appContext@AppContext {..} problem = do
    atomically $ modifyTVar' (appState ^. #systemProblems) (<> mError (newScope "storage") problem)
    -- Doesn't wait for it: it takes a write transaction, and the one
    -- that is being rolled back may well be holding the lock yet.
    void $ forkIO $ saveSystemProblems appContext

saveSystemProblems :: AppContext s -> IO ()
saveSystemProblems AppContext {..} = do
    let systemProblems = appState ^. #systemProblems
    problems <- atomically $ stateTVar systemProblems (, mempty)
    when (problems /= mempty) $ do
        saved <- UIO.tryAny $ DB.rwTxT database $ \tx -> 
            DB.getLatestVersion tx >>= \case 
                Nothing      -> pure False
                Just version -> do 
                    DB.addCommonValidations tx version (mempty & typed .~ problems)
                    pure True
        case saved of 
            Right True  -> pure ()
            Right False -> atomically $ modifyTVar' systemProblems (problems <>)
            Left e      -> do 
                logError logger [i|Could not save problems of the main process: #{fmtEx e}.|]
                atomically $ modifyTVar' systemProblems (problems <>)


-- | Read SLURM files, if there are any configured. Only the main process
-- does it, workers don't read any files that are not their own.
reReadSlurm :: AppContext s -> IO (ValidationState, Maybe Slurm)
reReadSlurm AppContext {..} =
    case appState ^. #readSlurm of
        Nothing       -> pure (mempty, Nothing)
        Just readFunc -> do
            logInfo logger [i|Re-reading and re-validating SLURM files.|]
            (z, vs) <- runValidatorIO (newScopes "read-slurm") readFunc
            case z of
                Left e -> do
                    logError logger [i|Failed to read SLURM files: #{e}|]
                    pure (vs, Nothing)
                Right slurm ->
                    pure (vs, Just slurm)

-- To be called by the validation worker process
runValidation :: AppContext s
            -> WorldVersion
            -> [TAL]
            -> [TaName]
            -> IO (ValidationState, Map TaName (Fetcheables, EarliestToExpire))
runValidation appContext@AppContext {..} worldVersion talsToValidate allTaNames = do           

    results <- validateMultipleTAs appContext worldVersion talsToValidate

    -- Save all the results into the database. SLURM is not read here, the 
    -- main process applies it after re-reading the payloads.
    ((deleted, updatedValidation), elapsed) <- timedMS $ DB.rwTxT database $ \tx -> do

        let results' = addVersionPerTA results
        updatedValidation <- addUniqueVrpCountsToMetrics tx results'

        let resultsToSave = toPerTA
                $ map (\(ta, r) -> (ta, (r ^. typed, r ^. typed)))
                $ Map.toList results'

        DB.saveValidationVersion tx worldVersion
            resultsToSave updatedValidation

        -- We want to keep not more than certain number of latest versions in the DB,
        -- so after adding one, check if the oldest one(s) should be deleted.
        deleted <- DB.deleteOldestVersionsIfNeeded tx (config ^. #versionNumberToKeep)

        let validations = updatedValidation <> snd (allTAs resultsToSave)

        handleValidations tx (validations ^. typed)

        pure (deleted, validations)

    let deletedStr = case deleted of
            [] -> "none"
            _  -> show deleted
    logDebug logger [i|Saved payloads for the version #{worldVersion}, deleted #{deletedStr} oldest version(s) in #{elapsed}ms.|]

    pure (updatedValidation, 
        Map.map (\r -> (r ^. #discoveredRepositories, r ^. #earliestNotValidAfter)) results)

  where
    addUniqueVrpCountsToMetrics tx results = do

        previousVersion <- DB.previousVersion tx worldVersion

        vrps <- forM allTaNames $ \taName -> do
            case Map.lookup taName results of
                Just p  -> pure (taName, toVrps $ p ^. typed)
                Nothing -> case previousVersion of
                        Nothing -> pure (taName, mempty)
                        Just pv -> (taName, ) <$> DB.getVrpsForTA tx pv taName
   
        pure $ addUniqueVRPCount (toPerTA vrps) mempty
      where
        addUniqueVRPCount vrps !vs = let
                vrpCountLens = typed @Metrics . #vrpCounts
                (perTaCounts, allTasCount) = uniqueVrpCounts vrps
                totalUnique = Count (fromIntegral allTasCount)
                perTaUnique = fmap (Count . fromIntegral) perTaCounts
            in vs & vrpCountLens . #totalUnique .~ totalUnique                
                  & vrpCountLens . #perTaUnique .~ perTaUnique

    addVersionPerTA :: Map TaName TopDownResult -> Map TaName TopDownResult
    addVersionPerTA = 
        fmap $ #topDownValidations . #topDownMetric . #validationMetrics 
                %~ fmap (#validatedBy .~ ValidatedBy worldVersion)         

    -- Here we do anything that needs to be done in case of specific 
    -- fetch/validation issues are present    
    handleValidations tx validations = do
        forceSnapshotForReferencialIssues tx validations
        -- other processings if needed
        -- TODO Add some logic that would reset the cache in case of storage integrity issues

    -- https://github.com/lolepezy/rpki-prover/issues/249
    -- This is to handle referential integrity issues, i.e. manifests referring to 
    -- objects that are not found in the cache. That might be caused by
    -- either a bug in the code, e.g. we GC-ed the referred object too early 
    -- or a problem in the repository. In either case we force snapshot fetch 
    -- to recover repository integrity. This is hacky and should be reconsidered 
    -- in the future, but it works well for now.
    forceSnapshotForReferencialIssues tx (Validations validations) = do
        Now now <- thisInstant
        for_ (Map.toList repositoriesWithManifestIntegrityIssues) $ \(rrdpUrl, issues) -> do
            DB.updateRrdpMetaM tx rrdpUrl $ \case
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
                                (config ^. #rrdpConf . #forcedSnapshotMinInterval) -> 
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
                VErr (ValidationE e)             -> isReferentialIntegrityError e
                VWarn (VWarning (ValidationE e)) -> isReferentialIntegrityError e                    
                _                                -> False
              where
                isReferentialIntegrityError = \case
                    MftFallback (ValidationE e) _    -> isReferentialIntegrityError e
                    ManifestEntryDoesn'tExist _ _    -> True
                    NoCRLExists _ _                  -> True                
                    ManifestEntryHasWrongFileType {} -> True                
                    ReferentialIntegrityError _      -> True                
                    _                                -> False

            mostNarrowPPScope (Scope s) = 
                take 1 [ url | PPFocus (RrdpU url) <- NonEmpty.toList s ]


-- Keep track of the earliest expiration time for each TA (i.e. the earlist time when some 
-- object of the TA will expire). Reschedule revalidation of the TA at the moment right after 
-- its earliest expiration time. Since this expiration time in practice keeps receding to the 
-- future as new objects are added, the revalidation is most likely not needed at all, 
-- that's why we double-check it once again before revalidation.
scheduleRevalidationOnExpiry :: AppContext s -> Map TaName EarliestToExpire -> WorkflowShared -> IO ()
scheduleRevalidationOnExpiry AppContext {..} expirationTimes WorkflowShared {..} = do
    Now now <- thisInstant

    -- Only the TAs whose expiration time actually changed need a new trigger
    updatedExpirations <- atomically $ stateTVar earliestToExpire $ \known -> 
            (Map.toList $ Map.differenceWith keepIfChanged expirationTimes known, 
             Map.union expirationTimes known)

    for_ updatedExpirations $ \(taName, expiration@(EarliestToExpire expiresAt)) -> do
        let timeToWait = instantDiff (Earlier now) (Later expiresAt)
        let expiresSoonEnough = timeToWait < config ^. #validationConfig . #revalidationInterval
        when (now < expiresAt && expiration /= mempty && expiresSoonEnough) $ do
            logDebug logger [i|The first object for #{taName} will expire at #{expiresAt}, will schedule re-validation right after.|]
            forkLogged logger [i|Exception in the expiration trigger for #{taName}|] $ do
                threadDelay $ toMicroseconds timeToWait
                latest <- Map.lookup taName <$> readTVarIO earliestToExpire
                case latest of 
                    -- The expiration time moved on since the trigger was scheduled, 
                    -- so there is a later trigger for this TA and this one can go.
                    Just t | t > expiration -> 
                        logDebug logger [i|Will cancel the re-validation for #{taName} scheduled after expiration at #{expiresAt}, new expiration time is #{t}.|]
                    _ -> do 
                        for_ latest $ \t -> 
                            logDebug logger [i|Will not cancel re-validation for #{taName} scheduled after expiration at #{expiresAt}, new expiration time is #{t}.|]
                        requestRevalidation tasToValidate $ Set.singleton taName
  where
    keepIfChanged new old = if new == old then Nothing else Just new


-- To be called from the cache cleanup worker
--
runCacheCleanup :: MaintainableStorage s
                => AppContext s
                -> WorldVersion
                -> IO DB.CleanUpResult
runCacheCleanup appContext@AppContext {..} worldVersion = do
    db <- readTVarIO database
    -- Use the latest completed validation moment as a cutting point.
    -- This is to prevent cleaning up objects actual object if they were
    -- untouched because prover was stopped for a long period.
    cutOffVersion <- DB.roTx db $ \tx ->
        fromMaybe worldVersion <$> DB.getLatestVersion tx

    let cutOffMoment = versionToInstant cutOffVersion
        tooOldLongLived  = versionIsOld cutOffMoment (config ^. #longLivedCacheLifeTime)
        tooOldShortLived = versionIsOld cutOffMoment (config ^. #shortLivedCacheLifeTime)

    r@DB.CleanUpResult {..} <- DB.deleteStaleContent db DB.DeletionCriteria {
            versionIsTooOld  = tooOldLongLived,
            objectIsTooOld = \version type_ ->
                case type_ of
                    -- Most of the object churn happens because of the manifest and CRL updates,
                    -- so they should be removed from the cache sooner than more long-lived objects
                    MFT -> tooOldShortLived version
                    CRL -> tooOldShortLived version
                    _   -> tooOldLongLived version,

            -- We don't want the warning about multiple locations to hang around for too long
            objectUrlIsTooOld = tooOldShortLived
        }

    -- Run storage maintenance (WAL checkpoint, incremental vacuum, optimize)
    when (deletedObjects > 0) $
        runMaintenance appContext

    pure r

-- | Load the state corresponding to the last completed validation version.
-- 
loadStoredAppState :: AppContext s -> IO (Maybe WorldVersion)
loadStoredAppState appContext@AppContext {..} = do
    Now now' <- thisInstant
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval    
    DB.roTxT database $ \tx ->
        DB.getLatestVersion tx >>= \case
            Nothing  -> pure Nothing

            Just lastVersion
                | versionIsOld now' revalidationInterval lastVersion -> do
                    logInfo logger [i|Last cached version #{lastVersion} is too old to be used, will re-run validation.|]
                    pure Nothing

                | otherwise -> do
                    (payloads, elapsed) <- timedMS $ do
                        -- SLURM is stored by the main process after the validation 
                        -- worker has saved the version, so if the main process died in 
                        -- between, there's none for this version. The files are what 
                        -- counts anyway, so read them rather than serve unfiltered payloads.
                        slurm <- DB.getSlurm tx lastVersion >>= \case
                                    Just stored -> pure $ Just stored
                                    Nothing     -> snd <$> reReadSlurm appContext
                        payloads <- DB.getRtrPayloads tx lastVersion
                        for_ payloads $ \payloads' -> do 
                            slurmedPayloads <- atomically $ completeVersion appState lastVersion payloads' slurm                            
                            when (config ^. #withValidityApi) $                                
                                -- do it in a separate thread to speed up the startup
                                forkLogged logger [i|Exception in updating prefix index for #{lastVersion}|]
                                    $ atomically $ updatePrefixIndex appState slurmedPayloads
                        pure payloads
                    for_ payloads $ \p -> do 
                        let vrps = p ^. #vrps
                        logInfo logger $ [i|Last cached version #{lastVersion} used to initialise |] <>
                                         [i|current state (#{estimateVrpCount vrps} VRPs), took #{elapsed}ms.|]
                    pure $ Just lastVersion


killWorkers :: AppContext s -> [WorkerInfo] -> IO ()
killWorkers AppContext {..} workers = do
    forConcurrently_ workers $ \WorkerInfo {..} -> do 
        r <- try $ do
                signalProcess killProcess workerPid
                logInfo logger [i|Killed worker process with PID #{workerPid}, #{cli}, it expired at #{endOfLife}.|]
        logException logger [i|Exception in worker process killer thread|] r

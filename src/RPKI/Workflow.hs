{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Workflow where

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

import           Data.Data
import           Data.Foldable                   (for_)
import qualified Data.Text                       as Text
import qualified Data.List                       as List
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Map.Monoidal.Strict        (MonoidalMap)
import qualified Data.Map.Monoidal.Strict        as MonoidalMap
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Maybe                      (fromMaybe)
import           Data.Int                        (Int64)
import           Data.Hourglass
import           Data.Time.Clock                 (NominalDiffTime, diffUTCTime, getCurrentTime)
import           Data.IxSet.Typed                (IxSet, Indexable, IsIndexOf, ixFun, ixList)
import qualified Data.IxSet.Typed                as IxSet

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
import qualified RPKI.Store.Database               as DB
import           RPKI.Validation.TopDown

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.Store.AppStorage
import           RPKI.TAL
import           RPKI.Parallel
import           RPKI.Util                     
import           RPKI.Time
import           RPKI.Worker
import           RPKI.SLURM.Types
import           RPKI.Http.Dto
import           RPKI.Http.Types
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

    - This architecture assumes that we can get into a situation with a running 
      fetcher at any given moment. Therefore we need a locking mechanism that would allow 
        * maintenance jobs (cleanup, compaction) to be able to block launches 
          of new fetchers and run exclusively
        * validations to be blocked only by running maintenance jobs, 
          but not by waiting maintenance jobs
        * temp/tx cleanups to be able blocked only by maintenance jobs

    - 
-}

data Fetchers = Fetchers {
        -- Fetchers that are currently running
        fetcheables     :: TVar Fetcheables,
        runningFetchers :: TVar (Map RpkiURL ThreadId),        
        fetchSemaphore  :: Semaphore,
        uriByTa         :: TVar UriTaIxSet
    }
    deriving stock (Generic)

type UriTaIxSet = IxSet Indexes UrlTA

data UrlTA = UrlTA RpkiURL TaName
    deriving stock (Show, Eq, Ord, Generic, Data, Typeable)    

type Indexes = '[RpkiURL, TaName]

instance Indexable Indexes UrlTA where
    indices = ixList
        (ixFun (\(UrlTA url _) -> [url]))
        (ixFun (\(UrlTA _ ta)  -> [ta]))        

deleteByIx :: (Indexable ixs a, IsIndexOf ix ixs) => ix -> IxSet ixs a -> IxSet ixs a
deleteByIx ix_ s = foldr IxSet.delete s $ IxSet.getEQ ix_ s

dropFetcher :: Fetchers -> RpkiURL -> IO ()
dropFetcher Fetchers {..} url = mask_ $ do
    readTVarIO runningFetchers >>= \running -> do
        for_ (Map.lookup url running) $ \thread -> do
            Conc.throwTo thread AsyncCancelled
            atomically $ do
                modifyTVar' runningFetchers $ Map.delete url
                modifyTVar' uriByTa $ deleteByIx url

-- | Adjust running fetchers to the latest discovered repositories
-- Updates fetcheables with new ones, creates fetchers for new URLs,
-- and stops fetchers that are no longer needed.
adjustFetchers :: Storage s => AppContext s -> Map TaName Fetcheables -> WorkflowShared -> IO ()
adjustFetchers appContext@AppContext {..} discoveredFetcheables workflowShared@WorkflowShared { fetchers = Fetchers {..} } = do
    (currentFetchers, toStop, toStart) <- atomically $ do            

        -- All the URLs that were discovered by the recent validations of every TA
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

    logDebug logger [i|Adjusting fetchers: toStop = #{toStop}, toStart = #{toStart}, currentFetchers = #{Map.keys currentFetchers}|]

    mask_ $ do
        -- Stop and remove fetchers for URLs that are no longer needed    
        for_ (Set.toList toStop) $ \url ->
            for_ (Map.lookup url currentFetchers) $ \thread -> do
                Conc.throwTo thread AsyncCancelled

        threads <- forM (Set.toList toStart) $ \url ->
            (url, ) <$> forkIO (newFetcher appContext workflowShared url)

        atomically $ do
            modifyTVar' runningFetchers $ \r -> do 
                let addedNewAsyncs = foldr (uncurry Map.insert) r threads
                foldr Map.delete addedNewAsyncs $ Set.toList toStop
                        

updateUriPerTa :: Map TaName Fetcheables -> UriTaIxSet -> UriTaIxSet
updateUriPerTa fetcheablesPerTa uriTa = uriTa'
  where 
    -- TODO Optimise it
    cleanedUpPerTa = foldr deleteByIx uriTa $ Map.keys fetcheablesPerTa        

    uriTa' = 
        IxSet.insertList [ UrlTA url ta | 
                (ta, Fetcheables fs) <- Map.toList fetcheablesPerTa,
                url <- MonoidalMap.keys fs
            ] cleanedUpPerTa 
    

newFetcher :: Storage s => AppContext s -> WorkflowShared -> RpkiURL -> IO ()
newFetcher appContext@AppContext {..} WorkflowShared { fetchers = fetchers@Fetchers {..}, ..} url = do
    go `finally` dropFetcher fetchers url
    `catch` 
        (\(_ :: SomeException) -> 
            -- Complain any something else then AsyncCancelled
            pure ())
  where
    go = do 
        Now start <- thisInstant        
        pauseIfNeeded start
        fetchLoop
      where
        fetchLoop = do 
            Now start <- thisInstant
            actuallyFetch >>= \case        
                Nothing -> do                
                    logInfo logger [i|Fetcher for #{url} is not needed and will be deleted.|]
                Just interval -> do 
                    Now end <- thisInstant
                    let pause = leftToWait start end interval
                    when (pause > 0) $
                        threadDelay $ fromIntegral pause

                    fetchLoop 

    pauseIfNeeded now = do 
        f <- getFetchable 
        for_ f $ \_ -> do 
            r <- roTxT database (\tx db -> DB.getRepository tx db url)
            for_ r $ \repository -> do          
                let lastFetchMoment = 
                        case getMeta repository ^. #status of
                            FetchedAt t -> Just t
                            FailedAt t  -> Just t
                            _           -> Nothing

                for_ lastFetchMoment $ \lastFetch -> do 
                    let interval :: Seconds = refreshInterval repository Nothing
                    let pause = leftToWait lastFetch now interval                                        
                    when (pause > 0) $ do  
                        let pauseSeconds = pause `div` 1_000_000
                        logDebug logger $ 
                            [i|Fetcher for #{url} finished at #{lastFetch}, now is #{now}, |] <> 
                            [i|interval is #{interval}, first fetch will be paused by #{pauseSeconds}s.|]
                        threadDelay $ fromIntegral pause


    actuallyFetch = do 
        getFetchable >>= \case        
            Nothing -> 
                pure Nothing

            Just _ -> do 
                let fetchConfig = asyncFetchConfig config
                worldVersion <- getOrCreateWorldVerion appState            
                repository <- fromMaybe (newRepository url) <$> 
                                roTxT database (\tx db -> DB.getRepository tx db url) 
                                
                -- TODO Do not fetch before some minimal period after the previous fetch

                ((r, validations), duratioMs) <-                 
                        withFetchLimits fetchConfig $ timedMS $ 
                            runValidatorT (newScopes' RepositoryFocus url) $ 
                                fetchRepository appContext fetchConfig worldVersion repository

                -- TODO Use duratioMs, it is the only time metric for failed and killed fetches 
                case r of
                    Right (repository', stats) -> do 
                        let repo = updateMeta' repository' (#status .~ FetchedAt (versionToMoment worldVersion))
                        saveFetchOutcome repo validations                        
                        when (hasUpdates validations) triggerTaRevalidation
                        pure $ Just $ refreshInterval repository stats
                    Left _ -> do
                        let repo = updateMeta' repository (#status .~ FailedAt (versionToMoment worldVersion))
                        saveFetchOutcome repo validations
                        getFetchable >>= \case
                            Nothing -> 
                                -- this whole fetcheable is gone
                                pure Nothing
                            Just fallbacks -> do  
                                fetchFallbacks fetchConfig worldVersion fallbacks
                                -- TODO It must be much longer?
                                pure $ Just $ 5 * refreshInterval repository Nothing 
      where
        fetchFallbacks fetchConfig worldVersion fallbacks = do 
            -- TODO Make it a bit smarter based on the overal number and overall load
            let maxThreads = 32
            repositories <- pooledForConcurrentlyN maxThreads (Set.toList fallbacks) $ \fallbackUrl -> do 

                repository <- fromMaybe (newRepository fallbackUrl) <$> 
                                roTxT database (\tx db -> DB.getRepository tx db fallbackUrl)
                                
                ((r, validations), duratioMs) <- 
                        withFetchLimits fetchConfig $ timedMS $ 
                            runValidatorT (newScopes' RepositoryFocus fallbackUrl) $ 
                                fetchRepository appContext fetchConfig worldVersion repository                

                let repo = case r of
                        Right (repository', stats) -> 
                            -- TODO Do something with RRDP stats
                            updateMeta' repository' (#status .~ FetchedAt (versionToMoment worldVersion))
                        Left _ -> do
                            updateMeta' repository (#status .~ FailedAt (versionToMoment worldVersion))
            
                pure (repo, validations)            

            rwTxT database $ \tx db -> do
                DB.saveRepositories tx db (map fst repositories)
                DB.saveRepositoryValidationStates tx db repositories

            when (any (hasUpdates . snd) repositories) triggerTaRevalidation
        
    getFetchable = do 
        Fetcheables fs <- readTVarIO fetcheables
        pure $ MonoidalMap.lookup url fs

    -- TODO Include all the adaptive logic here
    refreshInterval repository rrdpStats = 
        case repository of 
            RrdpR _  -> config ^. #validationConfig . #rrdpRepositoryRefreshInterval
            RsyncR _ -> config ^. #validationConfig . #rsyncRepositoryRefreshInterval

    saveFetchOutcome r validations =
        rwTxT database $ \tx db -> do
            DB.saveRepositories tx db [r]
            DB.saveRepositoryValidationStates tx db [(r, validations)]

    withFetchLimits fetchConfig f = do
        let Seconds (fromIntegral . (*1000_000) -> intervalMicroSeconds) = fetchConfig ^. #fetchLaunchWaitDuration
        withSemaphoreOrTimeout (fetchers ^. #fetchSemaphore) intervalMicroSeconds f

    hasUpdates validations = let 
            metrics = validations ^. #topDownMetric
            rrdps = MonoidalMap.elems $ unMetricMap $ metrics ^. #rrdpMetrics
            rsyncs = MonoidalMap.elems $ unMetricMap $ metrics ^. #rsyncMetrics                
        in any (\m -> rrdpRepoHasSignificantUpdates (m ^. typed)) rrdps ||
           any (\m -> rsyncRepoHasSignificantUpdates (m ^. typed)) rsyncs
            
    triggerTaRevalidation = atomically $ do 
        ut <- readTVar uriByTa        
        let tas = Set.fromList $ IxSet.indexKeys $ IxSet.getEQ url ut
        modifyTVar' tasToValidate $ (<>) tas





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
        tasToValidate :: TVar (Set TaName)
    }
    deriving stock (Generic)


withWorkflowShared :: (MonadBaseControl IO m, MonadIO m, Storage s) 
                    => AppContext s
                    -> PrometheusMetrics 
                    -> (WorkflowShared -> m b) 
                    -> m b
withWorkflowShared AppContext {..} prometheusMetrics f = do
    shared <- liftIO $ atomically $ do 
        deletedAnythingFromDb <- newTVar False
        runningTasks          <- newRunningTasks
        fetchers <- Fetchers <$>
                        newTVar mempty <*>
                        newTVar mempty <*>                        
                        newSemaphore (fromIntegral $ config ^. #parallelism . #fetchParallelism) <*>
                        newTVar mempty

        tasToValidate <- newTVar mempty
        pure WorkflowShared {..}

    f shared `finally`
        liftIO (mask_ $ do
            fs <- atomically $ Map.elems <$> readTVar (shared ^. #fetchers . #runningFetchers)
            for_ fs $ \thread -> Conc.throwTo thread AsyncCancelled)

-- Different types of periodic tasks that may run 
data TaskType =
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
            withWorkflowShared appContext prometheusMetrics $ \workflowShared ->             
                case config ^. #proverRunMode of     
                    -- TODO This one can be implemented the same as server mode, but
                    -- it should stop when 
                    --  1) no pending repositories exist anymore
                    --  2) one more validation run happened after that
                    OneOffMode vrpOutputFile -> oneOffRun workflowShared vrpOutputFile
                    ServerMode ->             
                        void $ concurrently
                            -- Run the main scheduler and RTR server if RTR is configured    
                            (concurrently
                                (runScheduledTasks workflowShared)
                                (revalidate workflowShared))
                            runRtrIfConfigured            
        )
  where
    allTaNames = map getTaName tals
    
    revalidate workflowShared = do 
        canValidateAgain <- newTVarIO True
        concurrently_ 
            (go canValidateAgain FirstRun)
            (let interval = config ^. typed @ValidationConfig . #revalidationInterval
             in forever $ do                 
                threadDelay (toMicroseconds interval)
                atomically $ writeTVar
                    (workflowShared ^. #tasToValidate) (Set.fromList allTaNames))
      where
        go canValidateAgain run = do 
            talsToValidate <- atomically $ do
                (`unless` retry) =<< readTVar canValidateAgain
                let reset = do 
                        writeTVar (workflowShared ^. #tasToValidate) mempty
                        writeTVar canValidateAgain False
                case run of
                    FirstRun -> reset >> pure tals
                    RanBefore -> do 
                        tas <- readTVar (workflowShared ^. #tasToValidate)
                        when (Set.null tas) retry
                        reset
                        pure $ filter (\tal -> getTaName tal `Set.member` tas) tals 

            worldVersion <- createWorldVersion
            
            void $ do 
                validateTAs workflowShared worldVersion talsToValidate
                forkIO $ do     
                    let minimalPeriodBetweenRevalidations = 30_000_000
                    Conc.threadDelay minimalPeriodBetweenRevalidations
                    atomically $ writeTVar canValidateAgain True
            
            go canValidateAgain RanBefore

    oneOffRun workflowShared vrpOutputFile = do 
        worldVersion <- createWorldVersion
        void $ validateTAs workflowShared worldVersion tals
        -- vrps <- roTxT database $ \tx db -> 
        --         DB.getLastValidationVersion db tx >>= \case 
        --             Nothing            -> pure Nothing  
        --             Just latestVersion -> DB.getVrps tx db latestVersion
        -- case vrps of 
        --     Nothing -> logWarn logger [i|Don't have any VRPs.|]
        --     _       -> LBS.writeFile vrpOutputFile $ unRawCSV $ vrpDtosToCSV $ toVrpDtos vrps

    schedules workflowShared = [            
            Scheduling {                 
                initialDelay = 600_000_000,                
                taskDef = (CacheCleanupTask, cacheCleanup workflowShared),
                persistent = True,
                interval = config ^. #cacheCleanupInterval
            },        
            Scheduling {                 
                initialDelay = 900_000_000,
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
        forConcurrently (schedules workflowShared) $ \Scheduling { taskDef = (task, action), ..} -> do                        
            let name = fmtGen task
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
                    Task task $ do
                        logDebug logger [i|Running task '#{name}'.|]
                        worldVersion <- createWorldVersion
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

    validateTAs workflowShared@WorkflowShared {..} worldVersion talsToValidate = do  
        let taNames = map getTaName talsToValidate
        logInfo logger [i|Validating TAs #{taNames}, world version #{worldVersion} |]
        executeOrDie
            processTALs
            (\(rtrPayloads, slurmedPayloads) elapsed -> do 
                let vrps = rtrPayloads ^. #vrps
                let slurmedVrps = slurmedPayloads ^. #vrps
                logInfo logger $
                    [i|Validated TAs #{taNames}, got #{estimateVrpCount vrps} VRPs (probably not unique), |] <>
                    [i|#{estimateVrpCount slurmedVrps} SLURM-ed VRPs, took #{elapsed}ms|])
      where
        processTALs = do
            ((z, workerVS), workerId) <- runValidationWorker worldVersion talsToValidate allTaNames                  
            case z of 
                Left e -> do 
                    logError logger [i|Validator process failed: #{e}.|]
                    rwTxT database $ \tx db -> do
                        -- DB.saveValidations tx db worldVersion (workerVS ^. typed)
                        -- DB.saveMetrics tx db worldVersion (workerVS ^. typed)
                        -- DB.completeValidationWorldVersion tx db worldVersion   

                        DB.saveValidationVersion tx db worldVersion allTaNames mempty workerVS
                    updatePrometheus (workerVS ^. typed) prometheusMetrics worldVersion
                    pure (mempty, mempty)

                Right wr@WorkerResult {..} -> do                              
                    let ValidationResult vs discoveredRepositories maybeSlurm = payload
                    adjustFetchers appContext discoveredRepositories workflowShared
                    
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
            cleanupOldObjects
            (\z elapsed -> 
                case z of 
                    Left message -> logError logger message
                    Right DB.CleanUpResult {..} -> do 
                        when (deletedObjects > 0) $ do
                            atomically $ writeTVar deletedAnythingFromDb True
                        logInfo logger $ [i|Cleanup: deleted #{deletedObjects} objects, kept #{keptObjects}, |] <>
                                         [i|deleted #{deletedURLs} dangling URLs, #{deletedVersions} old versions, took #{elapsed}ms.|])
      where
        cleanupOldObjects = do                 
            ((z, _), workerId) <- runCleanUpWorker worldVersion      
            case z of 
                Left e                     -> pure $ Left [i|Cache cleanup process failed: #{e}.|]
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
        now <- getCurrentTime
        files <- listDirectory tmpDir

        -- Temporary RRDP files cannot meaningfully live longer than that
        let Seconds (fromIntegral -> maxTimeout :: NominalDiffTime) = 
                10 + max
                    (config ^. #rrdpConf . #rrdpTimeout)
                    (config ^. #rrdpConf . #asyncRrdpTimeout)

        forM_ files $ \file -> do 
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
        
        -- Kill all orphan rsync processes that are still running and refusing to die
        -- Sometimes and rsync process can leak and linger, kill the expired ones
        removeExpiredRsyncProcesses appState >>=
            mapM_ (\(pid, WorkerInfo {..}) ->  
                -- Run it in a separate thread, if sending the signal fails
                -- the thread gets killed without no impact on anything else 
                -- ever. If it's successful, we'll log a message about it
                forkIO $ (do
                    signalProcess killProcess pid
                    logInfo logger [i|Killed rsync client process with PID #{pid}, #{cli}, it expired at #{endOfLife}.|])
                    `catch` 
                    (\(_ :: SomeException) -> pure ()))         

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
    runValidationWorker worldVersion talsToValidate allTaNames = do 
        let talsStr = Text.intercalate "," $ List.sort $ map (unTaName . getTaName) talsToValidate                    
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
                            ValidationParams {..}
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
            -> [TaName]
            -> IO (ValidationState, Map TaName Fetcheables, Maybe Slurm)
runValidation appContext@AppContext {..} worldVersion talsToValidate allTaNames = do           

    -- TopDownResult {..} <- addUniqueVRPCount . mconcat <$>
    --             validateMutlipleTAs appContext worldVersion tals

    results <- validateMutlipleTAs appContext worldVersion talsToValidate
    
    let resultsToSave = toPerTA 
            $ map (\(ta, r) -> (ta, (r ^. typed, r ^. typed))) 
            $ Map.toList results
    
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

    -- Calculate metrics common for all TAs
    -- TODO It is expensive to build a unique set of VRPs, 
    -- can we do something about it?
    let vrps = fmap (\(p, _) -> toVrps (p ^. #roas)) resultsToSave 
    let updatedValidation = addUniqueVRPCount vrps slurmValidations 

    -- Save all the results into LMDB    
    (deleted, elapsed) <- timedMS $ rwTxT database $ \tx db -> do              

        DB.saveValidationVersion tx db worldVersion 
            allTaNames resultsToSave slurmValidations                    
        

        -- DB.saveMetrics tx db worldVersion (topDownValidations ^. typed)
        -- DB.saveValidations tx db worldVersion (updatedValidation ^. typed)
        -- DB.saveRoas tx db roas worldVersion
        -- DB.saveSpls tx db (payloads ^. typed) worldVersion
        -- DB.saveAspas tx db (payloads ^. typed) worldVersion
        -- DB.saveGbrs tx db (payloads ^. typed) worldVersion
        -- DB.saveBgps tx db (payloads ^. typed) worldVersion        
        for_ maybeSlurm $ DB.saveSlurm tx db worldVersion        
        -- DB.saveTaValidationVersion tx db worldVersion (map getTaName tals)       
        -- DB.completeValidationWorldVersion tx db worldVersion        

        -- We want to keep not more than certain number of latest versions in the DB,
        -- so after adding one, check if the oldest one(s) should be deleted.
        DB.deleteOldestVersionsIfNeeded tx db (config ^. #versionNumberToKeep)

    logDebug logger [i|Saved payloads for the version #{worldVersion}, deleted #{deleted} oldest versions(s) in #{elapsed}ms.|]


    pure (updatedValidation, 
        Map.map (\r -> r ^. #discoveredRepositories) results, 
        maybeSlurm)


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
        fromMaybe worldVersion <$> DB.getLatestVersion db tx

    DB.deleteStaleContent db (versionIsOld (versionToMoment cutOffVersion) (config ^. #cacheLifeTime))


-- | Load the state corresponding to the last completed validation version.
-- 
loadStoredAppState :: Storage s => AppContext s -> IO (Maybe WorldVersion)
loadStoredAppState AppContext {..} = do
    Now now' <- thisInstant
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval    
    roTxT database $ \tx db ->
        DB.getLatestVersion db tx >>= \case
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
-- runAsyncFetches :: Storage s => AppContext s -> WorldVersion -> IO ValidationState
-- runAsyncFetches appContext@AppContext {..} worldVersion = do             
--     withRepositoriesProcessing appContext $ \repositoryProcessing -> do

--         pps <- readPublicationPoints repositoryProcessing      
--         -- We only care about the repositories that are maarked for asynchronous fetch 
--         let asyncRepos = Map.fromList $ findRepositoriesForAsyncFetch pps        

--         -- And mentioned on some certificates during the last validation.        
--         let problematicPpas = let 
--                 toPpa []   = Nothing
--                 toPpa urls = fmap PublicationPointAccess 
--                                 $ NE.nonEmpty 
--                                 $ map repositoryToPP
--                                 $ catMaybes
--                                 $ map (\u -> Map.lookup u asyncRepos) urls                 
                
--                 -- Also use only the ones filtered by config options 
--                 in catMaybes $ map (filterPPAccess config) $ 
--                    catMaybes $ map toPpa $ toList $ pps ^. #usedForAsync

--         unless (null problematicPpas) $ do                                     
--             let ppaToText (PublicationPointAccess ppas) = 
--                     Text.intercalate " -> " $ map (fmtGen . getRpkiURL) $ NE.toList ppas
--             let reposText = Text.intercalate ", " $ map ppaToText problematicPpas
--             let totalRepoCount = repositoryCount pps 
--             logInfo logger [i|Asynchronous fetch, version #{worldVersion}, #{length problematicPpas} out of #{totalRepoCount} repositories: #{reposText}|]

--         void $ forConcurrently (sortPpas asyncRepos problematicPpas) $ \ppAccess -> do                         
--             let url = getRpkiURL $ NE.head $ unPublicationPointAccess ppAccess
--             void $ runValidatorT (newScopes' RepositoryFocus url) $ 
--                     fetchWithFallback appContext repositoryProcessing 
--                         worldVersion (asyncFetchConfig config) ppAccess
                    
--         validationStateOfFetches repositoryProcessing   
--   where    
--     -- Sort them by the last fetch time, the fastest first. 
--     -- If a repository was never fetched, it goes to the end of the list
--     sortPpas asyncRepos ppas = map fst 
--             $ List.sortOn promisingFirst
--             $ map withRepo ppas    
--       where
--         promisingFirst (_, repo) = fmap duration repo

--         withRepo ppa = let 
--                 primary = NE.head $ unPublicationPointAccess ppa
--             in (ppa, Map.lookup (getRpkiURL primary) asyncRepos)

--         duration r = 
--             fromMaybe (TimeMs 1000_000_000) $ (getMeta r) ^. #lastFetchDuration

--     repositoryToPP = \case    
--         RsyncR r -> RsyncPP $ r ^. #repoPP
--         RrdpR r  -> RrdpPP r


isMaintenance :: TaskType -> Bool
isMaintenance = \case    
    CacheCleanupTask      -> True
    LmdbCompactTask       -> True
    LeftoversCleanupTask  -> True
    RsyncCleanupTask      -> True
    _                     -> False       

canRunInParallel :: TaskType -> TaskType -> Bool
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
        
    
newtype Tasks = Tasks { 
        running :: TVar (Map TaskType Natural)
    }
    deriving stock (Generic)

newRunningTasks :: STM Tasks
newRunningTasks = Tasks <$> newTVar mempty

runConcurrentlyIfPossible :: AppLogger -> Task -> Tasks -> IO ()
runConcurrentlyIfPossible logger (Task taskType action) Tasks {..} = do 
    {- 
        Theoretically, exclusive maintenance tasks can starve indefinitely and 
        never get picked up because of fechers running all the time.
        But in practice that is very unlikely to happen, so we'll gamble for now.
    -}    
    let canRunWith runningTasks =             
            all (canRunInParallel taskType . fst) $ Map.toList runningTasks

    (runningTasks, canRun) <- atomically $ do 
                runningTasks <- readTVar running
                pure (Map.toList runningTasks, canRunWith runningTasks)

    unless canRun $ 
        logDebug logger [i|Task #{taskType} cannot run concurrently with #{runningTasks} and has to wait.|]        

    join $ atomically $ do 
        runningTasks_ <- readTVar running
        if canRunWith runningTasks_
            then do 
                writeTVar running $ Map.insertWith (+) taskType 1 runningTasks_
                pure $ action
                        `finally` 
                        atomically (modifyTVar' running $ Map.alter (>>= 
                                    (\count -> if count > 1 then Just (count - 1) else Nothing)
                                ) taskType)
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

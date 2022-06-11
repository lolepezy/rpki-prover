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

    -- Fill in the current state if it's not too old.
    -- It is useful in case of restarts. 
    void $ loadStoredAppState appContext

    -- Run RTR server thread when rtrConfig is present in the AppConfig.  
    -- If not needed it will the an noop.  
    let rtrServer = initRtrIfNeeded

    -- Initialise prometheus metrics here
    prometheusMetrics <- createPrometheusMetrics config

    -- Some shared state between the threads for simplicity.
    sharedStuff <- newTVarIO newShared

    -- Threads that will run periodic jobs and persist timestaps of running them
    -- for consistent scheduling.
    periodicJobs <- periodicJobThreads sharedStuff

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

        periodicJobThreads sharedStuff = do 
            let availableJobs = [
                    -- These initial delay numbers are pretty arbitrary and chosen based 
                    -- on reasonable order of the jobs.
                    ("gcJob",               10_000_000, config ^. #cacheCleanupInterval, cacheGC sharedStuff),
                    ("cleanOldVersionsJob", 20_000_000, config ^. #cacheCleanupInterval, cleanOldVersions sharedStuff),
                    ("compactionJob",       30_000_000, config ^. #storageCompactionInterval, compact sharedStuff)]                    
            
            persistedJobs <- do
                db <- readTVarIO database
                roTx db $ \tx -> Map.fromList <$> allJobs tx db

            Now now <- thisInstant                   
            
            pure $ 
                flip map availableJobs $ 
                    \(job, defInitialDelay, interval, action) -> 
                        let initialDelay =                                     
                                case Map.lookup job persistedJobs of 
                                    Nothing           -> defInitialDelay
                                    Just lastExecuted -> fromIntegral $ leftToWait lastExecuted now interval
                        
                            jobAction worldVersion = do                                 
                                    action worldVersion
                                `finally` (do
                                    Now endTime <- thisInstant
                                    -- re-read `db` since it could have been changed by the time the
                                    -- job is finished (after compaction, for example)                            
                                    db <- readTVarIO database
                                    rwTx db $ \tx -> setJobTime tx db job endTime)

                        in \queue -> do 
                            let delaySeconds = initialDelay `div` 1_000_000
                            logDebug_ logger [i|Scheduling job '#{job}' with initial delay #{delaySeconds}s and interval #{interval}.|] 
                            generatePeriodicJob initialDelay interval jobAction queue                        
    
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

                processTALs db = do
                    (topDownValidations, maybeSlurm) <- processTALsWorker worldVersion
                    updatePrometheus (topDownValidations ^. typed) prometheusMetrics worldVersion

                    roTx db (\tx -> getVrps tx db worldVersion) >>= \case 
                        Nothing -> do 
                            logError_ logger [i|Something weird happened: .|]
                            pure (mempty, mempty)
                        Just vrps -> do                 
                            slurmedVrps <- atomically $ do
                                    setCurrentVersion appState worldVersion
                                    completeVersion appState worldVersion vrps maybeSlurm
                            pure (vrps, slurmedVrps)


        cacheGC sharedStuff worldVersion = do
            database' <- readTVarIO database
            executeOrDie
                (cleanObjectCache database' $ versionIsOld (versionToMoment worldVersion) (config ^. #cacheLifeTime))
                (\CleanUpResult {..} elapsed -> do 
                    when (deletedObjects > 0) $ 
                        atomically $ modifyTVar' sharedStuff (#deletedAnythingFromDb .~ True)
                    logInfo_ logger $ [i|Cleanup: deleted #{deletedObjects} objects, kept #{keptObjects}, |] <>
                                      [i|deleted #{deletedURLs} dangling URLs, took #{elapsed}ms.|])

        cleanOldVersions sharedStuff worldVersion = do
            let now = versionToMoment worldVersion
            database' <- readTVarIO database
            executeOrDie
                (deleteOldVersions database' $ versionIsOld now (config ^. #oldVersionsLifetime))
                (\deleted elapsed -> do 
                    when (deleted > 0) $ 
                        atomically $ modifyTVar' sharedStuff (#deletedAnythingFromDb .~ True)
                    logInfo_ logger [i|Done with deleting older versions, deleted #{deleted} versions, took #{elapsed}ms.|])

        compact sharedStuff worldVersion = do
            -- Some heuristics first to see if it's obvisouly too early to run compaction:
            -- if we have never deleted anything, there's no fragmentation, so no compaction needed.
            SharedStuff {..} <- readTVarIO sharedStuff
            if deletedAnythingFromDb then do 
                (_, elapsed) <- timedMS $ runMaintenance appContext
                logInfo_ logger [i|Done with compacting the storage, version #{worldVersion}, took #{elapsed}ms.|]
            else 
                logDebug_ logger [i|Nothing has been deleted from the storage, compaction is not needed.|]

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

        -- Write an action to the global queue with given interval.
        generatePeriodicJob delay interval action globalQueue = do
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
                                [i|Job queue is full. Normally that means that revalidation interval |] <>
                                [i|is too short for the time validation takes. It is recommended to restart |] <>
                                [i|with a higher re-validation interval value.|]
            atomically $ writeCQueue globalQueue z

        initRtrIfNeeded = 
            maybe (pure ()) (runRtrServer appContext) $ config ^. #rtrConfig                        


        processTALsWorker worldVersion = do 
            let talsStr = Text.intercalate "," $ sort $ map (unTaName . getTaName) tals
            let workerId = WorkerId $ "validator:" <> talsStr

            let maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount

            let arguments = 
                    [ worderIdS workerId ] <>
                    rtsArguments [ 
                        rtsN maxCpuAvailable, 
                        rtsA "24m", 
                        rtsAL "128m", 
                        rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #validationWorkerMemoryMb) ]
            
            (z, vs) <- runValidatorT 
                    (newScopes "validator") $ 
                        runWorker
                            logger
                            config
                            workerId 
                            (ValidationParams worldVersion tals)                        
                            (Timebox $ config ^. typed @ValidationConfig . #topDownTimeout)
                            arguments                            
            pure $ case z of 
                Left _                       -> (vs, Nothing)
                Right (ValidationResult v s) -> (vs <> v, s)


runValidation :: Storage s =>
                AppContext s
            -> WorldVersion
            -> [TAL]
            -> IO (ValidationState, Maybe Slurm)
runValidation appContext@AppContext {..} worldVersion tals = do       
    database' <- readTVarIO database

    TopDownResult {..} <- addUniqueVRPCount . mconcat <$>
                validateMutlipleTAs appContext worldVersion tals

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

    pure (topDownValidations, maybeSlurm)


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

data SharedStuff = SharedStuff { 
        deletedAnythingFromDb :: Bool
    }
    deriving (Show, Eq, Ord, Generic)

newShared :: SharedStuff
newShared = SharedStuff False
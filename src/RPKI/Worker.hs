{-# LANGUAGE OverloadedStrings #-}

module RPKI.Worker where

import           Effectful
import qualified Control.Exception               as IOExc
import           Effectful.Exception
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Lens

import           Conduit
import           Data.Foldable (for_)
import           Data.Maybe (isJust)
import           Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Map.Strict            as Map

import           Data.String.Interpolate.IsString
import           Data.Conduit.Process.Typed

import           GHC.Generics

import           System.IO (stdin, stdout)
import           System.Posix.Types
import           System.Posix.Process

import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Metrics.Process
import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.RRDP.Types
import           RPKI.TAL
import           RPKI.Logging
import           RPKI.Time
import           RPKI.Util (fmtEx, trimmed)
import           RPKI.SLURM.Types
import           RPKI.Store.Base.Serialisation
import qualified RPKI.Store.Database    as DB
import           RPKI.Meta.UniqueId


{- | This is to run worker processes for some code that is better to be executed in an isolated process.

Every worker 
 - Reads serialised parameters from its stdin (WorkerInput)
 - Writes serialised result to its stdout (WorkerResult)
 - Streams log messages to stderr (see `withWorkerLogger` in RPKI.Logging)

Running a worker is done using `runWorker` which handles all the process machinery, error handling, etc.

Every worker calls `executeWork` that takes care of worker lifecycle.

`WorkerInput` is used by all workers, `WorkerParams` is individual for every worker type.

Some of the machinery is also in Main.

-}
    
newtype WorkerId = WorkerId Text
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary)

instance Show WorkerId where
    show (WorkerId w) = unpack w

data WorkerParams = RrdpFetchParams { 
                scopes         :: Scopes, 
                rrdpRepository :: RrdpRepository,
                worldVersion   :: WorldVersion 
            } | 
            RsyncFetchParams { 
                scopes          :: Scopes, 
                fetchConfig     :: FetchConfig, 
                rsyncRepository :: RsyncRepository,
                worldVersion    :: WorldVersion 
            } | 
            ValidationParams {                 
                worldVersion   :: WorldVersion,
                allTaNames     :: [TaName],
                talsToValidate :: [TAL]
            } | 
            CacheCleanupParams { 
                worldVersion :: WorldVersion
            }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data WorkerInput = WorkerInput {
        workerId                :: WorkerId,
        params                  :: WorkerParams,
        config                  :: Config,
        initialParentId         :: ProcessID,
        workerTimeout           :: Timebox,
        cpuLimit                :: Maybe CPUTime,
        ioLimits                :: IoLimits,
        parentExecutableVersion :: ExecutableVersion
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

makeWorkerInput :: (MonadIO m) 
                => AppContext s 
                -> WorkerId
                -> WorkerParams
                -> Timebox
                -> Maybe CPUTime                
                -> m WorkerInput
makeWorkerInput AppContext {..} workerId params timeout cpuLimit = do 
    thisProcessId <- liftIO getProcessID    
    pure $ WorkerInput workerId params config thisProcessId 
                        timeout cpuLimit (ioLimitsFor params) executableVersion
  where
    ioLimitsFor = let SystemConfig {..} = config ^. #systemConfig in \case
        RrdpFetchParams {}    -> rrdpWorkerIoLimits
        RsyncFetchParams {}   -> rsyncWorkerIoLimits
        ValidationParams {}   -> validationWorkerIoLimits
        CacheCleanupParams {} -> cleanupWorkerIoLimits

newtype RrdpFetchResult = RrdpFetchResult 
                            (Either AppError (RrdpRepository, RrdpFetchStat), ValidationState)    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype RsyncFetchResult = RsyncFetchResult 
                            (Either AppError RsyncRepository, ValidationState)    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype CompactionResult = CompactionResult ()                             
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data ValidationResult = ValidationResult 
            ValidationState 
            (Map.Map TaName (Fetcheables, EarliestToExpire))
            (Maybe Slurm) 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype CacheCleanupResult = CacheCleanupResult DB.CleanUpResult
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)              

newtype ErrorResult = ErrorResult Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)              

data WorkerResult r = WorkerResult {
        payload   :: Either ErrorResult r,        
        clockTime :: TimeMs,
        stats     :: ProcessStats
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)    

-- Entry point for a worker. It is supposed to run within a worker process 
-- and do the actual work.
-- 
executeWork :: WorkerInput 
            -> (ExitCode -> IO ()) -- ^ How to exit the worker process.
            -> (WorkerInput -> (forall a . TheBinary a => a -> IO ()) -> IO ()) -- ^ Actual work to be executed.                            
            -> IO ()
executeWork input exitWith_ actualWork = 
    -- Check if version of the executable has changed compared to the parent.
    -- If that's the case, it usually means we are in the middle of an upgrade. 
    -- In this case bail out, it's likely we can do more harm then good
    if input ^. #parentExecutableVersion /= thisExecutableVersion
        then 
            exitWith_ replacedExecutableExitCode
        else do 
            exitCode <- newEmptyTMVarIO            
            let done ec = atomically $ void $ tryPutTMVar exitCode ec

            mapM_ (\w -> forkFinally w (const $ pure ())) [
                    (actualWork input writeWorkerOutput >> done ExitSuccess) 
                        `IOExc.onException` 
                        done exceptionExitCode,
                    dieIfParentDies done,
                    dieAfterTimeout done,
                    dieOfOveruse done
                ]
                
            exitWith_ =<< atomically (takeTMVar exitCode)
  where            
    -- Keep track of who's the current process parent: if it is not the same 
    -- as we started with then parent exited/is killed. Exit the worker as well,
    -- there's no point continuing.
    dieIfParentDies done = forever $ do
        threadDelay 500_000
        parentId <- getParentProcessID
        when (parentId /= input ^. #initialParentId) $
            done parentDiedExitCode

    -- Time bomb. Wait for the certain timeout and then exit.
    dieAfterTimeout done = do
        let Timebox timebox = input ^. #workerTimeout
        threadDelay $ toMicroseconds timebox
        done timeoutExitCode

    -- Exit if the worker has spent more than it is allowed of any of the 
    -- resources it is supposed to keep an eye on.
    dieOfOveruse done = forever $ do 
        let IoLimits {..} = input ^. #ioLimits

        for_ (input ^. #cpuLimit) $ \cpuLimit -> do 
            cpuTime <- getCpuTime
            when (cpuTime > cpuLimit) $ done outOfCpuTimeExitCode

        for_ maxIncomingTrafficMb $ \limit -> do 
            traffic <- getIncomingTraffic
            when (traffic > mbToSize limit) $ done tooMuchTrafficExitCode

        when (isJust maxDiskReadMb || isJust maxDiskWriteMb) $ do 
            DiskIO {..} <- getProcessDiskIO
            for_ maxDiskReadMb $ \limit -> 
                when (diskRead > mbToSize limit) $ done tooMuchDiskIoExitCode
            for_ maxDiskWriteMb $ \limit -> 
                when (diskWrite > mbToSize limit) $ done tooMuchDiskIoExitCode

        threadDelay 1_000_000


readWorkerInput :: (MonadIO m) => m WorkerInput
readWorkerInput = liftIO $ deserialise_ . LBS.toStrict <$> LBS.hGetContents stdin

execWithStats :: MonadIO m => m (Either ErrorResult r) -> m (WorkerResult r)
execWithStats f = do        
    (payload, clockTime) <- timedMS f
    stats <- processStat
    pure WorkerResult {..}


writeWorkerOutput :: TheBinary a => a -> IO ()
writeWorkerOutput = LBS.hPut stdout . LBS.fromStrict . serialise_

rtsArguments :: [String] -> [String]
rtsArguments args = [ "+RTS" ] <> defaultRts <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m

rtsN :: Int -> String
rtsN n = "-N" <> Prelude.show n

rtsMemValue :: Int -> String
rtsMemValue mb = Prelude.show mb <> "m"

-- Don't do idle GC, it only spins the CPU without any purpose.
--
-- -F and -Fd are pinned to the RTS defaults on purpose. The main process bakes
-- in a tighter -F/-Fd to keep its own long-lived heap close to its live data,
-- and since workers are the same executable they would otherwise inherit that
-- and quietly run under a different GC regime. They are short-lived and bounded
-- by -M instead, so trading their throughput for residency makes no sense.
-- Per-worker flags are appended after these and still override them (the rrdp
-- and rsync workers set -Fd1 of their own).
defaultRts :: [String]
defaultRts = [ "-I0", "-F2", "-Fd4" ]

parentDiedExitCode, timeoutExitCode, outOfCpuTimeExitCode, outOfMemoryExitCode :: ExitCode
exitKillByTypedProcess, exceptionExitCode, replacedExecutableExitCode :: ExitCode
tooMuchTrafficExitCode, tooMuchDiskIoExitCode :: ExitCode
exceptionExitCode    = ExitFailure 99
parentDiedExitCode   = ExitFailure 111
outOfCpuTimeExitCode = ExitFailure 113
tooMuchTrafficExitCode = ExitFailure 114
tooMuchDiskIoExitCode  = ExitFailure 115
timeoutExitCode      = ExitFailure 122
replacedExecutableExitCode = ExitFailure 123
outOfMemoryExitCode  = ExitFailure 251
exitKillByTypedProcess = ExitFailure (-2)


-- Main entry point to start a worker
-- 
runWorker :: (ValidatorIO es, TheBinary r, Show r) => AppLogger 
            -> WorkerInput            
            -> [String] 
            -> WorkerInfo 
            -> Eff es r
runWorker logger workerInput extraCli workerInfo = do
    let executableToRun = configValue $ workerInput ^. #config . #programBinaryPath
    let worker = 
            setStdin (byteStringInput $ LBS.fromStrict $ serialise_ workerInput) $             
            setStderr createSource $
            setStdout byteStringOutput $
                proc executableToRun $ [ "--worker" ] <> extraCli
    
    logDebug logger [i|Running worker: #{trimmed worker} with timeout #{timeout}.|]       

    runIt worker `catches` [                    
            Handler $ \e@(SomeAsyncException _) -> throwIO e,
            Handler $ \(_ :: IOException)       -> complain [i|Worker #{workerId} died/killed.|],
            Handler $ \e@(SomeException _)      -> complain [i|Worker #{workerId} died in a strange way: #{fmtEx e}|]       
        ] 
  where    

    timeout = unTimebox $ workerInput ^. #workerTimeout
    workerId = workerInput ^. #workerId

    waitForProcess conf f = IOExc.bracket start stop exec
      where
        start = do 
            p <- startProcess conf
            mpid <- getPid p
            forM_ mpid $ \pid -> 
                registerWorker logger $ workerInfo & #workerPid .~ pid
            pure (mpid, p)           

        stop (mpid, p) = do 
            stopProcess p
            forM_ mpid (deregisterWorker logger)

        exec (_, p) = (,) <$> f p <*> waitExitCode p        

    runIt workerConf = do   
        ((_, workerStdout), exitCode) <- 
            liftIO $ waitForProcess workerConf $ \p ->
                concurrently 
                    (runConduitRes $ getStderr p .| sinkLog logger)
                    (atomically $ getStdout p)

        case exitCode of  
            ExitSuccess -> 
                case deserialiseOrFail_ $ LBS.toStrict workerStdout of 
                    Left e -> 
                        complain [i|Failed to deserialise stdout, #{e}, worker #{workerId}, stdout = [#{workerStdout}]|]                             
                    Right r -> 
                        pure r            
            exit@(ExitFailure errorCode)
                | exit == timeoutExitCode -> do                     
                    let message = [i|Worker #{workerId} execution timed out.|]
                    logError logger message
                    trace WorkerTimeoutTrace
                    appError $ InternalE $ WorkerTimeout message
                | exit == outOfCpuTimeExitCode -> do                     
                    let message = [i|Worker #{workerId} ran out of CPU time.|]
                    logError logger message
                    trace WorkerCpuOveruseTrace
                    appError $ InternalE $ WorkerOutOfCpuTime message                    
                | exit == tooMuchTrafficExitCode -> do                     
                    let message = [i|Worker #{workerId} downloaded too much data.|]
                    logError logger message
                    trace WorkerIoOveruseTrace
                    appError $ InternalE $ WorkerTooMuchIO message
                | exit == tooMuchDiskIoExitCode -> do                     
                    let message = [i|Worker #{workerId} did too much disk IO.|]
                    logError logger message
                    trace WorkerIoOveruseTrace
                    appError $ InternalE $ WorkerTooMuchIO message
                | exit == outOfMemoryExitCode -> do                     
                    let message = [i|Worker #{workerId} ran out of memory.|]
                    logError logger message                    
                    appError $ InternalE $ WorkerOutOfMemory message
                | exit == replacedExecutableExitCode -> do                     
                    let message = [i|Worker #{workerId} detected that `rpki-prover` binary is different and exited for good.|]
                    logError logger message                    
                    appError $ InternalE $ WorkerDetectedDifferentExecutable message                
                | exit == exitKillByTypedProcess -> do
                    -- 
                    -- This is a hack to work around a problem in `readProcess`:
                    -- it apparently catches an async exception, kills the process (with some signal?)
                    -- but doesn't rethrow the exception, so all we have is the worker that exited 
                    -- with error code '-2'.
                    --
                    -- TODO try to find a way to fix with `typed-process` features.
                    -- TODO Otherwise make sure it's safe to assume it's always '-2'.
                    -- 
                    -- This logging message is slightly deceiving: it's not that just the worker 
                    -- was killed, but we also know that there was an asynchronous exception, which 
                    -- we retrow here to make sure "outer stack" knows about it.
                    --
                    logError logger [i|Worker #{workerId} died/killed.|]
                    throwIO AsyncCancelled                    
                | otherwise ->     
                    complain [i|Worker #{workerId} exited with code = #{errorCode}|]

    complain message = do 
        logError logger message
        appError $ InternalE $ InternalError message

logWorkerDone :: (Logger logger, MonadIO m) =>
                logger -> WorkerId -> WorkerResult r -> m ()
logWorkerDone logger workerId WorkerResult { stats = ProcessStats {..}, ..} = do    
    let DiskIO {..} = statDiskIO
    logDebug logger $
        [i|Worker #{workerId} completed, cpuTime: #{statCpuTime}ms, |] <>
        [i|clockTime: #{clockTime}ms, maxRtsHeap: #{statMaxRtsHeap}, maxProcessRss: #{statProcessRss}, |] <>
        [i|incoming traffic: #{sizeMb statIncomingTraffic}mb, |] <>
        [i|disk read: #{sizeMb diskRead}mb, disk write: #{sizeMb diskWrite}mb.|] 

{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Worker where

import           Control.Exception.Lifted
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Lens ((^.))

import           Conduit
import           Data.Text
import qualified Data.ByteString.Lazy as LBS

import           Data.String.Interpolate.IsString
import           Data.Hourglass
import           Data.Conduit.Process.Typed

import           GHC.Generics
import           GHC.Stats

import           System.Exit
import           System.IO (stdin, stdout)
import           System.Posix.Types
import           System.Posix.Process

import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.AppContext
import           RPKI.Config
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
import           RPKI.UniqueId


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
    show (WorkerId w) = show w

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
            CompactionParams { 
                targetLmdbEnv :: FilePath 
            } | 
            ValidationParams {                 
                worldVersion :: WorldVersion,
                tals         :: [TAL]
            } | 
            CacheCleanupParams { 
                worldVersion :: WorldVersion
            }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype Timebox = Timebox { unTimebox :: Seconds }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data WorkerInput = WorkerInput {
        workerId                :: WorkerId,
        params                  :: WorkerParams,
        config                  :: Config,
        initialParentId         :: ProcessID,
        workerTimeout           :: Timebox,
        cpuLimit                :: Maybe CPUTime,
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
                        timeout cpuLimit executableVersion

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

data ValidationResult = ValidationResult ValidationState (Maybe Slurm)
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data CacheCleanupResult = CacheCleanupResult DB.CleanUpResult
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)              

data WorkerResult r = WorkerResult {
        payload   :: r,        
        cpuTime   :: CPUTime,
        clockTime :: TimeMs,
        maxMemory :: MaxMemory
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)    

-- Entry point for a worker. It is supposed to run within a worker process 
-- and do the actual work.
-- 
executeWork :: WorkerInput 
            -> (WorkerInput -> (forall a . TheBinary a => a -> IO ()) -> IO ()) -- ^ Actual work to be executed.                
            -> IO ()
executeWork input actualWork = 
    -- Check if version of the executable has changed compared to the parent.
    -- If that's the case, bail out, it's likely we can do more harm then good
    if input ^. #parentExecutableVersion /= thisExecutableVersion
        then 
            exitWith replacedExecutableExitCode
        else do 
            exitCode <- newEmptyTMVarIO            
            let done = atomically . putTMVar exitCode 

            _ <- mapM_ forkIO [
                    ((actualWork input writeWorkerOutput >> done ExitSuccess) 
                        `onException` 
                    done exceptionExitCode),                
                    dieIfParentDies done,
                    dieOfTiming done
                ]
                
            exitWith =<< atomically (takeTMVar exitCode)
  where        
    whicheverHappensFirst a b = either id id <$> race a b

    -- Keep track of who's the current process parent: if it is not the same 
    -- as we started with then parent exited/is killed. Exit the worker as well,
    -- there's no point continuing.
    dieIfParentDies done = do 
        parentId <- getParentProcessID                    
        if parentId /= input ^. #initialParentId
            then done parentDiedExitCode            
            else threadDelay 500_000 >> dieIfParentDies done

    -- exit either because the time is up or too much CPU is spent
    dieOfTiming done = 
        case input ^. #cpuLimit of
            Nothing       -> dieAfterTimeout done
            Just cpuLimit -> whicheverHappensFirst (dieAfterTimeout done) (dieOutOfCpuTime cpuLimit done)

    -- Time bomb. Wait for the certain timeout and then exit.
    dieAfterTimeout done = do
        let Timebox timebox = input ^. #workerTimeout
        threadDelay $ toMicroseconds timebox
        done timeoutExitCode

    -- Exit if the worker consumed too much CPU time
    dieOutOfCpuTime cpuLimit done = go
      where
        go = do 
            cpuTime <- getCpuTime
            if cpuTime > cpuLimit 
                then done outOfCpuTimeExitCode
                else threadDelay 1_000_000 >> go


readWorkerInput :: (MonadIO m) => m WorkerInput
readWorkerInput = liftIO $ deserialise_ . LBS.toStrict <$> LBS.hGetContents stdin

execWithStats :: MonadIO m => m r -> m (WorkerResult r)
execWithStats f = do        
    (payload, clockTime) <- timedMS f
    (cpuTime, maxMemory) <- processStat    
    pure WorkerResult {..}
  

processStat :: MonadIO m => m (CPUTime, MaxMemory)
processStat = do 
    cpuTime <- getCpuTime
    RTSStats {..} <- liftIO getRTSStats
    let maxMemory = MaxMemory $ fromIntegral max_mem_in_use_bytes
    pure (cpuTime, maxMemory)


writeWorkerOutput :: TheBinary a => a -> IO ()
writeWorkerOutput = LBS.hPut stdout . LBS.fromStrict . serialise_

rtsArguments :: [String] -> [String]
rtsArguments args = [ "+RTS" ] <> defaultRts <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m

rtsN :: Int -> String
rtsN n = "-N" <> show n

rtsMemValue :: Int -> String
rtsMemValue mb = show mb <> "m"

-- Don't do idle GC, it only spins the CPU without any purpose
defaultRts :: [String]
defaultRts = [ "-I0" ]

parentDiedExitCode, timeoutExitCode, outOfCpuTimeExitCode, outOfMemoryExitCode :: ExitCode
exitKillByTypedProcess, exceptionExitCode, replacedExecutableExitCode :: ExitCode
exceptionExitCode    = ExitFailure 99
parentDiedExitCode   = ExitFailure 111
outOfCpuTimeExitCode = ExitFailure 113
timeoutExitCode      = ExitFailure 122
replacedExecutableExitCode = ExitFailure 123
outOfMemoryExitCode  = ExitFailure 251
exitKillByTypedProcess = ExitFailure (-2)

worderIdS :: WorkerId -> String
worderIdS (WorkerId w) = unpack w

-- Main entry point to start a worker
-- 
runWorker :: (TheBinary r, Show r)
            => AppLogger 
            -> WorkerInput            
            -> [String] 
            -> ValidatorT IO r
runWorker logger workerInput extraCli = do
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

    waitForProcess conf f = bracket
        (startProcess conf)
        stopProcess
        (\p -> (,) <$> f p <*> waitExitCode p) 

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
logWorkerDone logger workerId WorkerResult {..} = do    
    logDebug logger [i|Worker #{workerId} completed, cpuTime: #{cpuTime}ms, clockTime: #{clockTime}ms, maxMemory: #{maxMemory}.|]
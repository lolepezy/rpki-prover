{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Worker where

import           Codec.Serialise

import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Concurrent.Async

import           Control.Lens ((^.))

import           Data.Text
import qualified Data.ByteString.Lazy as LBS
import           Data.String
import           Data.String.Interpolate.IsString
import           Data.Hourglass

import           GHC.Generics

import           System.Exit
import           System.IO (stdin, stdout)
import           System.Process.Typed
import           System.Posix.Types
import           System.Posix.Process

import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Logging
import           RPKI.Util (fmtEx, textual, trimmed)


{- | This is to run worker processes for some code that is better to be executed in an isolated process.

Every worker 
 - Reads serialised parameters from its stdin (WorkerInput)
 - Writes serialised result to its stdout
 - Logs to stderr (see `withWorkerLogger` in RPKI.Logging)

Running a worker is done using `runWorker` which handles all the process machinery, error handling, etc.

Every worker calls `executeWork` that takes care of worker lifecycle.

`WorkerInput` is used by all workers, `WorkerParams` is individual for every worker type.

Some of the machinery is also in Main.

-}

newtype WorkerId = WorkerId Text
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (Serialise)

instance Show WorkerId where
    show (WorkerId w) = show w

data WorkerParams = RrdpFetchParams { 
                validatorPath :: ValidatorPath, 
                rrdpRepository :: RrdpRepository,
                worldVersion :: WorldVersion 
            }
        |  CompactionParams { 
                targetLmdbEnv :: FilePath 
            }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

newtype Timebox = Timebox Seconds
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data WorkerInput = WorkerInput {
        params          :: WorkerParams,
        config          :: Config,
        initialParentId :: ProcessID,
        workerTimeout    :: Timebox
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)


newtype RrdpFetchResult = RrdpFetchResult 
                            (Either AppError RrdpRepository, ValidationState)    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

newtype CompactionResult = CompactionResult ()                             
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)


runWorker :: (Serialise r, Show r) => 
            AppLogger 
            -> Config            
            -> WorkerId
            -> WorkerParams                             
            -> Timebox
            -> [String] 
            -> ValidatorT IO (r, LBS.ByteString)  
runWorker logger config workerId params timeout extraCli = do  
    thisProcessId <- liftIO getProcessID

    let binaryToRun = config ^. #programBinaryPath    
    let workerStdin = serialise $ WorkerInput params config thisProcessId timeout
    let worker = 
            setStdin (byteStringInput workerStdin) $             
                proc binaryToRun $ [ "--worker" ] <> extraCli

    logDebugM logger [i|Running worker: #{trimmed worker}|]    

    runIt worker `catches` [                    
            Handler $ \e@(SomeAsyncException _) -> throwIO e,
            Handler $ \(_ :: IOException)       -> complain [i|Worker #{workerId} died/killed.|],
            Handler $ \e@(SomeException _)      -> complain [i|Worker #{workerId} died in a strange way: #{fmtEx e}|]       
        ] 
  where    
    runIt worker = do   
        (exitCode, workerStdout, workerStderr) <- liftIO $ readProcess worker                                
        case exitCode of  
            exit@(ExitFailure errorCode)
                | exit == exitTimeout -> do                     
                    let message = [i|Worker #{workerId} execution timed out, stderr = [#{textual workerStderr}]|]
                    logErrorM logger message
                    appError $ InternalE $ WorkerTimeout message
                | exit == exitOutOfMemory -> do                     
                    let message = [i|Worker #{workerId} ran out of memory, stderr = [#{textual workerStderr}]|]
                    logErrorM logger message
                    appError $ InternalE $ WorkerOutOfMemory message
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
                    -- we retrow here to make sure "outer threads" know about it.
                    --
                    logErrorM logger [i|Worker #{workerId} died/killed.|]
                    throwIO AsyncCancelled                    
                | otherwise ->     
                    complain [i|Worker #{workerId} exited with code = #{errorCode}, stderr = [#{textual workerStderr}]|]
            ExitSuccess -> 
                case deserialiseOrFail workerStdout of 
                    Left e -> 
                        complain [i|Failed to deserialise stdout, #{e}, worker #{workerId}, stdout = [#{workerStdout}]|]                             
                    Right r -> 
                        pure (r, workerStderr)

    complain message = do 
        logErrorM logger message
        appError $ InternalE $ InternalError message


-- Entry point for a worker. It is supposed to run withing a worker process 
-- and do the actual work.
-- 
executeWork :: Serialise a => 
                WorkerInput 
            -> (WorkerInput -> (a -> IO ()) -> IO ()) -- ^ Actual work to be executed. 
                                                      -- Second argument is the result handler.
            -> IO ()
executeWork input actualWork = 
    void $ race (actualWork input writeWorkerOutput) (race suicideCheck timeoutWait)
  where    
    -- Keep track of who's the current process parent: if it is not the same 
    -- as we started with then parent exited/is killed. Exit the worker as well,
    -- there's no point continuing.
    suicideCheck = 
        forever $ do
            parentId <- getParentProcessID                    
            when (parentId /= input ^. #initialParentId) $ 
                exitWith exitParentDied    
            threadDelay 500_000                

    -- Time bomb. Wait for the certain timeout and then simply exit.
    timeoutWait = do
        let Timebox (Seconds s) = input ^. #workerTimeout
        threadDelay $ 1_000_000 * fromIntegral s        
        exitWith exitTimeout


readWorkerInput :: (MonadIO m) => m WorkerInput
readWorkerInput = liftIO $ deserialise <$> LBS.hGetContents stdin        

writeWorkerOutput :: Serialise a => a -> IO ()
writeWorkerOutput = LBS.hPut stdout . serialise

rtsArguments :: [String] -> [String]
rtsArguments args = [ "+RTS" ] <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m

rtsN :: Int -> String
rtsN n = "-N" <> show n

exitParentDied, exitTimeout, exitOutOfMemory, exitKillByTypedProcess :: ExitCode
exitParentDied  = ExitFailure 11
exitTimeout     = ExitFailure 12
exitOutOfMemory = ExitFailure 251
exitKillByTypedProcess = ExitFailure (-2)

workerLogMessage :: (Show t, Show s, IsString s) => s -> LBS.ByteString -> t -> s
workerLogMessage workerId stderr elapsed = 
    let workerLog = 
            if LBS.null stderr 
                then "" :: String
                else [i|, 
<worker-log #{workerId}> 
#{textual stderr}</worker-log>|]            
            in [i|Worker #{workerId} done, took #{elapsed}ms#{workerLog}|]  

worderIdS :: WorkerId -> String
worderIdS (WorkerId w) = unpack w
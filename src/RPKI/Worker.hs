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


{- | This is to run worker processes for some code that is better to execute in an isolated process.

Every worker 
 - Reads serialised parameters from its stdin (WorkerInput and WorkerParams)
 - Writes serialised result to its stdout
 - Logs to stderr 


-}

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
            -> WorkerParams                 
            -> Timebox
            -> [String] 
            -> ValidatorT IO (r, LBS.ByteString)  
runWorker logger config params timeout extraCli = do  
    thisProcessId <- liftIO $ getProcessID

    let binaryToRun = config ^. #programBinaryPath    
    let workerStdin = serialise $ WorkerInput params config thisProcessId timeout
    let worker = 
            setStdin (byteStringInput workerStdin) $             
                proc binaryToRun $ [ "--worker" ] <> extraCli

    logDebugM logger [i|Running worker: #{trimmed worker}|]    

    runIt worker `catches` [                    
            Handler $ \e@(SomeAsyncException _) -> throwIO e,
            Handler $ \(_ :: IOException)       -> complain "Worker died/killed",
            Handler $ \e@(SomeException _)      -> complain [i|Worker died in a strange way: #{fmtEx e}|]       
        ] 
  where    
    runIt worker = do   
        (exitCode, workerStdout, workerStderr) <- liftIO $ readProcess worker                                
        case exitCode of  
            exit@(ExitFailure errorCode)
                | exit == exitTimeout -> do                     
                    let message = [i|Worker execution timed out, stderr = [#{textual workerStderr}]|]
                    logErrorM logger message
                    appError $ InternalE $ WorkerTimeout message
                | exit == exitOutOfMemory -> do                     
                    let message = [i|Worker ran out of memory, stderr = [#{textual workerStderr}]|]
                    logErrorM logger message
                    appError $ InternalE $ WorkerOutOfMemory message
                | otherwise ->     
                    complain [i|Worker exited with code = #{errorCode}, stderr = [#{textual workerStderr}]|]
            ExitSuccess -> 
                case deserialiseOrFail workerStdout of 
                    Left e -> 
                        complain [i|Failed to deserialise stdout, #{e}, stdout = [#{workerStdout}]|]                             
                    Right r -> 
                        pure (r, workerStderr)

    complain message = do 
        logErrorM logger message
        appError $ InternalE $ InternalError message


-- This is for worker processes
executeWork :: WorkerInput 
            -> (WorkerInput -> IO ())
            -> IO ()
executeWork input actualWork = 
    void $ race (actualWork input) (race suicideCheck timeoutWait)
  where    
    -- Keep track of who's the current process parent: if it is not the same 
    -- as we started with then parent exited/is killed. Exit the worker as well.
    suicideCheck = 
        forever $ do
            parentId <- getParentProcessID                    
            when (parentId /= input ^. #initialParentId) $ 
                exitWith exitParentDied    
            threadDelay 500_000                

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

exitParentDied, exitTimeout, exitOutOfMemory :: ExitCode
exitParentDied  = ExitFailure 11
exitTimeout     = ExitFailure 12
exitOutOfMemory = ExitFailure 251

workerLogMessage :: (Show t, Show s, IsString s) => s -> LBS.ByteString -> t -> s
workerLogMessage workerId stderr elapsed = 
    let workerLog = 
            if LBS.null stderr 
                then "" :: String
                else [i|, 
<worker-log #{workerId}> 
#{textual stderr}</worker-log>|]            
            in [i|Worker #{workerId} done, took #{elapsed}ms#{workerLog}|]  
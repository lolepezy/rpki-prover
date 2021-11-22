{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.Process where

import           Codec.Serialise

import           Control.Exception.Lifted
import           Control.Monad.IO.Class

import           Control.Lens                     ((^.), (%~), (&))

import qualified Data.ByteString.Lazy as LBS
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           System.Exit
import           System.Process.Typed
import           System.Posix.Types
import           System.Posix.Process

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Logging
import           RPKI.Util (fmtEx)


data WorkerParams = RrdpFetchParams { 
                validatorPath :: ValidatorPath, 
                rrdpRepository :: RrdpRepository,
                worldVersion :: WorldVersion 
            }
        |  CompactionParams { 
                from :: FilePath, 
                to :: FilePath 
            }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)


data WorkerInput = WorkerInput {
        params          :: WorkerParams,
        config          :: Config,
        initialParentId :: ProcessID
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)


class Worker where
    launchWorker :: WorkerParams -> IO ()


runWorker :: (Serialise r, Show r) => 
            AppContext s -> 
                Config
            -> WorkerParams                 
            -> [String] 
            -> ValidatorT IO (r, LBS.ByteString)  
runWorker appContext config1 params extraCli = do  
    thisProcessId <- liftIO $ getProcessID

    let binaryToRun = config1 ^. #programBinaryPath    
    let stdin = serialise $ WorkerInput params config1 thisProcessId
    let worker = 
            setStdin (byteStringInput stdin) $             
                proc binaryToRun $ [ "--worker" ] <> extraCli

    logDebugM logger [i|Running worker: #{worker}|]    

    z <- liftIO $ try $ readProcess worker
    case z of 
        Left (e :: SomeException) -> 
            complain [i|Worker failed with #{fmtEx e}|]              
        Right (exitCode, stdout, stderr) ->                             
            case exitCode of  
                ExitFailure errorCode -> do 
                    complain [i|Worker exited with code = #{errorCode}|]                    
                ExitSuccess -> 
                    case deserialiseOrFail stdout of 
                        Left e -> 
                            complain [i|Failed to deserialise stdout, #{e}, stdout = #{stdout}|]                             
                        Right r -> 
                            pure (r, stderr)
  where
    logger = appContext ^. #logger
    complain message = do 
        logErrorM logger message
        appError $ InternalE $ InternalError message


rtsArguments :: [String] -> [String]
rtsArguments args = [ "+RTS" ] <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m

rtsN :: Int -> String
rtsN n = "-N" <> show n

parentDied :: ExitCode
parentDied = ExitFailure 11

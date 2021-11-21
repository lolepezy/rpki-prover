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

import           Data.Text                        (Text)
import qualified Data.Text                        as Text

import           GHC.Generics

import           System.Exit
import           System.Process.Typed

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database

import           RPKI.Metrics
import           RPKI.Store.Base.Storage
import           RPKI.Time

import           RPKI.Store.Base.LMDB
import           RPKI.Store.AppStorage
import           RPKI.Util (ifJust, fmtEx)


-- data WorkerInput = WorkerInput Text
--     deriving stock (Eq, Ord, Show, Generic)
--     deriving anyclass (Serialise)

data ProcessError = ProcessError Text


runWorker :: (Serialise r, Show r) => 
            AppContext s -> 
                Config
            -> WorkerParams            
            -> WorldVersion            
            -> [String] 
            -> ValidatorT IO (r, LBS.ByteString)  
runWorker AppContext {..} config1 argument worldVersion extraCli = do     
    let binaryToRun = config1 ^. #programBinaryPath
    let stdin = serialise (argument, worldVersion, config1)
    let worker = 
            setStdin (byteStringInput stdin) $             
                proc binaryToRun $  [ "--worker" ] <> extraCli

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
    complain message = do 
        logErrorM logger message
        appError $ InternalE $ InternalError message
    

data WorkerParams = RrdpFetchParams { 
                validatorPath :: ValidatorPath, 
                rrdpRepository :: RrdpRepository 
            }
        |  CompactionParams { 
                from :: FilePath, 
                to :: FilePath 
            }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

-- workerType :: WorkerParams -> String
-- workerType RrdpFetchParams {..}  = "rrdp-fetch"
-- workerType CompactionParams {..} = "compaction"

-- getWorkerType "rrdp-fetch" = Right RrdpFetch
-- getWorkerType "compaction" = Right Compaction
-- getWorkerType t            = Left $ "Unknown sub-process: " <> show t

rtsArguments args = [ "+RTS" ] <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m

rtsN :: Int -> String
rtsN n = "-N" <> show n
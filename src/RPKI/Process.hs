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

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Logging
import           RPKI.Util (fmtEx)


runWorker :: (Serialise r, Show r) => 
            AppContext s -> 
                Config
            -> WorkerParams            
            -> WorldVersion            
            -> [String] 
            -> ValidatorT IO (r, LBS.ByteString)  
runWorker appContext config argument worldVersion extraCli = do     
    let binaryToRun = config ^. #programBinaryPath
    let stdin = serialise (argument, worldVersion, config)
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

rtsArguments :: [String] -> [String]
rtsArguments args = [ "+RTS" ] <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m

rtsN :: Int -> String
rtsN n = "-N" <> show n
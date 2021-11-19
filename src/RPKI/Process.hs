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

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad

import           Control.Lens                     ((^.), (%~), (&))
import           Data.Generics.Product.Typed

import qualified Data.ByteString.Lazy as LBS

import           Data.Int                         (Int64)

import           Data.Hourglass
import           Data.String.Interpolate.IsString

import           Data.Text                        (Text)
import qualified Data.Text                        as Text

import           GHC.Generics

import           System.Exit
import           System.Process.Typed
import           System.Environment.FindBin

import           RPKI.AppContext
import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database

import           RPKI.Metrics
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time

import           RPKI.Store.Base.LMDB
import           RPKI.Store.AppStorage
import           RPKI.Util (ifJust, fmtEx)


data SubProcessInput = SubProcessInput Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data ProcessError = ProcessError Text


runSubProcess :: (Serialise r) => 
                AppContext s 
            -> SubProcessParams
            -> [String] 
            -> ValidatorT IO (r, LBS.ByteString)
  
runSubProcess AppContext {..} argument extraCli = do     
    let stdin = serialise argument
    let subProcess = 
            setStdin (byteStringInput stdin) $             
                proc __Bin__ 
                    $  [ "--sub-process", subProcessType argument ]
                    <> [ "--rpki-root-directory",  config ^. typed @Config . #rootDirectory ] 
                    <> [ "--cpu-count",           show $ config ^. #parallelism . #cpuCount ] 
                    <> extraCli

    (exitCode, stdout, stderr) <- fromTry (InternalE . InternalError . fmtEx) 
                                    $ readProcess subProcess
    case exitCode of  
        ExitFailure errorCode ->
            appError $ InternalE $ InternalError [i|Exit status #{errorCode}, stderr = #{stderr}$|]  
        ExitSuccess -> do
            r <- fromTry (InternalE . InternalError . fmtEx) $ pure $ deserialise stdout
            pure (r, stderr)    
    

data SubProcessParams = RrdpFetch { uri :: RrdpURL }
                    |  Compaction { from :: FilePath, to :: FilePath }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

subProcessType :: SubProcessParams -> String
subProcessType RrdpFetch {..} = "rrdp-fetch"
subProcessType Compaction {..} = "compaction"

rtsArguments args = [ "+RTS" ] <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m
rtsN n = "-AL" <> show n
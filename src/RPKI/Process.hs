{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
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

import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.TopDown

import           RPKI.AppContext
import           RPKI.Metrics
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time

import           RPKI.Store.Base.LMDB
import           RPKI.Store.AppStorage
import           RPKI.Util (ifJust)


data SubProcess = RrdpFetch | Validator | Compaction

data SubProcessInput = SubProcessInput Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)


runSubProcess :: (Serialise a, Serialise r) => 
                AppContext s 
            -> a
            -> [String] 
            -> IO (r, LBS.ByteString)
  
runSubProcess AppContext {..} argument cli = do     
    let subProcess = 
            setStdin (byteStringInput $ serialise argument) $             
                proc __Bin__ 
                    $ [ "--rpki-root-directory",  config ^. typed @Config . #rootDirectory ] <> cli

    (exitCode, out, err) <- readProcess subProcess
    let r = deserialise out

    case exitCode of  
        ExitFailure errorCode -> do
            pure (r, err)
        ExitSuccess -> do
            pure (r, err)

    pure (r, err)


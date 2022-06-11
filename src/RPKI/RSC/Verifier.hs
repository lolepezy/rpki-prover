{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.RSC.Verifier where

import           Control.Concurrent.Async
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Lens

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8

import           Data.Generics.Product.Typed
import           GHC.Generics (Generic)

import           Data.Bifunctor
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
import           RPKI.Parse.Parse

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Worker

import           RPKI.Store.AppStorage
import           RPKI.SLURM.Types
import           RPKI.Util

import qualified RPKI.Store.Database              as DB


rscVerify :: Storage s => AppContext s -> FilePath -> FilePath -> ValidatorT IO ()
rscVerify appContext@AppContext {..} rscFile directory = do 
    bs     <- fromTry (ParseE . ParseError . fmtEx) $ BS.readFile rscFile
    parsed <- vHoist $ fromEither $ first ParseE $ parseRSC bs

    case getAKI parsed of 
        Nothing  -> appError $ ValidationE NoAKI 
        Just aki -> do 
            taCerts <- getTACertificates
            findCertificateChainToTA aki taCerts
  where
    findCertificateChainToTA aki taCerts = do
        
        pure ()

    getTACertificates = do 
        pure []
    

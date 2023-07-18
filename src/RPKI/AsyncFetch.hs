{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module RPKI.AsyncFetch where

import           Control.Exception
import           Control.Monad
import           Control.Concurrent.STM

import           Control.Lens
import           Data.Generics.Product.Typed
import           GHC.Generics (Generic)

import           Data.List
import           Data.List.NonEmpty          (NonEmpty (..), fromList)
import           Data.Foldable (for_)
import           Data.Int                         (Int64)
import qualified Data.Text                        as Text
import qualified Data.Map.Strict as Map
import           Data.Maybe                       (fromMaybe)

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
import           RPKI.Messages
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Metrics.System
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.Validation.TopDown

import           RPKI.AppContext
import           RPKI.Metrics.Prometheus
import           RPKI.RTR.RtrServer
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Fetch
import           RPKI.Repository
import           RPKI.Worker
import           RPKI.Periodic

import           RPKI.Store.AppStorage
import           RPKI.SLURM.Types

{- 
    Run periodic fetches for slow repositories asychronously to the validation process.

    - Periodically check if there're repositories that don't have speed `Quick`
    - Try to refetch these repositories
    - 
-}
runAsyncFetcher appContext@AppContext {..} = do 
    pure ()


runFetches appContext@AppContext {..} = do     
    repositoryProcessing <- newRepositoryProcessingIO config
    fetchSlowRepos repositoryProcessing `finally` (cancelFetchTasks repositoryProcessing)
  where
    fetchSlowRepos repositoryProcessing = do 
        pps <- readTVarIO $ repositoryProcessing ^. #publicationPoints
        let problematicRepositories = findSpeedProblems pps
        for_ problematicRepositories $ \(url, repository) -> do 

            -- TODO This fiddling is weird and probably redundant
            let pp = case repository of 
                        RsyncR r -> RsyncPP $ r ^. #repoPP
                        RrdpR r  -> RrdpPP r

            let ppAccess = PublicationPointAccess $ fromList [pp]            
            worldVersion <- newWorldVersion
            r <- runValidatorT (newScopes' RepositoryFocus url) $ 
                    fetchPPWithFallback appContext repositoryProcessing worldVersion ppAccess
            pure ()
        fetchValidation <- validationStateOfFetches repositoryProcessing
        pure ()
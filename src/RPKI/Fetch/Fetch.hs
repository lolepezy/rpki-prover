{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Fetch.Fetch where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens hiding (indices, Indexable)
import           Control.Monad.Except

import qualified Data.List.NonEmpty          as NonEmpty

import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           Data.Data
import           Data.Foldable                   (for_)
import           Data.Maybe 
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map            
import qualified Data.Map.Monoidal.Strict        as MonoidalMap     
import           Data.String.Interpolate.IsString
import           Data.IxSet.Typed                (IxSet, Indexable, IsIndexOf, ixFun, ixList)
import qualified Data.IxSet.Typed                as IxSet

import           GHC.Generics

import           Time.Types

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Repository
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storage
import           RPKI.Time
import           RPKI.Parallel
import           RPKI.Util                       
import           RPKI.Rsync
import           RPKI.RRDP.Http
import           RPKI.TAL
import           RPKI.Fetch.Common
import           RPKI.RRDP.RrdpFetch

-- Fetch one individual repository. 
-- 
-- Returned repository has all the metadata updated (in case of RRDP session and serial).
-- The metadata is also updated in the database.
--
fetchRepository :: (Storage s) => 
                    AppContext s 
                -> FetchConfig
                -> WorldVersion
                -> Repository 
                -> ValidatorT IO Fetched
fetchRepository 
    appContext@AppContext {..}
    fetchConfig
    worldVersion
    repo = do
        logInfo logger [i|Fetching #{getURL repoURL}.|]   
        case repo of
            RsyncR r -> do 
                (r', updates) <- fetchRsyncRepository r
                pure $ Fetched (RsyncR r') (V.fromList updates) Nothing                
            RrdpR r  -> do 
                (r', stat, updates) <- fetchRrdpRepository r
                pure $ Fetched (RrdpR r') (V.fromList updates) (Just stat)                
  where
    repoURL = getRpkiURL repo    
    -- Give the process some time to kill itself, 
    -- before trying to kill it from here
    timeToKillItself = Seconds 5
    
    fetchRrdpRepository r = do
        let fetcherTimeout = fetchConfig ^. #rrdpTimeout
        let totalTimeout = fetcherTimeout + timeToKillItself
        timeoutVT totalTimeout
            (do
                let fetchConfig' = fetchConfig & #rrdpTimeout .~ fetcherTimeout
                (z, elapsed) <- timedMS $ fromTryM 
                                    (RrdpE . UnknownRrdpProblem . fmtEx) 
                                    (runRrdpFetchWorker appContext fetchConfig' worldVersion r)
                logInfo logger [i|Fetched #{getURL repoURL}, took #{elapsed}ms.|]
                pure z)            
            (do 
                logError logger [i|Couldn't fetch repository #{getURL repoURL} after #{totalTimeout}.|]
                trace WorkerTimeoutTrace
                appError $ RrdpE $ RrdpDownloadTimeout totalTimeout)

    fetchRsyncRepository r = do 
        let fetcherTimeout = fetchConfig ^. #rsyncTimeout
        let totalTimeout = fetcherTimeout + timeToKillItself
        timeoutVT 
            totalTimeout
            (do
                let fetchConfig' = fetchConfig & #rsyncTimeout .~ fetcherTimeout
                (z, elapsed) <- timedMS $ fromTryM 
                                    (RsyncE . UnknownRsyncProblem . fmtEx) 
                                    (runRsyncFetchWorker appContext fetchConfig' worldVersion r)
                logInfo logger [i|Fetched #{getURL repoURL}, took #{elapsed}ms.|]
                pure z)
            (do 
                logError logger [i|Couldn't fetch repository #{getURL repoURL} after #{totalTimeout}.|]
                trace WorkerTimeoutTrace
                appError $ RsyncE $ RsyncDownloadTimeout totalTimeout)        
          


-- | Fetch TA certificate based on TAL location(s)
--
fetchTACertificate :: AppContext s -> FetchConfig -> TAL -> ValidatorT IO (RpkiURL, RpkiObject)
fetchTACertificate appContext@AppContext {..} fetchConfig tal = 
    go $ sortRrdpFirst $ neSetToList $ unLocations $ talCertLocations tal
  where
    go []         = appError $ TAL_E $ TALError "None of the certificate locations could be fetched."
    go (u : uris) = tryFetch `catchError` goToNext 
      where 
        tryFetch = 
            timeoutVT timeout fetchTaCert (goToNext timeoutError)
        
        (timeout, timeoutError) = let 
            rsyncT = fetchConfig ^. #rsyncTimeout
            rrdpT  = fetchConfig ^. #rrdpTimeout
            in case u of 
                RsyncU _ -> (rsyncT, RsyncE $ RsyncDownloadTimeout rsyncT)
                RrdpU _  -> (rrdpT, RrdpE $ RrdpDownloadTimeout rrdpT)

        fetchTaCert = do                     
            logInfo logger [i|Fetching TA certificate from #{getURL u}.|]
            ro <- case u of 
                RsyncU rsyncU -> rsyncRpkiObject appContext fetchConfig rsyncU
                RrdpU rrdpU   -> downloadRpkiObject appContext fetchConfig rrdpU
            pure (u, ro)
            
        goToNext e = do            
            let message = [i|Failed to fetch #{getURL u}: #{e}|]
            logError logger message
            validatorWarning $ VWarning e
            go uris            



-- | Check if an URL need to be re-fetched, based on fetch status and current time.
--
needsFetching :: WithRpkiURL r => r -> Maybe Seconds -> FetchStatus -> ValidationConfig -> Now -> Bool
needsFetching r fetchInterval status ValidationConfig {..} (Now now) = 
    case status of
        Pending         -> True
        FetchedAt time  -> tooLongAgo time
        FailedAt time   -> not $ closeEnoughMoments (Earlier time) (Later now) minimalRepositoryRetryInterval
  where
    tooLongAgo momendTnThePast =      
        not $ closeEnoughMoments (Earlier momendTnThePast) (Later now) (interval $ getRpkiURL r)
      where 
        interval url = fromMaybe (defaultInterval url) fetchInterval            
        defaultInterval (RrdpU _)  = rrdpRepositoryRefreshInterval
        defaultInterval (RsyncU _) = rsyncRepositoryRefreshInterval          


getPrimaryRepositoryUrl :: PublicationPoints 
                         -> PublicationPointAccess 
                         -> RpkiURL
getPrimaryRepositoryUrl pps ppAccess = 
    let primary = NonEmpty.head $ unPublicationPointAccess ppAccess
    in maybe (getRpkiURL primary) getRpkiURL $ repositoryFromPP pps primary


getFetchables :: PublicationPoints -> PublicationPointAccess -> [(RpkiURL, FetchStatus)]
getFetchables pps ppAccess = 
    [ (getRpkiURL repo, getFetchStatus repo)
        | pp <- NonEmpty.toList $ unPublicationPointAccess ppAccess,                
          repo <- maybeToList $ repositoryFromPP pps pp ]

            
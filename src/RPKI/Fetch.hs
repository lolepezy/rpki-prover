{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Fetch where

import           Control.Concurrent              as Conc
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens hiding (indices, Indexable)
import           Control.Monad.Except

import qualified Data.List.NonEmpty          as NonEmpty

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
import           RPKI.RRDP.RrdpFetch


data Fetchers = Fetchers {
        -- All fetchables after the last validation, i.e. repositories
        -- with their fallbacks
        fetcheables :: TVar Fetcheables,

        -- Fetchers that are currently running
        runningFetchers :: TVar (Map RpkiURL ThreadId),        

        -- Version for the first fetch that was finished for every repository.
        -- We want to track this to for the "one-off" mode.
        firstFinishedFetchBy :: TVar (Map RpkiURL WorldVersion),        

        -- Semaphore for untrusted fetches, i.e fetches that have 
        -- no decent history of being successful 
        untrustedFetchSemaphore :: Semaphore,        

        -- Semaphore for trusted fetches, i.e. fetches 
        -- that have already succeeded
        trustedFetchSemaphore :: Semaphore,        

        -- Semaphore for rsync fetches per host, used to no exceed 
        -- the limit of connections per rsync host
        rsyncPerHostSemaphores  :: TVar (Map RsyncHost Semaphore),

        -- Mapping of repositories to the TAs they are mentioned in
        uriByTa :: TVar UriTaIxSet
    }
    deriving stock (Generic)

type UriTaIxSet = IxSet Indexes UrlTA

data UrlTA = UrlTA RpkiURL TaName
    deriving stock (Show, Eq, Ord, Generic, Data, Typeable)    

type Indexes = '[RpkiURL, TaName]

instance Indexable Indexes UrlTA where
    indices = ixList
        (ixFun (\(UrlTA url _) -> [url]))
        (ixFun (\(UrlTA _ ta)  -> [ta]))        

deleteByIx :: (Indexable ixs a, IsIndexOf ix ixs) => ix -> IxSet ixs a -> IxSet ixs a
deleteByIx ix_ s = foldr IxSet.delete s $ IxSet.getEQ ix_ s

dropFetcher :: Fetchers -> RpkiURL -> IO ()
dropFetcher Fetchers {..} url = mask_ $ do
    readTVarIO runningFetchers >>= \running -> do
        for_ (Map.lookup url running) $ \thread -> do
            Conc.throwTo thread AsyncCancelled
            atomically $ do
                modifyTVar' runningFetchers $ Map.delete url
                modifyTVar' uriByTa $ deleteByIx url

updateUriPerTa :: Map TaName Fetcheables -> UriTaIxSet -> UriTaIxSet
updateUriPerTa fetcheablesPerTa uriTa = uriTa'
  where 
    -- TODO Optimise it
    cleanedUpPerTa = foldr deleteByIx uriTa $ Map.keys fetcheablesPerTa        

    uriTa' = 
        IxSet.insertList [ UrlTA url ta | 
                (ta, Fetcheables fs) <- Map.toList fetcheablesPerTa,
                url <- MonoidalMap.keys fs
            ] cleanedUpPerTa 

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
                -> ValidatorT IO (Repository, Maybe RrdpFetchStat)
fetchRepository 
    appContext@AppContext {..}
    fetchConfig
    worldVersion
    repo = do
        logInfo logger [i|Fetching #{getURL repoURL}.|]   
        case repo of
            RsyncR r -> do 
                r' <- fetchRsyncRepository r
                pure (RsyncR r', Nothing)                
            RrdpR r  -> do 
                (r', stat) <- fetchRrdpRepository r
                pure (RrdpR r', Just stat)                
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
                logError logger [i|Couldn't fetch repository #{getURL repoURL} after #{totalTimeout}s.|]
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
                logError logger [i|Couldn't fetch repository #{getURL repoURL} after #{totalTimeout}s.|]
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

            
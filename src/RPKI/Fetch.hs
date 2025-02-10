{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Fetch where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.IO.Class

import           Control.Lens
import           Data.Generics.Product.Typed

import           Control.Exception

import           Control.Monad
import           Control.Monad.Except

import qualified Data.List.NonEmpty          as NonEmpty

import           Data.String.Interpolate.IsString
import           Data.Maybe                  
import qualified Data.Set                    as Set
import qualified StmContainers.Map           as StmMap
import qualified ListT

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
import           RPKI.Util                       
import           RPKI.Rsync
import           RPKI.RRDP.Http
import           RPKI.TAL
import           RPKI.RRDP.RrdpFetch
import           RPKI.Parallel 

    
{- 
  Fetch repositories in the sync mode (i.e. during top-down validation
  with blocking until function returns).

  For a given list of publication points
    * Download the first one in the list
    * If download failed, timed out or took time above "slow threshold"
    * mark all publication points in the list as ForAsyncFetch
    
  Essentially, to keep ForSyncFetch status a publication point needs to
  response successfully and be fast.

  Fall-back is disabled in the sync mode, so if primary repoistory doesn't 
  respond, we skip all the fall-backs and mark them all as ForAsyncFetch.
-}
fetchQuickly :: (MonadIO m, Storage s) => 
            AppContext s                       
        -> RepositoryProcessing
        -> WorldVersion
        -> PublicationPointAccess  
        -> ValidatorT m [FetchResult]
fetchQuickly appContext@AppContext {..}     
    repositoryProcessing@RepositoryProcessing {..}
    worldVersion
    ppa = do 
        pps <- readPublicationPoints repositoryProcessing
        case config ^. #validationConfig . #fetchMethod of 

            SyncOnly -> do 
                -- It is a bit artificial case, because in "sync-only" mode
                -- we only fetch the primary repository (i.e. RRDP one).
                -- It exists mainly for comparisons and measurements and 
                -- not very useful in practice.
                let primaryRepo = getPrimaryRepository ppa
                (:[]) <$>  
                    fetchOnePp appContext (syncFetchConfig config) 
                        repositoryProcessing worldVersion primaryRepo
                        (\meta _ -> pure meta)
                
            SyncAndAsync -> do 
                let (syncPp, asyncRepos) = onlyForSyncFetch pps ppa
                case syncPp of 
                    Nothing -> do 
                        -- There's nothing to be fetched in the sync mode, 
                        -- so just mark all of them for async fetching.                
                        markForAsyncFetch repositoryProcessing asyncRepos     
                        pure []           
                    Just syncPp_ -> do  
                        -- In sync mode fetch only the first PP
                        fetchResult <-                     
                            fetchOnePp appContext (syncFetchConfig config) 
                                repositoryProcessing worldVersion syncPp_ 
                                (newMetaCallback syncPp_ pps)

                        case fetchResult of 
                            FetchSuccess _ _ -> pure ()
                            FetchFailure _ _ -> do 
                                -- In case of failure mark all repositories ForAsyncFetch
                                ppsAfter <- readPublicationPoints repositoryProcessing
                                let toMarkAsync = mapMaybe (repositoryFromPP ppsAfter) 
                                                    $ NonEmpty.toList $ unPublicationPointAccess ppa
                                markForAsyncFetch repositoryProcessing toMarkAsync
                    
                        pure $! [fetchResult]
  where
    newMetaCallback syncPp_ pps newMeta fetchMoment = do
        -- Set fetchType to ForAsyncFetch to all fall-back URLs, 
        -- do not try them synchronously anymore
        let urlToSkip = getRpkiURL syncPp_
        case newMeta ^. #fetchType of     
            ForAsyncFetch _ -> 
                modifyTVar' publicationPoints $ \currentPps -> 
                    foldr 
                        (\pp pps_ -> 
                            if urlToSkip == getRpkiURL pp 
                                then pps_ 
                            else 
                                case repositoryFromPP pps pp of 
                                    Nothing   -> pps_
                                    Just repo -> let 
                                        newMeta_ = getMeta repo & #fetchType .~ ForAsyncFetch fetchMoment
                                        in updateMeta (pps_ ^. typed) [(repo, newMeta_)]
                            )
                        currentPps
                        (unPublicationPointAccess ppa)
            _ -> pure ()

        pure newMeta
                                                

{- 
  Fetch with fallback going through all (both RRDP and rsync) options.
-}
fetchWithFallback :: (MonadIO m, Storage s) => 
                    AppContext s       
                -> RepositoryProcessing
                -> WorldVersion
                -> FetchConfig
                -> PublicationPointAccess  
                -> ValidatorT m [FetchResult]
fetchWithFallback   
    appContext@AppContext {..}                  
    repositoryProcessing
    worldVersion
    fetchConfig
    ppa
    = go True $ NonEmpty.toList $ unPublicationPointAccess ppa        
  where
    go _ [] = pure []
    go isPrimary (pp : rest) = do     
        fetchResult <- fetchOnePp appContext fetchConfig 
                            repositoryProcessing worldVersion pp (newMetaCallback isPrimary)
        case fetchResult of     
            FetchFailure _ _ -> do 
                -- try the next one
                case rest of 
                    []            -> pure []
                    (ppNext : _ ) -> do                             
                        pps <- readPublicationPoints repositoryProcessing
                        case repositoryFromPP pps pp of     
                            Nothing -> do
                                logError logger [i|Internal error: #{pp} doesn't have corresponding repository, it should never happen.|]
                                pure []
                            Just repo -> do 
                                now' <- thisInstant
                                -- CHeck this only for more meaningful logging    
                                let nextOneNeedAFetch = needsFetching pp 
                                        (getMeta repo ^. #refreshInterval) 
                                        (getFetchStatus repo) 
                                        (config ^. #validationConfig) 
                                        now'
                                logWarn logger $ if nextOneNeedAFetch
                                    then [i|Failed to fetch #{getURL pp}, will fall-back to the next one: #{getURL $ getRpkiURL ppNext}.|]
                                    else [i|Failed to fetch #{getURL pp}, next one (#{getURL $ getRpkiURL ppNext}) is up-to-date.|]

                                (fetchResult :) <$> go False rest

            _ -> pure [fetchResult]
  
    newMetaCallback isPrimary newMeta fetchMoment = 
        case config ^. #proverRunMode of 
            OneOffMode {} -> pure newMeta
            ServerMode    ->
                -- We are doing async fetch here, so we are not going to promote fall-back 
                -- repositories back to ForSyncFetch type. I.e. if a CA has publication 
                -- points as "repo_a - fall-back-to -> repo_b", repo_b is never going to 
                -- become ForSyncFetch, only repo_a can become sync and only after it is 
                -- back to normal state.                
                pure $ if isPrimary 
                    then newMeta
                    else newMeta & #fetchType .~ ForAsyncFetch fetchMoment        


fetchOnePp :: (MonadIO m, Storage s) => 
                AppContext s       
            -> FetchConfig                  
            -> RepositoryProcessing
            -> WorldVersion
            -> PublicationPoint  
            -> (RepositoryMeta -> Instant -> STM RepositoryMeta)
            -> ValidatorT m FetchResult
fetchOnePp 
    appContext@AppContext {..}     
    fetchConfig
    repositoryProcessing@RepositoryProcessing {..}
    worldVersion
    pp 
    newMetaCallback
    = do 
        parentScope <- askScopes        
        fetchResult <- liftIO $ fetchOnce parentScope
        setFetchValidationState repositoryProcessing fetchResult
        pure fetchResult
  where

    fetchOnce parentScope = 
        join $ atomically $ do      
            ppsKey <- ppSeqKey 
            StmMap.lookup ppsKey fetchRuns >>= \case            
                Just Stub         -> retry
                Just (Fetching a) -> pure $ wait a
                Nothing -> do                                         
                    StmMap.insert Stub ppsKey fetchRuns
                    pure $ bracketOnError 
                            (async $ evaluate =<< fetchPPOnce parentScope)
                            (stopAndDrop fetchRuns ppsKey) 
                            (rememberAndWait fetchRuns ppsKey)
    ppSeqKey = do         
        case pp of
            RrdpPP _  -> pure $ getRpkiURL pp
            RsyncPP _ -> do  
                pps <- readTVar publicationPoints
                pure $ getRpkiURL $ getFetchablePP pps pp                
    
    fetchPPOnce parentScope = do 
        ((repoUrl, initialFreshness, (r, validations)), elapsed) <- timedMS doFetch      
        let validations' = updateFetchMetric repoUrl initialFreshness validations r elapsed       
        pure $ case r of
            Left _     -> FetchFailure repoUrl validations'                
            Right repo -> FetchSuccess repo validations'      
      where        
        doFetch = do 
            fetchMoment <- thisInstant
            join $ atomically $ do                                     
                (repoNeedAFetch, repo) <- needsAFetch fetchMoment
                let rpkiUrl = getRpkiURL repo
                pure $ if repoNeedAFetch then 
                    (rpkiUrl, Nothing, ) <$> fetchPP parentScope repo fetchMoment  
                else
                    pure (rpkiUrl, Just NoFetchNeeded, (Right repo, mempty))

        -- This is hacky but basically setting the "fetched/up-to-date" metric
        -- without ValidatorT/PureValidatorT (we can only run it in IO).
        updateFetchMetric repoUrl initialFreshness validations r elapsed = 
            case repoUrl of 
                RrdpU _ -> 
                    let 
                        updatedFreshness = rrdpMetricUpdate validations rrdpFreshness
                    in case r of 
                        Left _  -> rrdpMetricUpdate updatedFreshness (#totalTimeMs %~ updateTime)
                        Right _ -> updatedFreshness
                RsyncU _ -> let 
                        updatedFreshness = rsyncMetricUpdate validations rsyncFreshness
                    in case r of 
                        Left _  -> rsyncMetricUpdate updatedFreshness (#totalTimeMs %~ updateTime)
                        Right _ -> updatedFreshness           
          where
            rrdpFreshness metrics = 
                metrics & #fetchFreshness .~ g (rrdpRepoHasUpdates metrics)

            rsyncFreshness metrics = 
                metrics & #fetchFreshness .~ g (rsyncRepoHasUpdates metrics)
            
            g condition = flip fromMaybe initialFreshness $ case r of 
                            Left _ -> FetchFailed
                            Right _ 
                                | condition -> Updated
                                | otherwise -> NoUpdates

            repoScope = validatorSubScope' RepositoryFocus repoUrl parentScope     
            rrdpMetricUpdate v f  = v & typed @RawMetric . #rrdpMetrics  %~ updateMetricInMap (repoScope ^. typed) f
            rsyncMetricUpdate v f = v & typed @RawMetric . #rsyncMetrics %~ updateMetricInMap (repoScope ^. typed) f
            -- this is also a hack to make sure time is updated if the fetch has failed 
            -- and we probably don't have time at all if the worker timed out                                       
            updateTime t = if t == mempty then elapsed else t

      
    -- Do fetch the publication point and update the #publicationPoints
    -- 
    -- A fetcher can proceed if either
    --   - there's a free slot in the semaphore
    --   - we are waiting for a free slot for more than N seconds
    --
    -- In practice that means hanging timing out fetchers cannot 
    -- block the pool for too long and new fetchers will get through anyway.
    -- Fetching processes hanging on a connection don't take resources, 
    -- so it's safer to risk overloading the system with a lot of processes
    -- that blocking fetch entirely.
    --     
    fetchPP parentScope repo (Now fetchMoment) = do 
        let Seconds (fromIntegral . (*1000_000) -> intervalMicroSeconds) = fetchConfig ^. #fetchLaunchWaitDuration
        withSemaphoreAndTimeout fetchSemaphore intervalMicroSeconds $ do         
            let rpkiUrl = getRpkiURL repo                    
            let repoScope = validatorSubScope' RepositoryFocus rpkiUrl parentScope
            
            ((r, validations), duratioMs) <- timedMS 
                                            $ runValidatorT repoScope 
                                            $ fetchRepository appContext fetchConfig worldVersion repo
            newMeta_ <- atomically $ do
                let (newRepo, newStatus, rrdpStats) = case r of                             
                        Left _               -> (repo, FailedAt fetchMoment, Nothing)
                        Right (repo', stats) -> (repo', FetchedAt fetchMoment, stats)

                let newMeta = deriveNewMeta config fetchConfig newRepo validations 
                                            rrdpStats duratioMs newStatus fetchMoment

                -- `Call externally passed callback
                newMeta' <- newMetaCallback newMeta fetchMoment

                modifyTVar' publicationPoints $ \pps -> 
                        updateMeta (pps ^. typed) [(newRepo, newMeta')]

                pure newMeta'

            logDebug logger [i|New meta for #{rpkiUrl}: #{newMeta_}|]
            pure (fmap fst r, validations)

    stopAndDrop stubs key asyncR = liftIO $ do         
        cancel asyncR
        atomically $ StmMap.delete key stubs        

    rememberAndWait stubs key asyncR = liftIO $ do 
        atomically $ StmMap.insert (Fetching asyncR) key stubs
        wait asyncR

    needsAFetch now' = do 
        pps <- readTVar publicationPoints        
        let Just repo = repositoryFromPP pps pp
        let needsFetching' = needsFetching pp 
                (getMeta repo ^. #refreshInterval) 
                (getFetchStatus repo) 
                (config ^. #validationConfig) 
                now'
        pure (needsFetching', repo)


deriveNewMeta config fetchConfig repo validations rrdpStats 
              duration@(TimeMs duratioMs) status fetchMoment = 
    RepositoryMeta {..}
  where    
    lastFetchDuration = Just duration

    refreshInterval = let 
        -- For RRDP: 
        --   * Increase refresh interval if we know that are no updates
        --   * Decrease refresh interval if there are more that 1 delta in the update
        --   * Keep the same if there's exacty one delta            
        --   * Do not decrease further than 1 minute and don't increase for more than 10 minutes
        -- 
        -- For rsync keep refresh interval the same.
        --   
        vConfig = config ^. #validationConfig

        defaultRefreshInterval = 
            case repo of
                RrdpR _  -> vConfig ^. #rrdpRepositoryRefreshInterval
                RsyncR _ -> vConfig ^. #rsyncRepositoryRefreshInterval

        trimInterval interval = 
            max (vConfig ^. #minFetchInterval) 
                (min (vConfig ^. #maxFetchInterval) interval)            

        -- Extra seconds are to increase or decrese even very small values
        -- Increase by ~10% each time, decrease by ~30%
        increaseInterval (Seconds s) = Seconds $ s + 1 + s `div` 10        
        decreaseInterval (Seconds s) = Seconds $ s - s `div` 3 - 1

        moreThanOne = ( > 1) . NonEmpty.length

        in Just $ 
            case vConfig ^. #fetchIntervalCalculation of 
                Constant -> defaultRefreshInterval
                Adaptive -> 
                    case getMeta repo ^. #refreshInterval of 
                        Nothing -> defaultRefreshInterval
                        Just ri -> 
                            case rrdpStats of 
                                Nothing                 -> defaultRefreshInterval
                                Just RrdpFetchStat {..} -> 
                                    case action of 
                                        NothingToFetch _ -> trimInterval $ increaseInterval ri 
                                        FetchDeltas {..} 
                                            | moreThanOne sortedDeltas -> trimInterval $ decreaseInterval ri 
                                            | otherwise                -> ri                                    
                                        FetchSnapshot _ _ -> ri                    

    fetchType =
        -- If the fetch timed out then it's definitely for async fetch
        -- otherwise, check how long did it take to download
        if WorkerTimeoutTrace `Set.member` (validations ^. #traces)
            then ForAsyncFetch fetchMoment
            else 
                case status of                                         
                    FailedAt _ -> ForAsyncFetch fetchMoment
                    _          -> speedByTiming        
      where
        Seconds rrdpSlowSec  = fetchConfig ^. #rrdpSlowThreshold
        Seconds rsyncSlowSec = fetchConfig ^. #rsyncSlowThreshold        
        speedByTiming = 
            case repo of
                RrdpR _  -> deriveFetchType $ duratioMs > 1000 * rrdpSlowSec
                RsyncR _ -> deriveFetchType $ duratioMs > 1000 * rsyncSlowSec

        deriveFetchType condition = 
            if condition 
                then ForAsyncFetch fetchMoment
                else ForSyncFetch fetchMoment       


deriveNextTimeout :: Config -> Seconds -> RepositoryMeta -> Seconds
deriveNextTimeout config absoluteMaxDuration RepositoryMeta {..} = 
    case config ^. #validationConfig . #fetchTimeoutCalculation of 
        Constant -> absoluteMaxDuration
        Adaptive -> 
            case lastFetchDuration of
                Nothing       -> absoluteMaxDuration
                Just duration -> let
                    previousDuration = Seconds 1 + Seconds (unTimeMs duration `div` 1000)
                    heuristicalNextTimeout = 
                        if | previousDuration < Seconds 3  -> Seconds 10
                           | previousDuration < Seconds 10 -> Seconds 20
                           | previousDuration < Seconds 30 -> previousDuration + Seconds 30
                           | otherwise                     -> previousDuration + Seconds 60             
                    in min absoluteMaxDuration heuristicalNextTimeout


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
        let fetcherTimeout = deriveNextTimeout config (fetchConfig ^. #rrdpTimeout) (r ^. #meta)        
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
        let fetcherTimeout = deriveNextTimeout config (fetchConfig ^. #rsyncTimeout) (r ^. #meta)        
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
    go []         = appError $ TAL_E $ TALError "No of certificate location could be fetched."
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
        FailedAt time   -> not $ closeEnoughMoments time now minimalRepositoryRetryInterval
  where
    tooLongAgo momendTnThePast =      
        not $ closeEnoughMoments momendTnThePast now (interval $ getRpkiURL r)
      where 
        interval url = fromMaybe (defaultInterval url) fetchInterval            
        defaultInterval (RrdpU _)  = rrdpRepositoryRefreshInterval
        defaultInterval (RsyncU _) = rsyncRepositoryRefreshInterval          



validationStateOfFetches :: MonadIO m => RepositoryProcessing -> m ValidationState 
validationStateOfFetches repositoryProcessing = liftIO $ 
    atomically $ 
        fmap (foldr (\(_, vs) r -> r <> vs) mempty) $ 
            ListT.toList $ StmMap.listT $ 
                repositoryProcessing ^. #indivudualFetchResults    

setFetchValidationState :: MonadIO m => RepositoryProcessing -> FetchResult -> m ()
setFetchValidationState repositoryProcessing fr = liftIO $ do        
    let (u, vs) = case fr of
            FetchFailure r vs'    -> (r, vs')
            FetchSuccess repo vs' -> (getRpkiURL repo, vs')
        
    atomically $ StmMap.insert vs u (repositoryProcessing ^. #indivudualFetchResults)
    

cancelFetchTasks :: RepositoryProcessing -> IO ()    
cancelFetchTasks RepositoryProcessing {..} = do 
    fetches <- atomically $ ListT.toList $ StmMap.listT fetchRuns     
    mapM_ cancel [ a | (_, Fetching a) <- fetches ]    

readPublicationPoints :: MonadIO m => 
                        RepositoryProcessing                         
                        -> m PublicationPoints
readPublicationPoints repositoryProcessing = liftIO $ 
    readTVarIO $ repositoryProcessing ^. #publicationPoints    

getPrimaryRepositoryUrl :: PublicationPoints 
                         -> PublicationPointAccess 
                         -> RpkiURL
getPrimaryRepositoryUrl pps ppAccess = 
    let primary = getPrimaryRepository ppAccess
    in maybe (getRpkiURL primary) getRpkiURL $ repositoryFromPP pps primary

getPrimaryRepository :: PublicationPointAccess -> PublicationPoint
getPrimaryRepository ppAccess = 
    NonEmpty.head $ unPublicationPointAccess ppAccess    

getFetchablePP :: PublicationPoints -> PublicationPoint -> PublicationPoint
getFetchablePP pps = \case 
    r@(RrdpPP _) -> r
    r@(RsyncPP rpp@(RsyncPublicationPoint rsyncUrl)) -> 
        case rsyncRepository (mergeRsyncPP rpp pps) rsyncUrl of 
            Nothing   -> r
            Just repo -> RsyncPP $ repo ^. #repoPP            

onlyForSyncFetch :: PublicationPoints 
                -> PublicationPointAccess 
                -> (Maybe PublicationPoint, [Repository])
onlyForSyncFetch pps ppAccess = let
        
    ppaList = NonEmpty.toList $ unPublicationPointAccess ppAccess

    asyncOnes = [ r  | (isAsyncRepo, Just r) <- map checkAsync ppaList, isAsyncRepo ]
    syncOnes  = [ pp | (pp, (isAsync, _)) <- map (\pp -> (pp, checkAsync pp)) ppaList, not isAsync ]
    
    in (listToMaybe syncOnes, asyncOnes)

  where   
    checkAsync pp = 
        case repositoryFromPP pps pp of 
            Nothing -> (False, Nothing)
            Just r  -> (isForAsync $ getFetchType r, Just r)                        

resetForAsyncFetch ::  MonadIO m => RepositoryProcessing -> m ()
resetForAsyncFetch RepositoryProcessing {..} = liftIO $ atomically $ do 
    modifyTVar' publicationPoints (#usedForAsync .~ mempty)

markForAsyncFetch ::  MonadIO m => RepositoryProcessing -> [Repository] -> m ()
markForAsyncFetch RepositoryProcessing {..} repos = liftIO $ atomically $ 
    unless (null repos) $
        modifyTVar' publicationPoints
            (#usedForAsync %~ Set.insert (map getRpkiURL repos))


syncFetchConfig :: Config -> FetchConfig
syncFetchConfig config = let 
        rsyncConfig = config ^. typed @RsyncConf
        rrdpConfig = config ^. typed @RrdpConf
        rsyncTimeout = rsyncConfig ^. #rsyncTimeout
        rrdpTimeout  = rrdpConfig ^. #rrdpTimeout
        rsyncSlowThreshold = slowThreshold rsyncTimeout
        rrdpSlowThreshold = slowThreshold rrdpTimeout
        fetchLaunchWaitDuration = Seconds 30 
        cpuLimit = max (rrdpConfig ^. #cpuLimit) (rsyncConfig ^. #cpuLimit)
    in FetchConfig {..}

asyncFetchConfig :: Config -> FetchConfig
asyncFetchConfig config = let 
        rsyncConfig = config ^. typed @RsyncConf
        rrdpConfig = config ^. typed @RrdpConf
        rsyncTimeout = rsyncConfig ^. #asyncRsyncTimeout
        rrdpTimeout  = rrdpConfig ^. #asyncRrdpTimeout
        rsyncSlowThreshold = slowThreshold rsyncTimeout
        rrdpSlowThreshold = slowThreshold rrdpTimeout
        fetchLaunchWaitDuration = Seconds 60
        cpuLimit = max (rrdpConfig ^. #cpuLimit) (rsyncConfig ^. #cpuLimit)
    in FetchConfig {..}

slowThreshold :: Seconds -> Seconds
slowThreshold t = min t (Seconds 60)

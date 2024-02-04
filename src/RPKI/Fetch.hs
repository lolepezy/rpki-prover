{-# LANGUAGE DerivingStrategies #-}
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
import           Data.Maybe                  (listToMaybe, catMaybes)
import qualified Data.Set                    as Set
import qualified StmContainers.Map           as StmMap
import qualified ListT                       as ListT

import           Time.Types

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Repository
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
fetchSync :: (MonadIO m, Storage s) => 
            AppContext s                       
        -> RepositoryProcessing
        -> WorldVersion
        -> PublicationPointAccess  
        -> ValidatorT m (Maybe FetchResult)
fetchSync appContext@AppContext {..}     
    repositoryProcessing@RepositoryProcessing {..}
    worldVersion
    ppa = do 
        pps <- readPublicationPoints repositoryProcessing
        let (syncPp, asyncRepos) = onlyForSyncFetch pps ppa
        case syncPp of 
            Nothing -> do 
                -- There's nothing to be fetched in the sync mode, 
                -- so just mark all of them for async fetching.                
                markForAsyncFetch repositoryProcessing asyncRepos     
                pure Nothing           
            Just syncPp_ -> do  
                -- In sync mode fetch only the first PP
                fetchResult <-                     
                    fetchOnePp appContext (syncFetchConfig config) 
                        repositoryProcessing worldVersion syncPp_ 
                        (newMetaCallback syncPp_ pps)

                case fetchResult of 
                    FetchSuccess _ _ -> pure ()
                    FetchFailure _ _ -> do 
                        -- In case of failure mark ell repositories ForAsyncFetch
                        ppsAfter <- readPublicationPoints repositoryProcessing
                        let toMarkAsync = catMaybes 
                                            $ map (repositoryFromPP ppsAfter) 
                                            $ NonEmpty.toList $ unPublicationPointAccess ppa
                        markForAsyncFetch repositoryProcessing toMarkAsync
            
                pure $ Just fetchResult
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
  Fetch repositories in the async mode (i.e. concurrently to top-down validation).

  For a given list of publication points try to fetch them one-by-one.
  Return if one of them succeeds.
-}
fetchAsync :: (MonadIO m, Storage s) => 
                    AppContext s       
                -> RepositoryProcessing
                -> WorldVersion
                -> PublicationPointAccess  
                -> ValidatorT m [FetchResult]
fetchAsync   
    appContext@AppContext {..}                  
    repositoryProcessing
    worldVersion
    ppa
    = go True $ NonEmpty.toList $ unPublicationPointAccess ppa        
  where
    go _ [] = pure []
    go isPrimary (pp : rest) = do     
        fetchResult <- fetchOnePp appContext (asyncFetchConfig config) 
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
                                let nextOneNeedAFetch = needsFetching pp (getFetchStatus repo) (config ^. #validationConfig) now'                                
                                logWarn logger $ if nextOneNeedAFetch
                                    then [i|Failed to fetch #{getURL pp}, will fall-back to the next one: #{getURL $ getRpkiURL ppNext}.|]
                                    else [i|Failed to fetch #{getURL pp}, next one (#{getURL $ getRpkiURL ppNext}) is up-to-date.|]
                                go False rest

            _ -> pure [fetchResult]
  
    -- We are doing async fetch here, so we are not going to promote fall-back 
    -- repositories back to ForSyncFetch type. I.e. if a CA has publication 
    -- points as "repo_a - fall-back-to -> repo_b", repo_b is never going to 
    -- become ForSyncFetch, only repo_a can become sync and only after it is 
    -- back to normal state.
    newMetaCallback isPrimary newMeta fetchMoment = 
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
        ((repoUrl, fetchFreshness, (r, validations)), elapsed) <- timedMS doFetch      
        let validations' = updateFetchMetric repoUrl fetchFreshness validations r elapsed       
        pure $ case r of
            Left _     -> FetchFailure repoUrl validations'                
            Right repo -> FetchSuccess repo validations'      
      where        
        doFetch = do 
            fetchMoment <- thisInstant
            (rpkiUrl, fetchFreshness, fetchIO) <- atomically $ do                                     
                (repoNeedAFetch, repo) <- needsAFetch fetchMoment
                let rpkiUrl = getRpkiURL repo
                pure $ if repoNeedAFetch then 
                    (rpkiUrl, AttemptedFetch, fetchPP parentScope repo fetchMoment)                
                else
                    (rpkiUrl, UpToDate, pure (Right repo, mempty))                

            f <- fetchIO
            pure (rpkiUrl, fetchFreshness, f)

        -- This is hacky but basically setting the "fetched/up-to-date" metric
        -- without ValidatorT/PureValidatorT.
        updateFetchMetric repoUrl fetchFreshness validations r elapsed = let
                realFreshness = either (const FailedToFetch) (const fetchFreshness) r
                repoScope = validatorSubScope' RepositoryFocus repoUrl parentScope     
                rrdpMetricUpdate v f  = v & typed @RawMetric . #rrdpMetrics  %~ updateMetricInMap (repoScope ^. typed) f
                rsyncMetricUpdate v f = v & typed @RawMetric . #rsyncMetrics %~ updateMetricInMap (repoScope ^. typed) f
                -- this is also a hack to make sure time is updated if the fetch has failed 
                -- and we probably don't have time at all if the worker timed out                                       
                updateTime t = if t == mempty then elapsed else t
            in case repoUrl of 
                RrdpU _ -> let 
                        updatedFreshness = rrdpMetricUpdate validations (& #fetchFreshness .~ realFreshness)                            
                    in case r of 
                        Left _  -> rrdpMetricUpdate updatedFreshness (& #totalTimeMs %~ updateTime)
                        Right _ -> updatedFreshness
                RsyncU _ -> let 
                        updatedFreshness = rsyncMetricUpdate validations (& #fetchFreshness .~ realFreshness)                            
                    in case r of 
                        Left _  -> rsyncMetricUpdate updatedFreshness (& #totalTimeMs %~ updateTime)
                        Right _ -> updatedFreshness           
      
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
            
            ((r, validations), duratioMs) <- timedMS $ runValidatorT repoScope 
                                            $ fetchRepository appContext fetchConfig worldVersion repo
            atomically $ do
                let (newRepo, newStatus) = case r of                             
                        Left _      -> (repo, FailedAt fetchMoment)
                        Right repo' -> (repo', FetchedAt fetchMoment)

                let newMeta = deriveNewMeta fetchConfig newRepo validations duratioMs newStatus fetchMoment

                -- Call externally passed callback
                newMeta' <- newMetaCallback newMeta fetchMoment

                modifyTVar' publicationPoints $ \pps -> 
                        updateMeta (pps ^. typed) [(newRepo, newMeta')]

            pure (r, validations)

    stopAndDrop stubs key asyncR = liftIO $ do         
        cancel asyncR
        atomically $ StmMap.delete key stubs        

    rememberAndWait stubs key asyncR = liftIO $ do 
        atomically $ StmMap.insert (Fetching asyncR) key stubs
        wait asyncR

    needsAFetch now' = do 
        pps <- readTVar publicationPoints        
        let Just repo = repositoryFromPP pps pp
        let needsFetching' = needsFetching pp (getFetchStatus repo) (config ^. #validationConfig) now'
        pure (needsFetching', repo)




deriveNewMeta fetchConfig repo validations duration@(TimeMs duratioMs) status fetchMoment = 
    RepositoryMeta {..}
  where    
    lastFetchDuration = Just duration

    fetchType =
        -- If there fetch timed out then it's definitely for async fetch
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


deriveNextTimeout :: Seconds -> RepositoryMeta -> Seconds
deriveNextTimeout absoluteMaxDuration RepositoryMeta {..} = 
    case lastFetchDuration of
        Nothing       -> absoluteMaxDuration
        Just duration -> let
                previousSeconds = Seconds 1 + Seconds (unTimeMs duration `div` 1000)
                heuristicalNextTimeout = 
                    if | previousSeconds < Seconds 5  -> Seconds 10
                       | previousSeconds < Seconds 10 -> Seconds 20
                       | previousSeconds < Seconds 30 -> previousSeconds + Seconds 30
                       | otherwise                    -> previousSeconds + Seconds 60             
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
                -> ValidatorT IO Repository
fetchRepository 
    appContext@AppContext {..}
    fetchConfig
    worldVersion
    repo = do
        logInfo logger [i|Fetching #{getURL repoURL}.|]   
        case repo of
            RsyncR r -> RsyncR <$> fetchRsyncRepository r
            RrdpR r  -> RrdpR  <$> fetchRrdpRepository r
  where
    repoURL = getRpkiURL repo    
    -- Give the process some time to kill itself, 
    -- before trying to kill it from here
    timeToKillItself = Seconds 5
    
    fetchRrdpRepository r = do 
        let fetcherTimeout = deriveNextTimeout (fetchConfig ^. #rrdpTimeout) (r ^. #meta)        
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
        let fetcherTimeout = deriveNextTimeout (fetchConfig ^. #rsyncTimeout) (r ^. #meta)        
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
    go []         = appError $ TAL_E $ TALError "No certificate location could be fetched."
    go (u : uris) = fetchTaCert `catchError` goToNext 
      where 
        fetchTaCert = do                     
            logInfo logger [i|Fetching TA certicate from #{getURL u}.|]
            ro <- case u of 
                RsyncU rsyncU -> rsyncRpkiObject appContext fetchConfig rsyncU
                RrdpU rrdpU   -> fetchRpkiObject appContext fetchConfig rrdpU
            pure (u, ro)

        goToNext e = do            
            let message = [i|Failed to fetch #{getURL u}: #{e}|]
            logError logger message
            validatorWarning $ VWarning e
            go uris            



-- | Check if an URL need to be re-fetched, based on fetch status and current time.
--
needsFetching :: WithRpkiURL r => r -> FetchStatus -> ValidationConfig -> Now -> Bool
needsFetching r status ValidationConfig {..} (Now now) = 
    case status of
        Pending         -> True
        FetchedAt time  -> tooLongAgo time
        FailedAt time   -> not $ closeEnoughMoments time now minimalRepositoryRetryInterval
  where
    tooLongAgo momendTnThePast = 
        not $ closeEnoughMoments momendTnThePast now (interval $ getRpkiURL r)
      where 
        interval (RrdpU _)  = rrdpRepositoryRefreshInterval
        interval (RsyncU _) = rsyncRepositoryRefreshInterval          


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
    let primary = NonEmpty.head $ unPublicationPointAccess ppAccess
    in maybe (getRpkiURL primary) getRpkiURL $ repositoryFromPP pps primary

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
    modifyTVar' publicationPoints (& #usedForAsync .~ mempty)

markForAsyncFetch ::  MonadIO m => RepositoryProcessing -> [Repository] -> m ()
markForAsyncFetch RepositoryProcessing {..} repos = liftIO $ atomically $ 
    unless (null repos) $
        modifyTVar' publicationPoints
            (& #usedForAsync %~ (Set.insert (map getRpkiURL repos)))


syncFetchConfig :: Config -> FetchConfig
syncFetchConfig config = let 
        rsyncTimeout = config ^. typed @RsyncConf . #rsyncTimeout
        rrdpTimeout  = config ^. typed @RrdpConf . #rrdpTimeout
        rsyncSlowThreshold = slowThreshold rsyncTimeout
        rrdpSlowThreshold = slowThreshold rrdpTimeout
        fetchLaunchWaitDuration = Seconds 30 
    in FetchConfig {..}

asyncFetchConfig :: Config -> FetchConfig
asyncFetchConfig config = let 
        rsyncTimeout = config ^. typed @RsyncConf . #asyncRsyncTimeout
        rrdpTimeout  = config ^. typed @RrdpConf . #asyncRrdpTimeout
        rsyncSlowThreshold = slowThreshold rsyncTimeout
        rrdpSlowThreshold = slowThreshold rrdpTimeout
        fetchLaunchWaitDuration = Seconds 60 
    in FetchConfig {..}

slowThreshold :: Seconds -> Seconds
slowThreshold t = min t (Seconds 60)

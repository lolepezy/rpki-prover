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
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set

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
import           RPKI.Parallel (withSemaphore)


-- Main entry point: fetch repository using the cache of tasks.
-- It is guaranteed that every fetch happens only once.
--
-- Fall-back means that the function will try to fetch all the PPs in the 
-- `ppAccess` argumens until one of them succeed.
--
-- NOTE: This is an over-generalised version of fetching, since the current
-- RFC prescribes to only fallback from RRDP to rsync publication point,
-- i.e. `ppAccess` will always consist of 1 or 2 elements.
-- 
-- However, generic implementation is just simpler. Whether this generic
-- fetching procedure will be ever useful for fetching from more than
-- one RRDP links -- no idea.
--
fetchPPWithFallback :: (MonadIO m, Storage s) => 
                            AppContext s                         
                        -> RepositoryProcessing
                        -> WorldVersion
                        -> PublicationPointAccess  
                        -> ValidatorT m [FetchResult]
fetchPPWithFallback 
    appContext@AppContext {..}     
    repositoryProcessing@RepositoryProcessing {..}
    worldVersion
    ppAccess = do 
        parentScope <- askScopes        
        frs <- liftIO $ fetchOnce parentScope ppAccess        
        setValidationStateOfFetches repositoryProcessing frs            
  where
    runForKey runs key = Map.lookup key <$> readTVar runs            

    -- Use "run only once" logic for the whole list of PPs
    fetchOnce parentScope ppAccess' =          
        join $ atomically $ do      
            ppsKey <- ppSeqKey 
            runForKey ppSeqFetchRuns ppsKey >>= \case            
                Just Stub         -> retry
                Just (Fetching a) -> pure $ wait a

                Nothing -> do                                         
                    modifyTVar' ppSeqFetchRuns $ Map.insert ppsKey Stub
                    pure $ bracketOnError 
                                (async $ evaluate =<< fetchWithFallback parentScope (ppsToList ppAccess'))
                                (stopAndDrop ppSeqFetchRuns ppsKey) 
                                (rememberAndWait ppSeqFetchRuns ppsKey)                
      where   
        ppsToList = NonEmpty.toList . unPublicationPointAccess

        ppSeqKey = sequence 
                $ map (urlToDownload . getRpkiURL) 
                $ ppsToList ppAccess
          where
            urlToDownload u = 
                case u of
                    RrdpU _ -> pure u
                    RsyncU rsyncUrl -> do  
                        pps <- readTVar publicationPoints
                        pure $ maybe u (RsyncU . fst) $ infoInRsyncTree rsyncUrl (pps ^. typed)            


    fetchWithFallback :: Scopes -> [PublicationPoint] -> IO [FetchResult]

    fetchWithFallback _           []   = pure []
    fetchWithFallback parentScope [pp] = do 
        ((repoUrl, fetchFreshness, (r, validations)), elapsed) <- timedMS $ fetchPPOnce parentScope pp                
        let validations' = updateFetchMetric repoUrl fetchFreshness validations r elapsed     
        pure $ case r of
            Left _     -> [FetchFailure repoUrl validations']
            Right repo -> [FetchSuccess repo validations']        
      where
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

    fetchWithFallback parentScope (pp : pps') = do 
        fetch <- fetchWithFallback parentScope [pp]
        case fetch of            
            [FetchSuccess {}] -> pure fetch

            [FetchFailure {}] -> do                 
                -- some terribly hacky stuff for more meaningful logging
                let nextOne = head pps'
                now' <- thisInstant
                (nextOneNeedAFetch, _) <- atomically $ needsAFetch nextOne now'
                logWarn logger $ if nextOneNeedAFetch
                    then [i|Failed to fetch #{getURL $ getRpkiURL pp}, will fall-back to the next one: #{getURL $ getRpkiURL nextOne}.|]
                    else [i|Failed to fetch #{getURL $ getRpkiURL pp}, next one (#{getURL $ getRpkiURL nextOne}) is up-to-date.|]                

                nextFetch <- fetchWithFallback parentScope pps'
                pure $ fetch <> nextFetch
            
            _shouldNeverHappen -> pure []                    


    -- Use the same "run only once" logic for every repository that needs a fetch
    --     
    fetchPPOnce parentScope pp = do 
        now' <- thisInstant
        (rpkiUrl, fetchFreshness, fetchIO) <- atomically $ do                                     
            (repoNeedAFetch, repo) <- needsAFetch pp now'
            let rpkiUrl = getRpkiURL repo
            if repoNeedAFetch then 
                runForKey indivudualFetchRuns rpkiUrl >>= \case                    
                    Just Stub         -> retry
                    Just (Fetching a) -> pure (rpkiUrl, AttemptedFetch, wait a)

                    Nothing -> do                                         
                        modifyTVar' indivudualFetchRuns $ Map.insert rpkiUrl Stub
                        pure (rpkiUrl, AttemptedFetch, fetchPP parentScope repo)
            else
                pure (rpkiUrl, UpToDate, pure (Right repo, mempty))                

        f <- fetchIO
        pure (rpkiUrl, fetchFreshness, f)
      

    -- Do fetch the publication point and update the #publicationPoints
    -- 
    fetchPP parentScope repo = withSemaphore fetchSemaphore $ do         
        let rpkiUrl = getRpkiURL repo
        let launchFetch = async $ do               
                let repoScope = validatorSubScope' RepositoryFocus rpkiUrl parentScope
                Now fetchTime <- thisInstant
                (r, validations) <- runValidatorT repoScope $ fetchRepository appContext worldVersion repo                                
                atomically $ do
                    modifyTVar' indivudualFetchRuns $ Map.delete rpkiUrl                    

                    let (newRepo, newStatus) = case r of                             
                            Left _      -> (repo, FailedAt fetchTime)
                            Right repo' -> (repo', FetchedAt fetchTime)

                    let speed = if WorkerTimeoutTrace `Set.member` (validations ^. #traces)
                                    then Slow fetchTime
                                    else Quick fetchTime

                    modifyTVar' (repositoryProcessing ^. #publicationPoints) $ \pps -> 
                            adjustSucceededUrl rpkiUrl 
                                    $ updateStatuses (pps ^. typed @PublicationPoints) [(newRepo, newStatus, speed)]
                pure (r, validations)

        bracketOnError 
            launchFetch 
            (stopAndDrop indivudualFetchRuns rpkiUrl) 
            (rememberAndWait indivudualFetchRuns rpkiUrl) 
    
    stopAndDrop stubs key a = liftIO $ do         
        atomically $ modifyTVar' stubs $ Map.delete key
        cancel a

    rememberAndWait stubs key a = liftIO $ do 
        atomically $ modifyTVar' stubs $ Map.insert key (Fetching a)
        wait a

    needsAFetch pp now' = do 
        pps <- readTVar publicationPoints        
        let Just repo = repositoryFromPP (mergePP pp pps) (getRpkiURL pp)
        let needsFetching' = needsFetching pp (getFetchStatus repo) (config ^. #validationConfig) now'
        pure (needsFetching', repo)


-- Fetch one individual repository. 
-- 
-- Returned repository has all the metadata updated (in case of RRDP session and serial).
-- The metadata is also updated in the database.
--
fetchRepository :: (Storage s) => 
                    AppContext s 
                -> WorldVersion
                -> Repository 
                -> ValidatorT IO Repository
fetchRepository 
    appContext@AppContext {..}
    worldVersion
    repo = do
        logInfo logger [i|Fetching #{getURL repoURL}.|]   
        case repo of
            RsyncR r -> RsyncR <$> fetchRsyncRepository r
            RrdpR r  -> RrdpR  <$> fetchRrdpRepository r
  where
    repoURL = getRpkiURL repo    
    
    fetchRsyncRepository r = do 
        let Seconds maxDuration = config ^. typed @RsyncConf . #rsyncTimeout
        timeoutVT 
            (1_000_000 * fromIntegral maxDuration)                 
            (do
                (z, elapsed) <- timedMS $ fromTryM 
                                    (RsyncE . UnknownRsyncProblem . fmtEx) 
                                    (runRsyncFetchWorker appContext worldVersion r)
                logInfo logger [i|Fetched #{getURL repoURL}, took #{elapsed}ms.|]
                pure z)
            (do 
                logError logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDuration}s.|]
                trace WorkerTimeoutTrace
                appError $ RsyncE $ RsyncDownloadTimeout maxDuration)        
    
    fetchRrdpRepository r = do 
        let Seconds maxDuration = config ^. typed @RrdpConf . #rrdpTimeout
        timeoutVT 
            (1_000_000 * fromIntegral maxDuration)           
            (do
                (z, elapsed) <- timedMS $ fromTryM 
                                    (RrdpE . UnknownRrdpProblem . fmtEx) 
                                    (runRrdpFetchWorker appContext worldVersion r)
                logInfo logger [i|Fetched #{getURL repoURL}, took #{elapsed}ms.|]
                pure z)            
            (do 
                logError logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDuration}s.|]
                trace WorkerTimeoutTrace
                appError $ RrdpE $ RrdpDownloadTimeout maxDuration)                



anySuccess :: [FetchResult] -> Bool
anySuccess r = not $ null $ [ () | FetchSuccess{} <- r ]


fetchEverSucceeded :: MonadIO m => 
                   RepositoryProcessing
                -> PublicationPointAccess 
                -> m FetchEverSucceeded 
fetchEverSucceeded 
    repositoryProcessing
    (PublicationPointAccess ppAccess) = liftIO $ do        
        pps <- readTVarIO $ repositoryProcessing ^. #publicationPoints
        pure $ everSucceeded pps $ getRpkiURL $ NonEmpty.head ppAccess


-- | Fetch TA certificate based on TAL location(s)
--
fetchTACertificate :: AppContext s -> TAL -> ValidatorT IO (RpkiURL, RpkiObject)
fetchTACertificate appContext@AppContext {..} tal = 
    go $ sortRrdpFirst $ neSetToList $ unLocations $ talCertLocations tal
  where
    go []         = appError $ TAL_E $ TALError "No certificate location could be fetched."
    go (u : uris) = fetchTaCert `catchError` goToNext 
      where 
        goToNext e = do            
            let message = [i|Failed to fetch #{getURL u}: #{e}|]
            logError logger message
            validatorWarning $ VWarning e
            go uris

        fetchTaCert = do                     
            logInfo logger [i|Fetching TA certicate from #{getURL u}.|]
            ro <- case u of 
                RsyncU rsyncU -> rsyncRpkiObject appContext rsyncU
                RrdpU rrdpU   -> fetchRpkiObject appContext rrdpU
            pure (u, ro)



-- | Check if an URL need to be re-fetched, based on fetch status and current time.
--
needsFetching :: WithRpkiURL r => r -> FetchStatus -> ValidationConfig -> Now -> Bool
needsFetching r status ValidationConfig {..} (Now now) = 
    case status of
        Pending         -> True
        FetchedAt time  -> tooLongAgo time
        -- TODO This is an interesting question and needs some 
        -- It maybe should be 
        -- FailedAt time   -> tooLongAgo time
        FailedAt _      -> True
  where
    tooLongAgo momendTnThePast = 
        not $ closeEnoughMoments momendTnThePast now (interval $ getRpkiURL r)
      where 
        interval (RrdpU _)  = rrdpRepositoryRefreshInterval
        interval (RsyncU _) = rsyncRepositoryRefreshInterval          


validationStateOfFetches :: MonadIO m => RepositoryProcessing -> m ValidationState 
validationStateOfFetches repositoryProcessing = liftIO $
    mconcat . Map.elems 
        <$> readTVarIO (repositoryProcessing ^. #indivudualFetchResults)    


setValidationStateOfFetches :: MonadIO m => RepositoryProcessing -> [FetchResult] -> m [FetchResult] 
setValidationStateOfFetches repositoryProcessing frs = liftIO $ do
    atomically $ do 
        forM_ frs $ \fr -> do 
            let (u, vs) = case fr of
                    FetchFailure r vs'    -> (r, vs')
                    FetchSuccess repo vs' -> (getRpkiURL repo, vs')
                
            modifyTVar' (repositoryProcessing ^. #indivudualFetchResults)
                        $ Map.insert u vs
    pure frs

-- 
cancelFetchTasks :: RepositoryProcessing -> IO ()    
cancelFetchTasks rp = do 
    (ifr, ppSeqFr) <- atomically $ (,) <$>
                        readTVar (rp ^. #indivudualFetchRuns) <*>
                        readTVar (rp ^. #ppSeqFetchRuns)

    mapM_ cancel [ a | (_, Fetching a) <- Map.toList ifr]
    mapM_ cancel [ a | (_, Fetching a) <- Map.toList ppSeqFr]


getPrimaryRepositoryFromPP :: MonadIO m => RepositoryProcessing -> PublicationPointAccess -> m (Maybe RpkiURL)
getPrimaryRepositoryFromPP repositoryProcessing ppAccess = liftIO $ do
    pps <- readTVarIO $ repositoryProcessing ^. #publicationPoints    
    let primary = NonEmpty.head $ unPublicationPointAccess ppAccess
    pure $ getRpkiURL <$> repositoryFromPP (mergePP primary pps) (getRpkiURL primary)    



onlyForSyncFetch :: PublicationPoints 
            -> PublicationPointAccess 
            -> (Maybe PublicationPointAccess, [Repository])
onlyForSyncFetch pps ppAccess = let
        
    ppaList = NonEmpty.toList $ unPublicationPointAccess ppAccess

    slowFilteredOut = [ r  | (isSlowRepo, Just r) <- map checkSlow ppaList, isSlowRepo ]
    quickOnes       = [ pp | (pp, (isSlowRepo, _)) <- map (\pp -> (pp, checkSlow pp)) ppaList, not isSlowRepo ]

    in (PublicationPointAccess <$> NonEmpty.nonEmpty quickOnes, slowFilteredOut)

  where   
    checkSlow pp = 
        case repositoryFromPP (mergePP pp pps) (getRpkiURL pp) of 
            Nothing -> (False, Nothing)
            Just r  -> (isSlow $ getSpeed r, Just r)                        


markAsRequested ::  MonadIO m => RepositoryProcessing -> [Repository] -> m ()
markAsRequested RepositoryProcessing {..} filteredOutRepos = liftIO $ atomically $ do 
    modifyTVar' publicationPoints
        (& #slowRequested %~ (<> Set.fromList (map getRpkiURL filteredOutRepos)))

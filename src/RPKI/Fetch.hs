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

import           Control.Lens
import           Data.Generics.Product.Typed

import           Data.Monoid.Generic

import           Control.Exception

import           Control.Monad
import           Control.Monad.Except

import qualified Data.List.NonEmpty          as NonEmpty

import           Data.String.Interpolate.IsString

import qualified Data.Map.Strict                  as Map

import           Data.Set                         (Set)
import           Data.Maybe (mapMaybe)

import GHC.Generics (Generic)

import Time.Types

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


data RepositoryContext = RepositoryContext {
        publicationPoints  :: PublicationPoints,
        takenCareOf        :: Set RpkiURL
    } 
    deriving stock (Generic)  
    deriving Semigroup via GenericSemigroup RepositoryContext   
    deriving Monoid    via GenericMonoid RepositoryContext


fValidationState :: FetchResult -> ValidationState 
fValidationState (FetchSuccess _ vs) = vs
fValidationState (FetchFailure _ vs) = vs



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
                        -> Now 
                        -> PublicationPointAccess  
                        -> ValidatorT m [FetchResult]
fetchPPWithFallback 
    appContext@AppContext {..}     
    repositoryProcessing
    worldVersion
    now 
    ppAccess = do 
        parentPath <- askEnv         
        frs <- liftIO $ fetchOnce parentPath ppAccess        
        setValidationStateOfFetches repositoryProcessing frs            
  where

    ppSeqFetchRuns = repositoryProcessing ^. #ppSeqFetchRuns
    indivudualFetchRuns = repositoryProcessing ^. #indivudualFetchRuns
    publicationPoints = repositoryProcessing ^. #publicationPoints

    funRun runs key = Map.lookup key <$> readTVar runs            

    -- Use "run only once" logic for the whole list of PPs
    fetchOnce parentPath ppAccess =          
        join $ atomically $ do
            pps <- readTVar publicationPoints           
            let ppsKey = ppSeqKey pps 
            funRun ppSeqFetchRuns ppsKey >>= \case            
                Just Stub         -> retry
                Just (Fetching a) -> pure $ wait a

                Nothing -> do                                         
                    modifyTVar' ppSeqFetchRuns $ Map.insert ppsKey Stub
                    pure $ bracketOnError 
                                (async $ do 
                                    -- logDebug_ logger [i|ppAccess = #{ppAccess}, ppsKey = #{ppsKey}.|]
                                    fetchWithFallback parentPath $ NonEmpty.toList $ unPublicationPointAccess ppAccess) 
                                (stopAndDrop ppSeqFetchRuns ppsKey) 
                                (rememberAndWait ppSeqFetchRuns ppsKey)                
      where
        -- this is dubious, but works
        ppSeqKey pps = take 1 
            $ mapMaybe (\pp -> getRpkiURL <$> repositoryFromPP (mergePP pp pps) (getRpkiURL pp)) 
            $ NonEmpty.toList 
            $ unPublicationPointAccess ppAccess


    fetchWithFallback :: ValidatorPath -> [PublicationPoint] -> IO [FetchResult]
    fetchWithFallback _ [] = pure []

    fetchWithFallback parentPath [pp] = do 
        (repoUrl, fetchFreshness, (r, validations)) <- fetchPPOnce parentPath pp                
        let validations' = updateFetchMetric repoUrl fetchFreshness validations        
        pure $ case r of
            Left _     -> [FetchFailure repoUrl validations']
            Right repo -> [FetchSuccess repo validations']        
      where
        -- This is hacky but basically setting the "fetched/up-to-date" metric
        -- without ValidatorT/PureValidatorT.
        updateFetchMetric repoUrl fetchFreshness validations = 
            let repoPath = validatorSubPath (toText repoUrl) parentPath                   
            in case repoUrl of 
                RrdpU _  -> 
                    validations 
                        & typed @AppMetric . #rrdpMetrics 
                        %~ updateMetricInMap 
                            (repoPath ^. typed) 
                            (& #fetchFreshness .~ fetchFreshness)                     
                RsyncU _ -> 
                    validations 
                        & typed @AppMetric . #rsyncMetrics 
                        %~ updateMetricInMap 
                            (repoPath ^. typed) 
                            (& #fetchFreshness .~ fetchFreshness)                                                             
                    

    fetchWithFallback parentPath (pp : pps') = do 
        fetch <- fetchWithFallback parentPath [pp]
        case fetch of            
            [FetchSuccess {}] -> pure fetch

            [FetchFailure {}] -> do                 
                -- some terribly hacky stuff for more meaningful logging
                let nextOne = head pps'
                (nextOneNeedAFetch, _) <- atomically $ needsAFetch nextOne
                logWarn_ logger $ if nextOneNeedAFetch
                    then [i|Failed to fetch #{getRpkiURL pp}, will fall-back to the next one: #{getRpkiURL nextOne}.|]
                    else [i|Failed to fetch #{getRpkiURL pp}, next one (#{getRpkiURL nextOne}) is up-to-date.|]                

                nextFetch <- fetchWithFallback parentPath pps'
                pure $ fetch <> nextFetch
            
            _shouldNeverHappen -> pure []                    


    -- Use the same "run only once" logic for every repository that needs a fetch
    --     
    fetchPPOnce parentPath pp = do 
        (rpkiUrl, fetchFreshness, fetchIO) <- atomically $ do                                     
            (repoNeedAFetch, repo) <- needsAFetch pp
            let rpkiUrl = getRpkiURL repo
            if repoNeedAFetch 
                then 
                    funRun indivudualFetchRuns rpkiUrl >>= \case                    
                        Just Stub         -> retry
                        Just (Fetching a) -> pure (rpkiUrl, Fetched, wait a)

                        Nothing -> do                                         
                            modifyTVar' indivudualFetchRuns $ Map.insert rpkiUrl Stub
                            pure (rpkiUrl, Fetched, fetchPP parentPath repo rpkiUrl)
                else                         
                    pure (rpkiUrl, UpToDate, pure (Right repo, mempty))                

        f <- fetchIO
        pure (rpkiUrl, fetchFreshness, f)
      

    -- Do fetch the publication point and update the #publicationPoints
    -- 
    fetchPP parentPath repo rpkiUrl = do         
        let launchFetch = async $ do               
                let repoPath = validatorSubPath (toText rpkiUrl) parentPath
                (r, validations) <- runValidatorT repoPath $ fetchRepository appContext worldVersion repo                
                atomically $ do 
                    modifyTVar' indivudualFetchRuns $ Map.delete rpkiUrl                    

                    let (newRepo, newStatus) = case r of                             
                            Left _      -> (repo, FailedAt $ unNow now)
                            Right repo' -> (repo', FetchedAt $ unNow now)

                    modifyTVar' (repositoryProcessing ^. #publicationPoints) $ \pps -> 
                            adjustSucceededUrl rpkiUrl 
                                    $ updateStatuses (pps ^. typed @PublicationPoints) 
                                        [(newRepo, newStatus)]                
                pure (r, validations)

        bracketOnError 
            launchFetch 
            (stopAndDrop indivudualFetchRuns rpkiUrl) 
            (rememberAndWait indivudualFetchRuns rpkiUrl) 
    
    stopAndDrop stubs key a = liftIO $ do 
        cancel a
        atomically $ modifyTVar' stubs $ Map.delete key

    rememberAndWait stubs key a = liftIO $ do 
        atomically $ modifyTVar' stubs $ Map.insert key (Fetching a)
        wait a

    needsAFetch pp = do 
        pps <- readTVar $ repositoryProcessing ^. #publicationPoints
        let asIfMerged = mergePP pp pps
        let Just repo = repositoryFromPP asIfMerged (getRpkiURL pp)
        pure (
            needsFetching pp (getFetchStatus repo) (config ^. #validationConfig) now,
            repo)                                    



-- Fetch one individual repository
-- Returned repository has all the metadata updated (in case of RRDP session and serial)
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
        logInfoM logger [i|Fetching #{getURL repoURL}.|]   
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
                                    (updateObjectForRsyncRepository appContext worldVersion r)
                logInfoM logger [i|Fetched #{getURL repoURL}, took #{elapsed}ms.|]
                pure z)
            (do 
                logErrorM logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDuration}s.|]
                appError $ RsyncE $ RsyncDownloadTimeout maxDuration)        
    
    fetchRrdpRepository r = do 
        let Seconds maxDuration = config ^. typed @RrdpConf . #rrdpTimeout
        timeoutVT 
            (1_000_000 * fromIntegral maxDuration)           
            (do
                (z, elapsed) <- timedMS $ fromTryM 
                                    (RrdpE . UnknownRrdpProblem . fmtEx) 
                                    (runRrdpFetchWorker appContext worldVersion r)
                logInfoM logger [i|Fetched #{getURL repoURL}, took #{elapsed}ms.|]
                pure z)            
            (do 
                logErrorM logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDuration}s.|]
                appError $ RrdpE $ RrdpDownloadTimeout maxDuration)                



anySuccess :: [FetchResult] -> Bool
anySuccess r = not $ null $ [ () | FetchSuccess{} <- r ]


fetchEverSucceeded :: MonadIO m=> 
                    RepositoryProcessing
                -> PublicationPointAccess 
                -> m FetchEverSucceeded 
fetchEverSucceeded 
    repositoryProcessing
    (PublicationPointAccess ppAccess) = liftIO $ do
        let publicationPoints = repositoryProcessing ^. #publicationPoints
        pps <- readTVarIO publicationPoints
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
            logErrorM logger message
            validatorWarning $ VWarning e
            go uris

        fetchTaCert = do                     
            logInfoM logger [i|Fetching TA certicate from #{getURL u}.|]
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
        FailedAt time   -> tooLongAgo time
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
                    FetchFailure r vs   -> (r, vs)
                    FetchSuccess repo vs -> (getRpkiURL repo, vs)        
                
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

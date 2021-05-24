{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Fetch where
    
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Lens
import           Data.Generics.Product.Typed

import           Data.Monoid.Generic

import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.Except

import qualified Data.List.NonEmpty          as NonEmpty

import           Data.String.Interpolate.IsString
import qualified Data.Map.Strict                  as Map

import           Data.Set                         (Set)

import GHC.Generics (Generic)

import Time.Types
import System.Timeout

import           RPKI.AppContext
import           RPKI.AppMonad
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
fValidationState FetchUpToDate = mempty


-- Main entry point: fetch reposiutory using the cache of tasks.
-- It is guaranteed that every fetch happens only once.
--
fetchPPWithFallback :: (MonadIO m, Storage s) => 
                            AppContext s                         
                        -> ValidatorPath 
                        -> Now 
                        -> PublicationPointAccess  
                        -> m ([FetchResult], ValidationState)
fetchPPWithFallback 
    appContext@AppContext {..}     
    parentContext 
    now 
    (PublicationPointAccess ppAccess) = liftIO $ do 
        -- Merge them all in first, all these PPs will be stored 
        -- in `#publicationPoints` with the status 'Pending'
        -- atomically $ modifyTVar' 
        --     (repositoryProcessing ^. #publicationPoints) 
        --     (\pubPoints -> foldr mergePP pubPoints pps)  

        frs <- fetchWithFallback $ NonEmpty.toList ppAccess
        pure (frs, mconcat $ map fValidationState frs)

  where    
    fetchWithFallback :: [PublicationPoint] -> IO [FetchResult]
    fetchWithFallback []   = pure []
    fetchWithFallback [pp] = (:[]) <$> tryPP pp

    fetchWithFallback (pp : pps') = do 
        f <- fetchWithFallback [pp]
        case f of             
            [FetchUpToDate]   -> pure f
            [FetchSuccess {}] -> pure f  

            [FetchFailure {}] -> do                 
                -- some terribly hacky stuff for more meaningful logging
                (nextOneNeedAFetch, _) <- atomically $ needsAFetch (head pps')
                let message = 
                        if nextOneNeedAFetch
                            then [i|Failed to fetch #{getRpkiURL pp}, will fall-back to the next one: #{getRpkiURL $ head pps'}.|]
                            else [i|Failed to fetch #{getRpkiURL pp}, next one (#{getRpkiURL $ head pps'})' is up-to-date.|]

                logWarn_ logger message
                f' <- fetchWithFallback pps'
                pure $ f <> f'           

            _                -> pure []

    needsAFetch :: PublicationPoint -> STM (Bool, Repository)
    needsAFetch pp = do 
        pps <- readTVar $ repositoryProcessing ^. #publicationPoints
        let asIfMerged = mergePP pp pps            
        let Just repo = repositoryFromPP asIfMerged (getRpkiURL pp)
        pure (
            needsFetching pp (getFetchStatus repo) (config ^. #validationConfig) now,
            repo)
            

    tryPP :: PublicationPoint -> IO FetchResult
    tryPP pp = do 
        join $ atomically $ do             
            (repoNeedAFetch, repo) <- needsAFetch pp
            if repoNeedAFetch 
                then do 
                    let rpkiUrl = getRpkiURL repo
                    z <- readTVar $ repositoryProcessing ^. #fetchTasks
                    case Map.lookup rpkiUrl z of 
                        Just Stub           -> retry
                        Just (Fetching a)   -> pure $ wait a
                        Just (Done f)       -> pure $ pure f

                        Nothing -> do                                         
                            modifyTVar' (repositoryProcessing ^. #fetchTasks) $ Map.insert rpkiUrl Stub
                            pure $ fetchIt repo rpkiUrl
                else                         
                    pure $ pure FetchUpToDate                                   

      where
        fetchIt repo rpkiUrl = do 
            let fetchTasks = repositoryProcessing ^. #fetchTasks
            let publicationPoints = repositoryProcessing ^. #publicationPoints
            bracketOnError 
                (async $ do                                     
                    f <- fetchRepository_ appContext parentContext repo
                    atomically $ do                          
                        modifyTVar' fetchTasks (Map.insert rpkiUrl (Done f))

                        modifyTVar' publicationPoints $ \pps -> 
                                let r = pps ^. typed @PublicationPoints
                                    in adjustSucceededUrl rpkiUrl $ case f of
                                        FetchSuccess repo' _ -> updateStatuses r [(repo', FetchedAt $ unNow now)]
                                        FetchFailure repo' _ -> updateStatuses r [(repo', FailedAt $ unNow now)]
                                        FetchUpToDate        -> r
                                    
                                
                    -- TODO Remove it, it is actually not needed.
                    -- 
                    -- It is a funky way to say "schedule deletion to 10 seconds from now".
                    -- All the other threads waiting on the same url will most likely
                    -- be aware of all the updates after 10 seconds.
                    void $ forkFinally 
                                (threadDelay 10_000_000)                    
                                (\_ -> atomically $ modifyTVar' fetchTasks $ Map.delete rpkiUrl)

                    pure f) 
                (\a -> do 
                    cancel a
                    atomically $ modifyTVar' fetchTasks $ Map.delete rpkiUrl)
                (\a -> do 
                    atomically $ modifyTVar' fetchTasks $ Map.insert rpkiUrl (Fetching a)
                    wait a)                                                                   
                                       


-- Fetch specific repository
-- 
fetchRepository_ :: (Storage s) => 
                    AppContext s -> ValidatorPath -> Repository -> IO FetchResult
fetchRepository_ 
    appContext@AppContext {..} 
    parentContext     
    repo = do
        let (Seconds maxDuration, timeoutError) = case repoURL of
                RrdpU _  -> 
                    (config ^. typed @RrdpConf . #rrdpTimeout, 
                     RrdpE $ RrdpDownloadTimeout maxDuration)
                RsyncU _ -> 
                    (config ^. typed @RsyncConf . #rsyncTimeout, 
                     RsyncE $ RsyncDownloadTimeout maxDuration)
                
        r <- timeout (1_000_000 * fromIntegral maxDuration) fetchIt
        case r of 
            Nothing -> do 
                logErrorM logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDuration}s.|]
                pure $ FetchFailure repo (vState $ mError vContext' timeoutError)
            Just z -> pure z        
    where 
        repoURL      = getRpkiURL repo
        childContext = validatorSubPath (toText repoURL) parentContext
        vContext'    = childContext ^. typed @VPath

        fetchIt = do        
            logDebugM logger [i|Fetching repository #{getURL repoURL}.|]    
            ((r, v), elapsed) <- timedMS $ runValidatorT childContext $                 
                case repo of
                    RsyncR r -> do 
                            RsyncR <$> fromTryM 
                                    (RsyncE . UnknownRsyncProblem . fmtEx) 
                                    (updateObjectForRsyncRepository appContext r)                             
                    RrdpR r -> do                         
                        RrdpR <$> fromTryM 
                                    (RrdpE . UnknownRrdpProblem . fmtEx)
                                    (updateObjectForRrdpRepository appContext r) 
            case r of
                Left e -> do                        
                    logErrorM logger [i|Failed to fetch repository #{getURL repoURL}: #{e} |]
                    pure $ FetchFailure repo (vState (mError vContext' e) <> v)
                Right resultRepo -> do
                    logDebugM logger [i|Fetched repository #{getURL repoURL}, took #{elapsed}ms.|]
                    pure $ FetchSuccess resultRepo v          



anySuccess :: [FetchResult] -> Bool
anySuccess r = not $ null [ () | FetchSuccess{} <- r ]


fetchEverSucceeded :: (MonadIO m, Storage s) => 
                    AppContext s
                -> PublicationPointAccess 
                -> m FetchEverSucceeded 
fetchEverSucceeded 
    AppContext {..}         
    (PublicationPointAccess ppAccess) = liftIO $ do
        let publicationPoints = repositoryProcessing ^. #publicationPoints
        pps <- readTVarIO publicationPoints
        pure $ everSucceeded pps $ getRpkiURL $ NonEmpty.head ppAccess


-- | Fetch TA certificate based on TAL location(s)
--
fetchTACertificate :: AppContext s -> TAL -> ValidatorT IO (RpkiURL, RpkiObject)
fetchTACertificate appContext@AppContext {..} tal = 
    go $ neSetToList $ unLocations $ talCertLocations tal
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
            logInfoM logger [i|Fetching TA certicate from #{getURL u}..|]
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


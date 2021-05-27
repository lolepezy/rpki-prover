{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Fetch where

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


validationStateOfFetches :: (MonadIO m, Storage s) => AppContext s -> m ValidationState 
validationStateOfFetches AppContext {..} = liftIO $ atomically $ do 
    z <- readTVar $ repositoryProcessing ^. #fetchResults
    pure $ mconcat [ fValidationState f | (_, f) <- Map.toList z ]    

-- Main entry point: fetch reposiutory using the cache of tasks.
-- It is guaranteed that every fetch happens only once.
--
fetchPPWithFallback :: (MonadIO m, Storage s) => 
                            AppContext s                         
                        -> ValidatorPath 
                        -> Now 
                        -> PublicationPointAccess  
                        -> m [FetchResult]
fetchPPWithFallback 
    appContext@AppContext {..}     
    parentContext 
    now 
    (PublicationPointAccess ppAccess) = liftIO $
        fetchWithFallback $ NonEmpty.toList ppAccess
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
                let nextOne = head pps'
                (nextOneNeedAFetch, _) <- atomically $ needsAFetch nextOne
                logWarn_ logger $ if nextOneNeedAFetch
                    then [i|Failed to fetch #{getRpkiURL pp}, will fall-back to the next one: #{getRpkiURL nextOne}.|]
                    else [i|Failed to fetch #{getRpkiURL pp}, next one (#{getRpkiURL nextOne}) is up-to-date.|]                

                f' <- fetchWithFallback pps'
                pure $ f <> f'           
            
            _shouldNeverHappen -> pure []
            

    tryPP :: PublicationPoint -> IO FetchResult
    tryPP pp = 
        join $ atomically $ do             
            (repoNeedAFetch, repo) <- needsAFetch pp
            if repoNeedAFetch 
                then do 
                    let rpkiUrl = getRpkiURL repo
                    z <- readTVar $ repositoryProcessing ^. #fetches
                    case Map.lookup rpkiUrl z of 
                        Just Stub         -> retry
                        Just (Fetching a) -> pure $ wait a

                        Nothing -> do                                         
                            modifyTVar' (repositoryProcessing ^. #fetches) $ Map.insert rpkiUrl Stub
                            pure $ fetchIt repo rpkiUrl
                else                         
                    pure $ pure FetchUpToDate                                   

      where
        fetchIt repo rpkiUrl = do 
            let fetches = repositoryProcessing ^. #fetches            

            let launchFetch = async $ do                                     
                    f <- fetchRepository_ appContext parentContext repo
                    atomically $ do                          
                        modifyTVar' (repositoryProcessing ^. #fetches) $ Map.delete rpkiUrl
                        modifyTVar' (repositoryProcessing ^. #fetchResults) $ Map.insert rpkiUrl f

                        modifyTVar' (repositoryProcessing ^. #publicationPoints) $ \pps -> 
                            let r = pps ^. typed @PublicationPoints
                                in adjustSucceededUrl rpkiUrl $ case f of
                                    FetchSuccess repo' _ -> updateStatuses r [(repo', FetchedAt $ unNow now)]
                                    FetchFailure repo' _ -> updateStatuses r [(repo', FailedAt $ unNow now)]
                                    FetchUpToDate        -> r
                    pure f            

            let stopAndDrop a = do 
                    cancel a
                    atomically $ modifyTVar' fetches $ Map.delete rpkiUrl

            let addToMap a = do 
                    atomically $ modifyTVar' fetches $ Map.insert rpkiUrl (Fetching a)
                    wait a

            bracketOnError launchFetch stopAndDrop addToMap 
    
    needsAFetch pp = do 
        pps <- readTVar $ repositoryProcessing ^. #publicationPoints
        let asIfMerged = mergePP pp pps            
        let Just repo = repositoryFromPP asIfMerged (getRpkiURL pp)
        pure (
            needsFetching pp (getFetchStatus repo) (config ^. #validationConfig) now,
            repo)                                       


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
            ((r, validations), elapsed) <- timedMS $ runValidatorT childContext $                 
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
                    pure $ FetchFailure repo (vState (mError vContext' e) <> validations)
                Right resultRepo -> do
                    logDebugM logger [i|Fetched repository #{getURL repoURL}, took #{elapsed}ms.|]
                    pure $ FetchSuccess resultRepo validations



anySuccess :: [FetchResult] -> Bool
anySuccess r = not $ null $ [ () | FetchSuccess{} <- r ] <> [ () | FetchUpToDate <- r ]


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


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
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Data.Monoid.Generic

import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS

import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NonEmpty

import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import  Data.Maybe (maybeToList)

import GHC.Generics (Generic)

import Time.Types
import System.Timeout

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Time
import           RPKI.Util                       
import           RPKI.Validation.ObjectValidation
import           RPKI.AppState
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

data FetchTasks = FetchTasks {
    tasks :: TVar (Map RpkiURL (FetchTask FetchResult))    
}

newFetchTasks :: STM FetchTasks
newFetchTasks = FetchTasks <$> newTVar mempty         
           
newFetchTasksIO :: IO FetchTasks
newFetchTasksIO = atomically newFetchTasks

fRepository :: FetchResult -> Repository 
fRepository (FetchSuccess r _) = r
fRepository (FetchFailure r _) = r

fValidationState :: FetchResult -> ValidationState 
fValidationState (FetchSuccess _ vs) = vs
fValidationState (FetchFailure _ vs) = vs

-- This is pretty ad-hoc
mergeFetch :: FetchResult -> FetchResult -> FetchResult
mergeFetch f1 f2 = 
    let vs = fValidationState f1 <> fValidationState f2
    in case (f1, f2) of
        (FetchFailure {}, FetchFailure {}) -> FetchFailure (fRepository f2) vs
        (FetchSuccess {}, FetchFailure {}) -> FetchSuccess (fRepository f1) vs
        (FetchFailure {}, FetchSuccess {}) -> FetchSuccess (fRepository f2) vs
        (FetchSuccess {}, FetchSuccess {}) -> FetchSuccess (fRepository f2) vs


-- Main entry point: fetch reposiutory using the cache of tasks.
-- It is guaranteed that every fetch happens only once.
-- 
fetchRepository :: (MonadIO m, Storage s) => 
                    AppContext s 
                -> FetchTasks 
                -> ValidatorPath 
                -> Now 
                -> Repository 
                -> m FetchResult
fetchRepository appContext@AppContext {..} FetchTasks {..} parentContext now repo = liftIO $ do 
    let rpkiUrl = getRpkiURL repo
    join $ atomically $ do 
        t <- readTVar tasks
        case Map.lookup rpkiUrl t of 

            Just Stub -> retry

            Just (Done f) -> 
                pure $ pure f
                                
            Just (Fetching a) -> 
                pure $ wait a                    

            Nothing -> do 
                modifyTVar' tasks $ Map.insert rpkiUrl Stub
                
                pure $ bracketOnError 
                    (async $ do 
                        f <- fetchRepository_ appContext parentContext now repo
                        atomically $ modifyTVar' tasks $ Map.insert rpkiUrl (Done f)
                        pure f) 
                    (\a -> do 
                        cancel a
                        atomically $ modifyTVar' tasks $ Map.delete rpkiUrl)
                    (\a -> do 
                        atomically $ modifyTVar' tasks $ Map.insert rpkiUrl (Fetching a)
                        wait a)                                           



fetchRepository_ :: (Storage s) => 
                    AppContext s -> ValidatorPath -> Now -> Repository -> IO FetchResult
fetchRepository_ 
    appContext@AppContext {..} 
    parentContext 
    (Now now) 
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
    (PublicationPointAccess pps) = liftIO $ do                 
        frs <- fetchWithFallback $ NonEmpty.toList pps
        pure (frs, mconcat $ map fValidationState frs)

  where    
    fetchWithFallback :: [PublicationPoint] -> IO [FetchResult]
    fetchWithFallback [pp] = (:[]) <$> tryPP pp

    fetchWithFallback (pp : pps') = do 
        f <- fetchWithFallback [pp]
        case f of 
            []                -> pure []
            [FetchSuccess {}] -> pure f  
            [FetchFailure {}] -> (f <>) <$> fetchWithFallback pps'                

    tryPP :: PublicationPoint -> IO FetchResult
    tryPP pp = do 
        let rpkiUrl = getRpkiURL pp
        join $ atomically $ do 
            z <- readTVar (repositoryProcessing ^. #fetchTasks)                                
            case Map.lookup rpkiUrl z of 
                Just Stub           -> retry
                Just (Fetching a)   -> pure $ wait a
                Just (Done f)       -> pure $ pure f

                Nothing -> do 
                    pps <- readTVar $ repositoryProcessing ^. #publicationPoints
                    let asIfItIsMerged = pp `mergePP` pps
                    
                    let needAFetch = 
                            case findPublicationPointStatus rpkiUrl asIfItIsMerged of  
                                Nothing     -> True
                                Just status -> needsFetching pp status (config ^. #validationConfig) now                
                    
                    -- `repo` will always be there, since `asIfItIsMerged` would always 
                    -- produce some repository for the `pp`
                    let Just repo = repositoryFromPP asIfItIsMerged (getRpkiURL pp)                        

                    if needAFetch 
                        then do 
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
                    f <- fetchRepository_ appContext parentContext now repo
                    atomically $ do  
                        let newStatus = case f of
                                FetchFailure r _ -> FailedAt $ unNow now
                                FetchSuccess r _ -> FetchedAt $ unNow now
                        let updatedRepo r = updateStatuses r [(repo, newStatus)]

                        modifyTVar' fetchTasks (Map.insert rpkiUrl (Done f))

                        modifyTVar' publicationPoints $ 
                                          adjustSucceedUrl rpkiUrl 
                                        . (& typed @PublicationPoints %~ updatedRepo)                        

                    -- TODO Remove it, it is actually not needed.
                    -- 
                    -- It is a funky way to say "schedule deletion to 10 seconds from now".
                    -- All the other threads waiting on the same url will most likely
                    -- be aware of all the updates after 10 seconds.
                    forkFinally 
                        (threadDelay 10_000_000)                    
                        (\_ -> atomically $ modifyTVar' fetchTasks $ Map.delete rpkiUrl)

                    pure f) 
                (\a -> do 
                    cancel a
                    atomically $ modifyTVar' fetchTasks $ Map.delete rpkiUrl)
                (\a -> do 
                    atomically $ modifyTVar' fetchTasks $ Map.insert rpkiUrl (Fetching a)
                    wait a)                                                                   
                                       


anySuccess :: [FetchResult] -> Bool
anySuccess [] = False
anySuccess r = not $ null [ () | FetchSuccess{} <- r ]


fetchEverSucceeded :: (MonadIO m, Storage s) => 
                    AppContext s
                -> PublicationPointAccess 
                -> m FetchEverSucceeded 
fetchEverSucceeded 
    appContext@AppContext {..}         
    (PublicationPointAccess ppAccess) = liftIO $ do
        let publicationPoints = repositoryProcessing ^. #publicationPoints
        pps <- readTVarIO publicationPoints
        pure $ everSucceeded pps $ getRpkiURL $ NonEmpty.head ppAccess


-- | Fetch TA certificate based on TAL location(s)
--
fetchTACertificate :: AppContext s -> TAL -> ValidatorT IO (RpkiURL, RpkiObject)
fetchTACertificate appContext@AppContext {..} tal = 
    go $ neSetToList $ unLocations $ certLocations tal
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
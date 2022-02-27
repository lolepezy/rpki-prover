{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Fetch where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Lens
import           Data.Generics.Product.Typed

import           Control.Exception

import           Control.Monad
import           Control.Monad.Except

import qualified Data.List.NonEmpty          as NonEmpty

import           Data.String.Interpolate.IsString
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)

import GHC.Generics (Generic)

import Time.Types
import System.Random


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
                        -> PublicationPointAccess  
                        -> ValidatorT m [FetchResult]
fetchPPWithFallback 
    appContext@AppContext {..}     
    repositoryProcessing
    worldVersion
    ppAccess = do 
        parentScope <- askEnv         
        frs <- liftIO $ fetchOnce parentScope ppAccess        
        setValidationStateOfFetches repositoryProcessing frs            
  where

    ppSeqFetchRuns = repositoryProcessing ^. #ppSeqFetchRuns
    indivudualFetchRuns = repositoryProcessing ^. #indivudualFetchRuns
    publicationPoints = repositoryProcessing ^. #publicationPoints

    funRun runs key = Map.lookup key <$> readTVar runs            

    -- Use "run only once" logic for the whole list of PPs
    fetchOnce parentScope ppAccess' =          
        join $ atomically $ do      
            ppsKey <- ppSeqKey 
            funRun ppSeqFetchRuns ppsKey >>= \case            
                Just Stub         -> retry
                Just (Fetching a) -> pure $ wait a

                Nothing -> do                                         
                    modifyTVar' ppSeqFetchRuns $ Map.insert ppsKey Stub
                    pure $ bracketOnError 
                                (async $ do 
                                    evaluate =<< fetchWithFallback parentScope 
                                                    (NonEmpty.toList $ unPublicationPointAccess ppAccess')) 
                                (stopAndDrop ppSeqFetchRuns ppsKey) 
                                (rememberAndWait ppSeqFetchRuns ppsKey)                
      where          
        ppSeqKey = sequence 
                $ take 1 
                $ map (urlToDownload . getRpkiURL) 
                $ NonEmpty.toList 
                $ unPublicationPointAccess ppAccess
          where
            urlToDownload u = 
                case u of
                    RrdpU _ -> pure u
                    RsyncU rsyncUrl -> do  
                        pps <- readTVar publicationPoints
                        pure $ maybe u (RsyncU . fst) $ statusInRsyncTree rsyncUrl (pps ^. typed)


    fetchWithFallback :: Scopes -> [PublicationPoint] -> IO [FetchResult]

    fetchWithFallback _          []   = pure []
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
                updateTime t = if t == mempty then TimeMs elapsed else t
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
                logWarn_ logger $ if nextOneNeedAFetch
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
            if repoNeedAFetch 
                then 
                    funRun indivudualFetchRuns rpkiUrl >>= \case                    
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
    fetchPP parentScope repo = do         
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
        atomically $ modifyTVar' stubs $ Map.delete key
        cancel a

    rememberAndWait stubs key a = liftIO $ do 
        atomically $ modifyTVar' stubs $ Map.insert key (Fetching a)
        wait a

    needsAFetch pp now' = do 
        pps <- readTVar publicationPoints
        let asIfMerged = mergePP pp pps
        let Just repo = repositoryFromPP asIfMerged (getRpkiURL pp)
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
                                    (runRsyncFetchWorker appContext worldVersion r)
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


fetchEverSucceeded :: MonadIO m => 
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
                    FetchFailure r vs'    -> (r, vs')
                    FetchSuccess repo vs' -> (getRpkiURL repo, vs')
                
            modifyTVar' (repositoryProcessing ^. #indivudualFetchResults)
                        $ Map.insert u vs
    pure frs


setValidationStateOfFetches1 :: MonadIO m => RepositoryProcessing1 -> [FetchResult] -> m [FetchResult] 
setValidationStateOfFetches1 repositoryProcessing frs = liftIO $ do
    atomically $ do 
        forM_ frs $ \fr -> do 
            let (u, vs) = case fr of
                    FetchFailure r vs'    -> (r, vs')
                    FetchSuccess repo vs' -> (getRpkiURL repo, vs')                
            modifyTVar' (repositoryProcessing ^. #fetchResults) $ Map.insert u vs
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



fetchWithFallback1 :: (MonadIO m, Storage s) => 
                            AppContext s                         
                        -> RepositoryProcessing1
                        -> WorldVersion
                        -> Now 
                        -> PublicationPointAccess  
                        -> (Repository -> IO Fetched)
                        -> ValidatorT m [FetchResult]
fetchWithFallback1 
    appContext@AppContext {..}     
    repositoryProcessing@RepositoryProcessing1 {..}
    worldVersion
    now 
    ppAccess 
    fetchPP = do 
        parentScope <- askEnv
        frs <- liftIO $ maybe (pure []) stepSeq $ 
                    fetchSeq $ 
                    map getRpkiURL $ NonEmpty.toList $ 
                    unPublicationPointAccess ppAccess
        setValidationStateOfFetches1 repositoryProcessing frs     
        pure frs
  where

    fetchSeq :: [RpkiURL] -> Maybe FetchSeq
    fetchSeq [] = Nothing
    fetchSeq (u : urls) = Just $ case u of
        RrdpU rrdpUrl   -> rrdpFetchSeq rrdpUrl urls              
        RsyncU rsyncUrl -> rsyncFetchSeq rsyncUrl urls       

    mapLocked :: RrdpURL 
            -> TVar (Map RrdpURL (FetchTask b)) 
            -> STM t 
            -> (t -> IO b) 
            -> STM (IO b)
    mapLocked key actionMap stmAction ioAction = do 
        z <- readTVar actionMap
        case Map.lookup key z of 
            Just Stub         -> retry
            Just (Fetching a) -> pure $ wait a
            Nothing -> do                 
                s <- stmAction
                modifyTVar' actionMap $ Map.insert key Stub
                pure $ do                                         
                        a <- async $ ioAction s
                        atomically $ modifyTVar' actionMap $ \m -> do 
                            case Map.lookup key m of 
                                Just Stub -> Map.insert key (Fetching a) m
                                _         -> m
                        wait a                    

    mapUnlock rrdpUrl = 
        modifyTVar' fetchMap $ Map.delete rrdpUrl

    rrdpFetchSeq :: RrdpURL -> [RpkiURL] -> FetchSeq
    rrdpFetchSeq rrdpUrl otherUrls = do 
        let smtA = do 
                pps <- readTVar publicationPoints
                let repo = maybe (rrdpR rrdpUrl) RrdpR $ rrdpRepository pps rrdpUrl                        
                pure (repo, fetchSeq otherUrls)

        let ioA (repo, fallback) = do 
                if needsFetching repo (getFetchStatus repo) (config ^. #validationConfig) now
                    then do 
                        z@(r, _) <- fetchPP repo
                        let wrapUpCurrent = do 
                                applyFetchResult (RrdpU rrdpUrl) repo r
                                mapUnlock rrdpUrl
                        let fallback' = fmap (\(FetchSeq f) -> FetchSeq $ wrapUpCurrent >> f) fallback
                        pure (getRpkiURL repo, z, fallback')
                    else 
                        pure (getRpkiURL repo, (Right repo, mempty), Nothing)

        FetchSeq $ mapLocked rrdpUrl fetchMap smtA ioA

    treeLocked :: RsyncURL 
            -> STM t 
            -> (t -> IO (RpkiURL, Fetched, Maybe FetchSeq)) 
            -> STM (IO (RpkiURL, Fetched, Maybe FetchSeq))
    treeLocked rsyncUrl@(RsyncURL host path) stmAction ioAction = do 
        RsyncFetches fetchTree <- readTVar rsyncFetchTree
        case findInRsync' rsyncUrl fetchTree of
            Just (u, z) ->
                readTVar z >>= \case 
                    Stub -> retry
                    Fetching a -> pure $ wait a              

            Nothing -> do 
                leaf <- newTVar Stub
                branchNode <- newTVar NeverTried
                let (fetchTree', replaced) = setNode rsyncUrl fetchTree leaf branchNode
                s <- stmAction
                writeTVar rsyncFetchTree (RsyncFetches fetchTree')
                pure $ fetchAction s leaf
        where
            fetchAction s leaf = do 
                a <- async $ ioAction s                
                atomically $ readTVar leaf >>= \case 
                        Stub -> writeTVar leaf (Fetching a)
                        _    -> pure ()
                wait a                   

    treeUnlock rsyncUrl = 
        modifyTVar' rsyncFetchTree $ deleteNode rsyncUrl

        
    rsyncFetchSeq :: RsyncURL -> [RpkiURL] -> FetchSeq
    rsyncFetchSeq rsyncUrl otherUrls = do 
        let smtA = do 
                pps <- readTVar publicationPoints
                let repo = maybe (rsyncR rsyncUrl) RsyncR $ rsyncRepository pps rsyncUrl
                pure (repo, fetchSeq otherUrls)

        let ioA (repo, fallback) = do 
                if needsFetching repo (getFetchStatus repo) (config ^. #validationConfig) now
                    then do 
                        z@(r, _) <- fetchPP repo
                        let wrapUpCurrent = do 
                                applyFetchResult (RsyncU rsyncUrl) repo r
                                treeUnlock rsyncUrl                                
                        let fallback' = fmap (\(FetchSeq f) -> FetchSeq $ wrapUpCurrent >> f) fallback
                        pure (getRpkiURL repo, z, fallback')
                    else 
                        pure (getRpkiURL repo, (Right repo, mempty), Nothing)

        FetchSeq $ treeLocked rsyncUrl smtA ioA

    applyFetchResult rpkiUrl repo r = do                             
        let (newRepo, newStatus) = case r of                             
                Left _      -> (repo, FailedAt $ unNow now)
                Right repo' -> (repo', FetchedAt $ unNow now)

        modifyTVar' publicationPoints $ \pps -> 
                adjustSucceededUrl rpkiUrl 
                        $ updateStatuses (pps ^. typed @PublicationPoints) 
                        [(newRepo, newStatus)]          
            


fetchPPWithFallback1 :: (MonadIO m, Storage s) => 
                            AppContext s                         
                        -> RepositoryProcessing1
                        -> WorldVersion
                        -> Now 
                        -> PublicationPointAccess  
                        -> ValidatorT m [FetchResult]
fetchPPWithFallback1 appContext@AppContext {..}
    repositoryProcessing worldVersion now ppAccess = 
        fetchWithFallback1 appContext repositoryProcessing worldVersion now  ppAccess fetchPP       
  where    
    fetchPP repo = do
        logDebug_ logger [i|Test fetching #{repo}|]
        r <- randomRIO (1, 10)        
        threadDelay $ 10_000 * r
        pure (Right repo, mempty)


stepSeq :: FetchSeq -> IO [FetchResult]
stepSeq (FetchSeq fs) = do 
    io <- atomically fs
    (repoUrl, (r, vs), fallback) <- io
    case r of
        Left _ -> do 
            let fetchResult = FetchFailure repoUrl vs
            case fallback of
                Nothing -> pure [fetchResult]
                Just f  -> (fetchResult :) <$> stepSeq f    
        Right repo -> 
            pure [FetchSuccess repo vs]        


newtype FetchSeq = FetchSeq {
        unFetchStep :: STM (IO (RpkiURL, Fetched, Maybe FetchSeq))
    }


type Fetched = (Either AppError Repository, ValidationState)
type FTask = FetchTask (RpkiURL, Fetched, Maybe FetchSeq)

data CommonRoot = NeverTried | Unavailable | Working FTask 
    deriving stock (Eq, Ord, Generic)    

newtype RsyncFetches = RsyncFetches (Map RsyncHost (RsyncNode (TVar FTask) (TVar CommonRoot)))
    deriving stock (Eq, Generic)    

    
data RepositoryProcessing1 = RepositoryProcessing1 {
        fetchMap          :: TVar (Map RrdpURL FTask),
        rsyncFetchTree    :: TVar RsyncFetches,
        fetchResults      :: TVar (Map RpkiURL ValidationState),
        publicationPoints :: TVar PublicationPoints        
    }
    deriving stock (Eq, Generic)


newRepositoryProcessing1 :: STM RepositoryProcessing1
newRepositoryProcessing1 = do 
    fetchMap          <- newTVar mempty
    rsyncFetchTree    <- newTVar $ RsyncFetches mempty
    fetchResults      <- newTVar mempty
    publicationPoints <- newTVar newPPs
    pure RepositoryProcessing1 {..}

findRsyncFetch :: RsyncURL 
            -> RsyncFetches 
            -> Maybe (RsyncURL, Either (TVar FTask) (RsyncURL, TVar CommonRoot))
findRsyncFetch u (RsyncFetches t) = findInRsyncTree u t

findInRsyncTree :: RsyncURL 
                   -> Map RsyncHost (RsyncNode a b) 
                   -> Maybe (RsyncURL, Either a (RsyncURL, b))
findInRsyncTree (RsyncURL host path) hosts = 
    go path [] =<< Map.lookup host hosts
  where    
    -- TODO Make this 2 configurable
    maxChildrenBeforeAdvisingCommonRoot = 2

    go [] _  SubTree {} = Nothing

    go _ realPath (Leaf a) = 
        Just (RsyncURL host realPath, Left a)
    
    go (u: [lastChunk]) realPath SubTree {..} = 
        case Map.lookup u rsyncChildren of 
            Nothing -> Nothing 
            Just z@SubTree { rsyncChildren = ch }                
                | not (Map.member lastChunk ch) && 
                  Map.size ch > maxChildrenBeforeAdvisingCommonRoot 
                  -> 
                    let commonRoot = RsyncURL host realPath
                    in Just (commonRoot, Right (commonRoot, nodePayload))
                | otherwise ->
                    go [lastChunk] (realPath <> [u]) z
            Just z -> 
                go [lastChunk] (realPath <> [u]) z

    go (u: us) realPath SubTree {..} = 
        Map.lookup u rsyncChildren >>= go us (realPath <> [u])



findInRsync' :: RsyncURL 
            -> Map RsyncHost (RsyncNode a b) 
            -> Maybe (RsyncURL, a)
findInRsync' (RsyncURL host path) hosts = 
    go path [] =<< Map.lookup host hosts
  where    
    go [] _  SubTree {} = Nothing
    go _ realPath (Leaf a) = Just (RsyncURL host realPath, a)

    go (p: path) realPath SubTree {..} = 
        Map.lookup p rsyncChildren >>= go path (realPath <> [p])


setNode :: RsyncURL 
        -> Map RsyncHost (RsyncNode a b) 
        -> a 
        -> b
        -> (Map RsyncHost (RsyncNode a b), Maybe (RsyncNode a b))
setNode (RsyncURL host path) fetchTrees leafPayload branchNodePayload =
    case Map.lookup host fetchTrees of
        Nothing -> 
            (Map.insert host (buildTree path) fetchTrees, Nothing)
        Just t -> let 
                (t', replaced) = adjustTree path t
            in (Map.insert host t' fetchTrees, replaced)     
  where        
    adjustTree [] existing              = (Leaf leafPayload, Just existing)
    adjustTree (p : path) leaf@(Leaf _) = (buildTree path, Just leaf)

    adjustTree (p : path) subTree@SubTree { rsyncChildren = ch } = 
        case Map.lookup p ch of 
            Nothing -> let                    
                    s = SubTree {
                            rsyncChildren = Map.insert p (buildTree path) ch,
                            nodePayload   = branchNodePayload
                        }
                in (s, Nothing)
            Just child -> let 
                    (t', _) = adjustTree path child
                    s = subTree { 
                            rsyncChildren = Map.insert p t' ch 
                        }
                in (s, Just child)
        
    buildTree [] = Leaf leafPayload
    buildTree (u: us) = SubTree {
            rsyncChildren = Map.singleton u (buildTree us),
            nodePayload  = branchNodePayload
        }
 

deleteNode :: RsyncURL -> RsyncFetches -> RsyncFetches
deleteNode (RsyncURL host path) fs@(RsyncFetches fetches) = 
    RsyncFetches $ Map.alter f host fetches
  where
    f Nothing = Nothing
    f (Just tree) = withoutBranch path tree

    withoutBranch [] (Leaf _) = Nothing
    withoutBranch [] _       = Nothing

    withoutBranch (p: path) subTree@SubTree { rsyncChildren = ch } = Nothing
        

    
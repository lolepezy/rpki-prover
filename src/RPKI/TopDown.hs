{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.TopDown where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Trans.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           GHC.Generics (Generic)


import           Data.Either                      (fromRight)
import           Data.Char                        (toLower)
import           Data.Foldable
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid.Generic
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Proxy

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RRDP.Http
import           RPKI.RRDP.RrdpFetch
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.TAL
import           RPKI.Time
import           RPKI.CommonTypes
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.Validation.ObjectValidation
import           RPKI.AppState
import           RPKI.Metrics

import           Data.Hourglass
import           System.Timeout                   (timeout)
import Control.Concurrent.Async (forConcurrently)


-- List of hashes of certificates, validation contexts and verified resource sets 
-- that are waiting for a PP to be fetched. CA certificates, pointig to delegated 
-- CAs are normally getting in this list.
newtype WaitingList c =  WaitingList { unWList :: 
        MonoidMap RpkiURL (Set (T3 Hash c (Maybe (VerifiedRS PrefixesAndAsns))))
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype Semigroup
    deriving newtype Monoid

toWaitingList :: RpkiURL -> Hash -> c -> Maybe (VerifiedRS PrefixesAndAsns) -> WaitingList c
toWaitingList rpkiUrl hash vc resources =     
    WaitingList $ MonoidMap $ Map.singleton rpkiUrl (Set.singleton (T3 hash vc resources))


data RepositoryContext = RepositoryContext {
        publicationPoints  :: PublicationPoints,
        takenCareOf        :: Set RpkiURL
    } 
    deriving stock (Generic)  

-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext s = TopDownContext {    
        verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),    
        taName            :: TaName, 
        repositoryContext :: TVar RepositoryContext, 
        now               :: Now,
        worldVersion      :: WorldVersion,
        visitedHashes     :: TVar (Set Hash)
    }
    deriving stock (Generic)


data TopDownResult = TopDownResult {
        vrps          :: Set Vrp,
        tdValidations :: ValidationState
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving Semigroup via GenericSemigroup TopDownResult   
    deriving Monoid    via GenericMonoid TopDownResult


fromValidations :: ValidationState -> TopDownResult
fromValidations validationState = TopDownResult { 
        vrps          = mempty, 
        tdValidations = validationState 
    }


newTopDownContext :: MonadIO m => 
                    WorldVersion 
                    -> TaName 
                    -> TVar RepositoryContext
                    -> Now 
                    -> CerObject 
                    -> m (TopDownContext s)
newTopDownContext worldVersion taName repositoryContext now certificate = 
    liftIO $ atomically $ do    
        hashes <- newTVar Set.empty   
        pure $ TopDownContext 
                (Just $ createVerifiedResources certificate) 
                taName repositoryContext now worldVersion hashes

newRepositoryContext :: PublicationPoints -> RepositoryContext
newRepositoryContext publicationPoints = let 
    takenCareOf = Set.empty 
    in RepositoryContext {..}

createVerifiedResources :: CerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources (getRC -> ResourceCertificate certificate) = 
    VerifiedRS $ toPrefixesAndAsns $ withRFC certificate resources


            
validateMutlipleTAs :: Storage s => 
                    AppContext s 
                    -> WorldVersion 
                    -> [TAL]
                    -> IO [TopDownResult]
validateMutlipleTAs appContext@AppContext {..} worldVersion tals = do                    
    database' <- readTVarIO database 
    storedPubPoints   <- roTx database' $ \tx -> getPublicationPoints tx (repositoryStore database')    

    rs <- forConcurrently tals $ \tal -> do 
        repositoryContext <- newTVarIO $ newRepositoryContext storedPubPoints
        (r@TopDownResult{..}, elapsed) <- timedMS $ 
                validateTA appContext tal worldVersion repositoryContext 
        logInfo_ logger [i|Validated #{getTaName tal}, got #{length vrps} VRPs, took #{elapsed}ms|]
        pure (r, repositoryContext)

    mapException (AppException . storageError) $ do
        rwTx database' $ \tx -> do                             
            pubPointAfterTopDown <- liftIO $ atomically $ do 
                    mconcat . map (^. typed @PublicationPoints) <$> mapM (readTVar . snd) rs 
                    -- pps <- readTVar repositoryContext                
                    -- pure $ pps ^. typed @PublicationPoints
            savePublicationPoints tx (repositoryStore database') pubPointAfterTopDown                        

    pure $ map fst rs


-- | It is the main entry point for the top-down validation. 
-- Validates TA starting from its TAL.
--
validateTA :: Storage s => 
            AppContext s 
            -> TAL 
            -> WorldVersion 
            -> TVar RepositoryContext
            -> IO TopDownResult
validateTA appContext@AppContext {..} tal worldVersion repositories = do    
    r <- runValidatorT taContext $
            timedMetric (Proxy :: Proxy ValidationMetric) $ do 
                ((taCert, repos, _), elapsed) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                logDebugM logger [i|Fetched and validated TA certficate #{certLocations tal}, took #{elapsed}ms.|]        
                vrps <- validateFromTACert appContext (getTaName tal) taCert repos worldVersion repositories
                setVrpNumber $ Count $ fromIntegral $ Set.size vrps
                pure vrps

    case r of 
        (Left e, vs) -> 
            pure $! TopDownResult mempty vs
        (Right vrps, vs) ->            
            pure $! TopDownResult vrps vs
    where                     
        taContext = newValidatorPath taNameText
        TaName taNameText = getTaName tal


data TACertStatus = Existing | Updated

-- | Fetch and validated TA certificate starting from the TAL.
-- | 
-- | This function doesn't throw exceptions.
validateTACertificateFromTAL :: Storage s => 
                                AppContext s 
                                -> TAL 
                                -> WorldVersion 
                                -> ValidatorT IO (CerObject, NonEmpty Repository, TACertStatus)
validateTACertificateFromTAL appContext@AppContext {..} tal worldVersion = do
    let now = Now $ versionToMoment worldVersion
    let validationConfig = config ^. typed @ValidationConfig

    taStore  <- taStore <$> liftIO (readTVarIO database)
    taByName <- roAppTxEx taStore storageError $ \tx -> getTA tx taStore taName'
    case taByName of
        Nothing -> fetchValidateAndStore taStore now
        Just StorableTA { taCert, initialRepositories, fetchStatus }
            | needsFetching (getTaCertURL tal) fetchStatus validationConfig now ->
                fetchValidateAndStore taStore now
            | otherwise -> 
                pure (taCert, initialRepositories, Existing)
    where
        fetchValidateAndStore taStore (Now moment) = do 
            (uri', ro) <- fetchTACertificate appContext tal
            cert       <- vHoist $ validateTACert tal uri' ro
            rwAppTxEx taStore storageError $ \tx ->            
                case createRepositoriesFromTAL tal cert of
                    Left e      -> appError $ ValidationE e
                    Right repos -> do 
                        putTA tx taStore (StorableTA tal cert (FetchedAt moment) repos)
                        pure (cert, repos, Updated)

        taName' = getTaName tal          


-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: Storage s =>
                    AppContext s -> 
                    TaName -> 
                    CerObject -> 
                    NonEmpty Repository -> 
                    WorldVersion -> 
                    TVar RepositoryContext -> 
                    ValidatorT IO (Set Vrp)
validateFromTACert appContext@AppContext {..} taName' taCert initialRepos worldVersion repositoryContext = do  
    -- this will be used as the "now" in all subsequent time and period validations 
    let now = Now $ versionToMoment worldVersion

    let taURIContext = newValidatorPath $ toText $ NonEmpty.head $ getLocations taCert

    storedPubPoints <- (^. typed @PublicationPoints) <$> liftIO (readTVarIO repositoryContext)    

    let reposToFetch = map fst $ 
            -- filter the ones that are either new or need re-fetching
            filter (\(pp, status) -> needsFetching pp status (config ^. typed @ValidationConfig) now) $ 
            toRepoStatusPairs $ 
                -- merge repos that we want to be fetched with the ones that are stored                     
                -- to have the most up-to-date statuses
                mergeRepos initialRepos storedPubPoints
                -- we only care about URLs from 'repos', so shrink the PPs                        
                    `shrinkTo` 
                Set.fromList (map getRpkiURL $ NonEmpty.toList initialRepos)

    fetchStatuses <- 
        if config ^. typed @ValidationConfig . #dontFetch
            then pure []
            else inParallelVT
                    (ioBottleneck appBottlenecks)
                    reposToFetch 
                    (fetchRepository appContext taURIContext now)
            -- else forM
            --         reposToFetch 
            --         (fetchRepository appContext taURIContext now)

    case partitionFailedSuccess fetchStatuses of
        ([], _) -> do            
            -- Embed valdiation state accumulated from 
            -- the fetching into the current validation state.
            embedState (fetchStatuses2ValidationState fetchStatuses)

            let flattenedStatuses = flip map fetchStatuses $ \case 
                    FetchFailure r s _ -> (r, FailedAt s)
                    FetchSuccess r s _ -> (r, FetchedAt s)

            -- use publication points taken from the DB and updated with the 
            -- the fetchStatuses of the fetches that we just performed
            let fetchUpdatedPPs = updateStatuses storedPubPoints flattenedStatuses

            -- merge the publication points that we have just fetched in the global context.
            liftIO $ atomically $ modifyTVar' repositoryContext 
                                    (& typed @PublicationPoints %~ (<> fetchUpdatedPPs))

            topDownContext <- newTopDownContext worldVersion taName' repositoryContext now taCert             

            -- Do the tree descend, gather validation results and VRPs            
            T2 vrps validationState <- fromTry (\e -> UnspecifiedE (unTaName taName') (fmtEx e)) $
                validateCA appContext taURIContext topDownContext taCert                    

            embedState validationState
                        
            database' <- liftIO $ readTVarIO database
            

            pure vrps

        (broken, _) -> do
            let brokenUrls = map (getRpkiURL . (^. _1)) broken
            logErrorM logger [i|Will not proceed, repositories '#{brokenUrls}' failed to download.|]
            case findError $ mconcat $ map (^. _3 . typed) broken of                
                Just e  -> appError e    
                -- Failed to download and no idea why        
                Nothing -> appError $ UnspecifiedE "Failed to fetch initial repositories." (convert $ show broken)


data FetchResult = 
    FetchSuccess Repository Instant ValidationState | 
    FetchFailure Repository Instant ValidationState
    deriving stock (Show, Eq, Generic)

-- | Download repository, either rsync or RRDP.
fetchRepository :: (MonadIO m, Storage s) => 
                AppContext s -> ValidatorPath -> Now -> Repository -> m FetchResult
fetchRepository 
    appContext@AppContext {..} 
    parentContext 
    (Now now) 
    repo = liftIO $ do
        let (Seconds maxDduration, timeoutError) = case repoURL of
                RrdpU _  -> 
                    (config ^. typed @RrdpConf . #rrdpTimeout, 
                     RrdpE $ RrdpDownloadTimeout maxDduration)
                RsyncU _ -> 
                    (config ^. typed @RsyncConf . #rsyncTimeout, 
                     RsyncE $ RsyncDownloadTimeout maxDduration)
                
        r <- timeout (1_000_000 * fromIntegral maxDduration) fetchIt
        case r of 
            Nothing -> do 
                logErrorM logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDduration}s.|]
                pure $ FetchFailure repo now (vState $ mError vContext' timeoutError)
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
                    pure $! FetchFailure repo now (vState (mError vContext' e) <> v)
                Right resultRepo -> do
                    logDebugM logger [i|Fetched repository #{getURL repoURL}, took #{elapsed}ms.|]
                    pure $! FetchSuccess resultRepo now v


fetchTimeout :: Config -> RpkiURL -> Seconds
fetchTimeout config (RrdpU _)  = config ^. typed @RrdpConf .  #rrdpTimeout
fetchTimeout config (RsyncU _) = config ^. typed @RsyncConf . #rsyncTimeout

type RepoTriple = (Repository, Instant, ValidationState)

partitionFailedSuccess :: [FetchResult] -> ([RepoTriple], [RepoTriple])
partitionFailedSuccess = go
    where
        go [] = ([], [])
        go (FetchSuccess r rs v : frs) = let (fs, ss) = go frs in (fs, (r, rs, v) : ss)
        go (FetchFailure r rs v : frs) = let (fs, ss) = go frs in ((r, rs, v) : fs, ss)

fetchStatuses2ValidationState :: [FetchResult] -> ValidationState
fetchStatuses2ValidationState r = mconcat $ flip map r $ \case 
    FetchSuccess _ _ v -> v
    FetchFailure _ _ v -> v  
                

-- | Validate CA starting from its certificate.
-- 
validateCA :: Storage s =>
            AppContext s 
            -> ValidatorPath 
            -> TopDownContext s 
            -> CerObject 
            -> IO (T2 (Set Vrp) ValidationState)
validateCA appContext caVContext topDownContext certificate =
    validateCARecursively appContext caVContext topDownContext certificate
        `finally` 
    markValidatedObjects appContext topDownContext            


-- 
validateCARecursively :: Storage s => 
                        AppContext s 
                    -> ValidatorPath 
                    -> TopDownContext s
                    -> CerObject 
                    -> IO (T2 (Set Vrp) ValidationState)
validateCARecursively 
        appContext@AppContext {..} 
        vc 
        topDownContext@TopDownContext{..} 
        certificate = do     

    (r, validations) <- runValidatorT vc $ do     
            validateCaCertificate appContext topDownContext certificate
    case r of
        Left e -> 
            pure $! T2 mempty validations
        Right (T3 discoveredPPs waitingList vrps) -> do             
            results <- extractPPsAndValidateDown discoveredPPs waitingList
            pure $! mconcat results <> T2 vrps validations         

    where
        -- From the set of discovered PPs figure out which ones must be fetched, 
        -- fetch them and validate, starting from the cerfificates in their 
        -- waiting lists.        
        extractPPsAndValidateDown discoveredPPs waitingList = do            
            ppsToFetch' <- atomically $ getPPsToFetch repositoryContext discoveredPPs
            let (_, rootToPps) = repositoryHierarchy ppsToFetch'
            -- forM                
            --     (Map.keys rootToPps) $ \repo ->
            --         fetchAndValidateWaitingList rootToPps repo waitingList                    
            inParallel
                (cpuBottleneck appBottlenecks)
                (Map.keys rootToPps) $ \repo ->
                    fetchAndValidateWaitingList rootToPps repo waitingList                    
                    

        -- Fetch the PP and validate all the certificates from the waiting 
        -- list of this PP.
        fetchAndValidateWaitingList :: WithRpkiURL a => 
                                       Map Repository (Set a) 
                                    -> Repository 
                                    -> WaitingList ValidatorPath 
                                    -> IO (T2 (Set Vrp) ValidationState)
        fetchAndValidateWaitingList rootToPps repo (WaitingList (MonoidMap waitingList)) = do

            -- Gather the PPs and hashes of the objects from their waiting lists
            let ppsForTheRepo = fromMaybe Set.empty $ Map.lookup repo rootToPps            
            let waitingListForThesePPs = Set.toList $ fromMaybe Set.empty $ fold $ 
                    Set.map (\pp -> getRpkiURL pp `Map.lookup` waitingList) ppsForTheRepo
            
            -- try to recover the validation context
            let waitingPath = case waitingListForThesePPs of
                                []             -> vc
                                T3 _ vc' _ : _ -> vc'

            fetchResult <- liftIO $ fetchRepository appContext waitingPath now repo            

            let statusUpdate = case fetchResult of
                            FetchFailure r t _ -> (r, FailedAt t)
                            FetchSuccess r t _ -> (r, FetchedAt t)                      
            
            pps <- liftIO $ atomically $ do  
                        r <- readTVar repositoryContext
                        let pps' = updateStatuses (r ^. #publicationPoints) [statusUpdate]     
                        writeTVar repositoryContext $ r { publicationPoints = pps' }
                        pure pps'

            proceedUsingGracePeriod pps waitingListForThesePPs fetchResult                        


        -- Decide what to do with the result of PP fetching
        -- * if it succeeded validate the tree
        -- * if it failed
        --   - if we are still within the grace period, then validate the tree
        --   - if grace period is expired, stop here
        proceedUsingGracePeriod pps waitingListForThesePPs fetchResult = do            
            let proceedWithValidation validations = do 
                    z <- mconcat <$> validateWaitingList waitingListForThesePPs                    
                    pure $! z & typed %~ (<> validations)

            let noFurtherValidation validations = 
                    pure $ mempty & typed %~ (<> validations)

            let Now now' = now
            case fetchResult of
                FetchSuccess _ _ validations ->                    
                    proceedWithValidation validations

                FetchFailure r _ validations -> 
                    case everSucceeded pps $ getRpkiURL r of
                        Never -> do 
                            logWarn_ logger [i|Repository #{getRpkiURL r} failed, it never succeeded to fetch so tree validation will not proceed for it.|]    
                            noFurtherValidation validations
                        AtLeastOnce -> do                             
                            logWarn_ logger  
                                [i|Repository #{getRpkiURL r} failed, but it succeeded before, so cached objects will be used |]
                            proceedWithValidation validations


        -- Resume tree validation starting from every certificate on the waiting list.
        -- 
        validateWaitingList waitingList = do 
            objectStore <- objectStore <$> readTVarIO database
            -- forM
            inParallel
                (cpuBottleneck appBottlenecks)
                waitingList $ \(T3 hash certVContext verifiedResources') -> do
                    o <- roTx objectStore $ \tx -> getByHash tx objectStore hash
                    case o of 
                        Just c@(CerRO waitingCertificate) -> do
                            let childTopDownContext = topDownContext { 
                                    -- we should start from the resource set of this certificate
                                    -- as it is already has been verified
                                    verifiedResources = verifiedResources'                                                
                                }                            
                            validateCARecursively appContext certVContext childTopDownContext waitingCertificate 
                        ro -> do
                            logError_ logger [i| Something is really wrong with the hash #{hash} in waiting list, got #{ro}|]
                            pure mempty
        
    

-- | Do top-down validation starting from the given certificate
-- Returns the discovered publication points that are not registered 
-- in the top-down context yet.
validateCaCertificate :: Storage s =>
                        AppContext s ->
                        TopDownContext s ->
                        CerObject ->                
                        ValidatorT IO (T3 PublicationPoints (WaitingList ValidatorPath) (Set Vrp))
validateCaCertificate 
    appContext@AppContext {..} 
    topDownContext@TopDownContext {..} 
    certificate = do          

    globalPPs <- (^. #publicationPoints) <$> liftIO (readTVarIO repositoryContext)
    
    if appContext ^. typed @Config . typed @ValidationConfig . #dontFetch 
        -- Don't add anything to the awaited repositories
        -- Just expect all the objects to be already cached
        then validateThisCertAndGoDown 
        else checkPublicationPointsFirstAndDecide globalPPs

  where
    checkPublicationPointsFirstAndDecide globalPPs = 
        case publicationPointsFromCertObject certificate of
            Left e                    -> appError $ ValidationE e
            Right (url, discoveredPP) -> do
                let asIfItIsMerged = discoveredPP `mergePP` globalPPs
                let stopDescend = do 
                        -- remember to come back to this certificate when the PP is fetched
                        vContext' <- ask
                        pure $! T3 
                                (asIfItIsMerged `shrinkTo` Set.singleton url) 
                                (toWaitingList
                                    (getRpkiURL discoveredPP) 
                                    (getHash certificate) 
                                    vContext' 
                                    verifiedResources)
                                mempty

                case findPublicationPointStatus url asIfItIsMerged of 
                    -- this publication point hasn't been seen at all, so stop here
                    Nothing -> stopDescend

                    -- If it's been fetched too long ago, stop here and add the certificate 
                    -- to the waiting list of this PP
                    -- if the PP is fresh enough, proceed with the tree descend                
                    Just status -> let 
                        validationConfig = appContext ^. typed @Config . typed               
                        needToRefetch = needsFetching discoveredPP status validationConfig now                    
                        in if needToRefetch
                            then stopDescend 
                            else validateThisCertAndGoDown                    

    -- Here we do the detailed algorithm
    --  * get manifest
    --  * find CRL on it
    --  * make sure they both are valid
    --  * go through the manifest children and either 
    --     + validate them as signed objects
    --     + or valdiate them recursively as CAcertificates
    -- 
    -- Everything else is either extra checks or metrics.
    -- 
    validateThisCertAndGoDown = do            
        let (childrenAki, certLocations') = (toAKI $ getSKI certificate, getLocations certificate)        
        
        oneMoreCert            
        visitObject appContext topDownContext (CerRO certificate)

        mft <- findMft childrenAki certLocations'
        validateMftLocation mft certificate                    

        manifestResult <- inSubVPath (toText $ NonEmpty.head $ getLocations mft) $ do
            T2 _ crlHash <- case findCrlOnMft mft of 
                []    -> vError $ NoCRLOnMFT childrenAki certLocations'
                [crl] -> pure crl
                crls  -> vError $ MoreThanOneCRLOnMFT childrenAki certLocations' crls

            objectStore' <- (^. #objectStore) <$> liftIO (readTVarIO database)
            crlObject <- liftIO $ roTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
            case crlObject of 
                Nothing -> 
                    vError $ NoCRLExists childrenAki certLocations'    

                Just foundCrl@(CrlRO crl) -> do      
                    visitObject appContext topDownContext foundCrl
                    -- logDebugM logger [i|crl = #{NonEmpty.head $ getLocations crl} (#{thisUpdateTime $ signCrl crl}, #{nextUpdateTime $ signCrl crl})|]
                    -- validate CRL and MFT together
                    validCrl <- inSubVPath (toText $ NonEmpty.head $ getLocations crl) $ 
                                    vHoist $ do          
                                        -- checkCrlLocation crl certificate
                                        validateCrl now crl certificate
                    oneMoreCrl

                    -- MFT can be revoked by the CRL that is on this MFT -- detect it                                
                    void $ vHoist $ validateMft now mft 
                                        certificate validCrl verifiedResources
                                        
                    -- filter out CRL itself
                    let childrenHashes = filter (\(T2 _ hash') -> getHash crl /= hash') 
                                $ mftEntries $ getCMSContent $ cmsPayload mft

                    -- Mark all manifest entries as visited to avoid the situation
                    -- when some of the children are deleted from the cache and some
                    -- are still there. Do it both in case of successful validation
                    -- or a validation error.
                    let markAllEntriesAsVisited = 
                            visitObjects topDownContext $ map (\(T2 _ h) -> h) childrenHashes                

                    let processChildren = do
                            r <- inParallelVT
                                    (cpuBottleneck appBottlenecks)
                                    childrenHashes
                                    $ \(T2 filename hash') -> 
                                                validateManifestEntry filename hash' validCrl
                            markAllEntriesAsVisited
                            pure $! r                                       

                    childrenResults <- processChildren `catchError` 
                                    (\e -> markAllEntriesAsVisited >> throwError e)

                    -- Combine PPs and their waiting lists. On top of it, fix the 
                    -- last successfull validation times for PPs based on their fetch statuses.                        
                    pure $! mconcat childrenResults & typed @PublicationPoints %~ adjustLastSucceeded                        

                Just _  -> vError $ CRLHashPointsToAnotherObject crlHash certLocations'   

        oneMoreMft
        pure manifestResult

    --
    -- | Validate an entry of the manifest, i.e. a pair of filename and hash
    -- 
    validateManifestEntry filename hash' validCrl = do                    
        validateMftFileName
        visitedObjects <- liftIO $ readTVarIO visitedHashes

        objectStore' <- (^. #objectStore) <$> liftIO (readTVarIO database)
        ro <- liftIO $ roTx objectStore' $ \tx -> getByHash tx objectStore' hash'
        case ro of 
            Nothing -> 
                vError $ ManifestEntryDontExist hash' filename
            Just ro'
                | Set.member hash' visitedObjects ->
                    -- we have already visited this object before, so 
                    -- there're some circular references in the objects
                    vError $ CircularReference hash' (getLocations ro')
                | otherwise -> do
                    -- warn about names on the manifest mismatching names in the object URLs
                    let objectLocations = getLocations ro'
                    let nameMatches = NonEmpty.filter ((filename `Text.isSuffixOf`) . toText) objectLocations
                    when (null nameMatches) $ 
                        vWarn $ ManifestLocationMismatch filename objectLocations

                    -- Validate the MFT entry, i.e. validate a ROA/GBR/etc.
                    -- or recursively validate CA if the child is a certificate.                           
                    validateChild validCrl ro'
      where 
        allowedMftFileNameCharacters = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> "-_"
        validateMftFileName =                
            case Text.splitOn "." filename of 
                [ mainName, extension ] -> do
                    let loweredExtension = Text.toLower extension
                    unless (isSupportedExtension loweredExtension) $ 
                        vError $ BadFileNameOnMFT filename 
                                    ("Unsupported filename extension " <> extension)
                    unless (Text.all (`elem` allowedMftFileNameCharacters) mainName) $ do 
                        let badChars = Text.filter (`notElem` allowedMftFileNameCharacters) mainName
                        vError $ BadFileNameOnMFT filename 
                                    ("Unsupported characters in filename: '" <> badChars <> "'")
                somethgingElse -> 
                    vError $ BadFileNameOnMFT filename 
                                "Filename doesn't have exactly one DOT"            

    
    validateChild validCrl ro = do
        -- At the moment of writing RFC 6486-bis 
        -- (https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-03#page-12) 
        -- prescribes to consider the manifest invalid if any of the objects 
        -- referred by the manifest is invalid. 
        -- 
        -- That's why recursive validation of the child CA happens in the separate   
        -- runValidatorT (...) call, but all the other objects are supposed to be 
        -- validated within the same context of ValidatorT, i.e. have short-circuit
        -- logic implemented by ExceptT.
        parentContext <- ask
        case ro of
            CerRO childCert -> do 
                let TopDownContext{..} = topDownContext
                (r, validationState) <- liftIO $ runValidatorT parentContext $                     
                        inSubVPath (toText $ NonEmpty.head $ getLocations ro) $ do                                
                            childVerifiedResources <- vHoist $ do                 
                                    Validated validCert <- validateResourceCert now childCert certificate validCrl
                                    validateResources verifiedResources childCert validCert
                            let childTopDownContext = topDownContext { 
                                    verifiedResources = Just childVerifiedResources 
                                }
                            validateCaCertificate appContext childTopDownContext childCert                            
    
                -- case r of
                --     -- TODO Think about: it probably should be 
                --     -- Left e -> let 
                --     --             childCaError = mError (vContext $ toText $ NonEmpty.head $ getLocations ro) e)
                --     --           in T3 emptyPublicationPoints mempty (fromValidations childCaError)
                --     -- because we want to ignore all the errors down the tree when reporting up, they can be confusing.
                embedState validationState
                pure $! fromRight mempty r                    

            RoaRO roa -> inSubVPath (toText $ NonEmpty.head $ getLocations ro) $ 
                            allowRevoked $ do
                                void $ vHoist $ validateRoa now roa certificate validCrl verifiedResources
                                oneMoreRoa
                                
                                let vrps = getCMSContent $ cmsPayload roa
                                -- logDebugM logger [i|roa #{NonEmpty.head $ getLocations ro}, vrps = #{vrps}.|]
                                pure $! T3 mempty mempty $ Set.fromList vrps

            GbrRO gbr -> inSubVPath (toText $ NonEmpty.head $ getLocations ro) $ 
                            allowRevoked $ do
                                void $ vHoist $ validateGbr now gbr certificate validCrl verifiedResources
                                oneMoreGbr
                                pure $! mempty

            -- TODO Anything else?
            _ -> pure mempty

        where                
            -- In case of RevokedResourceCertificate error, manifest should not be considered 
            -- invalid, only the object with the revoked certificate is considered invalid.
            -- This is a slightly ad-hoc code, but works fine
            allowRevoked f =                
                catchAndEraseError f isRevokedCertError $ do 
                    vWarn RevokedResourceCertificate
                    pure $! mempty
                where                 
                    isRevokedCertError (ValidationE RevokedResourceCertificate) = True
                    isRevokedCertError _ = False


    findMft childrenAki locations = do
        objectStore' <- (^. #objectStore) <$> liftIO (readTVarIO database)
        mft' <- liftIO $ roTx objectStore' $ \tx -> 
            findLatestMftByAKI tx objectStore' childrenAki
        case mft' of
            Nothing  -> vError $ NoMFT childrenAki locations
            Just mft -> do
                visitObject appContext topDownContext mft
                pure mft

    -- TODO Is there a more reliable way to find it?
    findCrlOnMft mft = filter (\(T2 name _) -> ".crl" `Text.isSuffixOf` name) $
        mftEntries $ getCMSContent $ cmsPayload mft

    -- Check that manifest URL in the certificate is the same as the one 
    -- the manifest was actually fetched from.
    validateMftLocation mft certficate = 
        case getManifestUri $ cwsX509certificate $ getCertWithSignature certficate of
            Nothing     -> vError $ NoMFTSIA $ getLocations certficate
            Just mftSIA -> let 
                mftLocations = getLocations mft
                in case NonEmpty.filter ((mftSIA ==) . getURL) mftLocations of 
                    [] -> vWarn $ MFTOnDifferentLocation mftSIA mftLocations
                    _ ->  pure ()

    -- Check that CRL URL in the certificate is the same as the one 
    -- the CRL was actually fetched from. 
    -- 
    -- TODO Figure out if it's even needeed -- it generates a lot of useless warnings.
    -- checkCrlLocation crl certficate = 
    --     case getCrlDistributionPoint $ cwsX509certificate $ getCertWithSignature certficate of
    --         -- TODO TA certificate don't have CRL DP so don't emit errors/warnings here.
    --         -- But if CRL DP does present it needs to be equal to (one of) the CRL locations.
    --         Nothing    -> pure ()
    --         Just crlDP -> let 
    --             crlLocations = getLocations crl
    --             in case NonEmpty.filter ((crlDP ==) . getURL) crlLocations of 
    --                 [] -> vWarn $ CRLOnDifferentLocation crlDP crlLocations
    --                 _ ->  pure ()


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

-- Mark validated objects in the database.
markValidatedObjects :: (MonadIO m, Storage s) => 
                        AppContext s -> TopDownContext s -> m ()
markValidatedObjects AppContext { .. } TopDownContext {..} = do
    objectStore' <- (^. #objectStore) <$> liftIO (readTVarIO database)
    (size, elapsed) <- timedMS $ liftIO $ do 
            vhs <- readTVarIO visitedHashes        
            rwTx objectStore' $ \tx -> 
                for_ vhs $ \h -> 
                    markValidated tx objectStore' h worldVersion 
            pure $! Set.size vhs
    logDebugM logger [i|Marked #{size} objects as visited, took #{elapsed}ms.|]


-- | Figure out which PPs are to be fetched, based on the global RepositoryContext
-- and a set of PPs (usually just discovered by traversing the tree of objects)
-- 
getPPsToFetch :: TVar RepositoryContext -> PublicationPoints -> STM PublicationPoints
getPPsToFetch repositoryContext discoveredPPs = do 
    RepositoryContext {..} <- readTVar repositoryContext    

    let newGlobalPPs     = adjustLastSucceeded $ publicationPoints <> discoveredPPs
    let discoveredURIs   = allURIs discoveredPPs
    let urisToTakeCareOf = Set.difference discoveredURIs takenCareOf

    writeTVar repositoryContext $ RepositoryContext { 
        takenCareOf       = takenCareOf <> discoveredURIs,
        publicationPoints = newGlobalPPs
    }
    pure $ newGlobalPPs `shrinkTo` urisToTakeCareOf


-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
visitObject :: (MonadIO m, WithHash ro, Storage s) => 
                AppContext s -> TopDownContext s -> ro -> m ()
visitObject _ topDownContext ro = 
    visitObjects topDownContext [getHash ro]    

visitObjects :: MonadIO m => TopDownContext s -> [Hash] -> m ()
visitObjects TopDownContext {..} hashes =
    liftIO $ atomically $ modifyTVar' visitedHashes (<> Set.fromList hashes)


oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl, oneMoreGbr :: Monad m => ValidatorT m ()
oneMoreCert = updateMetric @ValidationMetric @_ (& #validCertNumber %~ (+1))
oneMoreRoa  = updateMetric @ValidationMetric @_ (& #validRoaNumber %~ (+1))
oneMoreMft  = updateMetric @ValidationMetric @_ (& #validMftNumber %~ (+1))
oneMoreCrl  = updateMetric @ValidationMetric @_ (& #validCrlNumber %~ (+1))
oneMoreGbr  = updateMetric @ValidationMetric @_ (& #validGbrNumber %~ (+1))

setVrpNumber n = updateMetric @ValidationMetric @_ (& #vrpNumber .~ n)


-- Sum up all the validation metrics from all TA to create 
-- the "alltrustanchors" validation metric
addTotalValidationMetric totalValidationResult =
    totalValidationResult & vmLens %~ Map.insert (newPath allTAsMetricsName) totalValidationMetric
  where
    uniqueVrps = totalValidationResult ^. #vrps
    totalValidationMetric = mconcat (Map.elems $ totalValidationResult ^. vmLens) 
                            & #vrpNumber .~ Count (fromIntegral $ Set.size uniqueVrps)
    vmLens = typed @ValidationState . 
            typed @AppMetric . 
            #validationMetrics . 
            #unMetricMap . 
            #unMonoidMap                    

-- | Fetch TA certificate based on TAL location(s)
fetchTACertificate :: AppContext s -> TAL -> ValidatorT IO (RpkiURL, RpkiObject)
fetchTACertificate appContext@AppContext {..} tal = 
    go $ NonEmpty.toList $ certLocations tal
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

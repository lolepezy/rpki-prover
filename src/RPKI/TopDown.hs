{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.TopDown where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Sum

import           GHC.Generics

import           Data.Foldable
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.List.Split                  (chunksOf)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text

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
import           RPKI.StrictTuple
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.Validation.ObjectValidation
import           RPKI.AppState

import           Data.Hourglass
import           System.Timeout                   (timeout)


-- data Stats = Stats {
--     validCount :: Int
-- }

-- List of hashes of certificates, validation contexts and verified resource sets 
-- that are waiting for a PP to be fetched. CA certificates, pointig to delegated 
-- CAs are normally getting in this list.
newtype WaitingList c =  WaitingList { unWList :: 
        Map RpkiURL (Set (T3 Hash c (Maybe (VerifiedRS PrefixesAndAsns))))
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype Monoid

instance Ord c => Semigroup (WaitingList c) where
    (WaitingList w1) <> (WaitingList w2) = WaitingList $ Map.unionWith (<>) w1 w2

toWaitingList :: RpkiURL -> Hash -> c -> Maybe (VerifiedRS PrefixesAndAsns) -> WaitingList c
toWaitingList rpkiUrl hash vc resources =     
    WaitingList $ Map.singleton rpkiUrl (Set.singleton (T3 hash vc resources))


-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext s = TopDownContext {    
    verifiedResources           :: Maybe (VerifiedRS PrefixesAndAsns),    
    publicationPoints           :: TVar PublicationPoints,
    takenCareOf                 :: TVar (Set RpkiURL),
    taName                      :: TaName, 
    now                         :: Now,
    worldVersion                :: WorldVersion,
    visitedHashes               :: TVar (Set Hash)
} deriving stock (Generic)


data TopDownResult = TopDownResult {
        vrps          :: [Vrp],
        tdValidations :: ValidationState
    }
    deriving stock (Show, Eq, Ord, Generic)

instance Monoid TopDownResult where
    mempty = TopDownResult mempty mempty

instance Semigroup TopDownResult where
    TopDownResult v1 vs1 <> TopDownResult v2 vs2 = TopDownResult (v1 <> v2) (vs1 <> vs2)

fromValidations :: ValidationState -> TopDownResult
fromValidations validationState = TopDownResult { 
        vrps          = mempty, 
        tdValidations = validationState 
    }

flatten :: (Either AppError TopDownResult, ValidationState) -> TopDownResult
flatten (Left _, vs)  = fromValidations vs
flatten (Right t, vs) = fromValidations vs <> t


newTopDownContext :: MonadIO m => 
                    AppContext s -> 
                    WorldVersion -> 
                    TaName -> 
                    PublicationPoints -> 
                    Now -> 
                    CerObject -> 
                    m (TopDownContext s)
newTopDownContext AppContext {..} worldVersion taName publicationPoints now certificate = liftIO $ do                 
    atomically $ TopDownContext (Just $ createVerifiedResources certificate) <$> 
        newTVar publicationPoints <*>
        newTVar Set.empty <*>
        pure taName <*> 
        pure now <*>
        pure worldVersion <*>
        newTVar Set.empty

createVerifiedResources :: CerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources (getRC -> ResourceCertificate certificate) = 
    VerifiedRS $ toPrefixesAndAsns $ withRFC certificate resources

-- | It is the main entry point for the top-down validation. 
-- Validates TA starting from its TAL.
--
validateTA :: Storage s => 
            AppContext s -> TAL -> WorldVersion -> IO TopDownResult
validateTA appContext@AppContext {..} tal worldVersion = do    
    r <- runValidatorT taContext $
            inSubVContext (toText $ getTaCertURL tal) $ do
                ((taCert, repos, _), elapsed) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                logDebugM logger [i|Fetched and validated TA certficate #{certLocations tal}, took #{elapsed}ms.|]        
                validateFromTACert appContext (getTaName tal) taCert repos worldVersion
    
    pure $! flatten r    
    where                            
        taContext = newValidatorContext taNameText
        TaName taNameText = getTaName tal


data TACertStatus = Existing | Updated

-- | Fetch and validated TA certificate starting from the TAL.
-- | 
-- | This function doesn't throw exceptions.
validateTACertificateFromTAL :: Storage s => 
                                AppContext s -> 
                                TAL -> 
                                WorldVersion ->
                                ValidatorT IO (CerObject, NonEmpty Repository, TACertStatus)
validateTACertificateFromTAL appContext@AppContext {..} tal worldVersion = do
    let now = Now $ versionToMoment worldVersion
    let validationConfig = config ^. typed @ValidationConfig

    taByName <- roAppTxEx taStore storageError $ \tx -> getTA tx taStore taName'
    case taByName of
        Nothing -> fetchValidateAndStore now
        Just StorableTA { taCert, initialRepositories, fetchStatus }
            | needsFetching (getTaCertURL tal) fetchStatus validationConfig now ->
                fetchValidateAndStore now
            | otherwise -> 
                pure (taCert, initialRepositories, Existing)
    where       
        fetchValidateAndStore now = do 
            (uri', ro) <- fetchTACertificate appContext tal
            newCert'   <- vHoist $ validateTACert tal uri' ro
            rwAppTxEx taStore storageError $ \tx -> storeTaCert tx newCert' now           

        storeTaCert tx cert (Now moment) = 
            case createRepositoriesFromTAL tal cert of
                Left e      -> appError $ ValidationE e
                Right repos -> do 
                    putTA tx taStore (StorableTA tal cert (FetchedAt moment) repos)
                    pure (cert, repos, Updated)

        taName' = getTaName tal  
        taStore = database ^. #taStore   


-- incValidObject :: TaName -> ValidatorT env IO ()
-- incValidObject taName = do 
--     modifyMetric
--         (MetricKey $ unTaName taName)
--         -- TODO Use a prism here
--         (\a -> let 
--             q = _Ctor @"ValidationM" . #validCertNumber
--             z = a ^? _Ctor @"ValidationM" . #validCertNumber
--             in z %~ (+1))




-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: Storage s =>
                    AppContext s -> 
                    TaName -> 
                    CerObject -> 
                    NonEmpty Repository -> 
                    WorldVersion -> 
                    ValidatorT IO TopDownResult
validateFromTACert appContext@AppContext {..} taName' taCert initialRepos worldVersion = do  
    -- this will be used as the "now" in all subsequent time and period validations 
    let now = Now $ versionToMoment worldVersion

    let taURIContext = newValidatorContext $ toText $ NonEmpty.head $ getLocations taCert

    storedPubPoints <- roAppTxEx database storageError $ \tx -> 
                    getTaPublicationPoints tx (repositoryStore database) taName'

    let reposToFetch = map fst $ 
            -- filter the ones that are either new or need refetching
            filter (\(pp, status) -> needsFetching pp status (config ^. typed @ValidationConfig) now) $ 
            toRepoStatusPairs $ 
                -- merge repos that we want to be fetched with the ones that are stored                     
                mergeRepos initialRepos storedPubPoints
                -- we only care about URLs from 'repos', so shrink the PPs                        
                    `shrinkTo` 
                Set.fromList (map getRpkiURL $ NonEmpty.toList initialRepos)

    fetchStatuses <- parallelTasks 
                        (ioBottleneck appBottlenecks)
                        reposToFetch 
                        (fetchRepository appContext taURIContext now)

    case partitionFailedSuccess fetchStatuses of 
        ([], _) -> do
            let flattenedStatuses = flip map fetchStatuses $ \case 
                    FetchFailure r s _ -> (r, FailedAt s)
                    FetchSuccess r s _ -> (r, FetchedAt s)

            -- use publication points taken from the DB and updated with the 
            -- the fetchStatuses of the fetches that we just performed
            let fetchUpdatedPPs = updateStatuses storedPubPoints flattenedStatuses

            topDownContext <- newTopDownContext appContext worldVersion taName' fetchUpdatedPPs now taCert             

            -- Do the tree descend, gather validation results and VRPs            
            topDownResult <- fromTry (\e -> UnspecifiedE (unTaName taName') (fmtEx e)) $
                validateCA appContext taURIContext topDownContext taCert                    

            -- get publication points from the topDownContext and save it to the database
            pubPointAfterTopDown <- liftIO $ readTVarIO (publicationPoints topDownContext)
            
            -- logDebugM logger [i|#{taName'} validCount = #{validCount} |]
            
            rwAppTxEx database storageError $ \tx -> do                
                -- 
                -- `latestStoreState` is the state in the storage that contains updates 
                -- made during validation, i.e. RRDP serials updates. In principle, in-memory 
                -- state (pubPointAfterTopDown) must be the same, but "just in case" we re-read 
                -- `latestStoreState` and merge them before applying the change set. 
                -- 
                -- TODO It needs to be refactored as it's obviously too complicated, brittle
                -- and nobody would understand what's going on there.
                -- 
                latestStoreState <- getTaPublicationPoints tx (repositoryStore database) taName'
                let changeSet' = changeSet storedPubPoints (pubPointAfterTopDown <> latestStoreState)
                applyChangeSet tx (repositoryStore database) changeSet' taName'
                        
            pure topDownResult

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
                AppContext s -> ValidatorContext -> Now -> Repository -> m FetchResult
fetchRepository 
    appContext@AppContext { database = DB {..}, ..} 
    parentContext 
    (Now now) 
    repo = liftIO $ do
        let (Seconds maxDduration, timeoutError) = case repoURL of
                RrdpU _  -> (config ^. typed @RrdpConf . #rrdpTimeout, RrdpE RrdpDownloadTimeout)
                RsyncU _ -> (config ^. typed @RsyncConf . #rsyncTimeout, RsyncE RsyncDownloadTimeout)
                
        r <- timeout (1_000_000 * fromIntegral maxDduration) fetchIt
        case r of 
            Nothing -> do 
                logErrorM logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDduration}s.|]
                pure $ FetchFailure repo now (vState $ mError vContext' timeoutError)
            Just z -> pure z        
    where 
        repoURL      = getRpkiURL repo
        childContext = validatorSubContext (toText repoURL) parentContext
        vContext'    = childContext ^. typed @VTrail

        fetchIt = do
            logDebugM logger [i|Fetching #{repoURL} |]
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
                    logErrorM logger [i|Fetching repository #{getURL repoURL} failed: #{e} |]
                    pure $ FetchFailure repo now (vState (mError vContext' e) <> v)
                Right resultRepo -> do
                    logDebugM logger [i|Fetched repository #{getURL repoURL}, took #{elapsed}ms.|]
                    pure $ FetchSuccess resultRepo now v


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


-- | Validate CA starting from its certificate.
-- 
validateCA :: Storage s =>
            AppContext s -> ValidatorContext -> TopDownContext s -> CerObject -> IO TopDownResult
validateCA appContext caVContext topDownContext certificate =
    validateCARecursively appContext caVContext topDownContext certificate
        `finally` 
    markValidatedObjects appContext topDownContext            


-- 
validateCARecursively :: Storage s => 
                        AppContext s 
                    -> ValidatorContext 
                    -> TopDownContext s
                    -> CerObject 
                    -> IO TopDownResult
validateCARecursively 
        appContext@AppContext {..} 
        vc 
        topDownContext@TopDownContext{..} 
        certificate = do     

    (r, validations) <- runValidatorT vc $ validateCaCertificate appContext topDownContext certificate
    case r of
        Left _alreadyQueued -> pure $ fromValidations validations
        Right (T3 discoveredPPs waitingList tdResult) -> do                    
            tdResults <- extractPPsAndValidateDown discoveredPPs waitingList
            pure $! mconcat tdResults <> tdResult <> fromValidations validations                

    where
        -- From the set of discovered PPs figure out which ones must be fetched, 
        -- fetch them and validate, starting from the cerfificates in their 
        -- waiting lists.        
        extractPPsAndValidateDown ppsToFetch waitingList = do            
            ppsToFetch' <- atomically $ do 
                    globalPPs           <- readTVar publicationPoints                    
                    alreadyTakenCareOf  <- readTVar takenCareOf

                    let newGlobalPPs     = adjustLastSucceeded $ globalPPs <> ppsToFetch
                    let discoveredURIs   = allURIs ppsToFetch
                    let urisToTakeCareOf = Set.difference discoveredURIs alreadyTakenCareOf

                    writeTVar publicationPoints newGlobalPPs                                        
                    modifyTVar' takenCareOf (<> discoveredURIs)
                            
                    pure $! newGlobalPPs `shrinkTo` urisToTakeCareOf

            let (_, rootToPps) = repositoryHierarchy ppsToFetch'

            -- For all discovered repositories that need fetching (new or failed 
            -- or fetched long ago), drill down recursively.
            parallelTasks 
                (ioBottleneck appBottlenecks) 
                (Map.keys rootToPps) $ \repo ->
                    fetchAndValidateWaitingList rootToPps repo waitingList                    
                    

        -- Fetch the PP and validate all the certificates from the waiting 
        -- list of this PP.
        fetchAndValidateWaitingList rootToPps repo (WaitingList waitingList) = do

            -- Gather the PPs and hashes of the objects from their waiting lists
            let ppsForTheRepo = fromMaybe Set.empty $ Map.lookup repo rootToPps            
            let waitingListForThesePPs = Set.toList $ fromMaybe Set.empty $ fold $ 
                    Set.map (\pp -> getRpkiURL pp `Map.lookup` waitingList) ppsForTheRepo
            
            -- try to recover the validation context
            let waitingVContext = case waitingListForThesePPs of
                                []             -> vc
                                T3 _ vc' _ : _ -> vc'

            fetchResult <- fetchRepository appContext waitingVContext now repo                                            

            let statusUpdate = case fetchResult of
                            FetchFailure r t _ -> (r, FailedAt t)
                            FetchSuccess r t _ -> (r, FetchedAt t)                      
            pps <- atomically $ do                     
                    modifyTVar' publicationPoints $ \pubPoints -> updateStatuses pubPoints [statusUpdate]
                    readTVar publicationPoints
            
            proceedUsingGracePeriod pps waitingListForThesePPs fetchResult


        -- Decide what to do with the result of PP fetching
        -- * if it succeeded validate the tree
        -- * if it failed
        --   - if we are still within the grace period, then validate the tree
        --   - if grace period is expired, stop here
        proceedUsingGracePeriod pps waitingListForThesePPs fetchResult = do
            let proceedWithValidation validations = do                
                    tdResults <- validateWaitingList waitingListForThesePPs
                    pure $! mconcat tdResults <> fromValidations validations

            let noFurtherValidation validations = pure $ fromValidations validations

            let Now now' = now
            case fetchResult of
                FetchSuccess _ _ validations -> proceedWithValidation validations
                FetchFailure r _ validations -> 
                    -- check when was the last successful fetch of this URL
                    case lastSuccess pps $ getRpkiURL r of
                        Nothing -> do 
                            logWarnM logger [i|Repository #{getRpkiURL r} failed, it never succeeded to fetch so tree validation will not proceed for it.|]    
                            noFurtherValidation validations
                        Just successInstant -> 
                            case appContext ^. typed @Config . typed @ValidationConfig . #repositoryGracePeriod of
                                Nothing -> noFurtherValidation validations
                                Just repositoryGracePeriod 
                                    | closeEnoughMoments now' successInstant repositoryGracePeriod -> do 
                                        logWarnM logger $ 
                                            [i|Repository #{getRpkiURL r} failed, but grace period of #{repositoryGracePeriod} is set, |] <>
                                            [i|last success #{successInstant}, current moment is #{now'}.|]    
                                        proceedWithValidation validations
                                    | otherwise -> do 
                                        logWarnM logger $
                                            [i|Repository #{getRpkiURL r} failed, grace period of #{repositoryGracePeriod} has expired, |] <>
                                            [i|last success #{successInstant}, current moment is #{now'}.|]    
                                        noFurtherValidation validations
                                 

        -- Resume tree validation starting from every certificate on the waiting list.
        -- 
        validateWaitingList waitingList =
            parallelTasks 
                (cpuBottleneck appBottlenecks) 
                waitingList $ \(T3 hash certVContext verifiedResources') -> do                    
                    o <- roTx database $ \tx -> getByHash tx (objectStore database) hash
                    case o of 
                        Just (CerRO waitingCertificate) -> do
                            -- logInfoM logger [i|From waiting list of #{getRpkiURL repo}: #{getLocations waitingCertificate}.|]
                            let childTopDownContext = topDownContext { 
                                    -- we should start from the resource set of this certificate
                                    -- as it is already has been verified
                                    verifiedResources = verifiedResources'                                                
                                }
                            validateCARecursively appContext certVContext childTopDownContext waitingCertificate 
                        ro -> do
                            logErrorM logger [i| Something is really wrong with the hash #{hash} in waiting list, got #{ro}|]            
                            pure mempty
        
    

-- | Do top-down validation starting from the given certificate
-- Returns the discovered publication points that are not registered 
-- in the top-down context yet.
validateCaCertificate :: Storage s =>
                        AppContext s ->
                        TopDownContext s ->
                        CerObject ->                
                        ValidatorT IO (T3 PublicationPoints (WaitingList ValidatorContext) TopDownResult)
validateCaCertificate appContext@AppContext {..} topDownContext certificate = do          
    globalPPs <- liftIO $ readTVarIO (topDownContext ^. #publicationPoints)

    let validationConfig = appContext ^. typed @Config . typed

    case publicationPointsFromCertObject certificate of
        Left e                    -> appError $ ValidationE e
        Right (url, discoveredPP) -> do
            let asIfItIsMerged = discoveredPP `mergePP` globalPPs

            let stopDescend = do 
                    -- remember to come back to this certificate when the PP is fetched
                    vContext' <- ask
                    pure $! T3 
                            (asIfItIsMerged `shrinkTo` (Set.singleton url)) 
                            (toWaitingList
                                (getRpkiURL discoveredPP) 
                                (getHash certificate) 
                                vContext' 
                                (verifiedResources topDownContext))
                            mempty

            case findPublicationPointStatus url asIfItIsMerged of 
                -- this publication point hasn't been seen at all, so stop here
                Nothing -> stopDescend

                -- If it's been fetched too long ago, stop here and add the certificate 
                -- to the waiting list of this PP
                -- if the PP is fresh enough, proceed with the tree descend                
                Just status -> let                
                    needToRefetch = needsFetching discoveredPP status validationConfig (now topDownContext)                    
                    in if needToRefetch
                        then stopDescend 
                        else validateThisCertAndGoDown                    
    where
        
        validateThisCertAndGoDown = do            
            let (childrenAki, certLocations') = (toAKI $ getSKI certificate, getLocations certificate)        

            -- this for the certificate
            -- incValidObject (topDownContext ^. typed)
            visitObject appContext topDownContext (CerRO certificate)

            mft <- findMft childrenAki certLocations'
            checkMftLocation mft certificate
                    
            manifestResult <- inSubVContext (toText $ NonEmpty.head $ getLocations mft) $ do
                -- find CRL on the manifest
                (_, crlHash) <- case findCrlOnMft mft of 
                    []    -> vError $ NoCRLOnMFT childrenAki certLocations'
                    [crl] -> pure crl
                    crls  -> vError $ MoreThanOneCRLOnMFT childrenAki certLocations' crls

                let objectStore' = objectStore database
                crlObject <- roAppTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
                case crlObject of 
                    Nothing          -> vError $ NoCRLExists childrenAki certLocations'    
                    Just foundCrl@(CrlRO crl) -> do      
                        visitObject appContext topDownContext foundCrl
                        -- logDebugM logger [i|crl = #{NonEmpty.head $ getLocations crl} (#{thisUpdateTime $ signCrl crl}, #{nextUpdateTime $ signCrl crl})|]
                        -- validate CRL and MFT together
                        validCrl <- inSubVContext (toText $ NonEmpty.head $ getLocations crl) $ 
                                        vHoist $ do          
                                            -- checkCrlLocation crl certificate
                                            validateCrl (now topDownContext) crl certificate

                        -- MFT can be revoked by the CRL that is on this MFT -- detect it                                
                        void $ vHoist $ validateMft (now topDownContext) mft certificate validCrl

                        -- this for the CRL
                        -- incValidObject (topDownContext ^. typed)          
                                            
                        -- filter out CRL itself
                        let childrenHashes = filter ((/= getHash crl) . snd) 
                                    $ mftEntries $ getCMSContent $ cmsPayload mft
                    
                        -- Mark all manifest entries as visited to avoid the situation
                        -- when some of the children are deleted from the cache and some
                        -- are still there. Do it both in case of successful validation
                        -- or a validation error.
                        let markAllEntriesAsVisited = 
                                visitObjects topDownContext $ map snd childrenHashes
                        
                        let processChildren = do 
                                -- let hashesInChunks = chunksOf 200 childrenHashes
                                -- r <- fmap mconcat $ parallelTasks 
                                --         (cpuBottleneck appBottlenecks) 
                                --         hashesInChunks 
                                --         $ mapM $ \(filename, hash') -> 
                                --                   validateManifestEntry filename hash' validCrl
                                r <- parallelTasks 
                                        (cpuBottleneck appBottlenecks) 
                                        childrenHashes
                                        $ \(filename, hash') -> 
                                                  validateManifestEntry filename hash' validCrl
                                markAllEntriesAsVisited
                                pure $! r
                        
                        childrenResults <- processChildren `catchError` 
                                        (\e -> markAllEntriesAsVisited >> throwError e)

                        -- Combine PPs and their waiting lists. On top of it, fix the 
                        -- last successfull validation times for PPs based on their fetch statuses.
                        let T3 pps waitingList vrps = mconcat childrenResults
                        pure $! T3 (adjustLastSucceeded pps) waitingList vrps

                    Just _  -> vError $ CRLHashPointsToAnotherObject crlHash certLocations'   

            -- this for the valid manifest
            -- incValidObject (topDownContext ^. typed)

            pure manifestResult

        --
        -- | Validate an entry of the manifest, i.e. a pair of filename and hash
        -- 
        validateManifestEntry filename hash' validCrl = do                    
            validateMftFileName
            -- 
            visitedObjects <- liftIO $ readTVarIO $ visitedHashes topDownContext

            let objectStore' = objectStore database
            ro <- roAppTx objectStore' $ \tx -> getByHash tx objectStore' hash'
            case ro of 
                Nothing -> vError $ ManifestEntryDontExist hash'
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
                validateMftFileName = do 
                    -- TODO Check characters in the filenames
                    pure ()


        -- 
        -- validateChild :: Validated CrlObject -> 
        --                 RpkiObject -> 
        --                 ValidatorT c IO (T3 PublicationPoints (WaitingList c) TopDownResult)
        validateChild validCrl ro = do
            -- At the moment of writing RFC 6486-bis 
            -- (https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-00#page-12) 
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
                            inSubVContext (toText $ NonEmpty.head $ getLocations ro) $ do
                                childVerifiedResources <- vHoist $ do                 
                                        Validated validCert <- validateResourceCert now childCert certificate validCrl
                                        validateResources verifiedResources childCert validCert 
                                let childTopDownContext = topDownContext { verifiedResources = Just childVerifiedResources }
                                validateCaCertificate appContext childTopDownContext childCert                            
        
                    pure $! case r of
                        -- TODO Think about: it probably should be 
                        -- Left e -> let 
                        --             childCaError = mError (vContext $ toText $ NonEmpty.head $ getLocations ro) e)
                        --           in T3 emptyPublicationPoints mempty (fromValidations childCaError)
                        -- because we want to ignore all the errors down the tree when reporting up, they can be confusing.
                        Left _                     -> T3 emptyPublicationPoints mempty (fromValidations validationState)                        
                        Right (T3 pps wl tdResult) -> T3 pps wl (tdResult <> fromValidations validationState)

                RoaRO roa ->
                    inSubVContext (toText $ NonEmpty.head $ getLocations ro) $ do
                        void $ vHoist $ validateRoa (now topDownContext) roa certificate validCrl
                                            
                        -- incValidObject (topDownContext ^. typed)
                            -- logDebugM logger [i|#{getLocations roa}, VRPs: #{getCMSContent (extract roa :: CMS [Vrp])}|]
                        let vrps = getCMSContent $ cmsPayload roa
                        pure $! T3 emptyPublicationPoints mempty (TopDownResult vrps mempty)

                GbrRO gbr -> withEmptyPPs $
                    inSubVContext (toText $ NonEmpty.head $ getLocations ro) $ do
                        void $ vHoist $ validateGbr (now topDownContext) gbr certificate validCrl                    
                        -- incValidObject (topDownContext ^. typed)                    

                -- TODO Anything else?
                _ -> withEmptyPPs $ pure ()

            where                
                withEmptyPPs f = f >> (pure $! T3 emptyPublicationPoints mempty mempty)
        

        findMft childrenAki locations = do
            mft' <- liftIO $ roTx (objectStore database) $ \tx -> 
                findLatestMftByAKI tx (objectStore database) childrenAki
            case mft' of
                Nothing  -> vError $ NoMFT childrenAki locations
                Just mft -> do
                    visitObject appContext topDownContext mft
                    pure mft

        -- TODO Is there a more reliable way to find it?
        findCrlOnMft mft = filter (\(name, _) -> ".crl" `Text.isSuffixOf` name) $ 
            mftEntries $ getCMSContent $ cmsPayload mft

        -- Check that manifest URL in the certificate is the same as the one 
        -- the manifest was actually fetched from.
        checkMftLocation mft certficate = 
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


-- Check if an URL need to be re-fetched, based on fetch status and current time.
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
markValidatedObjects AppContext { database = DB {..}, .. } TopDownContext {..} = do
    (size, elapsed) <- timedMS $ liftIO $ do 
            vhs <- readTVarIO visitedHashes        
            rwTx objectStore $ \tx -> 
                forM_ vhs $ \h -> 
                    markValidated tx objectStore h worldVersion 
            pure $ Set.size vhs
    logDebugM logger [i|Marked #{size} objects as visited, took #{elapsed}ms.|]


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
                    logInfoM logger [i|Fetching TA certiicate from #{getURL u}..|]
                    ro <- case u of 
                        RsyncU rsyncU -> rsyncRpkiObject appContext rsyncU
                        RrdpU rrdpU   -> fetchRpkiObject appContext rrdpU
                    pure (u, ro)

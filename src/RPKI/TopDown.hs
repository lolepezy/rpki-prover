{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.TopDown where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

-- import           Data.Generics.Product.Fields
import           GHC.Generics

import           Data.Bifunctor
import           Data.Foldable
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
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
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RRDP.Http
import           RPKI.RRDP.RrpdFetch
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Database
import           RPKI.Store.Repository
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.Validation.ObjectValidation
import           RPKI.Version

import           Data.Hourglass
import           System.Timeout                   (timeout)



data Stats = Stats {
    validCount :: Int
}

data Triple a b c = Triple a b c
    deriving stock (Show, Eq, Ord, Generic)
    
instance (Monoid a, Monoid b, Monoid c) => Monoid (Triple a b c) where
    mempty = Triple mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Triple a b c) where
    Triple a1 b1 c1 <> Triple a2 b2 c2 = Triple (a1 <> a2) (b1 <> b2) (c1 <> c2)

-- List of hashes of certificates, validation contexts and verified resource sets 
-- that are waiting for a PP to be fetched. CA certificates, pointig to delegated 
-- CAs are normally getting in this list.
newtype WaitingList =  WaitingList { unWList :: 
        (Map RpkiURL (Set (Triple Hash VContext (Maybe (VerifiedRS PrefixesAndAsns)))))
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype Monoid

instance Semigroup WaitingList where
    (WaitingList w1) <> (WaitingList w2) = WaitingList $ Map.unionWith (<>) w1 w2

toWaitingList :: RpkiURL -> Hash -> VContext -> Maybe (VerifiedRS PrefixesAndAsns) -> WaitingList
toWaitingList rpkiUrl hash vc resources =     
    WaitingList $ Map.singleton rpkiUrl (Set.singleton (Triple hash vc resources))


-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext s = TopDownContext {    
    verifiedResources           :: Maybe (VerifiedRS PrefixesAndAsns),    
    publicationPoints           :: TVar PublicationPoints,
    takenCareOf                 :: TVar (Set RpkiURL),
    taName                      :: TaName, 
    now                         :: Now,    
    objectStats                 :: TVar Stats,
    worldVersion                :: WorldVersion,
    visitedHashes               :: TVar (Set Hash)
} deriving stock (Generic)


data TopDownResult = TopDownResult {
        vrps          :: [Vrp],
        tdValidations :: Validations
    }
    deriving stock (Show, Eq, Ord, Generic)

instance Monoid TopDownResult where
    mempty = TopDownResult mempty mempty

instance Semigroup TopDownResult where
    TopDownResult v1 vs1 <> TopDownResult v2 vs2 = TopDownResult (v1 <> v2) (vs1 <> vs2)

fromValidations :: Validations -> TopDownResult
fromValidations validations = TopDownResult { vrps = mempty, tdValidations = validations }

flatten :: (Either AppError TopDownResult, Validations) -> TopDownResult
flatten (Left _, validations)  = fromValidations validations
flatten (Right t, validations) = fromValidations validations <> t


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
        newTVar (Stats 0) <*>
        pure worldVersion <*>
        newTVar Set.empty

createVerifiedResources :: CerObject -> VerifiedRS PrefixesAndAsns
createVerifiedResources (getRC -> ResourceCertificate certificate) = 
    VerifiedRS $ toPrefixesAndAsns $ withRFC certificate resources

incValidObject :: MonadIO m => TopDownContext s -> m ()
incValidObject TopDownContext {..} = liftIO $ atomically $ 
    modifyTVar' objectStats $ \s -> s { validCount = validCount s + 1 }


-- | Validate TA given that the TA certificate is downloaded and up-to-date.
--  is the main entry point for the top-down validation when it's known that
-- TA certificate is up-to-date and valid.
--
validateTA :: Storage s => 
            AppContext s -> TAL -> WorldVersion -> IO TopDownResult
validateTA appContext@AppContext {..} tal worldVersion = do    
    r <- runValidatorT taContext $
            forChild (toText $ getTaCertURL tal) $ do
                ((taCert, repos, _), elapsed) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                logDebugM logger [i|Fetched and validated TA certficate #{certLocations tal}, took #{elapsed}ms.|]        
                validateFromTACert appContext (getTaName tal) taCert repos worldVersion
          
    pure $ flatten r    
    where                            
        taContext = vContext taNameText
        TaName taNameText = getTaName tal


data TACertStatus = Existing | Updated

-- | Fetch and validated TA certificate starting from the TAL.
-- | 
-- | This function doesn't throw exceptions.
validateTACertificateFromTAL :: (WithVContext vc, Storage s) => 
                                AppContext s -> 
                                TAL -> 
                                WorldVersion ->
                                ValidatorT vc IO (CerObject, NonEmpty Repository, TACertStatus)
validateTACertificateFromTAL appContext@AppContext {..} tal worldVersion = do
    let now = Now $ versionToMoment worldVersion
    let validationConfig = config ^. typed @ValidationConfig

    r <- roAppTxEx taStore storageError $ \tx -> getTA tx taStore taName'
    case r of
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


-- | Do the actual validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: (WithVContext env, Storage s) =>
                    AppContext s -> 
                    TaName -> 
                    CerObject -> 
                    NonEmpty Repository -> 
                    WorldVersion -> 
                    ValidatorT env IO TopDownResult
validateFromTACert appContext@AppContext {..} taName' taCert repos worldVersion = do  
    -- this will be used as the "now" in all subsequent time and period validations 
    let now = Now $ versionToMoment worldVersion

    let taCertURI = vContext $ toText $ NonEmpty.head $ getLocations taCert

    storedPubPoints <- roAppTxEx database storageError $ \tx -> 
                    getTaPublicationPoints tx (repositoryStore database) taName'

    let reposToFetch = map fst $ 
            -- filter the ones that are either new or need refetching
            filter (\(pp, status) -> needsFetching pp status (config ^. typed @ValidationConfig) now) $ 
            toRepoStatusPairs $ 
                -- merge repos that we want to be fetched with the ones that are stored                     
                mergeRepos repos storedPubPoints
                -- we only care about URLs from 'repos', so shrink the PPs                        
                    `shrinkTo` 
                Set.fromList (map getRpkiURL $ NonEmpty.toList repos)

    fetchStatuses <- parallelTasks 
                        (ioBottleneck appBottlenecks)
                        reposToFetch 
                        (fetchRepository appContext taCertURI now)

    case partitionFailedSuccess fetchStatuses of 
        ([], _) -> do
            let flattenedStatuses = flip map fetchStatuses $ \case 
                    FetchFailure r s _ -> (r, FailedAt s)
                    FetchSuccess r s _ -> (r, FetchedAt s)

            -- use publication points taken from the DB and updated with the 
            -- the fetchStatuses of the fetches that we just performed
            let fetchUpdatedPPs = updateStatuses storedPubPoints flattenedStatuses

            topDownContext <- newTopDownContext appContext worldVersion taName' fetchUpdatedPPs now taCert 
            -- this is for TA cert
            incValidObject topDownContext

            -- Do the tree descend, gather validation results and VRPs            
            topDownResult <- fromTry (\e -> UnspecifiedE (unTaName taName') (fmtEx e)) $
                validateCA appContext taCertURI topDownContext taCert                    

            -- get publication points from the topDownContext and save it to the database
            (pubPointAfterTopDown, Stats {..}) <- liftIO $ atomically $ 
                (,) <$> readTVar (publicationPoints topDownContext) <*>
                        readTVar (objectStats topDownContext)
            
            logDebugM logger [i|#{taName'} validCount = #{validCount} |]
            
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
            case findError $ mconcat $ map (^._3) broken of                
                Just e  -> appError e    
                -- Failed to download and no idea why        
                Nothing -> appError $ UnspecifiedE "Failed to fetch initial repositories." (convert $ show broken)


data FetchResult = 
    FetchSuccess Repository Instant Validations | 
    FetchFailure Repository Instant Validations
    deriving stock (Show, Eq, Generic)

-- | Download repository, either rsync or RRDP.
fetchRepository :: (MonadIO m, Storage s) => 
                AppContext s -> VContext -> Now -> Repository -> m FetchResult
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
                pure $ FetchFailure repo now (mError vContext' timeoutError)
            Just z -> pure z        
    where 
        repoURL   = getRpkiURL repo
        vContext' = childVC (toText repoURL) parentContext

        fetchIt = do
            logDebugM logger [i|Fetching #{repoURL} |]
            ((r, v), elapsed) <- timedMS $ runValidatorT vContext' $ 
                case repo of
                    RsyncR r -> 
                        RsyncR <$> updateObjectForRsyncRepository appContext r 
                    RrdpR r -> 
                        RrdpR <$> updateObjectForRrdpRepository appContext r
            case r of
                Left e -> do                        
                    logErrorM logger [i|Fetching repository #{getURL repoURL} failed: #{e} |]
                    pure $ FetchFailure repo now (mError vContext' e <> v)
                Right resultRepo -> do
                    logDebugM logger [i|Fetched repository #{getURL repoURL}, took #{elapsed}ms.|]
                    pure $ FetchSuccess resultRepo now v


fetchTimeout :: Config -> RpkiURL -> Seconds
fetchTimeout config (RrdpU _)  = config ^. typed @RrdpConf .  #rrdpTimeout
fetchTimeout config (RsyncU _) = config ^. typed @RsyncConf . #rsyncTimeout

type RepoTriple = (Repository, Instant, Validations)

partitionFailedSuccess :: [FetchResult] -> ([RepoTriple], [RepoTriple])
partitionFailedSuccess = go
    where
        go [] = ([], [])
        go (FetchSuccess r rs v : frs) = let (fs, ss) = go frs in (fs, (r, rs, v) : ss)
        go (FetchFailure r rs v : frs) = let (fs, ss) = go frs in ((r, rs, v) : fs, ss)


-- | Validate CA starting from its certificate.
-- 
validateCA :: Storage s =>
            AppContext s -> VContext -> TopDownContext s -> CerObject -> IO TopDownResult
validateCA appContext caVContext topDownContext certificate =
    validateCARecursively appContext caVContext topDownContext certificate
        `finally` 
    markValidatedObjects appContext topDownContext            


-- 
validateCARecursively :: Storage s => 
                        AppContext s 
                    -> VContext 
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
        Right (Triple discoveredPPs waitingList tdResult) -> do                    
            tdResults <- pickUpNewPPsAndValidateDown discoveredPPs waitingList
            pure $ mconcat tdResults <> tdResult <> fromValidations validations                
    where
        -- From the set of discovered PPs figure out which ones must be fetched, 
        -- fetch them and validate, starting from the cerfificates in their 
        -- waiting lists.        
        pickUpNewPPsAndValidateDown ppsToFetch waitingList = do            
            ppsToFetch' <- atomically $ do 
                    globalPPs           <- readTVar publicationPoints                    
                    alreadyTakenCareOf  <- readTVar takenCareOf

                    let newGlobalPPs     = adjustLastSucceeded $ globalPPs <> ppsToFetch
                    let discoveredURIs   = allURIs ppsToFetch
                    let urisToTakeCareOf = Set.difference discoveredURIs alreadyTakenCareOf

                    writeTVar publicationPoints newGlobalPPs                                        
                    modifyTVar' takenCareOf (<> discoveredURIs)
                            
                    pure $ newGlobalPPs `shrinkTo` urisToTakeCareOf

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
                                []                -> vc
                                Triple _ vc' _ : _ -> vc'

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
                    pure $ mconcat tdResults <> fromValidations validations

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
                waitingList $ \(Triple hash certVContext verifiedResources') -> do                    
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
                ValidatorT VContext IO (Triple PublicationPoints WaitingList TopDownResult)
validateCaCertificate appContext@AppContext {..} topDownContext certificate = do          
    globalPPs <- liftIO $ readTVarIO (topDownContext ^. #publicationPoints)

    let validationConfig = appContext ^. typed @Config . typed

    case publicationPointsFromCertObject certificate of
        Left e                    -> appError $ ValidationE e
        Right (url, discoveredPP) -> do
            let asIfItIsMerged = discoveredPP `mergePP` globalPPs

            let stopDescend = do 
                    -- remember to come back to this certificate when the PP is fetched
                    vContext' <- asks getVC                    
                    pure $! Triple 
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

        validateThisCertAndGoDown :: ValidatorT VContext IO (Triple PublicationPoints WaitingList TopDownResult)
        validateThisCertAndGoDown = do            
            let (childrenAki, certLocations') = (toAKI $ getSKI certificate, getLocations certificate)        

            -- this for the certificate
            incValidObject topDownContext
            visitObject appContext topDownContext (CerRO certificate)

            mft <- findMft childrenAki certLocations'
            checkMftLocation mft certificate            
                    
            manifestResult <- forChild (toText $ NonEmpty.head $ getLocations mft) $ do
                -- find CRL on the manifest
                (_, crlHash) <- case findCrlOnMft mft of 
                    []    -> vError $ NoCRLOnMFT childrenAki certLocations'
                    [crl] -> pure crl
                    crls  -> vError $ MoreThanOneCRLOnMFT childrenAki certLocations' crls

                let objectStore' = objectStore database
                crlObject <- roAppTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
                case crlObject of 
                    Nothing          -> vError $ NoCRLExists childrenAki certLocations'    
                    Just (CrlRO crl) -> do      
                        -- validate CRL and MFT together
                        validCrl <- forChild (toText $ NonEmpty.head $ getLocations crl) $ do
                            vHoist $ do          
                                crl' <- validateCrl (now topDownContext) crl certificate
                                void $ validateMft (now topDownContext) mft certificate crl'
                                pure crl'                                        

                        -- this for the CRL
                        incValidObject topDownContext          
                        visitObject appContext topDownContext (CrlRO crl)

                        let childrenHashes = filter ((/= getHash crl) . snd) $ -- filter out CRL itself
                                                mftEntries $ getCMSContent $ extract mft                                                
                    
                        -- Mark all manifest entries as visited to avoid the situation
                        -- when some of the objects are deleted from the cache and some
                        -- are still there. Do it both in case of successful validation
                        -- or a validation error.
                        let markAllEntriesAsVisited = 
                                visitObjects topDownContext $ map snd childrenHashes
                        
                        let processChildren = do 
                                r <- parallelTasks 
                                    (cpuBottleneck appBottlenecks) 
                                    childrenHashes $ \(filename, hash') -> 
                                        validateManifestEntry filename hash' validCrl
                                markAllEntriesAsVisited
                                pure r
                        
                        childrenResults <- processChildren `catchError` 
                                        (\e -> markAllEntriesAsVisited >> throwError e)

                        -- Combine PPs and their waiting lists. On top of it, fix the 
                        -- last successfull validation times for PPs based on their fetch statuses.
                        let Triple pps waitingList vrps = mconcat childrenResults
                        pure $! Triple (adjustLastSucceeded pps) waitingList vrps

                    Just _  -> vError $ CRLHashPointsToAnotherObject crlHash certLocations'   

            -- this for the valid manifest
            incValidObject topDownContext

            pure manifestResult

        --
        -- | Validate an entry of the manifest, i.e. a pair of filename and hash
        -- 
        validateManifestEntry filename hash' validCrl = do                    
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

        -- 
        validateChild :: Validated CrlObject -> 
                        RpkiObject -> 
                        ValidatorT VContext IO (Triple PublicationPoints WaitingList TopDownResult)
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
            parentContext :: VContext <- asks getVC
            case ro of
                CerRO childCert -> do 
                    let TopDownContext{..} = topDownContext
                    (r, validations) <- liftIO $ runValidatorT parentContext $                     
                            forChild (toText $ NonEmpty.head $ getLocations ro) $ do
                                childVerifiedResources <- vHoist $ do                 
                                        Validated validCert <- validateResourceCert now childCert certificate validCrl
                                        validateResources verifiedResources childCert validCert 
                                let childTopDownContext = topDownContext { verifiedResources = Just childVerifiedResources }
                                validateCaCertificate appContext childTopDownContext childCert                            
        
                    pure $! case r of
                        Left _                         -> Triple emptyPublicationPoints mempty (fromValidations validations)
                        Right (Triple pps wl tdResult) -> Triple pps wl (tdResult <> fromValidations validations)

                RoaRO roa ->
                    forChild (toText $ NonEmpty.head $ getLocations ro) $ do
                        void $ vHoist $ validateRoa (now topDownContext) roa certificate validCrl
                                            
                        incValidObject topDownContext
                            -- logDebugM logger [i|#{getLocations roa}, VRPs: #{getCMSContent (extract roa :: CMS [Vrp])}|]
                        let vrps = getCMSContent (extract roa :: CMS [Vrp])
                        pure $! Triple emptyPublicationPoints mempty (TopDownResult vrps mempty)

                GbrRO gbr -> withEmptyPPs $
                    forChild (toText $ NonEmpty.head $ getLocations ro) $ do
                        void $ vHoist $ validateGbr (now topDownContext) gbr certificate validCrl                    
                        incValidObject topDownContext                    

                -- TODO Anything else?
                _ -> withEmptyPPs $ pure ()

            where                
                withEmptyPPs f = f >> (pure $! Triple emptyPublicationPoints mempty mempty)
        

        findMft childrenAki locations = do
            mft' <- liftIO $ roTx (objectStore database) $ \tx -> 
                findLatestMftByAKI tx (objectStore database) childrenAki
            case mft' of
                Nothing  -> vError $ NoMFT childrenAki locations
                Just mft -> do
                    visitObject appContext topDownContext mft
                    pure mft

        -- TODO Is there a more reliable way to find it? Compare it with SIA?
        findCrlOnMft mft = filter (\(name, _) -> ".crl" `Text.isSuffixOf` name) $ 
            mftEntries $ getCMSContent $ extract mft

        -- Check that manifest URL in the certificate is the same as the one 
        -- the manifest was actually fetched from.
        checkMftLocation mft certficate = do
            case getManifestUri $ cwsX509certificate $ getCertWithSignature certficate of
                Nothing     -> vError $ NoMFTSIA $ getLocations certficate
                Just mftSIA -> 
                    let 
                        mftLocations = getLocations mft
                        in case NonEmpty.filter ((mftSIA ==) . getURL) mftLocations of 
                            [] -> vWarn $ MFTOnDifferentLocation mftSIA mftLocations
                            _ ->  pure ()


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
-- we read it from the database and looked at it.
visitObject :: (MonadIO m, WithHash ro, WithLocations ro, Storage s) => 
                AppContext s -> TopDownContext s -> ro -> m ()
visitObject _ topDownContext ro = 
    visitObjects topDownContext [getHash ro]    

visitObjects :: MonadIO m => TopDownContext s -> [Hash] -> m ()
visitObjects TopDownContext {..} hashes =
    liftIO $ atomically $ modifyTVar' visitedHashes (<> Set.fromList hashes)



-- | Fetch TA certificate based on TAL location(s)
fetchTACertificate :: WithVContext vc => 
                    AppContext s -> TAL -> ValidatorT vc IO (RpkiURL, RpkiObject)
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

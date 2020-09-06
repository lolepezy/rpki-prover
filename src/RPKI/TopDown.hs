{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}


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

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RRDP.RrpdFetch
import           RPKI.RRDP.Http
import           RPKI.Parse.Parse
import           RPKI.Rsync
import           RPKI.Store.Base.Storage
import           RPKI.Store.Data
import           RPKI.Store.Repository
import           RPKI.Store.Database
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                        (fmtEx)
import           RPKI.Version
import           RPKI.Validation.ObjectValidation

import Data.Hourglass
import System.Timeout (timeout)


data Stats = Stats {
    validCount :: Int
}

newtype WaitingList =  WaitingList { unWList :: 
        (Map RpkiURL (Set (Hash, VContext, Maybe (VerifiedRS PrefixesAndAsns))))
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype Monoid

instance Semigroup WaitingList where
    (WaitingList w1) <> (WaitingList w2) = WaitingList $ Map.unionWith (<>) w1 w2

toWaitingList :: RpkiURL -> Hash -> VContext -> Maybe (VerifiedRS PrefixesAndAsns) -> WaitingList
toWaitingList rpkiUrl hash vc resources =     
    WaitingList $ Map.singleton rpkiUrl (Set.singleton (hash, vc, resources))

-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext s = TopDownContext {    
    verifiedResources           :: Maybe (VerifiedRS PrefixesAndAsns),

    -- Element of the queue used to asynchronously write discovered VRPs and 
    -- validation results (and potentially anything else) to the database.
    databaseQueue               :: ClosableQueue (Tx s 'RW -> IO ()),
    publicationPoints           :: TVar PublicationPoints,
    takenCareOf                 :: TVar (Set RpkiURL),
    taName                      :: TaName, 
    now                         :: Now,    
    objectStats                 :: TVar Stats,
    worldVersion                :: WorldVersion,
    visitedHashes               :: TVar (Set Hash)
} deriving stock (Generic)

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
        newCQueue 20000 <*>
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
            AppContext s -> TAL -> WorldVersion -> IO (Either AppError ())
validateTA appContext@AppContext {..} tal worldVersion = do    
    (z, validations) <- runValidatorT taContext $
            forChild (toText $ getTaCertURL tal) $ do
                ((taCert, repos, _), elapsed) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                logDebugM logger [i|Fetched and validated TA certficate #{certLocations tal}, took #{elapsed}ms.|]        
                validateFromTACert appContext (getTaName tal) taCert repos worldVersion

    mapException (AppException . storageError) <$> 
        writeVResult appContext validations worldVersion    
    pure z
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
                    ValidatorT env IO ()
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
            fromTry (UnspecifiedE . fmtEx) $
                validateCA appContext taCertURI topDownContext taCert                    

            -- get publication points from the topDownContext and save it to the database
            (pubPointAfterTopDown, Stats {..}) <- liftIO $ atomically $ 
                (,) <$> readTVar (publicationPoints topDownContext) <*>
                        readTVar (objectStats topDownContext)
            
            logDebugM logger [i|#{taName'} validCount = #{validCount} |]

            let changeSet' = changeSet storedPubPoints pubPointAfterTopDown
            rwAppTxEx database storageError $ \tx -> 
                applyChangeSet tx (repositoryStore database) changeSet' taName'

        (broken, _) -> do
            let brokenUrls = map (getRpkiURL . (^. _1)) broken
            logErrorM logger [i|Will not proceed, repositories '#{brokenUrls}' failed to download.|]

     

data FetchResult = 
    FetchSuccess !Repository !Instant !Validations | 
    FetchFailure !Repository !Instant !Validations
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
                RrdpU _ -> (config ^. typed @RrdpConf . #rrdpTimeout, RrdpE RrdpDownloadTimeout)
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
                        first RsyncR <$> updateObjectForRsyncRepository appContext r 
                    RrdpR r -> 
                        first RrdpR <$> updateObjectForRrdpRepository appContext r
            case r of
                Left e -> do                        
                    logErrorM logger [i|Fetching repository #{getURL repoURL} failed: #{e} |]
                    pure $ FetchFailure repo now (mError vContext' e <> v)
                Right (resultRepo, vs) -> do
                    logDebugM logger [i|Fetched repository #{getURL repoURL}, took #{elapsed}ms.|]
                    pure $ FetchSuccess resultRepo now (vs <> v)


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
            AppContext s -> VContext -> TopDownContext s -> CerObject -> IO ()
validateCA appContext vContext' topDownContext certificate = do        
    validateCAWithQueue appContext vContext' topDownContext certificate CreateQ


data QWhat = CreateQ | AlreadyCreatedQ

-- TODO Write a good explanation why do we use queues.
-- 
validateCAWithQueue :: Storage s => 
                        AppContext s -> 
                        VContext -> 
                        TopDownContext s -> 
                        CerObject -> 
                        QWhat -> IO ()
validateCAWithQueue 
        appContext@AppContext {..} 
        vc 
        topDownContext@TopDownContext{..} 
        certificate qWhat = do     

    -- logInfoM logger [i|Starting to validate #{getLocations certificate}.|]    

    let treeDescend = do 
            (r, validations) <- runValidatorT vc $ validateCaCertificate appContext topDownContext certificate
            queueVResult appContext topDownContext validations            
            case r of
                Left _alreadyQueued -> pure ()
                Right (discoveredPPs, waitingList, vrps) -> do
                    queueVRP appContext topDownContext (Set.toList vrps)
                    pickUpNewPPsAndValidateDown discoveredPPs waitingList

    (r, elapsed) <- timedMS $ case qWhat of 
            CreateQ -> do            
                -- Write validation results in a separate thread to avoid blocking on the 
                -- database with writing transactions during the validation process                     
                fst <$> concurrently 
                            (treeDescend `finally` atomically (closeQueue databaseQueue))
                            (executeQueuedTxs appContext topDownContext 
                                `finally` 
                                markValidatedObjects appContext topDownContext)
            
            AlreadyCreatedQ -> treeDescend

    -- logDebugM logger [i|Validated #{getLocations certificate}, took #{elapsed}ms.|]    
    pure r
            
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
            void $ parallelTasks 
                (ioBottleneck appBottlenecks) 
                (Map.keys rootToPps) $ \repo -> do
                    validations <- fetchAndValidateWaitingList rootToPps repo waitingList
                    queueVResult appContext topDownContext validations

        -- Fetch the PP and validate all the certificates from the waiting 
        -- list of this PP.
        fetchAndValidateWaitingList rootToPps repo (WaitingList waitingList) = do

            -- Gather the PPs and hashes of the objects from their waiting lists
            let ppsForTheRepo = fromMaybe Set.empty $ Map.lookup repo rootToPps            
            let waitingListForThesePPs = Set.toList $ fromMaybe Set.empty $ fold $ 
                    Set.map (\pp -> getRpkiURL pp `Map.lookup` waitingList) ppsForTheRepo
            
            -- try to recover the validation context
            let vContext' = case waitingListForThesePPs of
                                []              -> vc
                                (_, vc', _) : _ -> vc'

            fetchResult <- fetchRepository appContext vContext' now repo                                            

            pps <- atomically $ do 
                    let statusUpdate = case fetchResult of
                            FetchFailure r t _ -> (r, FailedAt t)
                            FetchSuccess r t _ -> (r, FetchedAt t)  
                    modifyTVar' publicationPoints $ \pubPoints -> updateStatuses pubPoints [statusUpdate]
                    readTVar publicationPoints

            let 
                proceedWithValidation validations = 
                        validateWaitingList waitingListForThesePPs >> pure validations                
                noFurtherValidation  = pure
                Now now' = now
                in case fetchResult of
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
        validateWaitingList waitingList = do 
            void $ parallelTasks 
                    (cpuBottleneck appBottlenecks) 
                    waitingList $ \(hash, certVContext, verifiedResources') -> do                    
                        o <- roTx database $ \tx -> getByHash tx (objectStore database) hash
                        case o of 
                            Just (CerRO waitingCertificate) -> do
                                -- logInfoM logger [i|From waiting list of #{getRpkiURL repo}: #{getLocations waitingCertificate}.|]
                                let childTopDownContext = topDownContext { 
                                        -- we should start from the resource set of this certificate
                                        -- as it is already has been verified
                                        verifiedResources = verifiedResources'                                                
                                    }
                                validateCAWithQueue appContext certVContext 
                                        childTopDownContext waitingCertificate AlreadyCreatedQ
                            ro ->
                                logErrorM logger
                                    [i| Something is really wrong with the hash #{hash} in waiting list, got #{ro}|]            
        
    

-- | Do top-down validation starting from the given certificate
-- Returns the discovered publication points that are not registered 
-- in the top-down context yet.
validateCaCertificate :: Storage s =>
                AppContext s ->
                TopDownContext s ->
                CerObject ->                
                ValidatorT VContext IO (PublicationPoints, WaitingList, Set Roa)
validateCaCertificate appContext@AppContext {..} topDownContext certificate = do          
    globalPPs <- liftIO $ readTVarIO (topDownContext ^. #publicationPoints)

    let validationConfig = appContext ^. typed @Config . typed

    case publicationPointsFromCertObject certificate of
        Left e                    -> appError $ ValidationE e
        Right (url, discoveredPP) -> do
            let asIfItIsMerged = discoveredPP `mergePP` globalPPs

            let stopDescend = do 
                    -- remember to come back to this certificate when the PP is fetched
                    -- logDebugM logger [i|to waiting list: #{getRpkiURL discoveredPP} + #{getHash certificate}.|]
                    vContext' <- asks getVC                    
                    pure (asIfItIsMerged `shrinkTo` (Set.singleton url), 
                            toWaitingList
                                (getRpkiURL discoveredPP) 
                                (getHash certificate) 
                                vContext' 
                                (verifiedResources topDownContext), mempty)

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

        validateThisCertAndGoDown :: ValidatorT VContext IO (PublicationPoints, WaitingList, Set Roa)
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
                        let (pps, waitingList, vrps) = mconcat childrenResults
                        pure (adjustLastSucceeded pps, waitingList, vrps)

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
                        ValidatorT VContext IO (PublicationPoints, WaitingList, Set Roa)
        validateChild validCrl ro = do
            -- At the moment of writing RFC 6486-bis 
            -- (https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-00#page-12) 
            -- prescribes to consider the manifest invalid if any of the objects 
            -- referred by the manifest is invalid. 
            -- 
            -- That's why recursive validation of the child CA happens in the separate   
            -- runValidatorT (...) call, but all the other objects are supposed to be 
            -- validated withing the same context of ValidatorT, i.e. have short-circuit
            -- logic implemented by ExceptT.
            parentContext :: VContext <- asks getVC
            case ro of
                CerRO childCert -> do 
                    let TopDownContext{..} = topDownContext
                    (r, validations) <- liftIO $ runValidatorT parentContext $ do                    
                            forChild (toText $ NonEmpty.head $ getLocations ro) $ do
                                childVerifiedResources <- vHoist $ do                 
                                        Validated validCert <- validateResourceCert now childCert certificate validCrl
                                        validateResources verifiedResources childCert validCert 
                                let childTopDownContext = topDownContext { verifiedResources = Just childVerifiedResources }
                                validateCaCertificate appContext childTopDownContext childCert                            
                                        
                    queueVResult appContext topDownContext validations                    
                    case r of
                        Left _               -> pure $ (emptyPublicationPoints, mempty, mempty)
                        Right z@(_, _, vrps) -> do 
                            queueVRP appContext topDownContext (Set.toList vrps)
                            pure z

                RoaRO roa ->
                    forChild (toText $ NonEmpty.head $ getLocations ro) $ do
                        void $ vHoist $ validateRoa (now topDownContext) roa certificate validCrl
                                            
                        incValidObject topDownContext
                            -- logDebugM logger [i|#{getLocations roa}, VRPs: #{getCMSContent (extract roa :: CMS [Roa])}|]
                        let vrps = getCMSContent (extract roa :: CMS [Roa])
                        pure (emptyPublicationPoints, mempty, Set.fromList vrps)                        

                GbrRO gbr -> withEmptyPPs $
                    forChild (toText $ NonEmpty.head $ getLocations ro) $ do
                        void $ vHoist $ validateGbr (now topDownContext) gbr certificate validCrl                    
                        incValidObject topDownContext                    

                -- TODO Anything else?
                _ -> withEmptyPPs $ pure ()

            where                
                withEmptyPPs f = f >> pure (emptyPublicationPoints, mempty, mempty)
        

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


queueVRP :: (MonadIO m, Storage s) =>
            AppContext s -> TopDownContext s -> [Roa] -> m ()
queueVRP AppContext { database = DB {..}, .. } TopDownContext {..} roas =    
    liftIO $ for_ roas $ \vrp -> 
        atomically $ writeCQueue databaseQueue $ \tx -> 
            putVRP tx vrpStore worldVersion vrp 


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


-- | Put validation result into a queue for writing
queueVResult :: (MonadIO m, Storage s) => 
                AppContext s -> TopDownContext s -> Validations -> m ()
queueVResult AppContext { database = DB {..} } TopDownContext {..} validations = liftIO $ 
    case validations of
        Validations validationsMap
            | emptyValidations validations -> pure ()
            | otherwise -> do
                void $ flip Map.traverseWithKey validationsMap $ 
                        \vc' problems -> 
                            let vResult = VResult (Set.toList problems) vc'   
                            in atomically $ writeCQueue databaseQueue $ 
                                    \tx -> putVResult tx resultStore worldVersion vResult

-- Write validation result synchrtonously
writeVResult :: (MonadIO m, Storage s) => 
                AppContext s -> Validations -> WorldVersion -> m ()
writeVResult AppContext { database = DB {..} } validations worldVersion = liftIO $ do
    case validations of
        Validations validationsMap
            | emptyValidations validations -> pure ()
            | otherwise -> do
                rwTx resultStore $ \tx -> 
                    void $ flip Map.traverseWithKey validationsMap $ 
                        \vc' problems -> do
                            let vResult = VResult (Set.toList problems) vc'   
                            putVResult tx resultStore worldVersion vResult                


-- Execute writing transactions from the queue
executeQueuedTxs :: Storage s => 
            AppContext s -> TopDownContext s -> IO ()
executeQueuedTxs AppContext {..} TopDownContext {..} = do
    -- read element in chunks to make transactions not too frequent
    readQueueChunked databaseQueue 2_000 $ \quuElems -> do
        rwTx database $ \tx -> 
            for_ quuElems $ \f -> f tx

completeWorldVersion :: Storage s => 
                        AppContext s -> WorldVersion -> IO ()
completeWorldVersion AppContext { database = DB {..} } worldVersion =
    rwTx versionStore $ \tx -> 
        putVersion tx versionStore worldVersion FinishedVersion



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



-- Utilities to have storage transaction in ValidatorT monad.
roAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (Tx s 'RO -> ValidatorT env IO a) -> ValidatorT env IO a 
roAppTx s f = appTx s f roTx    

rwAppTx :: (Storage s, WithStorage s ws) => 
            ws -> (forall mode . Tx s mode -> ValidatorT env IO a) -> ValidatorT env IO a
rwAppTx s f = appTx s f rwTx

appTx :: (Storage s, WithStorage s ws) => 
        ws -> (Tx s mode -> ValidatorT env IO a) -> 
        (ws -> (Tx s mode -> IO (Either AppError a, Validations))
            -> IO (Either AppError a, Validations)) -> 
        ValidatorT env IO a
appTx s f txF = do
    env <- ask
    validatorT $ txF s $ runValidatorT env . f

roAppTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> 
            (exc -> AppError) -> 
            (Tx s 'RO -> ValidatorT env IO a) -> 
            ValidatorT env IO a 
roAppTxEx ws err f = appTxEx ws err f roTx    

rwAppTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> (exc -> AppError) -> 
            (Tx s 'RW -> ValidatorT env IO a) -> ValidatorT env IO a
rwAppTxEx s err f = appTxEx s err f rwTx

appTxEx :: (Storage s, WithStorage s ws, Exception exc) => 
            ws -> (exc -> AppError) -> 
            (Tx s mode -> ValidatorT env IO a) -> 
            (s -> (Tx s mode -> IO (Either AppError a, Validations))
               -> IO (Either AppError a, Validations)) -> 
            ValidatorT env IO a
appTxEx ws err f txF = do
    env <- ask
    -- TODO Make it less ugly and complicated
    t <- liftIO $ try $ txF (storage ws) $ runValidatorT env . f
    validatorT $ pure $ case t of 
                            Left e  -> (Left (err e), mempty)
                            Right r -> r            
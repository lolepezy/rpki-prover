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
import           Control.Concurrent.Async (forConcurrently)
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)


import           Data.Either                      (fromRight)
import           Data.Foldable
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set.NonEmpty                as NESet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid.Generic
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch
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
-- import           RPKI.Fetch
import           RPKI.Metrics

import           Data.Hourglass
import           System.Timeout                   (timeout)


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


-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext s = TopDownContext {    
        verifiedResources :: Maybe (VerifiedRS PrefixesAndAsns),    
        taName            :: TaName, 
        repositoryContext :: TVar RepositoryContext, 
        fetchTasks        :: FetchTasks,
        now               :: Now,
        worldVersion      :: WorldVersion,
        visitedHashes     :: TVar (Set Hash),
        validManifests    :: TVar (Map AKI Hash)
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
                    -> FetchTasks
                    -> TVar RepositoryContext
                    -> Now 
                    -> CerObject 
                    -> m (TopDownContext s)
newTopDownContext worldVersion taName fetchTasks repositoryContext now certificate = 
    liftIO $ atomically $ do    
        let verifiedResources = Just $ createVerifiedResources certificate        
        visitedHashes     <- newTVar mempty
        validManifests    <- newTVar mempty        
        pure $ TopDownContext {..}

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
    storedPubPoints <- roTx database' $ \tx -> getPublicationPoints tx (repositoryStore database')    

    fetchTasks        <- newFetchTasksIO      
    repositoryContext <- newTVarIO $ newRepositoryContext storedPubPoints
    rs <- forConcurrently tals $ \tal -> do           
        ((r@TopDownResult{..}, rs), elapsed) <- timedMS $ 
                validateTA appContext tal worldVersion fetchTasks repositoryContext
        logInfo_ logger [i|Validated #{getTaName tal}, got #{length vrps} VRPs, took #{elapsed}ms|]
        pure (r, rs)

    mapException (AppException . storageError) $ do
        rwTx database' $ \tx -> do                             
            let pubPointAfterTopDown = mconcat $ map ((^. typed @PublicationPoints) . snd) rs
            savePublicationPoints tx (repositoryStore database') pubPointAfterTopDown                        

    pure $ map fst rs


-- | It is the main entry point for the top-down validation. 
-- Validates TA starting from its TAL.
--
validateTA :: Storage s => 
            AppContext s 
            -> TAL 
            -> WorldVersion 
            -> FetchTasks
            -> TVar RepositoryContext
            -> IO (TopDownResult, RepositoryContext)
validateTA appContext@AppContext {..} tal worldVersion fetchTasks repositoryContext = do    
    r <- runValidatorT taContext $
            timedMetric (Proxy :: Proxy ValidationMetric) $ do 
                ((taCert, repos, _), elapsed) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                logDebugM logger [i|Fetched and validated TA certficate #{certLocations tal}, took #{elapsed}ms.|]        

                -- this will be used as the "now" in all subsequent time and period validations 
                let now = Now $ versionToMoment worldVersion
                topDownContext <- newTopDownContext worldVersion 
                                (getTaName tal) 
                                fetchTasks 
                                repositoryContext
                                now  (taCert ^. #payload)                
                vrps <- validateFromTACert appContext topDownContext repos taCert
                setVrpNumber $ Count $ fromIntegral $ Set.size vrps
                rs <- liftIO $ readTVarIO $ topDownContext ^. #repositoryContext
                pure (vrps, rs)

    case r of 
        (Left e, vs) -> 
            pure (TopDownResult mempty vs, mempty)
        (Right (vrps, rs), vs) ->            
            pure (TopDownResult vrps vs, rs)
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
                                -> ValidatorT IO (Located CerObject, NonEmpty Repository, TACertStatus)
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
                pure (locatedTaCert (getTaCertURL tal) taCert, initialRepositories, Existing)
    where
        fetchValidateAndStore taStore (Now moment) = do 
            (uri', ro) <- fetchTACertificate appContext tal
            cert       <- vHoist $ validateTACert tal uri' ro
            rwAppTxEx taStore storageError $ \tx ->            
                case createRepositoriesFromTAL tal cert of
                    Left e      -> appError $ ValidationE e
                    Right repos -> do 
                        putTA tx taStore (StorableTA tal cert (FetchedAt moment) repos)
                        pure (locatedTaCert uri' cert, repos, Updated)

        taName' = getTaName tal          
        locatedTaCert url cert = Located (toLocations url) cert


-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: Storage s =>
                    AppContext s -> 
                    TopDownContext s ->                                        
                    NonEmpty Repository -> 
                    Located CerObject ->                     
                    ValidatorT IO (Set Vrp)
validateFromTACert 
    appContext@AppContext {..} 
    topDownContext@TopDownContext { .. } 
    initialRepos 
    taCert 
    = do  
    
    let taURIContext = newValidatorPath $ locationsToText $ taCert ^. #locations

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
                    (fetchRepository appContext fetchTasks taURIContext now)
            -- else forM
            --         reposToFetch 
            --         (fetchRepository appContext taURIContext now)

    case partitionFailedSuccess fetchStatuses of
        ([], _) -> do            
            -- Embed valdiation state accumulated from 
            -- the fetching into the current validation state.
            embedState (fetchStatuses2ValidationState fetchStatuses)

            let flattenedStatuses = flip map fetchStatuses $ \case 
                    FetchFailure r _ -> (r, FailedAt $ unNow now)
                    FetchSuccess r _ -> (r, FetchedAt $ unNow now)

            -- use publication points taken from the DB and updated with the 
            -- the fetchStatuses of the fetches that we just performed
            let fetchUpdatedPPs = updateStatuses storedPubPoints flattenedStatuses

            -- merge the publication points that we have just fetched in the global context.
            liftIO $ atomically $ modifyTVar' repositoryContext 
                                    (& typed @PublicationPoints %~ (<> fetchUpdatedPPs))            

            -- Do the tree descend, gather validation results and VRPs            
            T2 vrps validationState <- fromTry (\e -> UnspecifiedE (unTaName taName) (fmtEx e)) $
                validateCA appContext taURIContext topDownContext taCert                    

            embedState validationState
            
            pure vrps

        (broken, _) -> do
            let brokenUrls = map (getRpkiURL . (^. _1)) broken
            logErrorM logger [i|Will not proceed, repositories '#{brokenUrls}' failed to download.|]
            case findError $ mconcat $ map (^. _2 . typed) broken of                
                Just e  -> appError e    
                -- Failed to download and no idea why        
                Nothing -> appError $ UnspecifiedE "Failed to fetch initial repositories." (convert $ show broken)


partitionFailedSuccess :: [FetchResult] -> 
                        ([(Repository, ValidationState)], 
                         [(Repository, ValidationState)])
partitionFailedSuccess = go
    where
        go [] = ([], [])
        go (FetchSuccess r v : frs) = let (fs, ss) = go frs in (fs, (r, v) : ss)
        go (FetchFailure r v : frs) = let (fs, ss) = go frs in ((r, v) : fs, ss)

fetchStatuses2ValidationState :: [FetchResult] -> ValidationState
fetchStatuses2ValidationState r = mconcat $ flip map r $ \case 
    FetchSuccess _ v -> v
    FetchFailure _ v -> v  
                

-- | Validate CA starting from its certificate.
-- 
validateCA :: Storage s =>
            AppContext s 
            -> ValidatorPath 
            -> TopDownContext s 
            -> Located CerObject 
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
                    -> Located CerObject 
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
            pure $ T2 mempty validations
        Right (T3 discoveredPPs waitingList vrps) -> do             
            results <- extractPPsAndValidateDown discoveredPPs waitingList
            pure $ mconcat results <> T2 vrps validations         

    where
        -- From the set of discovered PPs figure out which ones must be fetched, 
        -- fetch them and validate, starting from the cerfificates in their 
        -- waiting lists.        
        extractPPsAndValidateDown discoveredPPs waitingList = do            
            ppsToFetch' <- atomically $ getPPsToFetch repositoryContext discoveredPPs
            let (_, rootToPps) = repositoryHierarchy ppsToFetch'
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

            fetchResult <- liftIO $ fetchRepository appContext fetchTasks waitingPath now repo            

            let statusUpdate = case fetchResult of
                            FetchFailure r _ -> (r, FailedAt $ unNow now)
                            FetchSuccess r _ -> (r, FetchedAt $ unNow now)                      
            
            pps <- liftIO $ atomically $ do  
                        r <- readTVar repositoryContext
                        let pps' = updateStatuses (r ^. #publicationPoints) [statusUpdate]     
                        writeTVar repositoryContext $ r { publicationPoints = pps' }
                        pure pps'
    
            let proceedWithValidation validations = do 
                    z <- mconcat <$> validateWaitingList waitingListForThesePPs                    
                    pure $ z & typed %~ (<> validations)

            let noFurtherValidation validations = 
                    pure $ mempty & typed %~ (<> validations)

            case fetchResult of
                FetchSuccess _ validations ->                    
                    proceedWithValidation validations

                FetchFailure r validations -> 
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
            inParallel
                (cpuBottleneck appBottlenecks <> ioBottleneck appBottlenecks)
                waitingList $ \(T3 hash certVContext verifiedResources') -> do
                    o <- roTx objectStore $ \tx -> getByHash tx objectStore hash
                    case o of 
                        Just c@(Located loc (CerRO waitingCertificate)) -> do
                            let childTopDownContext = topDownContext { 
                                    -- we should start from the resource set of this certificate
                                    -- as it is already has been verified
                                    verifiedResources = verifiedResources'                                                
                                }                            
                            validateCARecursively appContext certVContext 
                                childTopDownContext (Located loc waitingCertificate)
                        ro -> do
                            logError_ logger [i| Something is really wrong with the hash #{hash} in waiting list, got #{ro}|]
                            pure mempty
        
    

-- | Do top-down validation starting from the given certificate
-- Returns the discovered publication points that are not registered 
-- in the top-down context yet.
validateCaCertificate :: Storage s =>
                        AppContext s ->
                        TopDownContext s ->
                        Located CerObject ->                
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
        case publicationPointsFromCertObject (certificate ^. #payload) of
            Left e                    -> appError $ ValidationE e
            Right (url, discoveredPP) -> do
                case discoveredPP of 
                    RsyncPP rsync -> 
                        logDebugM logger [i|rsync = #{rsync}.|]      
                    _ -> pure ()

                let asIfItIsMerged = discoveredPP `mergePP` globalPPs
                let stopDescend = do 
                        -- remember to come back to this certificate when the PP is fetched
                        vContext' <- ask
                        pure $ T3 
                                (asIfItIsMerged `shrinkTo` Set.singleton url) 
                                (toWaitingList
                                    (getRpkiURL discoveredPP) 
                                    (getHash $ certificate ^. #payload) 
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
    -- 
    --  1) get the latest manifest
    --  2) find CRL on it
    --  3) make sure they both are valid
    --  4) go through the manifest children and either 
    --     + validate them as signed objects
    --     + or valdiate them recursively as CAcertificates
    -- 
    -- If anything falled, try to fetch previously latest cached 
    -- valid manifest and repeat (2) - (4) for it.

    -- Everything else is either extra checks or metrics.
    -- 
    validateThisCertAndGoDown = do            
        let (childrenAki, certLocations') = (toAKI $ getSKI certificate, getLocations certificate)        
        
        validateObjectLocations certificate

        oneMoreCert            
        visitObject appContext topDownContext (CerRO $ certificate ^. #payload)
                

        let tryManifest mft =
                validateManifestAndItsChildren mft childrenAki certLocations'
                    `finallyError`
                    -- manifest should be marked as visited regardless of its validitity
                    visitObject appContext topDownContext mft   

        let tryLatestValidCachedManifest latestMft e = do
                -- this "fetch" has failed so we are falling back to a latest valid 
                -- cached manifest for this CA               
                -- https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-03#section-6.7
                --
                findCachedLatestValidMft childrenAki >>= \case
                    Nothing             -> do 
                        -- logDebugM logger [i|00000, e = #{e}.|]        
                        throwError e
                    Just latestValidMft ->                         
                        case latestMft of 
                            Nothing -> do 
                                appWarn e
                                -- logDebugM logger [i|1111, e = #{e}.|]        
                                tryManifest latestValidMft                                
                            Just latestMft'
                                | getHash latestMft' == getHash latestValidMft 
                                    -- it doesn't make sense to try the same manifest again
                                    -- just re-trow the error
                                    -> do 
                                        -- logDebugM logger [i|2222, e = #{e}.|]
                                        throwError e
                                | otherwise -> do 
                                    appWarn e
                                    -- logDebugM logger [i|33333, e = #{e}.|]
                                    tryManifest latestValidMft
                                    

        -- first try to use the latest manifest 
        -- https://tools.ietf.org/html/draft-ietf-sidrops-6486bis-03#section-6.2                                     
        findLatestMft childrenAki >>= \case                        
            Nothing -> 
                -- Use awkward appError + catchError to force the error to 
                -- get into the ValidationResult in the state.
                vError (NoMFT childrenAki certLocations')
                    `catchError`
                    tryLatestValidCachedManifest Nothing 
                
            Just mft -> 
                tryManifest mft 
                    `catchError` 
                    tryLatestValidCachedManifest (Just mft)

      where                       

        validateManifestAndItsChildren locatedMft childrenAki certLocations' = do                         
            let mft = locatedMft ^. #payload

            validateObjectLocations locatedMft

            visitedObjects <- liftIO $ readTVarIO visitedHashes            
            when (getHash mft `Set.member` visitedObjects) $                 
                -- We have already visited this manifest before, so 
                -- there're some circular references in the objects.
                -- 
                -- NOTE: We are limiting cycle detection only to manfests
                -- to minimise the false positives where the same object
                -- is referenced from multiple manifests and we are treating 
                -- it as a cycle.
                vError $ CircularReference (getHash mft) (locatedMft ^. #locations)

            validateMftLocation locatedMft certificate

            manifestResult <- inSubVPath (locationsToText $ getLocations locatedMft) $ do
                T2 _ crlHash <- 
                    case findCrlOnMft mft of 
                        []    -> vError $ NoCRLOnMFT childrenAki certLocations'
                        [crl] -> pure crl
                        crls  -> vError $ MoreThanOneCRLOnMFT childrenAki certLocations' crls

                objectStore' <- (^. #objectStore) <$> liftIO (readTVarIO database)
                crlObject <- liftIO $ roTx objectStore' $ \tx -> getByHash tx objectStore' crlHash
                case crlObject of 
                    Nothing -> 
                        vError $ NoCRLExists childrenAki certLocations'    

                    Just foundCrl@(Located crlLocations (CrlRO crl)) -> do      
                        visitObject appContext topDownContext foundCrl                        
                        validateObjectLocations foundCrl
                        validCrl <- inSubVPath (locationsToText crlLocations) $ 
                                        vHoist $ do        
                                            let mftEECert = getEECert $ unCMS $ cmsPayload mft
                                            checkCrlLocation foundCrl mftEECert
                                            validateCrl now crl certificate
                        oneMoreCrl

                        -- MFT can be revoked by the CRL that is on this MFT -- detect it                                
                        void $ vHoist $ validateMft now mft 
                                            certificate validCrl verifiedResources
                                            
                        -- Validate entry list and filter out CRL itself
                        nonCrlChildren <- validateMftEntries mft (getHash crl)

                        -- Mark all manifest entries as visited to avoid the situation
                        -- when some of the children are deleted from the cache and some
                        -- are still there. Do it both in case of successful validation
                        -- or a validation error.
                        let markAllEntriesAsVisited = 
                                visitObjects topDownContext $ map (\(T2 _ h) -> h) nonCrlChildren                

                        let processChildren =
                                inParallelVT
                                    (cpuBottleneck appBottlenecks <> ioBottleneck appBottlenecks)
                                    nonCrlChildren
                                    $ \(T2 filename hash') -> 
                                                validateManifestEntry filename hash' validCrl

                        childrenResults <- processChildren `finallyError` markAllEntriesAsVisited

                        -- Combine PPs and their waiting lists. On top of it, fix the 
                        -- last successfull validation times for PPs based on their fetch statuses.                        
                        pure $ mconcat childrenResults & typed @PublicationPoints %~ adjustLastSucceeded                        

                    Just _  -> vError $ CRLHashPointsToAnotherObject crlHash certLocations'   

            oneMoreMft
            addValidMft topDownContext childrenAki mft
            pure manifestResult            


    -- Check manifest entries as a whole, without doing anything 
    -- with the objects they are pointing to.    
    validateMftEntries mft crlHash = do         
        let children = mftEntries $ getCMSContent $ cmsPayload mft
        let nonCrlChildren = filter (\(T2 _ hash') -> crlHash /= hash') children
                    
        -- Make sure all the entries are unique
        let entryMap = Map.fromListWith (<>) $ map (\(T2 f h) -> (h, [f])) nonCrlChildren
        let nonUniqueEntries = Map.filter longerThanOne entryMap

        -- Don't crash here, it's just a warning, at the moment RFC doesn't say anything 
        -- about uniqueness of manifest entries.
        unless (Map.null nonUniqueEntries) $ 
            vWarn $ NonUniqueManifestEntries $ Map.toList nonUniqueEntries

        pure nonCrlChildren
        where
            longerThanOne [_] = False
            longerThanOne []  = False            
            longerThanOne _   = True
        
        
    --
    -- | Validate an entry of the manifest, i.e. a pair of filename and hash
    -- 
    validateManifestEntry filename hash' validCrl = do                    
        validateMftFileName        

        objectStore' <- (^. #objectStore) <$> liftIO (readTVarIO database)
        ro <- liftIO $ roTx objectStore' $ \tx -> getByHash tx objectStore' hash'
        case ro of 
            Nothing -> 
                vError $ ManifestEntryDoesn'tExist hash' filename
            Just ro' -> do
                -- warn about names on the manifest mismatching names in the object URLs
                let objectLocations = getLocations ro'
                let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $ unLocations objectLocations
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
                somethingElse -> 
                    vError $ BadFileNameOnMFT filename 
                                "Filename doesn't have exactly one DOT"            

    
    validateChild validCrl child@(Located locations ro) = do
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
                        inSubVPath (toText $ pickLocation locations) $ do                                
                            childVerifiedResources <- vHoist $ do                 
                                    Validated validCert <- validateResourceCert 
                                            now childCert (certificate ^. #payload) validCrl
                                    validateResources verifiedResources childCert validCert
                            let childTopDownContext = topDownContext { 
                                    verifiedResources = Just childVerifiedResources 
                                }
                            validateCaCertificate appContext childTopDownContext (Located locations childCert)                            
    
                -- case r of
                --     -- TODO Think about: it probably should be 
                --     -- Left e -> let 
                --     --             childCaError = mError (vContext $ toText $ NonEmpty.head $ getLocations ro) e)
                --     --           in T3 emptyPublicationPoints mempty (fromValidations childCaError)
                --     -- because we want to ignore all the errors down the tree when reporting up, they can be confusing.
                embedState validationState
                pure $ fromRight mempty r                    

            RoaRO roa -> do 
                        validateObjectLocations child
                        inSubVPath (locationsToText locations) $ 
                            allowRevoked $ do
                                void $ vHoist $ validateRoa now roa certificate validCrl verifiedResources
                                oneMoreRoa
                                
                                let vrps = getCMSContent $ cmsPayload roa
                                -- logDebugM logger [i|roa #{NonEmpty.head $ getLocations ro}, vrps = #{vrps}.|]
                                pure $ T3 mempty mempty $ Set.fromList vrps

            GbrRO gbr -> do                
                        validateObjectLocations child
                        inSubVPath (locationsToText locations) $ 
                            allowRevoked $ do
                                void $ vHoist $ validateGbr now gbr certificate validCrl verifiedResources
                                oneMoreGbr
                                pure mempty

            -- TODO Anything else?
            _ -> pure mempty

        where                
            -- In case of RevokedResourceCertificate error, the whole manifest is not be considered 
            -- invalid, only the object with the revoked certificate is considered invalid.
            -- This is a slightly ad-hoc code, but works fine.
            allowRevoked f =                
                catchAndEraseError f isRevokedCertError $ do 
                    vWarn RevokedResourceCertificate
                    pure mempty
                where                 
                    isRevokedCertError (ValidationE RevokedResourceCertificate) = True
                    isRevokedCertError _ = False


    findLatestMft childrenAki = liftIO $ do 
        objectStore' <- (^. #objectStore) <$> readTVarIO database
        roTx objectStore' $ \tx -> 
            findLatestMftByAKI tx objectStore' childrenAki

    findCachedLatestValidMft childrenAki = liftIO $ do 
        objectStore' <- (^. #objectStore) <$> readTVarIO database
        roTx objectStore' $ \tx -> 
            getLatestValidMftByAKI tx objectStore' childrenAki

    -- TODO Is there a more reliable way to find it?
    findCrlOnMft mft = filter (\(T2 name _) -> ".crl" `Text.isSuffixOf` name) $
        mftEntries $ getCMSContent $ cmsPayload mft


    -- Check that manifest URL in the certificate is the same as the one 
    -- the manifest was actually fetched from.
    validateMftLocation mft certficate = 
        case getManifestUri $ cwsX509certificate $ getCertWithSignature certficate of
            Nothing     -> vError $ NoMFTSIA $ getLocations certficate
            Just mftSIA -> do 
                let mftLocations = getLocations mft
                when (Set.null $ NESet.filter ((mftSIA ==) . getURL) $ unLocations mftLocations) $ 
                    vWarn $ MFTOnDifferentLocation mftSIA mftLocations                    

    -- Check that CRL URL in the certificate is the same as the one 
    -- the CRL was actually fetched from. 
    -- 
    checkCrlLocation crl eeCert = 
        case getCrlDistributionPoint $ cwsX509certificate eeCert of
            Nothing    -> pure ()
            Just crlDP -> do
                let crlLocations = getLocations crl
                when (Set.null $ NESet.filter ((crlDP ==) . getURL) $ unLocations crlLocations) $ 
                    vError $ CRLOnDifferentLocation crlDP crlLocations

    -- Validate that the object has only one location: if not, 
    -- it's generally is a warning, not really an error.
    validateObjectLocations (getLocations -> locs@(Locations locSet)) =
        inSubVPath (locationsToText locs) $ 
            when (NESet.size locSet > 1) $ 
                vWarn ObjectHasMultipleLocations
        



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


-- Mark validated objects in the database, i.e.
-- 
-- - save all the visited hashes together with the current world version
-- - save all the valid manifests for each CA/AKI
-- 
markValidatedObjects :: (MonadIO m, Storage s) => 
                        AppContext s -> TopDownContext s -> m ()
markValidatedObjects AppContext { .. } TopDownContext {..} = liftIO $ do
    ((visitedSize, validMftsSize), elapsed) <- timedMS $ do 
            (vhs, vmfts, objectStore') <- atomically $ (,,) <$> 
                                readTVar visitedHashes <*> 
                                readTVar validManifests <*>
                                ((^. #objectStore) <$> readTVar database)

            rwTx objectStore' $ \tx -> do 
                for_ vhs $ \h -> 
                    markValidated tx objectStore' h worldVersion 
                for_ (Map.toList vmfts) $ \(aki, h) -> 
                    markLatestValidMft tx objectStore' aki h

            pure (Set.size vhs, Map.size vmfts)

    logDebug_ logger 
        [i|Marked #{visitedSize} objects as visited, #{validMftsSize} manifests as valid, took #{elapsed}ms.|]


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


-- Add manifest to the map of the valid ones
addValidMft :: (MonadIO m, Storage s) => 
                TopDownContext s -> AKI -> MftObject -> m ()
addValidMft TopDownContext {..} aki mft = 
    liftIO $ atomically $ modifyTVar' 
                validManifests (<> Map.singleton aki (getHash mft))    

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl, oneMoreGbr :: Monad m => ValidatorT m ()
oneMoreCert = updateMetric @ValidationMetric @_ (& #validCertNumber %~ (+1))
oneMoreRoa  = updateMetric @ValidationMetric @_ (& #validRoaNumber %~ (+1))
oneMoreMft  = updateMetric @ValidationMetric @_ (& #validMftNumber %~ (+1))
oneMoreCrl  = updateMetric @ValidationMetric @_ (& #validCrlNumber %~ (+1))
oneMoreGbr  = updateMetric @ValidationMetric @_ (& #validGbrNumber %~ (+1))

setVrpNumber n = updateMetric @ValidationMetric @_ (& #vrpNumber .~ n)


-- Sum up all the validation metrics from all TA to create 
-- the "alltrustanchors" validation metric
addTotalValidationMetric :: (HasType ValidationState s, HasField' "vrps" s (Set a)) => s -> s
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

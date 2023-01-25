{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}

module RPKI.Validation.TopDown where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)

import           Data.Either                      (fromRight, partitionEithers)
import           Data.Foldable
import qualified Data.Set.NonEmpty                as NESet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Monoid.Generic
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           UnliftIO.Async

import           RPKI.AppContext
import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Repository
import           RPKI.Parallel
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                        (fmtEx, fmtLocations)
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.Common


-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext s = TopDownContext {    
        verifiedResources       :: Maybe (VerifiedRS PrefixesAndAsns),    
        taName                  :: TaName,         
        now                     :: Now,
        worldVersion            :: WorldVersion,
        validManifests          :: TVar (Map AKI Hash),
        visitedHashes           :: TVar (Set Hash),        
        repositoryProcessing    :: RepositoryProcessing,
        currentPathDepth        :: Int,
        startingRepositoryCount :: Int,
        interruptedByLimit      :: TVar Limited,        
        updateQueue             :: ClosableQueue UpdateOp
    }
    deriving stock (Generic)    


data Limited = CanProceed | FirstToHitLimit | AlreadyReportedLimit
    deriving stock (Show, Eq, Ord, Generic)

data UpdateOp = UpdateObjectBrief Hash EEBrief
    deriving stock (Show, Eq, Generic)

data ObjectOrBrief = RObject (Located RpkiObject) | RBrief (Located EEBrief) Hash
    deriving stock (Show, Eq, Generic)

data TopDownResult = TopDownResult {
        payloads           :: Payloads Vrps,        
        topDownValidations :: ValidationState
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving Semigroup via GenericSemigroup TopDownResult   
    deriving Monoid    via GenericMonoid TopDownResult       


fromValidations :: ValidationState -> TopDownResult
fromValidations = TopDownResult mempty


newTopDownContext :: MonadIO m => 
                    WorldVersion 
                    -> TaName                     
                    -> Now 
                    -> CaCerObject 
                    -> RepositoryProcessing
                    -> ClosableQueue UpdateOp
                    -> m (TopDownContext s)
newTopDownContext worldVersion taName now certificate repositoryProcessing updateQueue = 
    liftIO $ atomically $ do    
        let verifiedResources = Just $ createVerifiedResources certificate        
        let currentPathDepth = 0        
        startingRepositoryCount <- fmap repositoryCount $ readTVar $ repositoryProcessing ^. #publicationPoints  
        visitedHashes           <- newTVar mempty
        validManifests          <- newTVar mempty                
        interruptedByLimit      <- newTVar CanProceed        
        pure $ TopDownContext {..}

newRepositoryContext :: PublicationPoints -> RepositoryContext
newRepositoryContext publicationPoints = let 
    takenCareOf = Set.empty 
    in RepositoryContext {..}


verifyLimit :: STM Bool -> TVar Limited -> STM Limited
verifyLimit hitTheLimit limit =
    readTVar limit >>= \case    
        CanProceed -> do 
            h <- hitTheLimit
            if h then do
                writeTVar limit FirstToHitLimit
                pure FirstToHitLimit
            else 
                pure CanProceed
        FirstToHitLimit -> do 
            writeTVar limit AlreadyReportedLimit
            pure AlreadyReportedLimit
        AlreadyReportedLimit -> 
            pure AlreadyReportedLimit

-- | It is the main entry point for the top-down validation. 
-- Validates a bunch of TAs starting from their TALs.  
validateMutlipleTAs :: Storage s => 
                    AppContext s 
                    -> WorldVersion 
                    -> [TAL]
                    -> IO [TopDownResult]
validateMutlipleTAs appContext@AppContext {..} worldVersion tals = do                                     
    db <- readTVarIO database 

    repositoryProcessing <- newRepositoryProcessingIO config    

    updateQueue <- newCQueueIO 3000
    fst <$> concurrently 
                (validateThem db repositoryProcessing updateQueue
                    `finally` do 
                        atomically $ closeCQueue updateQueue
                        cancelFetchTasks repositoryProcessing)
                (applyUpdates appContext updateQueue
                    `finally` 
                        atomically (closeCQueue updateQueue))
  where

    validateThem db repositoryProcessing updateQueue = do 
        -- set initial publication point state
        mapException (AppException . storageError) $ do             
            pps <- roTx db $ \tx -> getPublicationPoints tx db
            let pps' = addRsyncPrefetchUrls config pps
            atomically $ writeTVar (repositoryProcessing ^. #publicationPoints) pps'
     
        rs <- liftIO $ pooledForConcurrently tals $ \tal -> do           
            (r@TopDownResult{ payloads = Payloads {..}}, elapsed) <- timedMS $ 
                    validateTA appContext tal worldVersion repositoryProcessing updateQueue
            logInfo logger [i|Validated TA '#{getTaName tal}', got #{estimateVrpCount vrps} VRPs, took #{elapsed}ms|]
            pure r    

        -- save publication points state    
        mapException (AppException . storageError) $ do            
            pps <- readTVarIO $ repositoryProcessing ^. #publicationPoints    
            rwTx db $ \tx -> savePublicationPoints tx db pps            

        -- Get validations for all the fetches that happened during this top-down traversal
        fetchValidation <- validationStateOfFetches repositoryProcessing
        pure $ fromValidations fetchValidation : rs              

--
validateTA :: Storage s => 
            AppContext s 
            -> TAL 
            -> WorldVersion             
            -> RepositoryProcessing 
            -> ClosableQueue UpdateOp
            -> IO TopDownResult
validateTA appContext@AppContext{..} tal worldVersion repositoryProcessing updateQueue = do    
    let maxDuration = config ^. typed @ValidationConfig . #topDownTimeout
    (r, vs) <- runValidatorT taContext $ 
            timeoutVT 
                maxDuration
                validateFromTAL
                (do 
                    logError logger [i|Validation for TA #{taName} did not finish within #{maxDuration} and was interrupted.|]
                    appError $ ValidationE $ ValidationTimeout $ secondsToInt maxDuration) 
    
    let payloads = case r of 
                    Left _  -> mempty
                    Right p -> p & #vrps %~ newVrps taName
    
    pure $ TopDownResult payloads vs
  where
    taName = getTaName tal
    taContext = newScopes' TAFocus $ unTaName taName

    validateFromTAL = do 
        timedMetric (Proxy :: Proxy ValidationMetric) $ 
            inSubObjectVScope (toText $ getTaCertURL tal) $ do 
                ((taCert, repos, _), _) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                -- this will be used as the "now" in all subsequent time and period validations 
                let now = Now $ versionToMoment worldVersion
                topDownContext <- newTopDownContext worldVersion 
                                    taName
                                    now  
                                    (taCert ^. #payload)  
                                    repositoryProcessing
                                    updateQueue
                validateFromTACert appContext topDownContext repos taCert                


data TACertStatus = Existing | Updated

-- | Fetch and validated TA certificate starting from the TAL.
-- | 
-- | This function doesn't throw exceptions.
validateTACertificateFromTAL :: Storage s => 
                                AppContext s 
                                -> TAL 
                                -> WorldVersion 
                                -> ValidatorT IO (Located CaCerObject, PublicationPointAccess, TACertStatus)
validateTACertificateFromTAL appContext@AppContext {..} tal worldVersion = do
    let now = Now $ versionToMoment worldVersion
    let validationConfig = config ^. typed @ValidationConfig    

    taStore  <- taStore <$> liftIO (readTVarIO database)
    taByName <- roAppTxEx taStore storageError $ \tx -> getTA tx taStore (getTaName tal)
    case taByName of
        Nothing -> fetchValidateAndStore taStore now Nothing
        Just StorableTA { taCert, initialRepositories, fetchStatus = fs }
            | needsFetching (getTaCertURL tal) fs validationConfig now ->
                fetchValidateAndStore taStore now (Just taCert)
            | otherwise -> do
                logInfo logger [i|Not re-fetching TA certificate #{getURL $ getTaCertURL tal}, it's up-to-date.|]
                pure (locatedTaCert (getTaCertURL tal) taCert, initialRepositories, Existing)
  where
    fetchValidateAndStore taStore (Now moment) previousCert = do 
        (uri', ro) <- fetchTACertificate appContext tal
        cert       <- vHoist $ validateTACert tal uri' ro            
        -- Check for replay attacks
        actualCert <- case previousCert of 
                        Nothing       -> pure cert
                        Just previous -> vHoist $ validateTACertWithPreviousCert cert previous
        case publicationPointsFromTAL tal actualCert of
            Left e         -> appError $ ValidationE e
            Right ppAccess -> 
                rwAppTxEx taStore storageError $ \tx -> do 
                    putTA tx taStore (StorableTA tal actualCert (FetchedAt moment) ppAccess)
                    pure (locatedTaCert uri' actualCert, ppAccess, Updated)
            
    locatedTaCert url cert = Located (toLocations url) cert


-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: Storage s =>
                    AppContext s -> 
                    TopDownContext s ->                                        
                    PublicationPointAccess -> 
                    Located CaCerObject ->                     
                    ValidatorT IO (Payloads (Set Vrp))
validateFromTACert 
    appContext@AppContext {..}
    topDownContext@TopDownContext { .. } 
    initialRepos
    taCert 
  = do      
    for_ (filterPPAccess config initialRepos) $ \filteredRepos -> do        
        liftIO $ atomically $ modifyTVar' 
                    (repositoryProcessing ^. #publicationPoints)
                    (\pubPoints -> foldr mergePP pubPoints $ unPublicationPointAccess filteredRepos) 
        
        -- ignore return result here, because all the fetching statuses will be
        -- handled afterwards by getting them from `repositoryProcessing` 
        void $ fetchPPWithFallback appContext repositoryProcessing worldVersion filteredRepos
        
    -- Do the tree descend, gather validation results and VRPs            
    vp <- askScopes
    T2 vrps validationState <- fromTry 
                (\e -> UnspecifiedE (unTaName taName) (fmtEx e)) 
                (validateCA appContext vp topDownContext taCert)

    embedState validationState    
    pure vrps
         

-- | Validate CA starting from its certificate.
-- 
validateCA :: Storage s =>
            AppContext s 
            -> Scopes 
            -> TopDownContext s 
            -> Located CaCerObject 
            -> IO (T2 (Payloads (Set Vrp)) ValidationState)
validateCA appContext scopes topDownContext certificate =
    validateCARecursively 
        `finally`  
        markValidatedObjects appContext topDownContext       
  where
    validateCARecursively = do             
        (r, validations) <- runValidatorT scopes $
                                validateCaCertificate appContext topDownContext certificate        
        pure $! T2 (fromRight mempty r) validations
    

validateCaCertificate :: Storage s =>
                        AppContext s ->
                        TopDownContext s ->
                        Located CaCerObject ->                
                        ValidatorT IO (Payloads (Set Vrp))
validateCaCertificate 
    appContext@AppContext {..} 
    topDownContext@TopDownContext {..} 
    certificate = do          
    
    let validationConfig = config ^. typed @ValidationConfig

    -- First check if we have reached some limit for the total depth of the CA tree
    -- it's total size of the number of repositories. 

    -- Check and report for the maximal tree depth
    let treeDepthLimit = (
            pure (currentPathDepth > validationConfig ^. #maxCertificatePathDepth),
            do 
                logError logger [i|Interrupting validation on #{fmtLocations $ getLocations certificate}, maximum tree depth is reached.|]
                vError $ CertificatePathTooDeep 
                            (getLocations certificate) 
                            (validationConfig ^. #maxCertificatePathDepth)
            )

    -- Check and report for the maximal number of objects in the tree
    let visitedObjectCountLimit = (
            (> validationConfig ^. #maxTotalTreeSize) . Set.size <$> readTVar visitedHashes,
            do 
                logError logger [i|Interrupting validation on #{fmtLocations $ getLocations certificate}, maximum total object number in the tree is reached.|]
                vError $ TreeIsTooBig 
                            (getLocations certificate) 
                            (validationConfig ^. #maxTotalTreeSize)
            )

    -- Check and report for the maximal increase in the repository number
    let repositoryCountLimit = (
            do 
                pps <- readTVar $ repositoryProcessing ^. #publicationPoints
                pure $ repositoryCount pps - startingRepositoryCount > validationConfig ^. #maxTaRepositories,
            do 
                logError logger [i|Interrupting validation on #{fmtLocations $ getLocations certificate}, maximum total new repository count is reached.|]
                vError $ TooManyRepositories 
                            (getLocations certificate) 
                            (validationConfig ^. #maxTaRepositories)
            )
                
    let actuallyValidate = 
            case getPublicationPointsFromCertObject (certificate ^. #payload) of            
                Left e         -> vError e
                Right ppAccess ->   
                    case filterPPAccess config ppAccess of 
                        Nothing -> 
                            -- Both rrdp and rsync (and whatever else in the future?) are
                            -- disabled, don't fetch at all.
                            validateThisCertAndGoDown
                        Just filteredPPAccess -> do 
                            fetches    <- fetchPPWithFallback appContext repositoryProcessing worldVersion filteredPPAccess
                            primaryUrl <- getPrimaryRepositoryFromPP repositoryProcessing filteredPPAccess
                            let goFurther = 
                                    if anySuccess fetches                    
                                        then validateThisCertAndGoDown                            
                                        else do                             
                                            fetchEverSucceeded repositoryProcessing filteredPPAccess >>= \case                        
                                                Never       -> pure mempty
                                                AtLeastOnce -> validateThisCertAndGoDown                            
                            case primaryUrl of 
                                Nothing -> goFurther
                                Just rp -> inSubMetricScope' PPFocus rp goFurther

    -- This is to make sure that the error of hitting a limit
    -- is reported only by the thread that first hits it
    let checkAndReport (condition, report) nextOne = do
            z <- liftIO $ atomically $ verifyLimit condition interruptedByLimit
            case z of 
                CanProceed           -> nextOne
                FirstToHitLimit      -> report
                AlreadyReportedLimit -> pure mempty

    checkAndReport treeDepthLimit 
        $ checkAndReport visitedObjectCountLimit 
        $ checkAndReport repositoryCountLimit
        $ actuallyValidate

  where    

    validateThisCertAndGoDown = do            
        -- Here we do the following
        -- 
        --  1) get the latest manifest (latest by the validity period)
        --  2) find CRL on it
        --  3) make sure they both are valid
        --  4) go through the manifest children and either 
        --     + validate them as signed objects
        --     + or valdiate them recursively as CA certificates
        -- 
        -- If anything falled, try to fetch previously latest cached 
        -- valid manifest and repeat (2) - (4) for it.

        -- Everything else is either extra checks or metrics.
        --         
        let childrenAki   = toAKI $ getSKI certificate
        let certLocations = getLocations certificate        
        
        validateObjectLocations certificate

        oneMoreCert            
        visitObject appContext topDownContext (CerRO $ certificate ^. #payload)                                                    

        -- first try to use the latest manifest 
        -- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-6486bis-11#section-6.2
        -- 
        maybeMft <- findLatestMft database childrenAki
        let goForLatestValid = tryLatestValidCachedManifest appContext useManifest maybeMft childrenAki certLocations
        case maybeMft of                        
            Nothing -> 
                -- Use awkward vError + catchError to force the error to 
                -- get into the Validations in the state.
                vError (NoMFT childrenAki certLocations)
                    `catchError`
                    goForLatestValid
                
            Just mft -> 
                useManifest mft childrenAki certLocations
                    `catchError` 
                    goForLatestValid

      where                       

        useManifest mft childrenAki certLocations = do             
            validateManifestAndItsChildren mft childrenAki certLocations
                `recover`
                -- manifest should be marked as visited regardless of its validitity
                visitObject appContext topDownContext mft               


        validateManifestAndItsChildren locatedMft childrenAki certLocations = do                         
            let mft = locatedMft ^. #payload            

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

            -- General location validation
            validateObjectLocations locatedMft

            -- Manifest-specific location validation
            validateMftLocation locatedMft certificate

            manifestResult <- inSubObjectVScope (locationsToText $ locatedMft ^. #locations) $ do
                T2 _ crlHash <- 
                    case findCrlOnMft mft of 
                        []    -> vError $ NoCRLOnMFT childrenAki certLocations
                        [crl] -> pure crl
                        crls  -> vError $ MoreThanOneCRLOnMFT childrenAki certLocations crls

                db <- liftIO $ readTVarIO database
                crlObject <- liftIO $ roTx db $ \tx -> getByHash tx db crlHash
                case crlObject of 
                    Nothing -> 
                        vError $ NoCRLExists childrenAki certLocations    

                    Just foundCrl@(Located crlLocations (CrlRO crl)) -> do      
                        visitObject appContext topDownContext foundCrl                        
                        validateObjectLocations foundCrl
                        validCrl <- inSubObjectVScope (locationsToText crlLocations) $ 
                                        vHoist $ do        
                                            let mftEECert = getEECert $ unCMS $ cmsPayload mft
                                            checkCrlLocation foundCrl mftEECert
                                            validateCrl now crl certificate
                        oneMoreCrl

                        -- MFT can be revoked by the CRL that is on this MFT -- detect 
                        -- revocation as well                               
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
                                                
                        let processChildren = do 
                                -- this indicates the difeerence between RFC9286-bis 
                                -- version 02 (strict) and version 03 and later (more loose).                                                                                            
                                let gatherMftEntryValidations = 
                                        case config ^. #validationConfig . #manifestProcessing of
                                            {- 
                                            The latest version so far of the 
                                            https://datatracker.ietf.org/doc/rfc9286/
                                            item 6.4 says
                                                "If there are files listed in the manifest that cannot be retrieved 
                                                from the publication point, the fetch has failed.." 

                                            For that case validity of every object on the manifest is completely 
                                            separate from each other and don't influence the manifest validity.
                                            -}
                                            RFC9286 -> independentMftChildrenResults

                                            {- 
                                            https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/
                                            item 6.4 says
                                                "If there are files listed in the manifest that cannot be retrieved 
                                                from the publication point, or if they fail the validity tests 
                                                specified in [RFC6488], the fetch has failed...". 

                                            For that case invalidity of some of the objects (except child CA certificates, 
                                            because that would be completeely insane) on the manifest make the whole manifest invalid.
                                            -}
                                            RFC6486_Strict -> allOrNothingMftChildrenResults

                                gatherMftEntryResults =<< gatherMftEntryValidations nonCrlChildren validCrl                                                                       

                        mconcat <$> processChildren `recover` markAllEntriesAsVisited                                                

                    Just _ -> 
                        vError $ CRLHashPointsToAnotherObject crlHash certLocations   

            oneMoreMft
            addValidMft topDownContext childrenAki mft
            pure manifestResult            

    allOrNothingMftChildrenResults nonCrlChildren validCrl = do
        vp <- askScopes
        liftIO $ pooledForConcurrently
            nonCrlChildren
            $ \(T2 filename hash') -> runValidatorT vp $ do 
                    ro <- findManifestEntryObject filename hash' 
                    -- if failed this one interrupts the whole MFT valdiation
                    validateMftChild ro filename validCrl                

    independentMftChildrenResults nonCrlChildren validCrl = do
        vp <- askScopes
        liftIO $ pooledForConcurrently
            nonCrlChildren
            $ \(T2 filename hash') -> do 
                (r, vs) <- runValidatorT vp $ findManifestEntryObject filename hash' 
                case r of 
                    Left e   -> pure (Left e, vs)
                    Right ro -> do 
                        -- We are cheating here a little by faking the empty VRP set.
                        -- 
                        -- if failed, this one will result in the empty VRP set
                        -- while keeping errors and warning in the `vs'` value.
                        (z, vs') <- runValidatorT vp $ validateMftChild ro filename validCrl
                        pure $ case z of                             
                            Left _ -> (Right mempty, vs')
                            _      -> (z, vs')     

    gatherMftEntryResults mftEntryResults = do                 
        -- gather all the validation states from every MFT entry
        mapM_ (embedState . snd) mftEntryResults                

        case partitionEithers $ map fst mftEntryResults of
            ([], vrps) -> pure vrps
            (e : _, _) -> appError e

    -- Check manifest entries as a whole, without doing anything 
    -- with the objects they are pointing to.    
    validateMftEntries mft crlHash = do         
        let mftChildren = mftEntries $ getCMSContent $ cmsPayload mft
        when (null mftChildren) $ 
            vError ZeroManifestEntries

        let nonCrlChildren = filter (\(T2 _ hash') -> crlHash /= hash') mftChildren
                    
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

    getManifestEntry hash' filename = do
        ro <- liftIO $ do 
            db <- readTVarIO database
            roTx db $ \tx -> getByHash tx db hash'
        case ro of 
            Nothing  -> vError $ ManifestEntryDoesn'tExist hash' filename
            Just ro' -> pure ro'        

    findManifestEntryObject filename hash' = do                    
        validateMftFileName filename        
        db    <- liftIO $ readTVarIO database
        brief <- liftIO $ roTx db $ \tx -> getOjectBrief tx db hash'
        case brief of 
            Just b  -> pure $ RBrief b hash'
            Nothing -> RObject <$> getManifestEntry hash' filename


    validateMftChild child filename validCrl = do
        -- warn about names on the manifest mismatching names in the object URLs
        let objectLocations = case child of 
                RObject o  -> getLocations o
                RBrief o _ -> getLocations o

        let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $ unLocations objectLocations
        when (null nameMatches) $ 
            vWarn $ ManifestLocationMismatch filename objectLocations

        case child of             
            RBrief ((^. #payload) -> brief) h                
                | getHash certificate == brief ^. #parentHash ->
                    -- It's under the same parent, so it's definitely the same object.
                    validateBrief brief validCrl
                | otherwise -> do 
                    -- oops, brief is outdated, switch to the real object
                    me <- getManifestEntry h filename
                    validateRealObject me validCrl
            RObject o -> 
                validateRealObject o validCrl

    {- 
        There's a brief for the already validated object, so only check, 
        validity period and if it wasn't revoked.             
    -}    
    validateBrief brief validCrl = do
        vHoist $ do
            validateCertValidityPeriod brief now        
            if (isRevoked brief validCrl) then do
                vPureWarning RevokedResourceCertificate
                pure $! mempty
            else do 
                oneMoreBrief
                case brief ^. #briefType of 
                    RoaBrief  -> do 
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ Set.size $ brief ^. #payload . #vrps
                    AspaBrief -> oneMoreAspa
                    BgpBrief  -> oneMoreBgp
                    GbrBrief  -> oneMoreGbr
                pure $! brief ^. #payload

    {-         
        Validate manifest children according to 
        https://datatracker.ietf.org/doc/rfc9286/
    -}                    
    validateRealObject child validCrl = do        
        let locations = getLocations child
        case child ^. #payload of
            CerRO childCert -> do 
                parentContext <- ask
                {- 
                    Note that recursive validation of the child CA happens in the separate   
                    runValidatorT (...) call, it is to avoid short-circuit logic implemented by ExceptT.
                    It is done to prevent child CAs short-circuiting and breaking validation of parents.
                -}                
                (r, validationState) <- liftIO $ runValidatorT parentContext $                     
                        inSubObjectVScope (toText $ pickLocation locations) $ do
                            childVerifiedResources <- vHoist $ do                 
                                    Validated validCert <- validateResourceCert @_ @_ @'CACert 
                                            now childCert (certificate ^. #payload) validCrl
                                    validateResources verifiedResources childCert validCert
                            let childTopDownContext = topDownContext 
                                    & #verifiedResources ?~ childVerifiedResources  
                                    & #currentPathDepth %~ (+ 1)                            
                            validateCaCertificate appContext childTopDownContext (Located locations childCert)                            

                embedState validationState
                pure $! fromRight mempty r                

            RoaRO roa -> do 
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $ 
                    allowRevoked $ do
                        void $ vHoist $ validateRoa now roa certificate validCrl verifiedResources
                        let vrpList = getCMSContent $ cmsPayload roa                            
                        oneMoreRoa                            
                        moreVrps $ Count $ fromIntegral $ length vrpList
                        let payload = (mempty :: Payloads (Set Vrp)) { vrps = Set.fromList vrpList }
                        updateEEBrief roa RoaBrief payload
                        pure $! payload

            GbrRO gbr -> do                
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $ 
                    allowRevoked $ do
                        void $ vHoist $ validateGbr now gbr certificate validCrl verifiedResources
                        oneMoreGbr
                        pure $! mempty

            AspaRO aspa -> do                
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $ 
                    allowRevoked $ do
                        void $ vHoist $ validateAspa now aspa certificate validCrl verifiedResources
                        oneMoreAspa            
                        let aspa' = getCMSContent $ cmsPayload aspa
                        let payload = (mempty :: Payloads (Set Vrp)) { aspas = Set.singleton aspa' }
                        updateEEBrief aspa AspaBrief payload
                        pure $! payload

            BgpRO bgpCert -> do                
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $ 
                    allowRevoked $ do
                        bgpPayload <- vHoist $ validateBgpCert now bgpCert certificate validCrl
                        oneMoreBgp
                        let payload = (mempty :: Payloads (Set Vrp)) { bgpCerts = Set.singleton bgpPayload }
                        updateEEBrief bgpCert BgpBrief payload
                        pure $! payload
                            

            -- Any new type of object should be added here, otherwise
            -- they will emit a warning.
            _somethingElse -> do 
                logWarn logger [i|Unsupported type of object: #{locations}.|]
                pure $! mempty

        where                
              
            updateEEBrief :: (MonadIO m, WithHash object, WithValidityPeriod object, WithSerial object) => 
                            object -> BriefType -> Payloads (Set.Set Vrp) -> m ()
            updateEEBrief object briefType payload = liftIO $ do 
                let (notValidBefore, notValidAfter) = getValidityPeriod object
                let parentHash = getHash certificate
                let serial = getSerial object
                let !brief = EEBrief {..}
                atomically $ writeCQueue updateQueue $ UpdateObjectBrief (getHash object) brief                

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
   

-- Mark validated objects in the database, i.e.
-- 
-- - save all the visited hashes together with the current world version
-- - save all the valid manifests for each CA/AKI
-- - save all the object validation briefs
-- 
markValidatedObjects :: (MonadIO m, Storage s) => 
                        AppContext s -> TopDownContext s -> m ()
markValidatedObjects AppContext { .. } TopDownContext {..} = liftIO $ do
    ((visitedSize, validMftsSize), elapsed) <- timedMS $ do 
            (vhs, vmfts, db) <- atomically $ (,,) <$> 
                                readTVar visitedHashes <*> 
                                readTVar validManifests <*>
                                readTVar database

            rwTx db $ \tx -> do 
                for_ vhs $ \h -> 
                    markValidated tx db h worldVersion 
                for_ (Map.toList vmfts) $ \(aki, h) -> 
                    markLatestValidMft tx db aki h                

            pure (Set.size vhs, Map.size vmfts)

    logInfo logger $
        [i|Marked #{visitedSize} objects as used, #{validMftsSize} manifests as valid for TA #{unTaName taName}, took #{elapsed}ms.|]


applyUpdates :: (MonadIO m, Storage s) => 
                AppContext s -> ClosableQueue UpdateOp -> m ()
applyUpdates AppContext { .. } updateQueue = liftIO $ do
    db <- readTVarIO database
    readQueueChunked updateQueue 500 $ \chunks ->
        rwTx db $ \tx -> 
            forM_ chunks $ \(UpdateObjectBrief h brief) -> putObjectBrief tx db h brief
    

-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
visitObject :: (MonadIO m, WithHash ro, Storage s) => 
                AppContext s -> TopDownContext s -> ro -> m ()
visitObject _ topDownContext ro = 
    visitObjects topDownContext [getHash ro]    

-- visitObjects :: MonadIO m => TopDownContext s -> [Hash] -> m ()
-- visitObjects TopDownContext {..} hashes =
--     liftIO $ atomically $ modifyTVar' visitedHashes (<> Set.fromList hashes)

visitObjects :: MonadIO m => TopDownContext s -> [Hash] -> m ()
visitObjects TopDownContext {..} hashes =
    liftIO $ atomically $ modifyTVar' visitedHashes (<> Set.fromList hashes)
        

-- Add manifest to the map of the valid ones
addValidMft :: (MonadIO m, Storage s) => 
                TopDownContext s -> AKI -> MftObject -> m ()
addValidMft TopDownContext {..} aki mft = 
    liftIO $ atomically $ modifyTVar' 
                validManifests (<> Map.singleton aki (getHash mft))    

-- addValidMft :: (MonadIO m, Storage s) => 
--                 TopDownContext s -> AKI -> MftObject -> m ()
-- addValidMft TopDownContext {..} aki mft =     
--     liftIO $ atomically $ writeCQueue updateQueue $ MarkLatestManifest aki (getHash mft)
                

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl, oneMoreGbr, oneMoreAspa, oneMoreBgp :: Monad m => ValidatorT m ()
oneMoreCert = updateMetric @ValidationMetric @_ (& #validCertNumber %~ (+1))
oneMoreRoa  = updateMetric @ValidationMetric @_ (& #validRoaNumber %~ (+1))
oneMoreMft  = updateMetric @ValidationMetric @_ (& #validMftNumber %~ (+1))
oneMoreCrl  = updateMetric @ValidationMetric @_ (& #validCrlNumber %~ (+1))
oneMoreGbr  = updateMetric @ValidationMetric @_ (& #validGbrNumber %~ (+1))
oneMoreAspa = updateMetric @ValidationMetric @_ (& #validAspaNumber %~ (+1))
oneMoreBgp  = updateMetric @ValidationMetric @_ (& #validBgpNumber %~ (+1))
oneMoreBrief  = updateMetric @ValidationMetric @_ (& #validBriefNumber %~ (+1))

moreVrps :: Monad m => Count -> ValidatorT m ()
moreVrps n = updateMetric @ValidationMetric @_ (& #vrpCounter %~ (+n))


-- Number of unique VRPs requires explicit counting of the VRP set sizes, 
-- so just counting the number of VRPs in ROAs in not enough
addUniqueVRPCount :: (HasType ValidationState s, HasField' "payloads" s (Payloads Vrps)) => s -> s
addUniqueVRPCount s = let 
        vrpCountLens = typed @ValidationState . typed @RawMetric . #vrpCounts
    in s & vrpCountLens . #totalUnique .~ 
                Count (fromIntegral $ uniqueVrpCount $ (s ^. #payloads) ^. #vrps)
         & vrpCountLens . #perTaUnique .~
                MonoidalMap.map (Count . fromIntegral . Set.size) (unVrps $ (s ^. #payloads) ^. #vrps)    
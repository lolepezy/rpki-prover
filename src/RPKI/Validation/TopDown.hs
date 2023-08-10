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

import           Control.Concurrent.Async        (forConcurrently)
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)

import           Data.Either                      (fromRight, partitionEithers)
import           Data.Foldable
import           Data.IORef
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Monoid.Generic
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           UnliftIO.Async                   (pooledForConcurrently)

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
data TopDownContext = TopDownContext {
        verifiedResources       :: Maybe (VerifiedRS PrefixesAndAsns),
        taName                  :: TaName,
        allTas                  :: AllTasTopDownContext,
        currentPathDepth        :: Int,
        startingRepositoryCount :: Int,
        interruptedByLimit      :: TVar Limited
    }
    deriving stock (Generic)


data AllTasTopDownContext = AllTasTopDownContext {
        now                  :: Now,
        worldVersion         :: WorldVersion,
        validManifests       :: TVar ValidManifests,
        visitedHashes        :: TVar (Set Hash),
        repositoryProcessing :: RepositoryProcessing,
        briefs               :: TVar [BriefUpdate]
    }
    deriving stock (Generic)


data Limited = CanProceed | FirstToHitLimit | AlreadyReportedLimit
    deriving stock (Show, Eq, Ord, Generic)

data BriefUpdate = BriefUpdate Hash EEBrief
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
                    TaName
                    -> CaCerObject
                    -> AllTasTopDownContext
                    -> m TopDownContext
newTopDownContext taName certificate allTas =
    liftIO $ atomically $ do
        let verifiedResources = Just $ createVerifiedResources certificate
        let currentPathDepth = 0
        startingRepositoryCount <- fmap repositoryCount $ readTVar $ allTas ^. #repositoryProcessing . #publicationPoints
        interruptedByLimit      <- newTVar CanProceed
        pure $ TopDownContext {..}

newAllTasTopDownContext :: MonadIO m =>
                        WorldVersion
                        -> Now
                        -> RepositoryProcessing
                        -> m AllTasTopDownContext
newAllTasTopDownContext worldVersion now repositoryProcessing =
    liftIO $ atomically $ do
        visitedHashes  <- newTVar mempty
        validManifests <- newTVar makeValidManifests
        briefs         <- newTVar []
        pure $ AllTasTopDownContext {..}


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
withRepositoriesProcessing :: Storage s =>
                            AppContext s 
                        -> (RepositoryProcessing -> IO a) 
                        -> IO a
withRepositoriesProcessing AppContext {..} f = do      
    rp <- newRepositoryProcessingIO config
    g rp `finally` (cancelFetchTasks rp)
  where
    g rp = do 
        db <- readTVarIO database        

        mapException (AppException . storageError) $ do
            pps <- roTx db $ \tx -> getPublicationPoints tx db
            let pps' = addRsyncPrefetchUrls config pps
            atomically $ writeTVar (rp ^. #publicationPoints) pps'

        a <- f rp

        -- save publication points state    
        mapException (AppException . storageError) $ do
            pps <- readTVarIO $ (rp ^. #publicationPoints)
            rwTx db $ \tx -> savePublicationPoints tx db pps          

        pure a

-- | It is the main entry point for the top-down validation. 
-- Validates a bunch of TAs starting from their TALs.  
validateMutlipleTAs :: Storage s =>
                    AppContext s
                    -> WorldVersion
                    -> [TAL]
                    -> IO [TopDownResult]
validateMutlipleTAs appContext@AppContext {..} worldVersion tals = do

    withRepositoriesProcessing appContext $ \repositoryProcessing -> do 
        allTas <- newAllTasTopDownContext worldVersion
                    (Now $ versionToMoment worldVersion) repositoryProcessing

        validateThem allTas
            `finally` (applyValidationSideEffects appContext allTas)                    
  where

    validateThem allTas = do
        rs <- forConcurrently tals $ \tal -> do
            (r@TopDownResult{ payloads = Payloads {..}}, elapsed) <- timedMS $
                    validateTA appContext tal worldVersion allTas
            logInfo logger [i|Validated TA '#{getTaName tal}', got #{estimateVrpCount vrps} VRPs, took #{elapsed}ms|]
            pure r

        -- Get validations for all the fetches that happened during this top-down traversal
        fetchValidation <- validationStateOfFetches $ allTas ^. #repositoryProcessing
        pure $ fromValidations fetchValidation : rs

--
validateTA :: Storage s =>
            AppContext s
            -> TAL
            -> WorldVersion
            -> AllTasTopDownContext
            -> IO TopDownResult
validateTA appContext@AppContext{..} tal worldVersion allTas = do
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
                topDownContext <- newTopDownContext taName (taCert ^. #payload) allTas
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
    let validationConfig = config ^. typed

    db       <- liftIO $ readTVarIO database
    taByName <- roAppTxEx db storageError $ \tx -> getTA tx db (getTaName tal)
    case taByName of
        Nothing -> fetchValidateAndStore db now Nothing
        Just StorableTA { taCert, initialRepositories, fetchStatus = fs }
            | needsFetching (getTaCertURL tal) fs validationConfig now ->
                fetchValidateAndStore db now (Just taCert)
            | otherwise -> do
                logInfo logger [i|Not re-fetching TA certificate #{getURL $ getTaCertURL tal}, it's up-to-date.|]
                pure (locatedTaCert (getTaCertURL tal) taCert, initialRepositories, Existing)
  where
    fetchValidateAndStore db (Now moment) previousCert = do
        (uri', ro) <- fetchTACertificate appContext (syncFetchConfig config) tal
        cert       <- vHoist $ validateTACert tal uri' ro
        -- Check for replay attacks
        actualCert <- case previousCert of
                        Nothing       -> pure cert
                        Just previous -> vHoist $ validateTACertWithPreviousCert cert previous
        case publicationPointsFromTAL tal actualCert of
            Left e         -> appError $ ValidationE e
            Right ppAccess ->
                rwAppTxEx db storageError $ \tx -> do
                    saveTA tx db (StorableTA tal actualCert (FetchedAt moment) ppAccess)
                    pure (locatedTaCert uri' actualCert, ppAccess, Updated)

    locatedTaCert url cert = Located (toLocations url) cert


-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: Storage s =>
                    AppContext s ->
                    TopDownContext ->
                    PublicationPointAccess ->
                    Located CaCerObject ->
                    ValidatorT IO (Payloads (Set Vrp))
validateFromTACert
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    initialRepos
    taCert
  = do
    for_ (filterPPAccess config initialRepos) $ \filteredRepos -> do
        liftIO $ atomically $ modifyTVar'
                    (repositoryProcessing ^. #publicationPoints)
                    (\pubPoints -> foldr mergePP pubPoints $ unPublicationPointAccess filteredRepos)

        -- ignore return result here, because all the fetching statuses will be
        -- handled afterwards by getting them from `repositoryProcessing` 
        void $ fetchPPWithFallback appContext (syncFetchConfig config) 
                repositoryProcessing worldVersion filteredRepos        

    -- Do the tree descend, gather validation results and VRPs                
    payloads <- fromTryM
                (UnspecifiedE (unTaName taName) . fmtEx)
                (validateCa appContext topDownContext taCert)

    pure $! foldr (<>) mempty payloads


validateCa :: Storage s =>
            AppContext s ->
            TopDownContext ->
            Located CaCerObject ->
            ValidatorT IO (Seq (Payloads (Set Vrp)))
validateCa
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
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
                            -- Skip repositories that are known to be too slow 
                            -- or timed out (i.e. unavailable)
                            pps <- liftIO $ readTVarIO $ repositoryProcessing ^. #publicationPoints    
                            let (quickPPs, slowRepos) = onlyForSyncFetch pps filteredPPAccess                                                
                            case quickPPs of 
                                Nothing -> do 
                                    -- Even though we are skipping the repository we still need to remember
                                    -- that it was mentioned as a publication point on a certificate                                    
                                    markForAsyncFetch repositoryProcessing slowRepos
                                    validateThisCertAndGoDown

                                Just filteredPPAccess -> do 
                                    fetches <- fetchPPWithFallback appContext (syncFetchConfig config) 
                                                    repositoryProcessing worldVersion filteredPPAccess
                                    
                                    markForAsyncFetch repositoryProcessing 
                                        $ filterForAsyncFetch filteredPPAccess fetches slowRepos

                                    primaryUrl <- getPrimaryRepositoryFromPP repositoryProcessing filteredPPAccess
                                    case primaryUrl of
                                        Nothing -> do 
                                            -- try some heuristics here, use the slow filtered out one
                                            case slowRepos of 
                                                []    -> validateThisCertAndGoDown
                                                r : _ -> inSubMetricScope' PPFocus (getRpkiURL r) validateThisCertAndGoDown
                                        Just rp -> 
                                            inSubMetricScope' PPFocus rp validateThisCertAndGoDown

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
        visitObject topDownContext (CerRO $ certificate ^. #payload)

        -- first try to use the latest manifest 
        -- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-6486bis-11#section-6.2
        --         
        maybeMft <- liftIO $ do
                        db <- readTVarIO database
                        roTx db $ \tx -> findLatestMftByAKI tx db childrenAki
        let goForLatestValid = tryLatestValidCachedManifest appContext useManifest
                                ((^. #object) <$> maybeMft) validManifests childrenAki certLocations
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
                    visitObject topDownContext (mft ^. #object)                    


        validateManifestAndItsChildren keyedMft childrenAki certLocations = do
            let locatedMft = keyedMft ^. #object
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

                crlKeyed <- liftIO $ do
                        db <- readTVarIO database
                        roTx db $ \tx -> getKeyedByHash tx db crlHash
                case crlKeyed of
                    Nothing ->
                        vError $ NoCRLExists childrenAki certLocations

                    Just (Keyed foundCrl@(Located crlLocations (CrlRO crl)) _) -> do
                        visitObject topDownContext foundCrl                        
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
                        -- when some of the children are garbage-collected from the cache 
                        -- and some are still there. Do it both in case of successful 
                        -- validation or a validation error.
                        let markAllEntriesAsVisited =
                                visitObjects topDownContext $ map (\(T2 _ h) -> h) nonCrlChildren

                        let processChildren = do
                                -- this indicates the difeerence between RFC9286-bis 
                                -- version 02 (strict) and version 03 and later (more loose).                                                                                            
                                let gatherMftEntryValidations =
                                        case config ^. #validationConfig . #manifestProcessing of
                                            {-                                             
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

                        foldr (<>) mempty <$> processChildren `recover` markAllEntriesAsVisited

                    Just _ ->
                        vError $ CRLHashPointsToAnotherObject crlHash certLocations

            oneMoreMft
            addValidMft topDownContext childrenAki (keyedMft ^. #key)
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
            ([], payloads) -> pure payloads
            (e : _, _)     -> appError e

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
            roTx db $ \tx -> getKeyedByHash tx db hash'
        case ro of
            Nothing  -> vError $ ManifestEntryDoesn'tExist hash' filename
            Just (Keyed object _) -> pure object

    findManifestEntryObject filename hash' = do
        validateMftFileName filename
        db    <- liftIO $ readTVarIO database
        brief <- liftIO $ roTx db $ \tx -> getOjectBrief tx db hash'
        case brief of
            Just b  -> pure $! RBrief (b ^. #object) hash'
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
                    -- oops, brief is not as we expected it to be, switch to the real object
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
                void $ vPureWarning RevokedResourceCertificate
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
                pure $! Seq.singleton $ brief ^. #payload

    {-         
        Validate manifest children according to 
        https://datatracker.ietf.org/doc/rfc9286/
    -}
    validateRealObject child validCrl = do
        let locations = getLocations child
        case child ^. #payload of
            CerRO childCert -> do
                parentScope <- ask
                {- 
                    Note that recursive validation of the child CA happens in the separate   
                    runValidatorT (...) call, it is to avoid short-circuit logic implemented by ExceptT:
                    an error in child validation would interrupt validation of the parent with
                    ExceptT's exception logic.
                -}
                (r, validationState) <- liftIO $ runValidatorT parentScope $
                        inSubObjectVScope (toText $ pickLocation locations) $ do
                            childVerifiedResources <- vHoist $ do
                                    Validated validCert <- validateResourceCert @_ @_ @'CACert
                                            now childCert (certificate ^. #payload) validCrl
                                    validateResources verifiedResources childCert validCert

                            -- Check that AIA of the child points to the correct location of the parent
                            -- https://mailarchive.ietf.org/arch/msg/sidrops/wRa88GHsJ8NMvfpuxXsT2_JXQSU/
                            --                             
                            vHoist $ validateAIA @_ @_ @'CACert childCert certificate

                            let childTopDownContext = topDownContext
                                    & #verifiedResources ?~ childVerifiedResources
                                    & #currentPathDepth %~ (+ 1)

                            validateCa appContext childTopDownContext (Located locations childCert)

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
                        let payload = (mempty :: Payloads (Set Vrp)) & #vrps .~ Set.fromList vrpList
                        updateEEBrief roa RoaBrief payload

            GbrRO gbr -> do
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $
                    allowRevoked $ do
                        void $ vHoist $ validateGbr now gbr certificate validCrl verifiedResources
                        oneMoreGbr
                        let gbr' = getCMSContent $ cmsPayload gbr
                        let payload = (mempty :: Payloads (Set Vrp)) & #gbrs .~ Set.singleton (getHash gbr, gbr')
                        pure $! Seq.singleton $ payload

            AspaRO aspa -> do
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $
                    allowRevoked $ do
                        void $ vHoist $ validateAspa now aspa certificate validCrl verifiedResources
                        oneMoreAspa
                        let aspa' = getCMSContent $ cmsPayload aspa
                        let payload = (mempty :: Payloads (Set Vrp)) & #aspas .~ Set.singleton aspa'
                        updateEEBrief aspa AspaBrief payload

            BgpRO bgpCert -> do
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $
                    allowRevoked $ do
                        bgpPayload <- vHoist $ validateBgpCert now bgpCert certificate validCrl
                        oneMoreBgp
                        let payload = (mempty :: Payloads (Set Vrp)) { bgpCerts = Set.singleton bgpPayload }
                        updateEEBrief bgpCert BgpBrief payload


            -- Any new type of object should be added here, otherwise
            -- they will emit a warning.
            _somethingElse -> do
                logWarn logger [i|Unsupported type of object: #{locations}.|]
                pure $! Seq.singleton mempty

        where

            updateEEBrief :: (MonadIO m, WithHash object, WithValidityPeriod object, WithSerial object) =>
                            object -> BriefType -> Payloads (Set.Set Vrp) -> m (Seq (Payloads (Set Vrp)))
            updateEEBrief object briefType payload = liftIO $ do
                let (notValidBefore, notValidAfter) = getValidityPeriod object
                let parentHash = getHash certificate
                let serial = getSerial object
                let brief = EEBrief {..}
                -- Only save briefs for strict classical validation, 
                -- for reconsidered there will be potentially arbitrary verified resource set
                -- so we have to revalidate resource sets every time.
                case getRFC certificate of
                    StrictRFC -> 
                        atomically $ modifyTVar' briefs $ \b -> 
                            let !z = BriefUpdate (getHash object) brief in z : b
                    ReconsideredRFC -> 
                        pure ()
                pure $! Seq.singleton $ payload

            -- In case of RevokedResourceCertificate error, the whole manifest is not to be considered 
            -- invalid, only the object with the revoked certificate is considered invalid.
            -- Replace RevokedResourceCertificate error with a warmning and don't break the 
            -- validation process.            
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
applyValidationSideEffects :: (MonadIO m, Storage s) =>
                             AppContext s -> AllTasTopDownContext -> m ()
applyValidationSideEffects
    AppContext {..}
    AllTasTopDownContext {..} = liftIO $ do
    ((visitedSize, validMftsSize, briefNumber), elapsed) <- timedMS $ do
            (vhs, vmfts, briefs', db) <- atomically $ (,,,) <$>
                                readTVar visitedHashes <*>                                
                                readTVar validManifests <*>
                                readTVar briefs <*>
                                readTVar database

            briefCounter <- newIORef (0 :: Integer)
            rwTx db $ \tx -> do
                markAsValidated tx db vhs worldVersion
                saveLatestValidMfts tx db (vmfts ^. #valids)
                for_ briefs' $ \(BriefUpdate h brief) -> do
                    saveObjectBrief tx db h brief
                    modifyIORef' briefCounter (+1)

            c <- readIORef briefCounter
            pure (Set.size vhs, Map.size (vmfts ^. #valids), c)

    logInfo logger $
        [i|Marked #{visitedSize} objects as used, #{validMftsSize} manifests as valid, |] <>
        [i|saved #{briefNumber} brief objects, took #{elapsed}ms.|]



-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
visitObject :: (MonadIO m, WithHash ro) =>
                TopDownContext -> ro -> m ()
visitObject topDownContext ro =
    visitObjects topDownContext [getHash ro]

visitObjects :: MonadIO m => TopDownContext -> [Hash] -> m ()
visitObjects TopDownContext { allTas = AllTasTopDownContext {..} } hashes = 
    liftIO $ forM_ hashes $ \h -> 
                atomically $ modifyTVar' visitedHashes (Set.insert h)

-- Add manifest to the map of the valid ones
addValidMft :: MonadIO m => TopDownContext -> AKI -> ObjectKey -> m ()
addValidMft TopDownContext { allTas = AllTasTopDownContext {..}} aki mftKey =
    liftIO $ atomically $ modifyTVar'
                validManifests (& #valids %~ Map.insert aki mftKey)

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl :: Monad m => ValidatorT m ()
oneMoreGbr, oneMoreAspa, oneMoreBgp, oneMoreBrief :: Monad m => ValidatorT m ()
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
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
import qualified Data.List                        as List
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Sequence                    (Seq)
import qualified Data.Sequence                    as Seq
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
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
import           RPKI.Validation.Types
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
        visitedKeys          :: TVar (Set ObjectKey),        
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


type PayloadsType = Seq (Payloads (Set Vrp))

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
        visitedKeys    <- newTVar mempty
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



-- Do something within the bracket of RepositoryProcessing instance
-- 
withRepositoriesProcessing :: Storage s =>
                            AppContext s 
                        -> (RepositoryProcessing -> IO a) 
                        -> IO a
withRepositoriesProcessing AppContext {..} f = 
    bracket
        (newRepositoryProcessingIO config)
        cancelFetchTasks
        $ \rp -> do 
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

        resetForAsyncFetch repositoryProcessing
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
    ca = do

    let validationConfig = config ^. typed @ValidationConfig

    -- First check if we have reached some limit for the total depth of the CA tree
    -- it's total size of the number of repositories. 

    -- Check and report for the maximal tree depth
    let treeDepthLimit = (
            pure (currentPathDepth > validationConfig ^. #maxCertificatePathDepth),
            do
                logError logger [i|Interrupting validation on #{fmtLocations $ getLocations ca}, maximum tree depth is reached.|]
                vError $ CertificatePathTooDeep
                            (getLocations ca)
                            (validationConfig ^. #maxCertificatePathDepth)
            )

    -- Check and report for the maximal number of objects in the tree
    let visitedObjectCountLimit = (
            (> validationConfig ^. #maxTotalTreeSize) . Set.size <$> readTVar visitedHashes,
            do
                logError logger [i|Interrupting validation on #{fmtLocations $ getLocations ca}, maximum total object number in the tree is reached.|]
                vError $ TreeIsTooBig
                            (getLocations ca)
                            (validationConfig ^. #maxTotalTreeSize)
            )

    -- Check and report for the maximal increase in the repository number
    let repositoryCountLimit = (
            do
                pps <- readTVar $ repositoryProcessing ^. #publicationPoints
                pure $ repositoryCount pps - startingRepositoryCount > validationConfig ^. #maxTaRepositories,
            do
                logError logger [i|Interrupting validation on #{fmtLocations $ getLocations ca}, maximum total new repository count is reached.|]
                vError $ TooManyRepositories
                            (getLocations ca)
                            (validationConfig ^. #maxTaRepositories)
            )                

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
        $ validateCaNoLimitChecks appContext topDownContext (CaFull ca)


validateCaNoLimitChecks :: Storage s =>
            AppContext s ->
            TopDownContext ->
            Ca ->
            ValidatorT IO (Seq (Payloads (Set Vrp)))
validateCaNoLimitChecks
        appContext@AppContext {..}
        topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
        ca = 
    case extractPPAs ca of        
        Left e         -> vError e
        Right ppAccess ->
            case filterPPAccess config ppAccess of
                Nothing ->
                    -- Both rrdp and rsync (and whatever else in the future?) are
                    -- disabled, don't fetch at all.
                    validateThisCertAndGoDown
                Just filteredPPAccess -> do
                    -- Skip repositories that are marked as "slow"
                    pps <- liftIO $ readTVarIO $ repositoryProcessing ^. #publicationPoints    
                    let (quickPPs, slowRepos) = onlyForSyncFetch pps filteredPPAccess                                                
                    case quickPPs of 
                        Nothing -> do 
                            -- Even though we are skipping the repository we still need to remember
                            -- that it was mentioned as a publication point on a certificate                                    
                            markForAsyncFetch repositoryProcessing slowRepos                                
                            validateThisCertAndGoDown

                        Just quickPPAccess -> do                                     
                            fetches <- fetchPPWithFallback appContext (syncFetchConfig config) 
                                            repositoryProcessing worldVersion quickPPAccess                                    
                            -- Based on 
                            --   * which repository(-ries) were mentioned on the certificate
                            --   * which ones succeded, 
                            --   * which were skipped because they are slow,
                            -- derive which repository(-ies) should be picked up 
                            -- for async fetching later.
                            markForAsyncFetch repositoryProcessing 
                                $ filterForAsyncFetch filteredPPAccess fetches slowRepos

                            -- primaryUrl is used for set the focus to the publication point
                            primaryUrl <- getPrimaryRepositoryFromPP repositoryProcessing filteredPPAccess
                            case primaryUrl of
                                Nothing -> validateThisCertAndGoDown                                            
                                Just rp -> inSubMetricScope' PPFocus rp validateThisCertAndGoDown  
  where

    validateThisCertAndGoDown :: ValidatorT IO (Seq (Payloads (Set Vrp))) 
    validateThisCertAndGoDown = do    
        validateObjectLocations ca        
        z <- join $ makeNextAction (toAKI $ getSKI ca) ca
        oneMoreCert
        pure z

      where        

        makeNextAction :: AKI -> ValidatorT IO (ValidatorT IO PayloadsType)
        makeNextAction childrenAki = 
            roTxT database $ \tx db -> do 
                getMftShorcut tx db childrenAki >>= \case
                    Nothing -> do 
                        findLatestMftByAKI tx db childrenAki >>= \case 
                            Nothing -> do 
                                -- No current manifest and not shortcut as well, bail out 
                                pure $ vError $ NoMFT childrenAki
                            Just mft ->
                                -- No shortcut found, so do the full validation for the manifest
                                pure $ do  
                                    caFull <- getFullCa appContext ca
                                    T2 payloads _ <- childrenFullValidation caFull mft Nothing
                                    pure $! payloads

                    Just mftShortcut -> do 
                        -- Find the key of the latest real manifest
                        findLatestMftKeyByAKI tx db childrenAki >>= \case 
                            Nothing -> pure $ do 
                                -- That is really weird and should normally never happen. 
                                -- Do not interrupt validation here, but complain in the log
                                vWarn $ NoMFTButCachedMft childrenAki
                                let mftKey = mftShortcut ^. #key
                                let message = [i|Internal error, there is a manifest shortcut, but no manifest for the key #{mftKey}.|]
                                logError logger message
                                collectPayloads mftShortcut Nothing 
                                    (getFullCa appContext ca)
                                    -- getCrlByKey is the best we can have
                                    (getCrlByKey appContext (mftShortcut ^. #crlShortcut . #key))

                            Just mftKey 
                                | mftShortcut ^. #key == mftKey -> 
                                    -- Nothing has changed, the real manifest is the 
                                    -- same as the shortcut, so use the shortcut
                                    pure $ collectPayloads mftShortcut Nothing 
                                        (getFullCa appContext ca)
                                        (getCrlByKey appContext (mftShortcut ^. #crlShortcut . #key))
                                | otherwise ->                                     
                                    getMftByKey tx db mftKey >>= \case 
                                        Nothing -> pure $ 
                                            internalError [i|Internal error, can't find a maniesft by its key #{mftKey}.|]                                            
                                        Just mft -> pure $ do
                                            fullCa <- getFullCa appContext ca
                                            T2 r1 overlappingChildren <- childrenFullValidation fullCa mft (Just mftShortcut)
                                            r2 <- collectPayloads mftShortcut (Just overlappingChildren) 
                                                    (pure fullCa)
                                                    (findAndValidateCrl fullCa mft)
                                            pure $! r1 <> r2

          where

            -- Do the proper validation for the manifest and children mentioned in `childrenKeys`            
            childrenFullValidation :: 
                            Located CaCerObject
                            -> Keyed (Located MftObject) 
                            -> Maybe MftShortcut 
                            -> ValidatorT IO (T2 PayloadsType [T3 Text Hash ObjectKey]) 
            childrenFullValidation fullCa keyedMft mftShortcut = do 
                let (Keyed locatedMft@(Located locations _) mftKey) = keyedMft

                vks <- liftIO $ readTVarIO visitedKeys
                when ((keyedMft ^. #key) `Set.member` vks) $
                    -- We have already visited this manifest before, so 
                    -- there're some circular references in the objects.
                    -- 
                    -- NOTE: Limit cycle detection only to manifests
                    -- to minimise the false positives where the same object
                    -- is referenced from multiple manifests and we are treating 
                    -- it as a cycle.
                    vError $ CircularReference (getHash locatedMft)

                -- General location validation
                validateObjectLocations locatedMft

                -- Manifest-specific location validation
                validateMftLocation locatedMft fullCa

                manifestResult <- validateManifestWithChildren fullCa keyedMft mftShortcut
                
                oneMoreMft

                -- TODO Figure out if this still makes sense
                -- addValidMft topDownContext childrenAki mftKey

                pure manifestResult            

            validateManifestWithChildren :: 
                               Located CaCerObject
                            -> Keyed (Located MftObject) 
                            -> Maybe MftShortcut 
                            -> ValidatorT IO (T2 PayloadsType [T3 Text Hash ObjectKey]) 
            validateManifestWithChildren fullCa keyedMft@(Keyed (Located locations mft) mftKey) mftShortcut =                 
                inSubObjectVScope (locationsToText locations) $ do

                    -- TODO Add fiddling with shortcut version of CRL here                    
                    validCrl <- findAndValidateCrl fullCa keyedMft
                    oneMoreCrl

                    -- MFT can be revoked by the CRL that is on this MFT -- detect 
                    -- revocation as well, this is clearly and error                               
                    validMft <- vHoist $ validateMft now mft fullCa validCrl verifiedResources

                    -- Validate entry list and filter out CRL itself
                    nonCrlChildren <- validateMftEntries mft (getHash validCrl)

                    -- If MFT shortcut is present, filter children that need validation, 
                    -- children that are already on the shortcut are validated and can be 
                    -- skipped here
                    let (newChildren, overlappingChildren) =
                            case mftShortcut of 
                                Nothing       -> (nonCrlChildren, [])
                                Just mftShort -> mftDiff mftShort nonCrlChildren

                    -- Mark all manifest entries as visited to avoid the situation
                    -- when some of the children are garbage-collected from the cache 
                    -- and some are still there. Do it both in case of successful 
                    -- validation or a validation error.
                    let markAllEntriesAsVisited = do                             
                            forM_ (newChildren <> overlappingChildren) $ 
                                (\(T3 _ _ k) -> visitKey topDownContext k)

                    let processChildren = do                                              
                            -- Here we have the payloads for the fully validated MFT children
                            -- and the shortcut objects for these children
                            T2 (validatedPayloads :: PayloadsType) childrenShortcuts <- 
                                    (gatherMftEntryResults =<< gatherMftEntryValidations fullCa newChildren validCrl)
                                                    
                            let newEntries = makeEntriesWithMap 
                                    newChildren (Map.fromList childrenShortcuts)
                                    (\entry fileName -> entry fileName)

                            let nextChildrenShortcuts = 
                                    case mftShortcut of 
                                        Nothing               -> newEntries
                                        Just MftShortcut {..} -> 
                                            newEntries <> makeEntriesWithMap 
                                                overlappingChildren entries (\entry _ -> entry)
                                        
                            let mftShortcut = makeMftShortcut mftKey validMft nextChildrenShortcuts
                            addMftShortcut topDownContext mftShortcut
                            let allPayloads :: PayloadsType = foldr (<>) mempty <$> validatedPayloads
                            pure $! T2 allPayloads overlappingChildren

                    processChildren `recover` markAllEntriesAsVisited

            findCrl mft = do 
                T2 _ crlHash <-
                    case findCrlOnMft mft of
                        []    -> vError $ NoCRLOnMFT childrenAki 
                        [crl] -> pure crl
                        crls  -> vError $ MoreThanOneCRLOnMFT childrenAki crls

                z <- roTxT database $ \tx db -> getKeyedByHash tx db crlHash
                case z of
                    Nothing -> 
                        vError $ NoCRLExists childrenAki
                    Just crl@(Keyed (Located _ (CrlRO _)) _) -> 
                        pure crl
                    _ -> 
                        vError $ CRLHashPointsToAnotherObject crlHash   


            findAndValidateCrl fullCa (Keyed (Located _ mft) _) = do 
                Keyed locatedCrl@(Located crlLocations (CrlRO crl)) _ <- findCrl mft
                visitObject topDownContext locatedCrl                        
                validateObjectLocations locatedCrl
                inSubObjectVScope (locationsToText crlLocations) $
                    vHoist $ do
                        let mftEECert = getEECert $ unCMS $ cmsPayload mft
                        checkCrlLocation locatedCrl mftEECert
                        validateCrl now crl fullCa                
                        pure $! Validated crl

            -- Calculate difference bentween a manifest shortcut 
            -- and a real manifest object.
            mftDiff MftShortcut {..} newMftChidlren = 
                List.partition (\(T3 fileName _  k) -> 
                        isNewEntry k fileName entries) newMftChidlren
              where
                -- it's not in the map of shortcut children or it has changed 
                -- its name (very unlikely but can happen in theory)                
                isNewEntry key fileName shortcutChildren = 
                    case Map.lookup key shortcutChildren of
                        Nothing -> True
                        Just e  -> e ^. #fileName /= fileName 

            -- utility for repeated peace of code
            makeEntriesWithMap childrenList entryMap makeEntry = 
                [ (key, makeEntry entry fileName) 
                        | (key, fileName, Just entry) <- [ (key, fileName, Map.lookup key entryMap) 
                        | (T3 fileName _ key) <- childrenList ]]


            -- this indicates the difeerence between RFC9286-bis 
            -- version 02 (strict) and version 03 and later (more loose).                                                                                            
            gatherMftEntryValidations =
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
                    because that would be completeely insane) on the manifest make the whole 
                    manifest invalid.
                    -}
                    RFC6486_Strict -> allOrNothingMftChildrenResults

    
    allOrNothingMftChildrenResults fullCa nonCrlChildren validCrl = do
        scopes <- askScopes
        liftIO $ pooledForConcurrently
            nonCrlChildren
            $ \(T3 filename hash' key) -> do 
                (z, vs) <- runValidatorT scopes $ do
                                ro <- getManifestEntry filename hash'
                                -- if failed this one interrupts the whole MFT valdiation
                                validateMftChild fullCa ro filename validCrl
                pure $ case z of
                    -- In this case invalid child is considered invalid entry 
                    -- and the whole manifest is invalid
                    Left e                     -> InvalidEntry e vs
                    Right (T2 r childShortcut) -> Valid r vs childShortcut key    
    
    independentMftChildrenResults fullCa nonCrlChildren validCrl = do
        scopes <- askScopes
        liftIO $ pooledForConcurrently
            nonCrlChildren
            $ \(T3 filename hash' key) -> do
                (r, vs) <- runValidatorT scopes $ getManifestEntry filename hash'
                case r of
                    Left e   -> pure $ InvalidEntry e vs
                    Right ro -> do
                        -- We are cheating here a little by faking empty payload set.
                        -- 
                        -- if failed, this one will result in the empty VRP set
                        -- while keeping errors and warning in the `vs'` value.
                        (z, vs') <- runValidatorT scopes $ validateMftChild fullCa ro filename validCrl
                        pure $ case z of
                                Left e                     -> InvalidChild e vs' key
                                Right (T2 r childShortcut) -> Valid r vs' childShortcut key
    
    -- gatherMftEntryResults :: _ -> ValidatorT IO [T2 PayloadsType (ObjectKey, Text -> MftEntry)]
    gatherMftEntryResults =        
        foldM (\(T2 payloads childrenShortcuts) r -> do 
                embedState $ manifestEntryVs r
                case r of 
                    InvalidEntry e _ -> 
                        appError e
                    InvalidChild _ vs key -> 
                        pure $! T2 payloads ((key, makeTroubledChild key) : childrenShortcuts)
                    Valid payload vs childShortcut key -> do 
                        let payloads' = payload <> payloads
                        pure $! T2 payloads' ((key, childShortcut) : childrenShortcuts)
            ) mempty

        
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
        -- TODO Or does it? Maybe something in ASN1 encoding as Set?
        unless (Map.null nonUniqueEntries) $
            vWarn $ NonUniqueManifestEntries $ Map.toList nonUniqueEntries

        db <- liftIO $ readTVarIO database
        forM nonCrlChildren $ \(T2 fileName hash) -> do
                k <- roAppTx db $ \tx -> getKeyByHash tx db hash            
                case k of 
                    Nothing  -> vError $ ManifestEntryDoesn'tExist hash fileName
                    Just key -> pure $! T3 fileName hash key        
        where
            longerThanOne = \case 
                [_] -> False
                []  -> False
                _   -> True

    getManifestEntry filename hash' = do
        ro <- roTxT database $ \tx db -> getKeyedByHash tx db hash'
        case ro of
            Nothing -> vError $ ManifestEntryDoesn'tExist hash' filename
            Just ro -> pure ro

    validateMftChild caFull child filename validCrl = do
        -- warn about names on the manifest mismatching names in the object URLs
        let objectLocations = getLocations child

        let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $ unLocations objectLocations
        when (null nameMatches) $
            vWarn $ ManifestLocationMismatch filename objectLocations

        validateChildObject caFull child validCrl


    {-         
        Validate manifest children according to 
        https://datatracker.ietf.org/doc/rfc9286/

        And return shortcuts created for them
    -}
    validateChildObject :: 
            Located CaCerObject
            -> Keyed (Located RpkiObject) 
            -> Validated CrlObject
            -> ValidatorT IO (T2 PayloadsType (Text -> MftEntry))
    validateChildObject fullCa (Keyed child@(Located locations childRo) childKey) validCrl = do        
        let emptyPayloads = mempty :: Payloads (Set Vrp)
        case childRo of
            CerRO childCert -> do
                parentScope <- ask
                {- 
                    Note that recursive validation of the child CA happens in the separate   
                    runValidatorT (...) call, it is to avoid short-circuit logic implemented by ExceptT:
                    otherwise an error in child validation would interrupt validation of the parent with
                    ExceptT's exception logic.
                -}
                (r, validationState) <- liftIO $ runValidatorT parentScope $
                        inSubObjectVScope (toText $ pickLocation locations) $ do
                            childVerifiedResources <- vHoist $ do
                                    Validated validCert <- validateResourceCert @_ @_ @'CACert
                                            now childCert fullCa validCrl
                                    validateResources verifiedResources childCert validCert

                            -- Check that AIA of the child points to the correct location of the parent
                            -- https://mailarchive.ietf.org/arch/msg/sidrops/wRa88GHsJ8NMvfpuxXsT2_JXQSU/
                            --                             
                            vHoist $ validateAIA @_ @_ @'CACert childCert fullCa

                            let childTopDownContext = topDownContext
                                    & #verifiedResources ?~ childVerifiedResources
                                    & #currentPathDepth %~ (+ 1)

                            validateCa appContext childTopDownContext (Located locations childCert)

                embedState validationState
                case r of 
                    Left e        -> pure $! T2 mempty (makeTroubledChild childKey)
                    Right payload -> do 
                        case getPublicationPointsFromCertObject childCert of 
                            -- It's not going to happen?
                            Left e     -> vError e
                            Right ppas -> pure $! T2 payload (makeCaShortcut childKey (Validated childCert) ppas)

            RoaRO roa -> do
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $
                    allowRevoked $ do
                        validRoa <- vHoist $ validateRoa now roa fullCa validCrl verifiedResources
                        let vrpList = getCMSContent $ cmsPayload roa
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ length vrpList
                        let payload = emptyPayloads & #vrps .~ Set.fromList vrpList                   
                        pure $! T2 (Seq.singleton payload) (makeRoaShortcut childKey validRoa vrpList) 


            GbrRO gbr -> do
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $
                    allowRevoked $ do
                        validGbr <- vHoist $ validateGbr now gbr fullCa validCrl verifiedResources
                        oneMoreGbr
                        let gbr' = getCMSContent $ cmsPayload gbr
                        let gbrPayload = (getHash gbr, gbr')
                        let payload = emptyPayloads & #gbrs .~ Set.singleton gbrPayload
                        pure $! T2 (Seq.singleton payload) (makeGbrShortcut childKey validGbr gbrPayload) 
                        

            AspaRO aspa -> do
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $
                    allowRevoked $ do
                        validAspa <- vHoist $ validateAspa now aspa fullCa validCrl verifiedResources
                        oneMoreAspa
                        let aspaPayload = getCMSContent $ cmsPayload aspa
                        let payload = emptyPayloads & #aspas .~ Set.singleton aspaPayload
                        pure $! T2 (Seq.singleton payload) (makeAspaShortcut childKey validAspa aspaPayload) 

            BgpRO bgpCert -> do
                validateObjectLocations child
                inSubObjectVScope (locationsToText locations) $
                    allowRevoked $ do
                        (validaBgpCert, bgpPayload) <- vHoist $ validateBgpCert now bgpCert fullCa validCrl
                        oneMoreBgp
                        let payload = emptyPayloads { bgpCerts = Set.singleton bgpPayload }
                        pure $! T2 (Seq.singleton payload) (makeBgpSecShortcut childKey validaBgpCert bgpPayload) 


            -- Any new type of object should be added here, otherwise
            -- they will emit a warning.
            _somethingElse -> do
                logWarn logger [i|Unsupported type of object: #{locations}.|]                
                pure $! T2 (Seq.singleton mempty) (makeTroubledChild childKey)

        where

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

    -- TODO Implement it
    collectPayloads :: MftShortcut 
                -> Maybe [T3 Text Hash ObjectKey] 
                -> ValidatorT IO (Located CaCerObject)
                -> ValidatorT IO (Validated CrlObject)
                -> ValidatorT IO PayloadsType 
    collectPayloads mftShortcut@MftShortcut {..} maybeChildren findFullCa findValidCrl = do      
        -- TODO Get the CRL as well
        let troubled = [ () | (_, MftEntry { child = TroubledChild _} ) <- Map.toList entries ]

        -- For children that are invalid we'll have to fall back to full validation, 
        -- for that we beed the parent CA and a valid CRL.
        troubledValidation <-
                case [ () | (_, MftEntry { child = TroubledChild _} ) <- Map.toList entries ] of 
                    [] -> 
                        -- Should never happen
                        pure $ \_ -> errorOnTroubledChild
                    _  -> do 
                        caFull   <- findFullCa
                        validCrl <- findValidCrl
                        pure $ \childKey -> validateTroubledChild caFull validCrl childKey

        z <- forM (Map.toList entries) $ getChildPayloads troubledValidation

        pure $! foldr (<>) mempty z
      where
        errorOnTroubledChild = internalError [i|Impossible happened!|]

        validateTroubledChild caFull validCrl childKey = do  
            -- It was an invalid child and nothing about it is cached, so 
            -- we have to process full validation for i
            z <- roTxT database $ \tx db -> getLocatedByKey tx db childKey
            case z of 
                Nothing          -> internalError [i|Internal error, can't find a troubled child by its key #{childKey}.|]
                Just childObject -> do 
                    T2 payloads _ <- validateChildObject caFull (Keyed childObject childKey) validCrl
                    pure $1 payloads

        getChildPayloads troubledValidation (_, MftEntry {..}) = do 
            let emptyPayloads = mempty :: Payloads (Set Vrp)
            case child of 
                CaChild caShortcut _ -> 
                    validateCaNoLimitChecks appContext topDownContext caShortcut

                RoaChild r@RoaShortcut {..} _ -> do 
                    vHoist $ validateCertValidityPeriod r now
                    oneMoreRoa
                    moreVrps $ Count $ fromIntegral $ length vrps                            
                    pure $! Seq.singleton $! emptyPayloads & #vrps .~ Set.fromList vrps
                    
                AspaChild a@AspaShortcut {..} _ -> do  
                    vHoist $ validateCertValidityPeriod a now
                    oneMoreAspa
                    pure $! Seq.singleton $! emptyPayloads & #aspas .~ Set.singleton aspa

                BgpSecChild b@BgpSecShortcut {..} _ -> do 
                    vHoist $ validateCertValidityPeriod b now                            
                    oneMoreBgp
                    pure $! Seq.singleton $! emptyPayloads & #bgpCerts .~ Set.singleton bgpSec

                GbrChild g@GbrShortcut {..} _ -> do
                    vHoist $ validateCertValidityPeriod g now         
                    oneMoreGbr                   
                    pure $! Seq.singleton $! emptyPayloads & #gbrs .~ Set.singleton gbr

                TroubledChild childKey -> troubledValidation childKey  


getFullCa AppContext {..} ca = 
    case ca of     
        Located loc (CaFull c)              -> pure $! Located loc c
        Located _ (CaShort CaShortcut {..}) -> do   
            z <- roTxT database $ \tx db -> getLocatedByKey tx db key
            case z of 
                Just (Located loc (CerRO c)) -> pure $! Located loc c
                _ -> internalError [i|Internal error, can't find a CA by its key #{key}.|]
    
getCrlByKey AppContext {..} crlKey = do    
    z <- roTxT database $ \tx db -> getObjectByKey tx db crlKey
    case z of 
        Just (CrlRO c) -> pure $! Validated c
        _ -> internalError [i|Internal error, can't find a CRL by its key #{crlKey}.|]
    
internalError message = do     
    logError logger message
    appError $ InternalE $ InternalError message  

makeCaShortcut :: ObjectKey -> Validated CaCerObject -> PublicationPointAccess -> Text -> MftEntry
makeCaShortcut key (Validated certificate) ppas fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod certificate            
        ski = getSKI certificate
        serial = getSerial certificate
        child = CaChild (CaShortcut {..}) serial
    in MftEntry {..}

makeRoaShortcut :: ObjectKey -> Validated RoaObject -> [Vrp] -> Text -> MftEntry
makeRoaShortcut key (Validated roa) vrps fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod roa    
        serial = getSerial roa
        child = RoaChild (RoaShortcut {..}) serial
    in MftEntry {..}    

makeAspaShortcut :: ObjectKey -> Validated AspaObject -> Aspa -> Text -> MftEntry
makeAspaShortcut key (Validated aspaObject) aspa fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod aspaObject            
        serial = getSerial aspaObject
        child = AspaChild (AspaShortcut {..}) serial
    in MftEntry {..}    

makeGbrShortcut :: ObjectKey -> Validated GbrObject -> (Hash, Gbr) -> Text -> MftEntry
makeGbrShortcut key (Validated gbrObject) gbr fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod gbrObject    
        serial = getSerial gbrObject
        child = GbrChild (GbrShortcut {..}) serial       
    in MftEntry {..}    


makeBgpSecShortcut :: ObjectKey -> Validated BgpCerObject -> BGPSecPayload -> Text -> MftEntry
makeBgpSecShortcut key (Validated bgpCert) bgpSec fileName = let         
        (notValidBefore, notValidAfter) = getValidityPeriod bgpCert                  
        serial = getSerial bgpCert
        child = BgpSecChild (BgpSecShortcut {..}) serial
    in MftEntry {..}    


makeMftShortcut :: ObjectKey -> Validated MftObject -> [(ObjectKey, MftEntry)] -> MftShortcut
makeMftShortcut key (Validated mftObject) children = let         
        (notValidBefore, notValidAfter) = getValidityPeriod mftObject      
        -- TODO 
        entries = Map.fromList children
    in MftShortcut { .. }

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


addMftShortcut :: MonadIO m => TopDownContext -> MftShortcut -> m ()
addMftShortcut TopDownContext { allTas = AllTasTopDownContext {..} } mftShortcut = liftIO $ 
    -- TODO 
    pure ()


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

visitKey :: MonadIO m => TopDownContext -> ObjectKey -> m ()
visitKey TopDownContext { allTas = AllTasTopDownContext {..} } k =
    liftIO $ atomically $ modifyTVar' visitedKeys (Set.insert k)

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
oneMoreBrief = updateMetric @ValidationMetric @_ (& #validBriefNumber %~ (+1))

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


extractPPAs :: Ca -> Either ValidationError PublicationPointAccess
extractPPAs = \case 
    CaShort (CaShortcut {..}) -> Right ppas 
    CaFull c                  -> getPublicationPointsFromCertObject c


data ManifestValidity e r v = InvalidEntry e v 
                            | InvalidChild e v ObjectKey 
                            | Valid r v (Text -> MftEntry) ObjectKey

manifestEntryVs :: ManifestValidity e r v -> v
manifestEntryVs = \case 
    InvalidEntry _ vs   -> vs
    InvalidChild _ vs _ -> vs
    Valid _ vs _ _      -> vs


makeTroubledChild :: ObjectKey -> Text -> MftEntry
makeTroubledChild childKey fileName = 
    MftEntry { child = TroubledChild childKey, .. }     



newtype Once m a = Once (TVar (OnceState m a)) 

data OnceState m a = EmptyO (m a) 
                    | WaitO 
                    | ReadyO a

once :: MonadIO m => m a -> m (Once m a)
once f = liftIO $ do 
    z <- newTVarIO (Empty f)
    pure $ Once z

compute :: MonadIO m => Once m a -> m a 
compute (Once s) =  
    join $ liftIO $ atomically $ do 
        readTVar s >>= \case 
            WaitO    -> retry
            ReadyO a -> pure $ pure a            
            EmptyO f -> do 
                writeTVar s WaitO
                pure $ do 
                    a <- f
                    liftIO $ atomically $ writeTVar s $ ReadyO a
                    pure a

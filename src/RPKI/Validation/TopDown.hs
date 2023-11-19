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
import           Control.Monad.Trans.Maybe

import           Control.Lens
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)

import           Data.Either                      (fromRight, partitionEithers)
import           Data.Foldable
import           Data.IORef
import           Data.Maybe
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
import           RPKI.Parallel
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
        briefs               :: TVar [BriefUpdate],
        shortcutQueue        :: ClosableQueue MftShortcutOp
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
                        -> RepositoryProcessing
                        -> ClosableQueue MftShortcutOp
                        -> m AllTasTopDownContext
newAllTasTopDownContext worldVersion repositoryProcessing shortcutQueue = do 
    let now = Now $ versionToMoment worldVersion
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

    fst <$> bracketChanClosable
                1000
                validateMutlipleTAs'
                (storeShortcuts appContext worldVersion)
                (\_ -> pure ())

  where
    validateMutlipleTAs' queue = do 
        withRepositoriesProcessing appContext $ \repositoryProcessing -> do 
            allTas <- newAllTasTopDownContext worldVersion repositoryProcessing queue
            resetForAsyncFetch repositoryProcessing
            validateThem allTas
                `finally` (applyValidationSideEffects appContext allTas)                    

    
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
            inSubVScope' LocationFocus (getURL $ getTaCertURL tal) $ do
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
            ValidatorT IO PayloadsType
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
                        ValidatorT IO PayloadsType
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

    validateThisCertAndGoDown :: ValidatorT IO PayloadsType
    validateThisCertAndGoDown = do    
        aki <- case ca of 
            CaFull c -> 
                inSubVScope' LocationFocus (getURL $ pickLocation $ getLocations c) $ do
                    validateObjectLocations c
                    pure $ toAKI $ getSKI c
            CaShort c -> do                 
                validateLocationForShortcut $ c ^. #key
                pure $ toAKI $ c ^. #ski

        z :: PayloadsType <- join $ makeNextAction aki
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
                                    T2 payloads _ <- manifestFullValidation caFull mft Nothing                                    
                                    oneMoreMft
                                    visitKey topDownContext (mft ^. typed)
                                    pure $! payloads

                    Just mftShortcut -> do                         
                        -- Find the key of the latest real manifest
                        findLatestMftKeyByAKI tx db childrenAki >>= \case 
                            Nothing -> pure $ do                                 
                                -- That is really weird and should normally never happen. 
                                -- Do not interrupt validation here, but complain in the log
                                vWarn $ NoMFTButCachedMft childrenAki
                                let mftShortKey = mftShortcut ^. #key
                                let message = [i|Internal error, there is a manifest shortcut, but no manifest for the key #{mftShortKey}.|]
                                logError logger message                                
                                collectPayloads mftShortcut Nothing 
                                    (getFullCa appContext ca)
                                    -- getCrlByKey is the best we can have
                                    (getCrlByKey appContext (mftShortcut ^. #crlKey))                                    
                                `andThen` 
                                    (oneMoreMft >> visitKey topDownContext (mftShortcut ^. #key))

                            Just mftKey 
                                | mftShortcut ^. #key == mftKey -> 
                                    -- Nothing has changed, the real manifest is the 
                                    -- same as the shortcut, so use the shortcut
                                    pure $ collectPayloads mftShortcut Nothing 
                                                (getFullCa appContext ca)
                                                (getCrlByKey appContext (mftShortcut ^. #crlKey))
                                            `andThen` 
                                                (oneMoreMft >> visitKey topDownContext mftKey)
                                | otherwise ->                                     
                                    getMftByKey tx db mftKey >>= \case 
                                        Nothing -> pure $ 
                                            internalError appContext [i|Internal error, can't find a manifest by its key #{mftKey}.|]
                                        Just mft -> pure $ do                                            
                                            fullCa <- getFullCa appContext ca
                                            T2 r1 overlappingChildren <- manifestFullValidation fullCa mft (Just mftShortcut)
                                            r2 <- collectPayloads mftShortcut (Just overlappingChildren) 
                                                        (pure fullCa)
                                                        (findAndValidateCrl fullCa mft)
                                            for_ [mft ^. typed, mftKey] $ visitKey topDownContext
                                            (pure $! r1 <> r2) 
                                                 `andThen`
                                                    oneMoreMft
          where

            -- Proceed with full validation for children mentioned in the full manifest 
            -- and children mentioned in the manifest shortcut. Create a diff between them,
            -- run full validation only for new children and create a new manifest shortcut
            -- with updated set children.
            manifestFullValidation :: 
                            Located CaCerObject
                            -> Keyed (Located MftObject) 
                            -> Maybe MftShortcut 
                            -> ValidatorT IO (T2 PayloadsType [T3 Text Hash ObjectKey]) 
            manifestFullValidation fullCa keyedMft mftShortcut = do 
                let (Keyed locatedMft@(Located locations mft) mftKey) = keyedMft

                inSubVScope' LocationFocus (getURL $ pickLocation locations) $ do 
                    vks <- liftIO $ readTVarIO visitedKeys
                    when (mftKey `Set.member` vks) $
                        -- We have already visited this manifest before, so 
                        -- there're some circular references in the objects.
                        -- 
                        -- NOTE: Limit cycle detection only to manifests
                        -- to minimise the false positives where the same object
                        -- is referenced from multiple manifests and we are treating 
                        -- it as a cycle. It has happened im the past.
                        vError $ CircularReference (getHash locatedMft)

                    -- General location validation
                    validateObjectLocations locatedMft

                    -- Manifest-specific location validation
                    validateMftLocation locatedMft fullCa

                    -- TODO Add fiddling with shortcut version of CRL here                    
                    keyedValidCrl@(Keyed validCrl crlKey) <- findAndValidateCrl fullCa keyedMft
                    oneMoreCrl >> visitKey topDownContext crlKey

                    -- MFT can be revoked by the CRL that is on this MFT -- detect 
                    -- revocation as well, this is clearly and error                               
                    validMft <- vHoist $ validateMft now mft fullCa validCrl verifiedResources

                    -- Validate entry list and filter out CRL itself
                    nonCrlChildren <- validateMftEntries mft (getHash validCrl)

                    -- If MFT shortcut is present, filter children that need validation, 
                    -- children that are already on the shortcut are validated and can be 
                    -- skipped here
                    let (newChildren, overlappingChildren, somethingDeleted) =
                            case mftShortcut of 
                                Nothing       -> (nonCrlChildren, [], False)
                                Just mftShort -> mftDiff mftShort nonCrlChildren

                    -- If CRL has changed, we have to recheck if all the children are 
                    -- not revoked. If will be checked by full validation for newChildren
                    -- but it needs to be explicitly checked for overlappingChildren
                    forM_ mftShortcut $ \mftShortcut -> 
                        checkNoChildrenAreRevoked mftShortcut overlappingChildren keyedValidCrl

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
                            T2 validatedPayloads childrenShortcuts <- 
                                    (gatherMftEntryResults =<< gatherMftEntryValidations fullCa newChildren validCrl)
                                                    
                            let newEntries = makeEntriesWithMap 
                                    newChildren (Map.fromList childrenShortcuts)
                                    (\entry fileName -> entry fileName)

                            let nextChildrenShortcuts = 
                                    case mftShortcut of 
                                        Nothing               -> newEntries
                                        Just MftShortcut {..} -> 
                                            newEntries <> makeEntriesWithMap 
                                                overlappingChildren nonCrlEntries (\entry _ -> entry)
                                        
                            let nextMftShortcut = makeMftShortcut mftKey validMft nextChildrenShortcuts keyedValidCrl

                            let aki = toAKI $ getSKI fullCa
                            when (maybe True ((/= mftKey) . (^. #key)) mftShortcut) $  
                                addMftShortcut topDownContext 
                                    (UpdateMftShortcut aki nextMftShortcut)

                            -- Update manifest shortcut children in case there are new 
                            -- or deleted children in the new manifest
                            when (not (null newChildren) || somethingDeleted)  $
                                addMftShortcut topDownContext 
                                    (UpdateMftShortcutChildren aki nextMftShortcut)

                            pure $! T2 validatedPayloads overlappingChildren

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

            findAndValidateCrl :: Located CaCerObject 
                            -> (Keyed (Located MftObject)) 
                            -> ValidatorT IO (Keyed (Validated CrlObject))
            findAndValidateCrl fullCa (Keyed (Located _ mft) _) = do  
                Keyed locatedCrl@(Located crlLocations (CrlRO crl)) crlKey <- findCrl mft
                visitObject topDownContext locatedCrl                
                inSubLocationScope (getURL $ pickLocation crlLocations) $ do 
                    validateObjectLocations locatedCrl
                    vHoist $ do
                        let mftEECert = getEECert $ unCMS $ cmsPayload mft
                        checkCrlLocation locatedCrl mftEECert
                        validateCrl now crl fullCa                
                        pure $! Keyed (Validated crl) crlKey

            -- Calculate difference bentween a manifest shortcut 
            -- and a real manifest object.
            mftDiff MftShortcut {..} (newMftChidlren :: [T3 Text a ObjectKey]) =                
                (newOnes, overlapping, not $ Map.null $ deletedEntries nonCrlEntries)
              where
                (newOnes, overlapping) = List.partition (\(T3 fileName _  k) -> 
                        isNewEntry k fileName nonCrlEntries) newMftChidlren                

                -- it's not in the map of shortcut children or it has changed 
                -- its name (very unlikely but can happen in theory)                
                isNewEntry key fileName shortcutChildren = 
                    case Map.lookup key shortcutChildren of
                        Nothing -> True
                        Just e  -> e ^. #fileName /= fileName 
                                
                deletedEntries shortcutChildren = 
                    -- If we delete everything from shortcutChildren that is present in newMftChidlren
                    -- we only have the entries left that were deleted from shortcutChildren
                    foldr (\(T3 fileName _ key) shortcutChildren -> 
                            case Map.lookup key shortcutChildren of 
                                Nothing -> shortcutChildren
                                Just e 
                                    | e ^. #fileName == fileName -> Map.delete key shortcutChildren
                                    | otherwise -> shortcutChildren) 
                            nonCrlEntries
                            newMftChidlren
                

            -- Utility for repeated peace of code
            makeEntriesWithMap childrenList entryMap makeEntry = 
                [ (key, makeEntry entry fileName) 
                        | (key, fileName, Just entry) <- [ (key, fileName, Map.lookup key entryMap) 
                        | (T3 fileName _ key) <- childrenList ]]


            -- Check if shortcut children are revoked
            checkNoChildrenAreRevoked MftShortcut {..} children (Keyed validCrl newCrlKey) = 
                when (newCrlKey /= crlKey) $ do                        
                    forM_ children $ \(T3 _ _ key) ->
                        for_ (Map.lookup key nonCrlEntries) $ \MftEntry {..} ->
                            for_ (getMftChildSerial child) $ \serial ->
                                when (isRevoked serial validCrl) $
                                    inSubVScope' ObjectFocus key $                                       
                                        vWarn RevokedResourceCertificate   


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
                    Left e                           -> InvalidEntry e vs
                    Right (T2 payload childShortcut) -> Valid payload vs childShortcut key    
    
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
                                Left e                           -> InvalidChild e vs' key
                                Right (T2 payload childShortcut) -> Valid payload vs' childShortcut key
    
    gatherMftEntryResults :: [ManifestValidity AppError PayloadsType ValidationState] 
                          -> ValidatorT IO (T2 PayloadsType [(ObjectKey, Text -> MftEntry)])
    gatherMftEntryResults =        
        foldM (\(T2 (payloads :: PayloadsType) childrenShortcuts) r -> do 
                embedState $ manifestEntryVs r
                case r of 
                    InvalidEntry e _ -> 
                        appError e
                    InvalidChild _ vs key -> 
                        pure $! T2 payloads ((key, makeTroubledChild key) : childrenShortcuts)
                    Valid (payload :: PayloadsType) vs childShortcut key -> do 
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

    validateMftChild caFull child@(Keyed (Located objectLocations _) _) filename validCrl = do
        -- warn about names on the manifest mismatching names in the object URLs        
        let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $ 
                            unLocations objectLocations
        when (null nameMatches) $
            vWarn $ ManifestLocationMismatch filename objectLocations

        validateChildObject caFull child validCrl


    -- Optimised version of location validation when all we have is a key of the CA
    -- 
    validateLocationForShortcut key = do  
        -- TODO Measure how much it costs and if it's noticeably costly 
        -- validate them all after the main traverse
        count <- roTxT database $ \tx db -> getLocationCountByKey tx db key
        when (count > 1) $ do 
            z <- roTxT database $ \tx db -> getLocationsByKey tx db key
            case z of 
                Nothing -> 
                    -- That's weird and it means DB inconsitency                                
                    internalError appContext 
                        [i|Internal error, can't find locations for the object #{key} with positive location count #{count}.|]
                Just locations -> 
                    inSubVScope' LocationFocus (getURL $ pickLocation locations) $
                        validateObjectLocations locations


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
                        inSubLocationScope (getURL $ pickLocation locations) $ do
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

            RoaRO roa -> 
                inSubVScope' LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child                    
                    allowRevoked childKey $ do
                        validRoa <- vHoist $ validateRoa now roa fullCa validCrl verifiedResources
                        let vrpList = getCMSContent $ cmsPayload roa
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ length vrpList
                        let payload = emptyPayloads & #vrps .~ Set.fromList vrpList                   
                        let z :: PayloadsType = Seq.singleton payload
                        pure $! T2 z (makeRoaShortcut childKey validRoa vrpList) 


            GbrRO gbr ->                 
                inSubVScope' LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child                    
                    allowRevoked childKey $ do
                        validGbr <- vHoist $ validateGbr now gbr fullCa validCrl verifiedResources
                        oneMoreGbr
                        let gbr' = getCMSContent $ cmsPayload gbr
                        let gbrPayload = (getHash gbr, gbr')
                        let payload = emptyPayloads & #gbrs .~ Set.singleton gbrPayload
                        pure $! T2 (Seq.singleton payload) (makeGbrShortcut childKey validGbr gbrPayload) 
                        

            AspaRO aspa -> 
                inSubVScope' LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child                    
                    allowRevoked childKey $ do
                        validAspa <- vHoist $ validateAspa now aspa fullCa validCrl verifiedResources
                        oneMoreAspa
                        let aspaPayload = getCMSContent $ cmsPayload aspa
                        let payload = emptyPayloads & #aspas .~ Set.singleton aspaPayload
                        pure $! T2 (Seq.singleton payload) (makeAspaShortcut childKey validAspa aspaPayload) 

            BgpRO bgpCert ->
                inSubVScope' LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child
                    allowRevoked childKey $ do
                        (validaBgpCert, bgpPayload) <- vHoist $ validateBgpCert now bgpCert fullCa validCrl
                        oneMoreBgp
                        let payload = emptyPayloads & #bgpCerts .~ Set.singleton bgpPayload
                        pure $! T2 (Seq.singleton payload) (makeBgpSecShortcut childKey validaBgpCert bgpPayload) 


            -- Any new type of object should be added here, otherwise
            -- they will emit a warning.
            _somethingElse -> do
                logWarn logger [i|Unsupported type of object: #{locations}.|]                
                pure $! T2 mempty (makeTroubledChild childKey)

        where

            -- In case of RevokedResourceCertificate error, the whole manifest is not to be considered 
            -- invalid, only the object with the revoked certificate is considered invalid.
            -- Replace RevokedResourceCertificate error with a warmning and don't break the 
            -- validation process.            
            -- This is a hacky and ad-hoc, but it works fine.
            allowRevoked childKey f =
                catchAndEraseError f isRevokedCertError $ do
                    vWarn RevokedResourceCertificate
                    pure $! T2 mempty (makeTroubledChild childKey)
                where
                    isRevokedCertError (ValidationE RevokedResourceCertificate) = True
                    isRevokedCertError _ = False


    collectPayloads :: MftShortcut 
                -> Maybe [T3 Text Hash ObjectKey] 
                -> ValidatorT IO (Located CaCerObject)
                -> ValidatorT IO (Keyed (Validated CrlObject))
                -> ValidatorT IO PayloadsType 
    collectPayloads mftShortcut@MftShortcut {..} maybeChildren findFullCa findValidCrl = do      

        let troubled = [ () | (_, MftEntry { child = TroubledChild _} ) <- Map.toList nonCrlEntries ]

        -- For children that are invalid we'll have to fall back to full validation, 
        -- for that we beed the parent CA and a valid CRL.
        troubledValidation <-
                case [ () | (_, MftEntry { child = TroubledChild _} ) <- Map.toList nonCrlEntries ] of 
                    [] -> 
                        -- Should never happen
                        pure $ \_ -> errorOnTroubledChild
                    _  -> do 
                        caFull   <- findFullCa
                        validCrl <- findValidCrl
                        pure $ \childKey -> validateTroubledChild caFull validCrl childKey

        -- Filter children that we actually want to go through here
        let filteredChildren = 
                case maybeChildren of 
                    Nothing -> Map.toList nonCrlEntries
                    Just ch -> catMaybes [ (k,) <$> Map.lookup k nonCrlEntries | T3 _ _ k <- ch ]
    
        collectResultsInParallel filteredChildren (getChildPayloads troubledValidation)

      where
        collectResultsInParallel children f = do 
            scopes <- askScopes
            z <- liftIO $ pooledForConcurrently children (runValidatorT scopes . f)
            foldM (\payloads (r, vs) ->
                    case r of 
                        Left e   -> appError e
                        Right r' -> do 
                            embedState vs
                            pure $! payloads <> r'
                ) mempty z

        errorOnTroubledChild = internalError appContext [i|Impossible happened!|]

        validateTroubledChild caFull (Keyed validCrl _) childKey = do  
            -- It was an invalid child and nothing about it is cached, so 
            -- we have to process full validation for i
            z <- roTxT database $ \tx db -> getLocatedByKey tx db childKey
            case z of 
                Nothing          -> internalError appContext [i|Internal error, can't find a troubled child by its key #{childKey}.|]
                Just childObject -> do 
                    T2 payloads _ <- validateChildObject caFull (Keyed childObject childKey) validCrl
                    pure $! payloads

        getChildPayloads troubledValidation (_, MftEntry {..}) = do 
            let emptyPayloads = mempty :: Payloads (Set Vrp)
            inSubVScope' ObjectFocus key $
                case child of 
                    CaChild caShortcut _ -> 
                        validateCaNoLimitChecks appContext topDownContext (CaShort caShortcut)

                    RoaChild r@RoaShortcut {..} _ -> do 
                        vHoist $ validateObjectValidityPeriod r now
                        validateLocationForShortcut key
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ length vrps                            
                        pure $! Seq.singleton $! emptyPayloads & #vrps .~ Set.fromList vrps
                        
                    AspaChild a@AspaShortcut {..} _ -> do  
                        vHoist $ validateObjectValidityPeriod a now
                        validateLocationForShortcut key
                        oneMoreAspa
                        pure $! Seq.singleton $! emptyPayloads & #aspas .~ Set.singleton aspa

                    BgpSecChild b@BgpSecShortcut {..} _ -> do 
                        vHoist $ validateObjectValidityPeriod b now
                        validateLocationForShortcut key
                        oneMoreBgp
                        pure $! Seq.singleton $! emptyPayloads & #bgpCerts .~ Set.singleton bgpSec

                    GbrChild g@GbrShortcut {..} _ -> do
                        vHoist $ validateObjectValidityPeriod g now         
                        validateLocationForShortcut key
                        oneMoreGbr                   
                        pure $! Seq.singleton $! emptyPayloads & #gbrs .~ Set.singleton gbr

                    TroubledChild childKey -> troubledValidation childKey  


getFullCa appContext@AppContext {..} ca = 
    case ca of     
        CaFull c  -> pure c            
        CaShort CaShortcut {..} -> do   
            z <- roTxT database $ \tx db -> getLocatedByKey tx db key
            case z of 
                Just (Located loc (CerRO c)) -> pure $! Located loc c
                _ -> internalError appContext [i|Internal error, can't find a CA by its key #{key}.|]
    
getCrlByKey appContext@AppContext {..} crlKey = do    
    z <- roTxT database $ \tx db -> getObjectByKey tx db crlKey
    case z of 
        Just (CrlRO c) -> pure $! Keyed (Validated c) crlKey 
        _ -> internalError appContext [i|Internal error, can't find a CRL by its key #{crlKey}.|]
    
internalError AppContext {..} message = do     
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


makeMftShortcut :: ObjectKey 
                -> Validated MftObject -> [(ObjectKey, MftEntry)] 
                -> Keyed (Validated CrlObject) 
                -> MftShortcut   
makeMftShortcut key (Validated mftObject) (Map.fromList -> nonCrlEntries) (Keyed validCrl crlKey) = let         
        (notValidBefore, notValidAfter) = getValidityPeriod mftObject                              
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


addMftShortcut :: MonadIO m => TopDownContext -> MftShortcutOp -> m ()
addMftShortcut TopDownContext { allTas = AllTasTopDownContext {..} } mftShortcutOp = 
    liftIO $ atomically $ writeCQueue shortcutQueue  mftShortcutOp
    

data MftShortcutOp = UpdateMftShortcut AKI MftShortcut
                   | UpdateMftShortcutChildren AKI MftShortcut

storeShortcuts :: (Storage s, MonadIO m) => AppContext s 
            -> WorldVersion 
            -> ClosableQueue MftShortcutOp -> m ()
storeShortcuts AppContext {..} worldVersion shortcutQueue = liftIO $ do 
    readQueueChunked shortcutQueue 100 $ \mftShortcuts -> 
        rwTxT database $ \tx db -> 
            for_ mftShortcuts $ \case 
                UpdateMftShortcut aki s         -> saveMftShorcutMeta tx db aki s
                UpdateMftShortcutChildren aki s -> saveMftShorcutChildren tx db aki s
            

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
    CaFull c                  -> getPublicationPointsFromCertObject $ c ^. #payload


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
    z <- newTVarIO (EmptyO f)
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

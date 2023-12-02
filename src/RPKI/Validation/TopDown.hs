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
import           Control.Monad.Reader

import           Control.Lens hiding (children)
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics (Generic)

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

import           UnliftIO.Async                   (pooledForConcurrently, pooledForConcurrentlyN)

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
import           RPKI.Store.Base.Storable
import           RPKI.Store.Database
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                        (fmtEx, fmtLocations, increment)
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
        visitedKeys          :: TVar (Set ObjectKey),        
        repositoryProcessing :: RepositoryProcessing,
        shortcutQueue        :: ClosableQueue MftShortcutOp,
        shortcutsCreated     :: TVar [MftShortcutOp],
        topDownCounters      :: TopDownCounters
    }
    deriving stock (Generic)


data TopDownCounters = TopDownCounters {
        originalCa :: IORef Int,
        shortcutCa :: IORef Int,
        originalMft :: IORef Int,
        shortcutMft :: IORef Int,
        originalCrl :: IORef Int,
        shortcutCrl :: IORef Int,
        newChildren :: IORef Int,
        overlappingChildren :: IORef Int,
        updateMftMeta       :: IORef Int,
        updateMftChildren   :: IORef Int,

        originalRoa    :: IORef Int,
        originalAspa   :: IORef Int,        

        shortcutRoa    :: IORef Int,
        shortcutAspa   :: IORef Int,        

        shortcutTroubled :: IORef Int
    }
    deriving stock (Generic)

data Limited = CanProceed | FirstToHitLimit | AlreadyReportedLimit
    deriving stock (Show, Eq, Ord, Generic)

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
newAllTasTopDownContext worldVersion repositoryProcessing shortcutQueue = liftIO $ do 
    let now = Now $ versionToMoment worldVersion
    topDownCounters <- newTopDownCounters
    atomically $ do        
        visitedKeys    <- newTVar mempty
        validManifests <- newTVar makeValidManifests        
        shortcutsCreated <- newTVar []
        pure $ AllTasTopDownContext {..}

newTopDownCounters :: IO TopDownCounters
newTopDownCounters = do 
    originalCa <- newIORef 0
    shortcutCa <- newIORef 0
    originalMft <- newIORef 0
    shortcutMft <- newIORef 0
    originalCrl <- newIORef 0
    shortcutCrl <- newIORef 0
    newChildren <- newIORef 0
    overlappingChildren <- newIORef 0
    updateMftMeta     <- newIORef 0
    updateMftChildren <- newIORef 0    

    shortcutRoa  <- newIORef 0        
    shortcutAspa <- newIORef 0            
    shortcutTroubled <- newIORef 0        

    originalRoa  <- newIORef 0        
    originalAspa <- newIORef 0                  
   
    pure TopDownCounters {..}

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
                10000
                validateMutlipleTAs'
                (storeShortcuts appContext)
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
            vFocusOn LocationFocus (getURL $ getTaCertURL tal) $ do
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
            (> validationConfig ^. #maxTotalTreeSize) . Set.size <$> readTVar visitedKeys,
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
        topDownContext@TopDownContext { allTas = AllTasTopDownContext {..} }
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
                    pps <- readPublicationPoints repositoryProcessing      
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
                            ppsAfterFetch <- readPublicationPoints repositoryProcessing
                            markForAsyncFetch repositoryProcessing 
                                $ filterForAsyncFetch (getFetchablePPA ppsAfterFetch filteredPPAccess) fetches slowRepos

                            -- primaryUrl is used for set the focus to the publication point                            
                            case getPrimaryRepositoryFromPP ppsAfterFetch filteredPPAccess of
                                Nothing         -> validateThisCertAndGoDown                                            
                                Just primaryUrl -> metricFocusOn PPFocus primaryUrl validateThisCertAndGoDown
  where
    validateThisCertAndGoDown = validateCaNoFetch appContext topDownContext ca


validateCaNoFetch :: Storage s =>
                    AppContext s 
                -> TopDownContext 
                -> Ca 
                -> ValidatorT IO PayloadsType
validateCaNoFetch
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    ca = do 
    
    aki <- case ca of 
        CaFull c -> do 
            increment $ topDownCounters ^. #originalCa 
            markAsReadHash appContext topDownContext (getHash c) 
            vFocusOn LocationFocus (getURL $ pickLocation $ getLocations c) $ do
                validateObjectLocations c
                vHoist $ validateObjectValidityPeriod c now
                pure $ toAKI $ getSKI c
        CaShort c -> do
            increment $ topDownCounters ^. #shortcutCa 
            markAsRead topDownContext (c ^. #key) 
            validateLocationForShortcut (c ^. #key)
            vHoist $ validateObjectValidityPeriod c now
            pure $ toAKI $ c ^. #ski
    
    let nextAction =
            case config ^. typed @ValidationConfig . typed @ValidationAlgorithm of 
                FullEveryIteration -> makeNextFullValidationAction
                Incremental        -> makeNextIncrementalAction
        
    z <- join $ nextAction aki
    oneMoreCert         
    pure z

  where
    -- Do not create shortctus when validation algorithm is not incremental
    createPayloadAndShortcut = 
        case config ^. typed @ValidationConfig . typed @ValidationAlgorithm of 
            FullEveryIteration -> \payload _        -> T2 payload Nothing
            Incremental        -> \payload shortcut -> T2 payload (Just $! shortcut)               

    makeNextFullValidationAction :: AKI -> ValidatorT IO (ValidatorT IO PayloadsType)
    makeNextFullValidationAction childrenAki = do 
        roTxT database $ \tx db -> do 
            findLatestMftByAKI tx db childrenAki >>= \case 
                Nothing ->                        
                    pure $ vError $ NoMFT childrenAki
                Just mft ->                        
                    pure $ do                      
                        markAsRead topDownContext (mft ^. typed)                
                        caFull <- getFullCa appContext ca
                        T2 payloads _ <- manifestFullValidation caFull mft Nothing childrenAki                                   
                        oneMoreMft >> oneMoreCrl                            
                        pure $! payloads


    makeNextIncrementalAction :: AKI -> ValidatorT IO (ValidatorT IO PayloadsType)
    makeNextIncrementalAction childrenAki = 
        roTxT database $ \tx db -> do 
            getMftShorcut tx db childrenAki >>= \case
                Nothing -> do 
                    findLatestMftByAKI tx db childrenAki >>= \case 
                        Nothing -> do 
                            -- No current manifest and not shortcut as well, bail out 
                            pure $ vError $ NoMFT childrenAki
                        Just mft -> do 
                            increment $ topDownCounters ^. #originalMft
                            -- No shortcut found, so do the full validation for the manifest
                            pure $ do                   
                                markAsRead topDownContext (mft ^. typed)                   
                                caFull <- getFullCa appContext ca
                                T2 payloads _ <- manifestFullValidation caFull mft Nothing childrenAki                                    
                                oneMoreMft >> oneMoreCrl                                    
                                pure $! payloads

                Just mftShortcut -> do
                    let mftShortKey = mftShortcut ^. #key
                    increment $ topDownCounters ^. #shortcutMft
                    -- logDebug logger [i|Found shortcut for AKI #{childrenAki}|]
                    -- Find the key of the latest real manifest
                    action <- findLatestMftKeyByAKI tx db childrenAki >>= \case 
                            Nothing -> pure $ do                                             
                                -- That is really weird and should normally never happen. 
                                -- Do not interrupt validation here, but complain in the log
                                vWarn $ NoMFTButCachedMft childrenAki                                    
                                let crlKey = mftShortcut ^. #crlShortcut . #key       
                                let message = [i|Internal error, there is a manifest shortcut, but no manifest for the key #{mftShortKey}.|]
                                logError logger message
                                collectPayloads mftShortcut Nothing 
                                    (getFullCa appContext ca)
                                    -- getCrlByKey is the best we can have
                                    (getCrlByKey appContext crlKey)
                                    `andThen` 
                                        (markAsRead topDownContext crlKey)

                            Just mftKey 
                                | mftShortKey == mftKey -> do
                                    -- logDebug logger [i|Option 1|]            
                                    -- Nothing has changed, the real manifest is the 
                                    -- same as the shortcut, so use the shortcut
                                    let crlKey = mftShortcut ^. #crlShortcut . #key                                                                        
                                    pure $ collectPayloads mftShortcut Nothing 
                                                (getFullCa appContext ca)
                                                (getCrlByKey appContext crlKey)
                                            `andThen` 
                                                (markAsRead topDownContext crlKey)

                                | otherwise -> do 
                                    -- logDebug logger [i|Option 2|]            
                                    getMftByKey tx db mftKey >>= \case 
                                        Nothing -> pure $ 
                                            internalError appContext [i|Internal error, can't find a manifest by its key #{mftKey}.|]
                                        Just mft -> pure $ do
                                            increment $ topDownCounters ^. #shortcutMft                      
                                            fullCa <- getFullCa appContext ca
                                            T2 r1 overlappingChildren <- manifestFullValidation fullCa mft (Just mftShortcut) childrenAki
                                            r2 <- collectPayloads mftShortcut (Just overlappingChildren) 
                                                        (pure fullCa)
                                                        (findAndValidateCrl fullCa mft childrenAki)
                                            markAsRead topDownContext mftKey
                                            pure $! r1 <> r2                                                     
                    pure $ do 
                        markAsRead topDownContext mftShortKey
                        action `andThen`
                            (oneMoreMft >> oneMoreCrl >> oneMoreMftShort)          

    -- Proceed with full validation for children mentioned in the full manifest 
    -- and children mentioned in the manifest shortcut. Create a diff between them,
    -- run full validation only for new children and create a new manifest shortcut
    -- with updated set children.
    manifestFullValidation :: 
                    Located CaCerObject
                    -> Keyed (Located MftObject) 
                    -> Maybe MftShortcut 
                    -> AKI
                    -> ValidatorT IO (T2 PayloadsType [T3 Text Hash ObjectKey]) 
    manifestFullValidation fullCa keyedMft mftShortcut childrenAki = do 
        let (Keyed locatedMft@(Located locations mft) mftKey) = keyedMft

        vFocusOn LocationFocus (getURL $ pickLocation locations) $ do 
            -- General location validation
            validateObjectLocations locatedMft

            -- Manifest-specific location validation
            validateMftLocation locatedMft fullCa

            -- TODO Add fiddling with shortcut version of CRL here                    
            keyedValidCrl@(Keyed validCrl crlKey) <- findAndValidateCrl fullCa keyedMft childrenAki                

            -- MFT can be revoked by the CRL that is on this MFT -- detect 
            -- revocation as well, this is clearly and error                               
            validMft <- vHoist $ validateMft now mft fullCa validCrl verifiedResources

            -- Validate entry list and filter out CRL itself
            nonCrlChildren <- validateMftEntries mft (getHash validCrl)

            -- If MFT shortcut is present, filter children that need validation, 
            -- children that are on the shortcut are already validated.
            let (newChildren, overlappingChildren, isSomethingDeleted) =
                    case mftShortcut of 
                        Nothing       -> (nonCrlChildren, [], False)
                        Just mftShort -> manifestDiff mftShort nonCrlChildren

            bumpCounterBy topDownCounters (#newChildren) (length newChildren)
            bumpCounterBy topDownCounters (#overlappingChildren) (length overlappingChildren)

            -- If CRL has changed, we have to recheck if children are not revoked. 
            -- If will be checked by full validation for newChildren but it needs 
            -- to be explicitly checked for overlappingChildren
            forM_ mftShortcut $ \mftShort -> 
                when (crlKey /= mftShort ^. #crlShortcut . #key) $
                    checkForRevokedChildren mftShort overlappingChildren keyedValidCrl            

            -- Mark all manifest entries as read to avoid the situation
            -- when some of the children are garbage-collected from the cache 
            -- and some are still there. Do it both in case of successful 
            -- validation or a validation error.
            let markAllEntriesAsVisited = do                             
                    forM_ (newChildren <> overlappingChildren) $ 
                        (\(T3 _ _ k) -> markAsRead topDownContext k)

            let processChildren = do                                              
                    -- Here we have the payloads for the fully validated MFT children
                    -- and the shortcut objects for these children                        
                    T2 validatedPayloads maybeChildrenShortcuts <- 
                            (gatherMftEntryResults =<< 
                                gatherMftEntryValidations fullCa newChildren validCrl)
                                            
                    let childrenShortcuts = [ (k, s) | (k, Just s) <- maybeChildrenShortcuts ]

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

                    case config ^. typed @ValidationConfig . typed @ValidationAlgorithm of 
                        FullEveryIteration -> pure ()
                        Incremental        -> do 
                            let aki = toAKI $ getSKI fullCa                        
                            when (maybe True ((/= mftKey) . (^. #key)) mftShortcut) $ do                                
                                updateMftShortcut topDownContext aki nextMftShortcut
                                increment $ topDownCounters ^. #updateMftMeta

                            -- Update manifest shortcut children in case there are new 
                            -- or deleted children in the new manifest
                            when (isNothing mftShortcut || not (null newChildren) || isSomethingDeleted) $ do
                                updateMftShortcutChildren topDownContext aki nextMftShortcut
                                increment $ topDownCounters ^. #updateMftChildren

                    pure $! T2 validatedPayloads overlappingChildren

            processChildren `recover` markAllEntriesAsVisited

    findCrl mft childrenAki = do 
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
                    -> AKI
                    -> ValidatorT IO (Keyed (Validated CrlObject))
    findAndValidateCrl fullCa (Keyed (Located _ mft) _) childrenAki = do  
        Keyed locatedCrl@(Located crlLocations (CrlRO crl)) crlKey <- findCrl mft childrenAki            
        markAsRead topDownContext crlKey
        inSubLocationScope (getURL $ pickLocation crlLocations) $ do 
            validateObjectLocations locatedCrl
            vHoist $ do
                let mftEECert = getEECert $ unCMS $ cmsPayload mft
                checkCrlLocation locatedCrl mftEECert
                void $ validateCrl now crl fullCa                
                pure $! Keyed (Validated crl) crlKey
            

    -- Utility for repeated peace of code
    makeEntriesWithMap childrenList entryMap makeEntry = 
        [ (key, makeEntry entry fileName) 
                | (key, fileName, Just entry) <- [ (key, fileName, Map.lookup key entryMap) 
                | (T3 fileName _ key) <- childrenList ]]


    -- Check if shortcut children are revoked
    checkForRevokedChildren MftShortcut {..} children (Keyed validCrl newCrlKey) = 
        when (newCrlKey /= crlShortcut ^. #key) $ do                        
            forM_ children $ \(T3 _ _ childKey) ->
                for_ (Map.lookup childKey nonCrlEntries) $ \MftEntry {..} ->
                    for_ (getMftChildSerial child) $ \serial ->
                        when (isRevoked serial validCrl) $
                            vFocusOn ObjectFocus childKey $                                       
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
                          -> ValidatorT IO (T2 PayloadsType [(ObjectKey, Maybe (Text -> MftEntry))])
    gatherMftEntryResults =        
        foldM (\(T2 (payloads :: PayloadsType) childrenShortcuts) r -> do                 
                case r of 
                    InvalidEntry e vs -> do
                        embedState vs
                        appError e
                    InvalidChild _ vs key -> do
                        embedState vs
                        pure $! T2 payloads ((key, Just (makeTroubledChild key)) : childrenShortcuts)
                    Valid (payload :: PayloadsType) vs childShortcut key -> do 
                        embedState vs
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

    getManifestEntry filename hash' = 
        roTxT database (\tx db -> getKeyedByHash tx db hash') >>= \case        
            Nothing -> vError $ ManifestEntryDoesn'tExist hash' filename
            Just ro -> pure ro

    validateMftChild caFull child@(Keyed (Located objectLocations _) _) 
                     filename validCrl = do
        -- warn about names on the manifest mismatching names in the object URLs        
        let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $ 
                            unLocations objectLocations
        when (null nameMatches) $
            vWarn $ ManifestLocationMismatch filename objectLocations

        validateChildObject caFull child validCrl


    -- Optimised version of location validation when all we have is a key of the CA
    -- 
    validateLocationForShortcut key = do  
        -- pure ()
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
                    vFocusOn LocationFocus (getURL $ pickLocation locations) $
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
            -> ValidatorT IO (T2 PayloadsType (Maybe (Text -> MftEntry)))
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
                    Left _        -> pure $! createPayloadAndShortcut mempty (makeTroubledChild childKey)
                    Right payload -> do 
                        case getPublicationPointsFromCertObject childCert of 
                            -- It's not going to happen?
                            Left e     -> vError e
                            Right ppas -> pure $! createPayloadAndShortcut 
                                payload (makeCaShortcut childKey (Validated childCert) ppas)
            RoaRO roa -> 
                vFocusOn LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child                    
                    allowRevoked $ do
                        validRoa <- vHoist $ validateRoa now roa fullCa validCrl verifiedResources
                        let vrpList = getCMSContent $ cmsPayload roa
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ length vrpList
                        let payload = emptyPayloads & #vrps .~ Set.fromList vrpList      
                        increment $ topDownCounters ^. #originalRoa 
                        pure $! createPayloadAndShortcut 
                                    (Seq.singleton payload) 
                                    (makeRoaShortcut childKey validRoa vrpList) 

            GbrRO gbr ->                 
                vFocusOn LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child                    
                    allowRevoked $ do
                        validGbr <- vHoist $ validateGbr now gbr fullCa validCrl verifiedResources
                        oneMoreGbr
                        let gbr' = getCMSContent $ cmsPayload gbr
                        let gbrPayload = (getHash gbr, gbr')
                        let payload = emptyPayloads & #gbrs .~ Set.singleton gbrPayload                        
                        pure $! createPayloadAndShortcut 
                                    (Seq.singleton payload) 
                                    (makeGbrShortcut childKey validGbr gbrPayload)                         

            AspaRO aspa -> 
                vFocusOn LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child                    
                    allowRevoked $ do
                        validAspa <- vHoist $ validateAspa now aspa fullCa validCrl verifiedResources
                        oneMoreAspa
                        let aspaPayload = getCMSContent $ cmsPayload aspa
                        let payload = emptyPayloads & #aspas .~ Set.singleton aspaPayload
                        increment $ topDownCounters ^. #originalAspa
                        pure $! createPayloadAndShortcut 
                                    (Seq.singleton payload) 
                                    (makeAspaShortcut childKey validAspa aspaPayload) 

            BgpRO bgpCert ->
                vFocusOn LocationFocus (getURL $ pickLocation locations) $ do
                    validateObjectLocations child
                    allowRevoked $ do
                        (validaBgpCert, bgpPayload) <- vHoist $ validateBgpCert now bgpCert fullCa validCrl
                        oneMoreBgp
                        let payload = emptyPayloads & #bgpCerts .~ Set.singleton bgpPayload
                        pure $! createPayloadAndShortcut 
                                    (Seq.singleton payload) 
                                    (makeBgpSecShortcut childKey validaBgpCert bgpPayload) 


            -- Any new type of object should be added here, otherwise
            -- they will emit a warning.
            _somethingElse -> do
                logWarn logger [i|Unsupported type of object: #{locations}.|]                
                pure $! createPayloadAndShortcut mempty (makeTroubledChild childKey)

        where

            -- In case of RevokedResourceCertificate error, the whole manifest is not to be considered 
            -- invalid, only the object with the revoked certificate is considered invalid.
            -- Replace RevokedResourceCertificate error with a warmning and don't break the 
            -- validation process.            
            -- This is a hacky and ad-hoc, but it works fine.
            allowRevoked f =
                catchAndEraseError f isRevokedCertError $ do
                    vWarn RevokedResourceCertificate
                    pure $! createPayloadAndShortcut mempty (makeTroubledChild childKey)
                where
                    isRevokedCertError (ValidationE RevokedResourceCertificate) = True
                    isRevokedCertError _ = False


    collectPayloads :: MftShortcut 
                -> Maybe [T3 Text Hash ObjectKey] 
                -> ValidatorT IO (Located CaCerObject)
                -> ValidatorT IO (Keyed (Validated CrlObject))             
                -> ValidatorT IO PayloadsType 
    collectPayloads mftShortcut maybeChildren findFullCa findValidCrl = do      
        
        let nonCrlEntries = mftShortcut ^. #nonCrlEntries
                
        vFocusOn ObjectFocus (mftShortcut ^. #key) $ do
            vHoist $ validateObjectValidityPeriod mftShortcut now
            vFocusOn ObjectFocus (mftShortcut ^. #crlShortcut . #key) $
                vHoist $ validateObjectValidityPeriod (mftShortcut ^. #crlShortcut) now

            -- For children that are invalid we'll have to fall back to full validation, 
            -- for that we beed the parent CA and a valid CRL.
            troubledValidation <-
                    case [ () | (_, MftEntry { child = TroubledChild _} ) <- Map.toList nonCrlEntries ] of 
                        [] ->                         
                            pure $ \_ -> 
                                -- Should never happen, there are no troubled children
                                errorOnTroubledChild
                        _  -> do 
                            caFull   <- findFullCa
                            validCrl <- findValidCrl
                            pure $! \childKey -> validateTroubledChild caFull validCrl childKey

            -- Filter children that we actually want to go through here
            let filteredChildren = 
                    case maybeChildren of 
                        Nothing -> Map.toList nonCrlEntries
                        Just ch -> catMaybes [ (k,) <$> Map.lookup k nonCrlEntries | T3 _ _ k <- ch ]

            -- Gather all PPAs from all Ca on the MftShortcut and fetch all of them beforehand to avoid
            -- a lot of STM transaction restarts in `fetchPPWithFallback`                
            caCount <- prefetchRepositories filteredChildren            

            collectResultsInParallel caCount filteredChildren (getChildPayloads troubledValidation)

      where
        collectResultsInParallel caCount children f = do 
            -- Pick up some good parallelism to avoid too many threads, 
            -- but also process big manifests quicker
            let caPerThread = 50            
            let eePerThread = 100
            let threads = min 
                    (caCount `div` caPerThread + (length children - caCount) `div` eePerThread)
                    (fromIntegral $ config ^. #parallelism . #cpuParallelism)            

            scopes <- askScopes
            let forAllChidlren = 
                    if threads <= 1
                        then forM
                        else pooledForConcurrentlyN threads
            z <- liftIO $ forAllChidlren children (runValidatorT scopes . f)
            embedState $ mconcat $ map snd z
            pure $! mconcat [ p | (Right p, _) <- z ]            

        errorOnTroubledChild = internalError appContext [i|Impossible happened!|]

        validateTroubledChild caFull (Keyed validCrl _) childKey = do  
            -- It was an invalid child and nothing about it is cached, so 
            -- we have to process full validation for it
            z <- roTxT database $ \tx db -> getLocatedByKey tx db childKey
            case z of 
                Nothing          -> internalError appContext [i|Internal error, can't find a troubled child by its key #{childKey}.|]
                Just childObject -> do 
                    T2 payloads _ <- validateChildObject caFull (Keyed childObject childKey) validCrl
                    pure $! payloads

        getChildPayloads troubledValidation (childKey, MftEntry {..}) = do 
            markAsRead topDownContext childKey
            let emptyPayloads = mempty :: Payloads (Set Vrp)
            pps <- readPublicationPoints repositoryProcessing            
            case child of 
                CaChild caShortcut _ -> do      
                    let validateCaShortcut = validateCaNoFetch appContext topDownContext (CaShort caShortcut)              
                    case filterPPAccess config (caShortcut ^. #ppas) of 
                        Nothing          -> validateCaShortcut
                        Just filteredPpa -> 
                            case getPrimaryRepositoryFromPP pps filteredPpa of 
                                Nothing         -> validateCaShortcut
                                Just primaryUrl -> metricFocusOn PPFocus primaryUrl validateCaShortcut
                        
                RoaChild r@RoaShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do 
                        vHoist $ validateObjectValidityPeriod r now
                        validateLocationForShortcut key
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ length vrps             
                        increment $ topDownCounters ^. #shortcutRoa               
                        pure $! Seq.singleton $! emptyPayloads & #vrps .~ Set.fromList vrps
                    
                AspaChild a@AspaShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do 
                        vHoist $ validateObjectValidityPeriod a now
                        validateLocationForShortcut key
                        oneMoreAspa
                        increment $ topDownCounters ^. #shortcutAspa
                        pure $! Seq.singleton $! emptyPayloads & #aspas .~ Set.singleton aspa

                BgpSecChild b@BgpSecShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do 
                        vHoist $ validateObjectValidityPeriod b now
                        validateLocationForShortcut key
                        oneMoreBgp
                        pure $! Seq.singleton $! emptyPayloads & #bgpCerts .~ Set.singleton bgpSec

                GbrChild g@GbrShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do 
                        vHoist $ validateObjectValidityPeriod g now         
                        validateLocationForShortcut key
                        oneMoreGbr                   
                        pure $! Seq.singleton $! emptyPayloads & #gbrs .~ Set.singleton gbr

                TroubledChild childKey_ -> 
                    vFocusOn ObjectFocus childKey $ do 
                        increment $ topDownCounters ^. #shortcutTroubled
                        troubledValidation childKey_


        prefetchRepositories children = do 
            let allPpas = [ ppas | (_, MftEntry { child = CaChild CaShortcut {..} _}) <- children ]

            -- Do all the necessary filtering based on fetch config and 
            -- sync(quick) and async(slow) repository statuses.
            pps <- readPublicationPoints repositoryProcessing
            let allSyncPpas =
                    foldr (\ppa syncPpas ->
                        case filterPPAccess config ppa of 
                            Nothing          -> syncPpas
                            Just filteredPpa -> 
                                let (quickPPs, _) = onlyForSyncFetch pps filteredPpa
                                in case quickPPs of 
                                    Nothing       -> syncPpas
                                    Just quickPpa -> quickPpa : syncPpas)
                        mempty 
                        allPpas
            
            let uniquePpas = getFetchablePPAs pps allSyncPpas

            unless (null uniquePpas) $
                logDebug logger [i|Will prefetch (if it's time): #{uniquePpas}|]

            scopes <- askScopes
            let fetchConfig = syncFetchConfig config
            z <- liftIO $ pooledForConcurrently uniquePpas $ \ppa -> 
                    fmap (ppa,) $ runValidatorT scopes $                     
                        fetchPPWithFallback appContext fetchConfig repositoryProcessing worldVersion ppa
                        
            ppsAfterFetch <- readPublicationPoints repositoryProcessing
            forM_ z $ \(ppa, (r, vs)) -> do 
                embedState vs
                for_ r $ \fetches -> do 
                    let (_, slowRepos) = onlyForSyncFetch ppsAfterFetch ppa
                    markForAsyncFetch repositoryProcessing 
                            $ filterForAsyncFetch ppa fetches slowRepos                   

            pure $! length allPpas


-- Calculate difference bentween a manifest shortcut 
-- and the list of children of the new manifest object.
manifestDiff :: MftShortcut 
            -> [T3 Text a ObjectKey] 
            -> ([T3 Text a ObjectKey], [T3 Text a ObjectKey], Bool)
manifestDiff mftShortcut newMftChidlren =                
    (newOnes, overlapping, not $ Map.null deletedEntries)
    where
    (newOnes, overlapping) = List.partition 
            (\(T3 fileName _  k) -> isNewEntry k fileName) newMftChidlren                

    -- it's not in the map of shortcut children or it has changed 
    -- its name (very unlikely but can happen in theory)                
    isNewEntry key_ fileName  = 
        case Map.lookup key_ (mftShortcut ^. #nonCrlEntries) of
            Nothing -> True
            Just e  -> e ^. #fileName /= fileName 
                    
    deletedEntries = 
        -- If we delete everything from mftShortcut.nonCrlEntries that is present in 
        -- newMftChidlren, we only have the entries that are not present on the new manifest
        foldr (\(T3 fileName _ key_) entries -> 
                case Map.lookup key_ (mftShortcut ^. #nonCrlEntries) of 
                    Nothing -> entries
                    Just e 
                        | e ^. #fileName == fileName -> Map.delete key_ entries
                        | otherwise -> entries) 
                (mftShortcut ^. #nonCrlEntries)
                newMftChidlren


getFullCa :: Storage s => AppContext s -> Ca -> ValidatorT IO (Located CaCerObject)
getFullCa appContext@AppContext {..} ca = 
    case ca of     
        CaFull c  -> pure c            
        CaShort CaShortcut {..} -> do   
            z <- roTxT database $ \tx db -> getLocatedByKey tx db key
            case z of 
                Just (Located loc (CerRO c)) -> pure $! Located loc c
                _ -> internalError appContext [i|Internal error, can't find a CA by its key #{key}.|]
    
getCrlByKey :: Storage s => AppContext s -> ObjectKey -> ValidatorT IO (Keyed (Validated CrlObject))
getCrlByKey appContext@AppContext {..} crlKey = do    
    z <- roTxT database $ \tx db -> getObjectByKey tx db crlKey
    case z of 
        Just (CrlRO c) -> pure $! Keyed (Validated c) crlKey 
        _ -> internalError appContext [i|Internal error, can't find a CRL by its key #{crlKey}.|]
    
internalError :: AppContext s -> Text -> ValidatorT IO a
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
makeMftShortcut key 
    (Validated mftObject) (Map.fromList -> nonCrlEntries) 
    (Keyed (Validated validCrl) crlKey) = 
  let 
    (notValidBefore, notValidAfter) = getValidityPeriod mftObject        
    crlShortcut = let 
        SignCRL {..} = validCrl ^. #signCrl
        -- That must always match, since it's a validated CRL
        Just nextUpdateTime' = nextUpdateTime            
        in CrlShortcut {
            key = crlKey,
            notValidBefore = thisUpdateTime,
            notValidAfter = nextUpdateTime'
        }    
    in MftShortcut { .. }  


-- Mark validated objects in the database, i.e.
-- 
-- - save all the visited hashes together with the current world version
-- - save all the valid manifests for each CA/AKI
-- 
applyValidationSideEffects :: (MonadIO m, Storage s) =>
                              AppContext s -> AllTasTopDownContext -> m ()
applyValidationSideEffects
    appContext@AppContext {..}
    AllTasTopDownContext {..} = liftIO $ do
    shortcutMetaUpdates     <- newIORef 0
    shortcutChildrenUpdates <- newIORef 0 
    ((visitedSize, validMftsSize), elapsed) <- timedMS $ do
            (vks, vmfts, shortcuts) <- atomically $ (,,) <$>
                                readTVar visitedKeys <*>                                
                                readTVar validManifests <*>
                                readTVar shortcutsCreated                                
            
            rwTxT database $ \tx db -> do
                markAsValidated tx db vks worldVersion
                saveLatestValidMfts tx db (vmfts ^. #valids)         
                for_ shortcuts $ \case 
                    UpdateMftShortcut aki s -> do 
                        saveMftShorcutMeta tx db aki s
                        increment shortcutMetaUpdates
                    UpdateMftShortcutChildren aki s -> do 
                        saveMftShorcutChildren tx db aki s
                        increment shortcutChildrenUpdates       
            
            pure (Set.size vks, Map.size (vmfts ^. #valids))

    sm :: Int <- readIORef shortcutMetaUpdates
    sc :: Int <- readIORef shortcutChildrenUpdates
    liftIO $ reportCounters appContext topDownCounters
    logDebug logger $ [i|Marked #{visitedSize} objects as used, updated #{sm} MFT shortcuts, |] <> 
                      [i|#{sc} MFT shortcut children, took #{elapsed}ms.|]


reportCounters AppContext {..} TopDownCounters {..} = do
    originalCaN <- readIORef originalCa
    shortcutCaN <- readIORef shortcutCa
    originalMftN <- readIORef originalMft
    shortcutMftN <- readIORef shortcutMft    
    originalCrlN <- readIORef originalCrl
    shortcutCrlN <- readIORef shortcutCrl

    newChildrenN         <- readIORef newChildren
    overlappingChildrenN <- readIORef overlappingChildren

    updateMftMetaN <- readIORef updateMftMeta
    updateMftChildrenN <- readIORef updateMftChildren

    shortcutRoaN <- readIORef shortcutRoa
    shortcutAspaN <- readIORef shortcutAspa
    shortcutTroubledN <- readIORef shortcutTroubled
                            
    originalRoaN <- readIORef originalRoa
    originalAspaN <- readIORef originalAspa    

    logDebug logger $ [i|Counters: originalCaN = #{originalCaN}, shortcutCaN = #{shortcutCaN}, |] <> 
                      [i|originalMftN = #{originalMftN}, shortcutMftN = #{shortcutMftN}, |] <>
                      [i|originalCrlN = #{originalCrlN}, shortcutCrlN = #{shortcutCrlN}, |] <>
                      [i|newChildrenN = #{newChildrenN}, overlappingChildrenN = #{overlappingChildrenN}, |] <>
                      [i|updateMftMetaN = #{updateMftMetaN}, updateMftChildrenN = #{updateMftChildrenN}, |] <>
                      [i|shortcutRoaN = #{shortcutRoaN}, , originalRoaN = #{originalRoaN}, |] <>
                      [i|shortcutAspaN = #{shortcutAspaN}, originalAspaN = #{originalAspaN}, |] <>
                      [i|shortcutTroubledN = #{shortcutTroubledN}|]
                      

updateMftShortcut :: MonadIO m => TopDownContext -> AKI -> MftShortcut -> m ()
updateMftShortcut TopDownContext { allTas = AllTasTopDownContext {..} } aki MftShortcut {..} = 
    liftIO $ do 
        let raw = Raw $ toStorable $ Compressed MftShortcutMeta {..}
        -- atomically $ writeCQueue shortcutQueue (UpdateMftShortcut aki raw)
        atomically $ modifyTVar' shortcutsCreated (UpdateMftShortcut aki raw :)

updateMftShortcutChildren :: MonadIO m => TopDownContext -> AKI -> MftShortcut -> m ()
updateMftShortcutChildren TopDownContext { allTas = AllTasTopDownContext {..} } aki MftShortcut {..} = 
    liftIO $ do 
        let !raw = Raw $ toStorable $ Compressed MftShortcutChildren {..}
        -- atomically $ writeCQueue shortcutQueue (UpdateMftShortcutChildren aki raw)        
        atomically $ modifyTVar' shortcutsCreated (UpdateMftShortcutChildren aki raw :)
    
storeShortcuts :: (Storage s, MonadIO m) => 
                AppContext s 
             -> ClosableQueue MftShortcutOp -> m ()
storeShortcuts AppContext {..} shortcutQueue = liftIO $ do
    pure ()
    -- shortcutMetaUpdates     <- newIORef 0
    -- shortcutChildrenUpdates <- newIORef 0 
    -- totalDbTime             <- newIORef 0 
    -- readQueueChunked shortcutQueue 2000 $ \mftShortcuts -> do 
    --     (_, ms) <- timedMS $ rwTxT database $ \tx db -> 
    --         for_ mftShortcuts $ \case 
    --             UpdateMftShortcut aki s -> do 
    --                 saveMftShorcutMeta tx db aki s
    --                 increment shortcutMetaUpdates
    --             UpdateMftShortcutChildren aki s -> do 
    --                 saveMftShorcutChildren tx db aki s
    --                 increment shortcutChildrenUpdates
    --     modifyIORef totalDbTime $ (<> ms)
    --     logDebug logger [i|Saved a batch of MFT shortcut metadata and children, took #{ms}ms.|]
    -- sm :: Int <- readIORef shortcutMetaUpdates
    -- sc :: Int <- readIORef shortcutChildrenUpdates
    -- ms <- readIORef totalDbTime
    -- when (sm > 0 || sc > 0) $
    --     logDebug logger [i|Created/updated #{sm} MFT shortcut metadata and #{sc} shortcut children, took #{ms}ms.|]
            

data MftShortcutOp = UpdateMftShortcut AKI (Raw (Compressed MftShortcutMeta))
                   | UpdateMftShortcutChildren AKI (Raw (Compressed MftShortcutChildren))            

-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
markAsRead :: TopDownContext -> ObjectKey -> ValidatorT IO ()
markAsRead TopDownContext { allTas = AllTasTopDownContext {..} } k = do 
    -- vks <- liftIO $ readTVarIO visitedKeys
    -- when (k `Set.member` vks) $
    --     -- We have already visited this object before, so 
    --     -- there're some circular references createed by the object.
    --     -- 
    --     -- NOTE: Limit cycle detection only to manifests
    --     -- to minimise the false positives where the same object
    --     -- is referenced from multiple manifests and we are treating 
    --     -- it as a cycle. It has happened im the past.
    --     vError $ CircularReference $ KeyIdentity k

    liftIO $ atomically $ modifyTVar' visitedKeys (Set.insert k)

markAsReadHash :: Storage s => 
                AppContext s -> TopDownContext -> Hash -> ValidatorT IO ()
markAsReadHash AppContext {..} topDownContext hash = do
    key <- roTxT database $ \tx db -> getKeyByHash tx db hash
    for_ key $ markAsRead topDownContext              

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl :: Monad m => ValidatorT m ()
oneMoreGbr, oneMoreAspa, oneMoreBgp :: Monad m => ValidatorT m ()
oneMoreMftShort :: Monad m => ValidatorT m ()
oneMoreCert = updateMetric @ValidationMetric @_ (& #validCertNumber %~ (+1))
oneMoreRoa  = updateMetric @ValidationMetric @_ (& #validRoaNumber %~ (+1))
oneMoreMft  = updateMetric @ValidationMetric @_ (& #validMftNumber %~ (+1))
oneMoreCrl  = updateMetric @ValidationMetric @_ (& #validCrlNumber %~ (+1))
oneMoreGbr  = updateMetric @ValidationMetric @_ (& #validGbrNumber %~ (+1))
oneMoreAspa = updateMetric @ValidationMetric @_ (& #validAspaNumber %~ (+1))
oneMoreBgp  = updateMetric @ValidationMetric @_ (& #validBgpNumber %~ (+1))
oneMoreMftShort = updateMetric @ValidationMetric @_ (& #mftShortcutNumber %~ (+1))

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
                            | Valid r v (Maybe (Text -> MftEntry)) ObjectKey

makeTroubledChild :: ObjectKey -> Text -> MftEntry
makeTroubledChild childKey fileName = 
    MftEntry { child = TroubledChild childKey, .. }     


-- bumpCounter :: Monad m => 
--             TopDownCounters 
--         -> LensLike m TopDownCounters () (IORef a) () 
--         -> m ()
bumpCounter counters counterLens = 
     increment $ counters ^. counterLens

bumpCounterBy counters counterLens n = liftIO $     
    atomicModifyIORef' (counters ^. counterLens) $ \c -> (c + n, ())        
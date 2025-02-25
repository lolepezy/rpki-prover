{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Validation.TopDown (
    TopDownResult(..),
    validateMutlipleTAs,    
    withRepositoriesProcessing,
    addUniqueVRPCount
)
where

import           Control.Concurrent.Async        (forConcurrently)
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad
import           Control.Monad.Reader

import           Control.Lens hiding (children)

import           Barbies

import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields
import           GHC.Generics

import           Data.Foldable
import           Data.IORef
import           Data.Maybe
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Set.NonEmpty                as NESet
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Monoid.Generic
import qualified Data.List                        as List
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Vector                      as V
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           UnliftIO.Async                   (pooledForConcurrentlyN)

import           RPKI.AppContext
import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Messages
import           RPKI.Parallel
import           RPKI.Repository
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Storable
import           RPKI.Store.Database    (DB)
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util                        
import           RPKI.Validation.Types
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.ResourceValidation
import           RPKI.Validation.Common


data PayloadBuilder = PayloadBuilder {
        vrps     :: IORef [T2 [Vrp] ObjectKey],        
        spls     :: IORef [SplPayload],        
        aspas    :: IORef [Aspa],
        gbrs     :: IORef [T2 Hash Gbr],
        bgpCerts :: IORef [BGPSecPayload]
    }
    deriving stock (Generic)        

newPayloadBuilder :: IO PayloadBuilder 
newPayloadBuilder = PayloadBuilder <$> 
            newIORef mempty <*>
            newIORef mempty <*>
            newIORef mempty <*>
            newIORef mempty <*>
            newIORef mempty
    
-- Auxiliarry structure used in top-down validation. It has a lot of global variables 
-- but it's lifetime is limited to one top-down validation run.
data TopDownContext = TopDownContext {
        verifiedResources       :: Maybe (VerifiedRS PrefixesAndAsns),
        taName                  :: TaName,
        allTas                  :: AllTasTopDownContext,
        currentPathDepth        :: Int,
        startingRepositoryCount :: Int,
        interruptedByLimit      :: TVar Limited,
        payloadBuilder          :: PayloadBuilder,
        overclaimingHappened    :: Bool
    }
    deriving stock (Generic)


data AllTasTopDownContext = AllTasTopDownContext {
        now                  :: Now,
        worldVersion         :: WorldVersion,
        visitedKeys          :: TVar (Set ObjectKey),        
        repositoryProcessing :: RepositoryProcessing,
        shortcutQueue        :: ClosableQueue MftShortcutOp,
        shortcutsCreated     :: TVar [MftShortcutOp],
        topDownCounters      :: TopDownCounters IORef 
    }
    deriving stock (Generic)


data TopDownCounters f = TopDownCounters {
        originalCa   :: f Int,
        shortcutCa   :: f Int,
        originalMft  :: f Int,
        shortcutMft  :: f Int,
        originalCrl  :: f Int,
        shortcutCrl  :: f Int,        
        originalRoa  :: f Int,
        originalSpl  :: f Int,
        originalAspa :: f Int,        
        shortcutRoa  :: f Int,
        shortcutSpl  :: f Int,
        shortcutAspa :: f Int,        
        shortcutTroubled    :: f Int,
        newChildren         :: f Int,
        overlappingChildren :: f Int,
        updateMftMeta       :: f Int,
        updateMftChildren   :: f Int,
        readOriginal :: f Int,
        readParsed   :: f Int
    }
    deriving stock (Generic)
    deriving (FunctorB, TraversableB, ApplicativeB, ConstraintsB)

deriving instance AllBF Show f TopDownCounters => Show (TopDownCounters f)

data Limited = CanProceed | FirstToHitLimit | AlreadyReportedLimit
    deriving stock (Show, Eq, Ord, Generic)

data TopDownResult = TopDownResult {
        payloads           :: Payloads Vrps,
        roas               :: Roas,
        topDownValidations :: ValidationState
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving Semigroup via GenericSemigroup TopDownResult
    deriving Monoid    via GenericMonoid TopDownResult

fromValidations :: ValidationState -> TopDownResult
fromValidations = TopDownResult mempty mempty

newTopDownContext :: MonadIO m =>
                    TaName
                    -> CaCerObject
                    -> AllTasTopDownContext
                    -> m TopDownContext
newTopDownContext taName certificate allTas = 
    liftIO $ do 
        payloadBuilder <- newPayloadBuilder
        atomically $ do
            let verifiedResources = Just $ createVerifiedResources certificate
                currentPathDepth = 0
                overclaimingHappened = False       
            startingRepositoryCount <- fmap repositoryCount $ readTVar $ allTas ^. #repositoryProcessing . #publicationPoints
            interruptedByLimit      <- newTVar CanProceed                 
            pure $! TopDownContext {..}

newAllTasTopDownContext :: MonadIO m =>
                        WorldVersion
                        -> RepositoryProcessing
                        -> ClosableQueue MftShortcutOp
                        -> m AllTasTopDownContext
newAllTasTopDownContext worldVersion repositoryProcessing shortcutQueue = liftIO $ do 
    let now = Now $ versionToMoment worldVersion
    topDownCounters <- newTopDownCounters
    atomically $ do        
        visitedKeys      <- newTVar mempty
        shortcutsCreated <- newTVar []        
        pure $! AllTasTopDownContext {..}

newTopDownCounters :: IO (TopDownCounters IORef)
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
    shortcutSpl  <- newIORef 0        
    shortcutAspa <- newIORef 0            
    shortcutTroubled <- newIORef 0        

    originalRoa  <- newIORef 0        
    originalSpl  <- newIORef 0        
    originalAspa <- newIORef 0        

    readOriginal <- newIORef 0   
    readParsed   <- newIORef 0             
   
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
                pps <- roTx db $ \tx -> DB.getPublicationPoints tx db
                let pps' = addRsyncPrefetchUrls config pps
                atomically $ writeTVar (rp ^. #publicationPoints) pps'

            a <- f rp

            -- save publication points state    
            mapException (AppException . storageError) $ do
                pps <- readTVarIO $ (rp ^. #publicationPoints)
                rwTx db $ \tx -> DB.savePublicationPoints tx db pps          

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
                5000
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
        pure $! fromValidations fetchValidation : rs
    

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
                    appError $ ValidationE $ ValidationTimeout maxDuration)    

    case r of
        Left _                    -> pure $ TopDownResult mempty mempty vs       
        Right (topDownContext, _) -> do     
            let builder = topDownContext ^. #payloadBuilder
            vrps     <- readIORef $ builder ^. #vrps 
            aspas    <- fmap Set.fromList $ readIORef $ builder ^. #aspas 
            gbrs     <- fmap Set.fromList $ readIORef $ builder ^. #gbrs 
            bgpCerts <- fmap Set.fromList $ readIORef $ builder ^. #bgpCerts    

            splPayloads <- readIORef $ builder ^. #spls            
            let spls = Set.fromList [ SplN asn prefix | 
                                      SplPayload asn prefixes <- splPayloads, prefix <- prefixes ]

            let payloads = Payloads {..}        
            let payloads' = payloads & #vrps .~ 
                                newVrps taName (V.fromList [ v | T2 vrp _ <- vrps, v <- vrp ])

            let roas = newRoas taName $ MonoidalMap.fromList $ 
                            map (\(T2 vrp k) -> (k, V.fromList vrp)) vrps 

            pure $ TopDownResult payloads' roas vs

  where
    taName = getTaName tal
    taContext = newScopes' TAFocus $ unTaName taName

    validateFromTAL = do
        timedMetric (Proxy :: Proxy ValidationMetric) $
            vFocusOn LocationFocus (getURL $ getTaCertURL tal) $ do
                ((taCert, repos), _) <- timedMS $ validateTACertificateFromTAL appContext tal worldVersion
                -- this will be used as the "now" in all subsequent time and period validations                 
                topDownContext <- newTopDownContext taName (taCert ^. #payload) allTas
                (topDownContext, ) <$> validateFromTACert appContext topDownContext repos taCert

        

data WhichTA = FetchedTA RpkiURL RpkiObject | CachedTA StorableTA

-- | Fetch and validated TA certificate starting from the TAL.
-- | 
-- | This function doesn't throw exceptions.
validateTACertificateFromTAL :: Storage s =>
                                AppContext s
                                -> TAL
                                -> WorldVersion
                                -> ValidatorT IO (Located CaCerObject, PublicationPointAccess)
validateTACertificateFromTAL appContext@AppContext {..} tal worldVersion = do
    let now = Now $ versionToMoment worldVersion
    let validationConfig = config ^. typed

    db <- liftIO $ readTVarIO database
    ta <- DB.roAppTxEx db storageError $ \tx -> DB.getTA tx db (getTaName tal)
    case ta of
        Nothing -> fetchValidateAndStore db now Nothing
        Just storedTa
            | needsFetching (getTaCertURL tal) Nothing (storedTa ^. #fetchStatus) validationConfig now ->
                fetchValidateAndStore db now (Just storedTa)
            | otherwise -> do
                logInfo logger [i|Not re-fetching TA certificate #{getURL $ getTaCertURL tal}, it's up-to-date.|]
                let located = locatedTaCert (getTaCertURL tal) (storedTa ^. #taCert)
                pure (located, storedTa ^. #initialRepositories)

  where
    fetchValidateAndStore db (Now moment) storableTa = do
        z <- (do 
                (u, ro) <- fetchTACertificate appContext (syncFetchConfig config) tal
                pure $ FetchedTA u ro)
            `catchError`
                tryToFallbackToCachedCopy

        case z of     
            FetchedTA actualUrl object -> do                                 
                certToUse <- case storableTa of
                    Nothing  -> vHoist $ validateTACert tal actualUrl object
                    Just StorableTA { taCert = cachedTaCert } -> 
                        (vHoist $ do 
                            cert <- validateTACert tal actualUrl object
                            chooseTaCert cert cachedTaCert)
                        `catchError`
                            (\e -> do
                                logError logger [i|Fetched TA certificate is invalid with error #{e}, will use cached copy.|]
                                pure cachedTaCert)                            
                
                case publicationPointsFromTAL tal certToUse of
                    Left e         -> appError $ ValidationE e
                    Right ppAccess ->
                        DB.rwAppTxEx db storageError $ \tx -> do
                            DB.saveTA tx db (StorableTA tal certToUse (FetchedAt moment) ppAccess actualUrl)
                            pure (locatedTaCert actualUrl certToUse, ppAccess)

            CachedTA StorableTA { tal = _, ..} -> do 
                void (vHoist $ validateTACert tal actualUrl (CerRO taCert))
                    `catchError`
                    (\e -> do
                        logError logger [i|Will delete cached TA certificate, it is invalid with the error: #{e}|]
                        DB.rwAppTxEx db storageError $ \tx -> DB.deleteTA tx db tal                            
                        appError e)

                pure (locatedTaCert actualUrl taCert, initialRepositories)

      where
        tryToFallbackToCachedCopy e =
            case storableTa of
                Nothing -> do 
                    logError logger $
                        [i|Could not download TA certiicate for #{getTaName tal}, error: #{e}|] <>
                        [i| and there is no cached copy of it.|]
                    appError e

                Just cached -> do  
                    logError logger $ 
                        [i|Could not download TA certiicate for #{getTaName tal}, error: #{e}|] <> 
                        [i| will use cached copy.|]                                        

                    pure $ CachedTA cached

    locatedTaCert url cert = Located (toLocations url) cert


-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: Storage s =>
                    AppContext s ->
                    TopDownContext ->
                    PublicationPointAccess ->
                    Located CaCerObject ->
                    ValidatorT IO ()
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

    -- Do the tree descend, gather validation results and VRPs                
    fromTryM
        (UnspecifiedE (unTaName taName) . fmtEx)
        (validateCa appContext topDownContext (CaFull taCert))


validateCa :: Storage s =>
            AppContext s ->
            TopDownContext ->
            Ca ->
            ValidatorT IO ()
validateCa 
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    ca = 
    checkAndReport treeDepthLimit
        $ checkAndReport visitedObjectCountLimit
        $ checkAndReport repositoryCountLimit
        $ validateCaNoLimitChecks appContext topDownContext ca        
  where

    -- Check and report for the maximal tree depth
    treeDepthLimit = (
        pure (currentPathDepth > validationConfig ^. #maxCertificatePathDepth),
        logCheck 
            (CertificatePathTooDeep $ validationConfig ^. #maxCertificatePathDepth)
            (\loc -> [i|Interrupting validation on #{fmtLocations loc}, maximum tree depth is reached.|])
        )

    -- Check and report for the maximal number of objects in the tree
    visitedObjectCountLimit = (
        (> validationConfig ^. #maxTotalTreeSize) . Set.size <$> readTVar visitedKeys,
        logCheck 
            (TreeIsTooBig $ validationConfig ^. #maxTotalTreeSize)
            (\loc -> [i|Interrupting validation on #{fmtLocations loc}, maximum total object number in the tree is reached.|])
        )

    -- Check and report for the maximal increase in the repository number
    repositoryCountLimit = (
        do
            pps <- readTVar $ repositoryProcessing ^. #publicationPoints
            pure $! repositoryCount pps - startingRepositoryCount > validationConfig ^. #maxTaRepositories,
        logCheck
            (TooManyRepositories $ validationConfig ^. #maxTaRepositories)
            (\loc -> [i|Interrupting validation on #{fmtLocations loc}, maximum total new repository count is reached.|])
        )                

    logCheck validationError errorText = do 
        locations <- getCaLocations appContext ca
        for_ locations $ \loc -> 
            logError logger (errorText loc)
        vError validationError                

    -- This is to make sure that the error of hitting a limit
    -- is reported only by the thread that first hits it
    checkAndReport (condition, report) nextOne = do
            z <- liftIO $ atomically $ verifyLimit condition interruptedByLimit
            case z of
                CanProceed           -> nextOne
                FirstToHitLimit      -> report
                AlreadyReportedLimit -> pure mempty

    validationConfig = config ^. typed @ValidationConfig
    


validateCaNoLimitChecks :: Storage s =>
                        AppContext s ->
                        TopDownContext ->
                        Ca ->
                        ValidatorT IO ()
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
                    
                Just filteredPpa -> do
                    let fetch = case config ^. #proverRunMode of 
                            ServerMode -> do 
                                fetchQuickly appContext repositoryProcessing worldVersion filteredPpa
                            OneOffMode {} -> 
                                fetchWithFallback appContext repositoryProcessing worldVersion 
                                            (syncFetchConfig config) filteredPpa                                            
                    fetch >>= \case                     
                        [] -> 
                            -- Nothing has been fetched
                            validateThisCertAndGoDown
                        _  -> do     
                            pps <- readPublicationPoints repositoryProcessing      
                            let primaryUrl = getPrimaryRepositoryUrl pps filteredPpa
                            metricFocusOn PPFocus primaryUrl validateThisCertAndGoDown

  where
    validateThisCertAndGoDown = validateCaNoFetch appContext topDownContext ca


validateCaNoFetch :: Storage s =>
                    AppContext s 
                -> TopDownContext 
                -> Ca 
                -> ValidatorT IO ()
validateCaNoFetch
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    ca = do 
    
    case ca of 
        CaFull c -> do
            increment $ topDownCounters ^. #originalCa
            markAsReadByHash appContext topDownContext (getHash c)                         
            validateObjectLocations c
            vHoist $ validateObjectValidityPeriod c now
            oneMoreCert
            join $! nextAction $ toAKI $ getSKI c
        CaShort c -> do 
            increment $ topDownCounters ^. #shortcutCa 
            markAsRead topDownContext (c ^. #key) 
            validateLocationForShortcut (c ^. #key)
            vHoist $ validateObjectValidityPeriod c now
            oneMoreCert
            join $! nextAction $ toAKI (c ^. #ski)
  where    
    validationAlgorithm = config ^. typed @ValidationConfig . typed @ValidationAlgorithm
    validationRFC = config ^. typed @ValidationConfig . typed @ValidationRFC

    nextAction =
        case validationAlgorithm of 
            FullEveryIteration -> makeNextFullValidationAction
            Incremental        -> makeNextIncrementalAction

    newShortcut = 
        case validationAlgorithm of 
            -- Do not create shortctus when validation algorithm is not incremental
            FullEveryIteration -> const Nothing
            Incremental        -> (Just $!)

    makeNextFullValidationAction :: AKI -> ValidatorT IO (ValidatorT IO ())
    makeNextFullValidationAction childrenAki = do 
        roTxT database $ \tx db -> do 
            DB.findLatestMftByAKI tx db childrenAki >>= \case 
                Nothing ->                        
                    pure $! vError $ NoMFT childrenAki
                Just mft ->                        
                    pure $! do
                        markAsRead topDownContext (mft ^. typed)
                        caFull <- getFullCa appContext topDownContext ca
                        void $ manifestFullValidation caFull mft Nothing childrenAki                                   
                        oneMoreMft >> oneMoreCrl                                                    


    makeNextIncrementalAction :: AKI -> ValidatorT IO (ValidatorT IO ())
    makeNextIncrementalAction childrenAki = 
        roTxT database $ \tx db -> do 
            DB.getMftShorcut tx db childrenAki >>= \case
                Nothing -> do 
                    DB.findLatestMftByAKI tx db childrenAki >>= \case 
                        Nothing -> do 
                            -- No current manifest and not shortcut as well, bail out 
                            pure $! vError $ NoMFT childrenAki
                        Just mft -> do 
                            increment $ topDownCounters ^. #originalMft
                            -- No shortcut found, so do the full validation for the manifest
                            pure $! do                   
                                markAsRead topDownContext (mft ^. typed)                   
                                caFull <- getFullCa appContext topDownContext ca
                                void $ manifestFullValidation caFull mft Nothing childrenAki                              
                                oneMoreMft >> oneMoreCrl                                                                    

                Just mftShortcut -> do
                    let mftShortKey = mftShortcut ^. #key
                    increment $ topDownCounters ^. #shortcutMft
                    -- Find the key of the latest real manifest
                    action <- 
                        DB.findLatestMftKeyByAKI tx db childrenAki >>= \case 
                            Nothing -> pure $! do                                             
                                -- That is really weird and should normally never happen. 
                                -- Do not interrupt validation here, but complain in the log
                                vWarn $ NoMFTButCachedMft childrenAki                                    
                                let crlKey = mftShortcut ^. #crlShortcut . #key
                                markAsRead topDownContext crlKey
                                let message = [i|Internal error, there is a manifest shortcut, but no manifest for the key #{mftShortKey}.|]
                                logError logger message
                                collectPayloadsFromShortcuts mftShortcut Nothing 
                                    (getFullCa appContext topDownContext ca)
                                    -- getCrlByKey is the best we can have
                                    (getCrlByKey appContext crlKey)
                                    (getResources ca)

                            Just mftKey 
                                | mftShortKey == mftKey -> do
                                    -- Nothing has changed, the real manifest is the 
                                    -- same as the shortcut, so use the shortcut
                                    let crlKey = mftShortcut ^. #crlShortcut . #key                                    
                                    pure $! do 
                                        markAsRead topDownContext crlKey
                                        collectPayloadsFromShortcuts mftShortcut Nothing 
                                                (getFullCa appContext topDownContext ca)
                                                (getCrlByKey appContext crlKey)
                                                (getResources ca)

                                | otherwise -> do    
                                    DB.getMftByKey tx db mftKey >>= \case 
                                        Nothing -> pure $! 
                                            internalError appContext [i|Internal error, can't find a manifest by its key #{mftKey}.|]
                                        Just mft -> pure $! do
                                            increment $ topDownCounters ^. #shortcutMft
                                            markAsRead topDownContext mftKey
                                            fullCa <- getFullCa appContext topDownContext ca
                                            let crlKey = mftShortcut ^. #crlShortcut . #key
                                            markAsRead topDownContext crlKey
                                            let combineShortcutAndNewMft = do                                                     
                                                    overlappingChildren <- manifestFullValidation fullCa mft (Just mftShortcut) childrenAki
                                                    collectPayloadsFromShortcuts mftShortcut (Just overlappingChildren) 
                                                                (pure fullCa)
                                                                (findAndValidateCrl fullCa mft childrenAki)   
                                                                (getResources ca)

                                            let useShortcutOnly =                                                    
                                                    collectPayloadsFromShortcuts mftShortcut Nothing 
                                                            (getFullCa appContext topDownContext ca)
                                                            (getCrlByKey appContext crlKey)
                                                            (getResources ca)
                                            
                                            combineShortcutAndNewMft
                                                `catchError`
                                                (\e -> do 
                                                    vFocusOn ObjectFocus mftKey $ vWarn $ MftFallback e
                                                    let mftLocation = pickLocation $ getLocations $ mft ^. #object
                                                    logWarn logger [i|Falling back to the previous manifest for #{mftLocation}, error: #{toMessage e}|]
                                                    useShortcutOnly)                                            
                    pure $! do 
                        markAsRead topDownContext mftShortKey
                        action `andThen`
                            (oneMoreMft >> oneMoreCrl >> oneMoreMftShort)          

    -- Proceed with full validation for children mentioned in the full manifest 
    -- and children mentioned in the manifest shortcut. Create a diff between them,
    -- run full validation only for new children and create a new manifest shortcut
    -- with updated set of children.
    manifestFullValidation :: 
                    Located CaCerObject
                    -> Keyed (Located MftObject) 
                    -> Maybe MftShortcut 
                    -> AKI
                    -> ValidatorT IO [T3 Text Hash ObjectKey]
    manifestFullValidation fullCa 
        keyedMft@(Keyed locatedMft@(Located mftLocations mft) mftKey) 
        mftShortcut childrenAki = do         
        vUniqueFocusOn LocationFocus (getURL $ pickLocation mftLocations)
            doValidate
            (vError $ CircularReference $ KeyIdentity mftKey)
      where
        doValidate = do 
            -- General location validation
            validateObjectLocations locatedMft

            -- Manifest-specific location validation
            validateMftLocation locatedMft fullCa

            -- TODO Add fiddling with shortcut version of CRL here                    
            keyedValidCrl@(Keyed validCrl crlKey) <- findAndValidateCrl fullCa keyedMft childrenAki                

            -- MFT can be revoked by the CRL that is on this MFT -- detect 
            -- revocation as well, this is clearly and error                               
            validMft <- vHoist $ validateMft (config ^. #validationConfig . typed) 
                                    now mft fullCa validCrl verifiedResources

            -- Validate entry list and filter out CRL itself
            nonCrlChildren <- validateMftEntries mft (getHash validCrl)

            -- If MFT shortcut is present, filter children that need validation, 
            -- children that are on the shortcut are already validated.
            let (newChildren, overlappingChildren, isSomethingDeleted) =
                    case mftShortcut of 
                        Nothing       -> (nonCrlChildren, [], False)
                        Just mftShort -> manifestDiff mftShort nonCrlChildren

            bumpCounterBy topDownCounters #newChildren (length newChildren)
            bumpCounterBy topDownCounters #overlappingChildren (length overlappingChildren)
            
            forM_ mftShortcut $ \mftShort -> do          
                -- If CRL has changed, we have to recheck if children are not revoked. 
                -- If will be checked by full validation for newChildren but it needs 
                -- to be explicitly checked for overlappingChildren                
                when (crlKey /= mftShort ^. #crlShortcut . #key) $ do     
                    increment $ topDownCounters ^. #originalCrl                         
                    checkForRevokedChildren mftShort keyedMft overlappingChildren validCrl            

                -- manifest number must increase 
                -- https://www.rfc-editor.org/rfc/rfc9286.html#name-manifest
                let mftNumber = getCMSContent (mft ^. #cmsPayload) ^. #mftNumber 
                when (mftNumber < mftShort ^. #manifestNumber) $ do 
                    -- Here we have to do a bit of hackery: 
                    -- * Calling vError will interrupt this function and call for fall-back to 
                    --   the "latest valid manifest" which is the shortcut
                    -- * But the shortcut may have expired already and there will no be any options left,
                    --   so we need to be careful and just emit a warning in this case
                    -- 
                    let (beforeMft, afterMft) = getValidityPeriod mftShort
                    let issue = ManifestNumberDecreased (mftShort ^. #manifestNumber) mftNumber
                    if beforeMft < unNow now && unNow now > afterMft
                        then vError issue
                        else vWarn issue

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
                    --                                            
                    childrenShortcuts <- 
                        fmap (\shortcuts -> [ (k, s) | T2 k (Just s) <- shortcuts ]) $
                            gatherMftEntryResults =<< 
                                gatherMftEntryValidations fullCa newChildren validCrl

                    let newEntries = makeEntriesWithMap newChildren (Map.fromList childrenShortcuts)                            

                    let nextChildrenShortcuts = 
                            case mftShortcut of 
                                Nothing               -> newEntries
                                Just MftShortcut {..} -> 
                                    newEntries <> makeEntriesWithMap overlappingChildren nonCrlEntries
                                
                    let nextMftShortcut = makeMftShortcut mftKey validMft nextChildrenShortcuts keyedValidCrl

                    case validationAlgorithm of 
                        -- Only create shortcuts for case of incremental validation.
                        -- 
                        -- NOTE: That means that in case of full validation falling back to 
                        -- the previous valid manifest will not work, since there are no
                        -- shortcuts of previous manifests to fall back to.                  
                        Incremental -> do  
                            issues <- vHoist thisScopeIssues
                            -- Do no create shortcuts for manifests with warnings 
                            -- (or errors, obviously)
                            when (Set.null issues) $ do   
                                let aki = toAKI $ getSKI fullCa                        
                                when (maybe True ((/= mftKey) . (^. #key)) mftShortcut) $ do                                
                                    updateMftShortcut topDownContext aki nextMftShortcut
                                    increment $ topDownCounters ^. #updateMftMeta

                                -- Update manifest shortcut children in case there are new 
                                -- or deleted children in the new manifest. They are updated
                                -- separately since changes to non-CRL children are less common
                                -- then updates to metadata, hence we can save quite some 
                                -- CPU on serialisation.
                                when (isNothing mftShortcut || not (null newChildren) || isSomethingDeleted) $ do
                                    updateMftShortcutChildren topDownContext aki nextMftShortcut
                                    increment $ topDownCounters ^. #updateMftChildren

                        _  -> pure ()

                    pure $! overlappingChildren

            processChildren `recover` markAllEntriesAsVisited


    findAndValidateCrl :: Located CaCerObject
                    -> Keyed (Located MftObject) 
                    -> AKI
                    -> ValidatorT IO (Keyed (Validated CrlObject))
    findAndValidateCrl fullCa (Keyed (Located _ mft) _) aki = do  
        MftPair _ crlHash <-
            case findCrlOnMft mft of
                []    -> vError $ NoCRLOnMFT aki 
                [crl] -> pure crl
                crls  -> vError $ MoreThanOneCRLOnMFT aki crls

        db <- liftIO $ readTVarIO database
        DB.roAppTx db $ \tx -> 
            DB.getKeyByHash tx db crlHash >>= \case         
                Nothing  -> vError $ NoCRLExists aki crlHash
                Just key -> do           
                    increment $ topDownCounters ^. #readParsed
                    -- CRLs are not parsed right after fetching, so try to get the blob
                    z <- getParsedObject tx db key $ vError $ NoCRLExists aki crlHash
                    case z of 
                        Keyed locatedCrl@(Located crlLocations (CrlRO crl)) crlKey -> do
                            markAsRead topDownContext crlKey
                            inSubLocationScope (getURL $ pickLocation crlLocations) $ do 
                                validateObjectLocations locatedCrl
                                vHoist $ do
                                    let mftEECert = getEECert $ unCMS $ cmsPayload mft
                                    checkCrlLocation locatedCrl mftEECert
                                    void $ validateCrl now crl fullCa                
                                    pure $! Keyed (Validated crl) crlKey                                        
                        _ -> 
                            vError $ CRLHashPointsToAnotherObject crlHash   
            

    -- Utility for repeated peace of code
    makeEntriesWithMap childrenList entryMap = 
        [ (key, entry) | 
            T3 _ _ key <- childrenList,
            entry      <- maybeToList $ Map.lookup key entryMap ]


    -- Check if shortcut children are revoked
    checkForRevokedChildren mftShortcut (Keyed (Located _ mft) _) children validCrl = do        
        when (isRevoked (getSerial mft) validCrl) $
            vWarn RevokedResourceCertificate   
        forM_ children $ \(T3 _ _ childKey) ->
            for_ (Map.lookup childKey (mftShortcut ^. #nonCrlEntries)) $ \MftEntry {..} ->
                for_ (getMftChildSerial child) $ \childSerial ->
                    when (isRevoked childSerial validCrl) $
                        vFocusOn ObjectFocus childKey $
                            vWarn RevokedResourceCertificate


    -- this indicates the difference between RFC9286-bis 
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
        liftIO $ forChildren
            nonCrlChildren
            $ \(T3 filename hash' key) -> do 
                (z, vs) <- runValidatorT scopes $ do
                                ro <- getManifestEntry filename hash'
                                -- if failed this one interrupts the whole MFT valdiation
                                validateMftChild fullCa ro filename validCrl
                pure $! case z of
                    -- In this case invalid child is considered invalid entry 
                    -- and the whole manifest is invalid
                    Left e              -> InvalidEntry e vs
                    Right childShortcut -> ValidEntry vs childShortcut key filename   
    
    independentMftChildrenResults fullCa nonCrlChildren validCrl = do
        scopes <- askScopes
        liftIO $ forChildren
            nonCrlChildren
            $ \(T3 filename hash key) -> do
                (r, vs) <- runValidatorT scopes $ getManifestEntry filename hash
                case r of
                    Left e -> do 
                        -- Decide if the error is related to the manifest itself 
                        -- of to the object it points to based on the scope of the 
                        -- reported issues. It's a bit hacky, but it works nicely.                        
                        let manifestIssues = getIssues (scopes ^. typed) (vs ^. typed )
                        pure $! if Set.null manifestIssues 
                            then InvalidChild e vs key filename
                            else InvalidEntry e vs
                    Right ro -> do
                        -- We are cheating here a little by faking empty payload set.
                        -- 
                        -- if failed, this one will result in the empty VRP set
                        -- while keeping errors and warning in the `vs'` value.
                        (z, vs') <- runValidatorT scopes $ validateMftChild fullCa ro filename validCrl
                        pure $! case z of
                                Left e              -> InvalidChild e vs' key filename
                                Right childShortcut -> ValidEntry vs' childShortcut key filename
    
    forChildren nonCrlChildren = let
        itemsPerThread = 20
        threads = min 
                (length nonCrlChildren `div` itemsPerThread)
                (fromIntegral $ config ^. #parallelism . #cpuParallelism)

        forAllChildren = 
                if threads <= 1
                    then forM 
                    else pooledForConcurrentlyN threads        

        in forAllChildren nonCrlChildren 

    gatherMftEntryResults =        
        foldM (\childrenShortcuts r -> do                 
            case r of 
                InvalidEntry e vs -> do
                    embedState vs
                    appError e
                InvalidChild _ vs key fileName -> do
                    embedState vs
                    pure $! T2 key (Just $! makeChildWithIssues key fileName) : childrenShortcuts
                ValidEntry vs childShortcut key fileName -> do 
                    embedState vs
                    -- Don't create shortcuts for objects having either errors or warnings,
                    -- otherwise warnings will disappear after the first validation 
                    if emptyValidations (vs ^. typed)
                        then do                            
                            pure $! T2 key childShortcut : childrenShortcuts
                        else do 
                            pure $! T2 key (Just $! makeChildWithIssues key fileName) : childrenShortcuts
            ) mempty

        
    -- Check manifest entries as a whole, without doing anything 
    -- with the objects they are pointing to.    
    validateMftEntries mft crlHash = do
        let mftChildren = mftEntries $ getCMSContent $ cmsPayload mft
        when (null mftChildren) $
            vError ZeroManifestEntries        

        let nonCrlChildren = filter (\(MftPair _ hash') -> crlHash /= hash') mftChildren

        -- Make sure all the entries are unique
        let entryMap = Map.fromListWith (<>) $ map (\(MftPair f h) -> (h, [f])) nonCrlChildren
        let nonUniqueEntries = Map.filter longerThanOne entryMap

        -- Don't crash here, it's just a warning, at the moment RFC doesn't say anything 
        -- about uniqueness of manifest entries. 
        -- TODO Or does it? Maybe something in ASN1 encoding as Set?
        unless (Map.null nonUniqueEntries) $
            vWarn $ NonUniqueManifestEntries $ Map.toList nonUniqueEntries

        db <- liftIO $ readTVarIO database
        forM nonCrlChildren $ \(MftPair fileName hash) -> do
                k <- DB.roAppTx db $ \tx -> DB.getKeyByHash tx db hash            
                case k of 
                    Nothing  -> vError $ ManifestEntryDoesn'tExist hash fileName
                    Just key -> pure $! T3 fileName hash key        
        where
            longerThanOne = \case 
                [_] -> False
                []  -> False
                _   -> True

    -- Given MFT entry with hash and filename, get the object it refers to
    -- 
    getManifestEntry filename hash' = do
        let objectType = textObjectType filename
        db <- liftIO $ readTVarIO database
        ro <- DB.roAppTx db $ \tx -> do             
            DB.getKeyByHash tx db hash' >>= \case                     
                Nothing  -> vError $ ManifestEntryDoesn'tExist hash' filename            
                Just key -> 
                    vFocusOn ObjectFocus key $
                        case objectType of 
                            Just type_ -> do
                                increment $ topDownCounters ^. #readParsed
                                -- we expect certificates to be stored in the parsed-serialised form                                                            
                                getParsedObject tx db key $ do
                                    increment $ topDownCounters ^. #readOriginal
                                    -- try to get the original blob and (re-)parse 
                                    -- it to at least complain about it at the right place
                                    getLocatedOriginal tx db key type_ $ 
                                        vError $ ManifestEntryDoesn'tExist hash' filename
                            Nothing -> do
                                -- If we don't know anything about the type from the manifest, 
                                -- it may still be possible that the object was parsed based
                                -- on the rsync/rrdp url and the entry in the manifest is just
                                -- wrong
                                increment $ topDownCounters ^. #readParsed
                                getParsedObject tx db key $ 
                                    vError $ ManifestEntryDoesn'tExist hash' filename

        -- The type of the object that is deserialised doesn't correspond 
        -- to the file extension on the manifest
        let realObjectType = getRpkiObjectType $ ro ^. #object

        let complain = vWarn $ ManifestEntryHasWrongFileType hash' filename realObjectType
        case objectType of 
            Nothing -> complain
            Just ot -> unless (realObjectType `isOfType` ot) complain

        pure ro                        


    validateMftChild caFull child@(Keyed (Located objectLocations _) _) 
                     filename validCrl = do
        -- warn about names on the manifest mismatching names in the object URLs        
        let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $ 
                            unLocations objectLocations
        when (null nameMatches) $
            vWarn $ ManifestLocationMismatch filename objectLocations

        validateChildObject caFull child filename validCrl


    -- Optimised version of location validation when all we have is a key of an object
    -- 
    validateLocationForShortcut key = do  
        count <- roTxT database $ \tx db -> DB.getLocationCountByKey tx db key
        when (count > 1) $ do 
            z <- roTxT database $ \tx db -> DB.getLocationsByKey tx db key
            case z of 
                Nothing -> 
                    -- That's weird and it means DB inconsitency                                
                    internalError appContext 
                        [i|Internal error, can't find locations for the object #{key} with positive location count #{count}.|]
                Just locations -> 
                    vFocusOn LocationFocus (getURL $ pickLocation locations) $
                        validateObjectLocations locations


    {-         
        Validate manifest child according to 
        https://datatracker.ietf.org/doc/rfc9286/

        And return shortcut created for it
    -}
    validateChildObject :: 
            Located CaCerObject
            -> Keyed (Located RpkiObject) 
            -> Text
            -> Validated CrlObject
            -> ValidatorT IO (Maybe MftEntry)
    validateChildObject fullCa (Keyed child@(Located locations childRo) childKey) fileName validCrl = do        
        let focusOnChild = vFocusOn LocationFocus (getURL $ pickLocation locations)
        -- let validationRFC = config ^. #validationConfig . typed
        case childRo of
            CerRO childCert -> do
                parentScope <- askScopes                
                {- 
                    Note that recursive validation of the child CA happens in the separate   
                    runValidatorT (...) call, it is to avoid short-circuit logic implemented by ExceptT:
                    otherwise an error in child validation would interrupt validation of the parent with
                    ExceptT's exception logic.
                -}
                (r, validationState) <- liftIO $ runValidatorT parentScope $       
                    vFocusOn LocationFocus (getURL $ pickLocation locations) $ do
                        -- Check that AIA of the child points to the correct location of the parent
                        -- https://mailarchive.ietf.org/arch/msg/sidrops/wRa88GHsJ8NMvfpuxXsT2_JXQSU/
                        --                             
                        vHoist $ validateAIA @_ @_ @'CACert childCert fullCa

                        (childVerifiedResources, overlclaiming) 
                            <- vHoist $ do
                                Validated validCert <- validateResourceCert @_ @_ @'CACert
                                                            now childCert fullCa validCrl
                                validateResources (config ^. #validationConfig . typed) 
                                    verifiedResources childCert validCert

                        let childTopDownContext = topDownContext
                                & #verifiedResources ?~ childVerifiedResources
                                & #currentPathDepth %~ (+ 1)
                                & #overclaimingHappened .~ isJust overlclaiming

                        validateCa appContext childTopDownContext (CaFull (Located locations childCert))

                embedState validationState
                case r of 
                    Left _  -> pure $! Just $! makeChildWithIssues childKey fileName
                    Right _  -> do 
                        case getPublicationPointsFromCertObject childCert of 
                            -- It's not going to happen?
                            Left e     -> vError e
                            Right ppas -> do 
                                -- Look at the issues for the child CA to decide if CA shortcut should be made
                                shortcut <- vFocusOn LocationFocus (getURL $ pickLocation locations) $ 
                                                vHoist $ shortcutIfNoIssues childKey fileName
                                                        (makeCaShortcut childKey (Validated childCert) ppas)
                                pure $! newShortcut shortcut
            RoaRO roa -> 
                focusOnChild $ do
                    validateObjectLocations child                    
                    allowRevoked $ do
                        validRoa <- vHoist $ validateRoa validationRFC now roa fullCa validCrl verifiedResources
                        let vrpList = getCMSContent $ cmsPayload roa
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ length vrpList
                        increment $ topDownCounters ^. #originalRoa                        
                        shortcut <- vHoist $ shortcutIfNoIssues childKey fileName 
                                            (makeRoaShortcut childKey validRoa vrpList)                        
                        rememberPayloads typed (T2 vrpList childKey :)
                        pure $! newShortcut shortcut                  

            SplRO spl -> 
                focusOnChild $ do
                    validateObjectLocations child                    
                    allowRevoked $ do
                        validSpl <- vHoist $ validateSpl validationRFC now spl fullCa validCrl verifiedResources
                        let spls = getCMSContent $ cmsPayload spl
                        oneMoreSpl                        
                        increment $ topDownCounters ^. #originalSpl
                        shortcut <- vHoist $ shortcutIfNoIssues childKey fileName 
                                            (makeSplShortcut childKey validSpl spls)
                        rememberPayloads typed (spls :)
                        pure $! newShortcut shortcut                        

            AspaRO aspa -> 
                focusOnChild $ do
                    validateObjectLocations child                    
                    allowRevoked $ do
                        validAspa <- vHoist $ validateAspa validationRFC now aspa fullCa validCrl verifiedResources
                        oneMoreAspa
                        let aspaPayload = getCMSContent $ cmsPayload aspa                        
                        increment $ topDownCounters ^. #originalAspa
                        shortcut <- vHoist $ shortcutIfNoIssues childKey fileName
                                            (makeAspaShortcut childKey validAspa aspaPayload)                        
                        rememberPayloads typed (aspaPayload :)    
                        pure $! newShortcut shortcut

            BgpRO bgpCert ->
                focusOnChild $ do
                    validateObjectLocations child
                    allowRevoked $ do
                        (validaBgpCert, bgpPayload) <- vHoist $ validateBgpCert now bgpCert fullCa validCrl
                        oneMoreBgp
                        shortcut <- vHoist $ shortcutIfNoIssues childKey fileName
                                            (makeBgpSecShortcut childKey validaBgpCert bgpPayload)    
                        
                        rememberPayloads typed (bgpPayload :)
                        pure $! newShortcut shortcut

            GbrRO gbr ->                 
                focusOnChild $ do
                    validateObjectLocations child                    
                    allowRevoked $ do
                        validGbr <- vHoist $ validateGbr validationRFC now gbr fullCa validCrl verifiedResources
                        oneMoreGbr
                        let gbr' = getCMSContent $ cmsPayload gbr
                        let gbrPayload = T2 (getHash gbr) gbr'                        
                        shortcut <- vHoist $ shortcutIfNoIssues childKey fileName
                                            (makeGbrShortcut childKey validGbr gbrPayload)
                        rememberPayloads typed (gbrPayload :)                        
                        pure $! newShortcut shortcut       

            -- Any new type of object should be added here, otherwise
            -- they will emit a warning.
            _somethingElse -> 
                focusOnChild $ do
                    logWarn logger [i|Unsupported type of object: #{locations}.|]                
                    pure $! newShortcut (makeChildWithIssues childKey fileName)

        where
            -- In case of RevokedResourceCertificate error, the whole manifest is not to be considered 
            -- invalid, only the object with the revoked certificate is considered invalid.
            -- Replace RevokedResourceCertificate error with a warning and don't break the 
            -- validation process.            
            -- This is a hacky and ad-hoc, but it works fine.
            allowRevoked f =
                catchAndEraseError f isRevokedCertError $ do
                    vWarn RevokedResourceCertificate
                    pure $! newShortcut (makeChildWithIssues childKey fileName)
                where
                    isRevokedCertError (ValidationE RevokedResourceCertificate) = True
                    isRevokedCertError _ = False

    -- Don't create shortcuts for objects with warnings in their scope, 
    -- otherwise warnings will be reported only once for the original 
    -- and never for shortcuts.
    shortcutIfNoIssues key fileName makeShortcut = do 
        issues <- thisScopeIssues
        pure $! if Set.null issues 
                    then makeShortcut fileName
                    else makeChildWithIssues key fileName

    thisScopeIssues :: PureValidatorT (Set VIssue)
    thisScopeIssues = 
        withCurrentScope $ \scopes vs -> 
            getIssues (scopes ^. typed) (vs ^. typed)


    collectPayloadsFromShortcuts :: MftShortcut 
                                -> Maybe [T3 Text Hash ObjectKey] 
                                -> ValidatorT IO (Located CaCerObject)
                                -> ValidatorT IO (Keyed (Validated CrlObject))             
                                -> AllResources
                                -> ValidatorT IO ()
    collectPayloadsFromShortcuts mftShortcut childrenToCheck findFullCa findValidCrl parentCaResources = do      
        
        let nonCrlEntries = mftShortcut ^. #nonCrlEntries

        -- Filter children that we actually want to go through here
        let filteredChildren = 
                case childrenToCheck of 
                    Nothing -> Map.toList nonCrlEntries
                    Just ch -> catMaybes [ (k,) <$> Map.lookup k nonCrlEntries | T3 _ _ k <- ch ]

        let T3 caCount troubledCount totalCount = 
                foldr (\(_, MftEntry {..}) (T3 cas troubled total) -> 
                        case child of 
                            CaChild {}       -> T3 (cas + 1) troubled       (total + 1)
                            TroubledChild {} -> T3 cas       (troubled + 1) (total + 1)
                            _                -> T3 cas       troubled       (total + 1)
                    ) (T3 0 0 0 :: T3 Int Int Int) filteredChildren

        vFocusOn ObjectFocus (mftShortcut ^. #key) $ do
            validateLocationForShortcut (mftShortcut ^. #key)
            vHoist $ validateObjectValidityPeriod mftShortcut now
            vFocusOn ObjectFocus (mftShortcut ^. #crlShortcut . #key) $
                vHoist $ validateObjectValidityPeriod (mftShortcut ^. #crlShortcut) now    

            -- For children that are problematic we'll have to fall back 
            -- to full validation, for that we beed the parent CA and a valid CRL.
            -- Construct the validation function for such problematic children
            troubledValidation <-
                    case troubledCount of 
                        0 -> pure $! \_ _ -> 
                                -- Should never happen, there are no troubled children
                                internalError appContext [i|Impossible happened!|]
                        _  -> do 
                            caFull   <- findFullCa
                            validCrl <- findValidCrl
                            pure $! \childKey fileName -> 
                                    validateTroubledChild caFull fileName validCrl childKey                        

            collectResultsInParallel caCount totalCount filteredChildren (getChildPayloads troubledValidation)

      where
        collectResultsInParallel caCount totalCount children f = do 
            -- Pick up some good parallelism to avoid too many threads, 
            -- but also process big manifests quicker
            let caPerThread = 50            
            let eePerThread = 500
            let threads = min 
                    (caCount `div` caPerThread + (totalCount - caCount) `div` eePerThread)
                    (fromIntegral $ config ^. #parallelism . #cpuParallelism)                        
            
            let forAllChildren = 
                    if threads <= 1
                        then forM 
                        else pooledForConcurrentlyN threads

            scopes <- askScopes
            z <- liftIO $ forAllChildren children $ runValidatorT scopes . f
            embedState $ mconcat $ map snd z                 

        validateTroubledChild caFull fileName (Keyed validCrl _) childKey = do  
            -- It was an invalid child and nothing about it is cached, so 
            -- we have to process full validation for it           
            db <- liftIO $ readTVarIO database            
            childObject <- 
                DB.roAppTx db $ \tx -> do
                    increment $ topDownCounters ^. #readParsed
                    getParsedObject tx db childKey $ do 
                        increment $ topDownCounters ^. #readOriginal
                        getLocatedOriginalUnknownType tx db childKey $     
                            internalError appContext 
                                [i|Internal error, can't find a troubled child by its key #{childKey}.|]

            void $ validateChildObject caFull childObject fileName validCrl

        getChildPayloads troubledValidation (childKey, MftEntry {..}) = do 
            markAsRead topDownContext childKey            
            case child of 
                CaChild caShortcut _ ->                     
                    validateCa appContext topDownContext (CaShort caShortcut)
                        
                RoaChild r@RoaShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do                    
                        validateShortcut r key                   
                        oneMoreRoa
                        moreVrps $ Count $ fromIntegral $ length vrps
                        increment $ topDownCounters ^. #shortcutRoa
                        rememberPayloads typed (T2 vrps childKey :)

                SplChild s@SplShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do
                        validateShortcut s key
                        oneMoreSpl                        
                        increment $ topDownCounters ^. #shortcutSpl
                        rememberPayloads typed (splPayload :)
                
                AspaChild a@AspaShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do 
                        validateShortcut a key
                        oneMoreAspa
                        increment $ topDownCounters ^. #shortcutAspa                        
                        rememberPayloads typed (aspa :)                        

                BgpSecChild b@BgpSecShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do 
                        validateShortcut b key
                        oneMoreBgp                
                        rememberPayloads typed (bgpSec :)

                GbrChild g@GbrShortcut {..} _ -> 
                    vFocusOn ObjectFocus childKey $ do
                        validateShortcut g key 
                        oneMoreGbr                 
                        rememberPayloads typed (gbr :)

                TroubledChild childKey_ -> do
                    increment $ topDownCounters ^. #shortcutTroubled
                    troubledValidation childKey_ fileName
    
        validateShortcut :: (WithValidityPeriod s, HasField' "resources" s AllResources) => s -> ObjectKey -> ValidatorT IO ()
        validateShortcut r key = do
            validateLocationForShortcut key
            vHoist $ do                 
                validateObjectValidityPeriod r now
                case validationRFC of
                    StrictRFC       -> pure ()
                    ReconsideredRFC 
                        | overclaimingHappened -> 
                            void $ validateChildParentResources validationRFC 
                                (r ^. #resources) parentCaResources verifiedResources
                        | otherwise -> pure ()
            


    -- TODO This is pretty bad, it's easy to forget to do it
    rememberPayloads :: forall m a . MonadIO m => Getting (IORef a) PayloadBuilder (IORef a) -> (a -> a) -> m ()
    rememberPayloads lens_ f = do
        pure ()
        let builder = topDownContext ^. #payloadBuilder . lens_        
        liftIO $! atomicModifyIORef' builder $ \b -> let !z = f b in (z, ())


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
        -- newMftChidlren, we only have the entries that are not present on the new manifest,
        -- i.e. the deleted ones.
        foldr (\(T3 fileName _ key_) entries -> 
                case Map.lookup key_ (mftShortcut ^. #nonCrlEntries) of 
                    Nothing -> entries
                    Just e 
                        | e ^. #fileName == fileName -> Map.delete key_ entries
                        | otherwise -> entries) 
                (mftShortcut ^. #nonCrlEntries)
                newMftChidlren


getLocatedOriginal :: Storage s => Tx s mode -> DB s -> ObjectKey -> RpkiObjectType         
                    -> ValidatorT IO (Keyed (Located RpkiObject))
                    -> ValidatorT IO (Keyed (Located RpkiObject))
getLocatedOriginal tx db key type_ ifNotFound =
    getLocatedOriginal' tx db key (Just type_) ifNotFound

getLocatedOriginalUnknownType :: Storage s => Tx s mode -> DB s -> ObjectKey                                           
                                -> ValidatorT IO (Keyed (Located RpkiObject))
                                -> ValidatorT IO (Keyed (Located RpkiObject))
getLocatedOriginalUnknownType tx db key ifNotFound =
    getLocatedOriginal' tx db key Nothing ifNotFound

getLocatedOriginal' :: Storage s =>                    
                    Tx s mode                    
                    -> DB s
                    -> ObjectKey           
                    -> Maybe RpkiObjectType         
                    -> ValidatorT IO (Keyed (Located RpkiObject))
                    -> ValidatorT IO (Keyed (Located RpkiObject))
getLocatedOriginal' tx db key type_ ifNotFound = do
    DB.getOriginalBlob tx db key >>= \case 
        Nothing                    -> ifNotFound
        Just (ObjectOriginal blob) -> do 
            case type_  of 
                Nothing -> 
                    DB.getObjectMeta tx db key >>= \case            
                        Nothing               -> ifNotFound
                        Just (ObjectMeta _ t) -> parse blob t
                Just t -> 
                    parse blob t
  where                    
    parse blob t = do
        ro <- vFocusOn ObjectFocus key $ 
                    vHoist $ readObjectOfType t blob
        DB.getLocationsByKey tx db key >>= \case                                             
            Nothing        -> ifNotFound
            Just locations -> pure $! Keyed (Located locations ro) key

getParsedObject :: Storage s =>
                    Tx s mode
                    -> DB s
                    -> ObjectKey
                    -> ValidatorT IO (Keyed (Located RpkiObject))
                    -> ValidatorT IO (Keyed (Located RpkiObject))
getParsedObject tx db key ifNotFound = do
    DB.getLocatedByKey tx db key >>= \case 
        Just ro -> pure $! Keyed ro key
        Nothing -> ifNotFound


getFullCa :: Storage s => AppContext s -> TopDownContext -> Ca -> ValidatorT IO (Located CaCerObject)
getFullCa appContext@AppContext {..} topDownContext = \case    
    CaFull c -> pure c            
    CaShort CaShortcut {..} -> do   
        db <- liftIO $ readTVarIO database
        DB.roAppTx db $ \tx -> do 
            increment $ topDownContext ^. #allTas . #topDownCounters . #readParsed
            z <- getParsedObject tx db key $ do
                    increment $ topDownContext ^. #allTas . #topDownCounters . #readOriginal
                    getLocatedOriginal tx db key CER $ 
                        internalError appContext 
                            [i|Internal error, can't find a CA by its key #{key}.|]            
            case z of 
                Keyed (Located locations (CerRO ca_)) _ -> pure $! Located locations ca_
                _ -> internalError appContext 
                        [i|Internal error, wrong type of the CA found by its key #{key}.|]            
    

getCrlByKey :: Storage s => AppContext s -> ObjectKey -> ValidatorT IO (Keyed (Validated CrlObject))
getCrlByKey appContext@AppContext {..} crlKey = do        
    z <- roTxT database $ \tx db -> DB.getObjectByKey tx db crlKey
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
        resources = getRawCert certificate ^. #resources
        child = CaChild (CaShortcut {..}) serial
    in MftEntry {..}

makeRoaShortcut :: ObjectKey -> Validated RoaObject -> [Vrp] -> Text -> MftEntry
makeRoaShortcut key (Validated roa) vrps fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod roa    
        serial = getSerial roa
        resources = getRawCert roa ^. #resources
        child = RoaChild (RoaShortcut {..}) serial
    in MftEntry {..}    

makeSplShortcut :: ObjectKey -> Validated SplObject -> SplPayload -> Text -> MftEntry
makeSplShortcut key (Validated spl) splPayload fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod spl
        serial = getSerial spl
        resources = getRawCert spl ^. #resources
        child = SplChild (SplShortcut {..}) serial
    in MftEntry {..}    

makeAspaShortcut :: ObjectKey -> Validated AspaObject -> Aspa -> Text -> MftEntry
makeAspaShortcut key (Validated aspaObject) aspa fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod aspaObject            
        serial = getSerial aspaObject
        resources = getRawCert aspaObject ^. #resources
        child = AspaChild (AspaShortcut {..}) serial
    in MftEntry {..}    

makeGbrShortcut :: ObjectKey -> Validated GbrObject -> T2 Hash Gbr -> Text -> MftEntry
makeGbrShortcut key (Validated gbrObject) gbr fileName = let 
        (notValidBefore, notValidAfter) = getValidityPeriod gbrObject    
        serial = getSerial gbrObject
        resources = getRawCert gbrObject ^. #resources
        child = GbrChild (GbrShortcut {..}) serial       
    in MftEntry {..}    

makeBgpSecShortcut :: ObjectKey -> Validated BgpCerObject -> BGPSecPayload -> Text -> MftEntry
makeBgpSecShortcut key (Validated bgpCert) bgpSec fileName = let         
        (notValidBefore, notValidAfter) = getValidityPeriod bgpCert                  
        serial = getSerial bgpCert
        resources = getRawCert bgpCert ^. #resources        
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
    serial = getSerial mftObject
    manifestNumber = (getCMSContent $ cmsPayload mftObject) ^. #mftNumber
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

-- Same as vFocusOn but it checks that there are no duplicates in the scope focuses, 
-- i.e. we are not returning to the same object again. That would mean we have detected
-- a loop in references.
vUniqueFocusOn :: Monad m => (a -> Focus) -> a -> ValidatorT m r -> ValidatorT m () -> ValidatorT m r
vUniqueFocusOn c a f nonUniqueError = do
    Scopes { validationScope = Scope vs } <- vHoist $ withCurrentScope $ \scopes _ -> scopes
    let focus = c a
    unless (null (NonEmpty.filter (==focus) vs)) nonUniqueError
    vFocusOn c a f 
        

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
    (visitedSize, elapsed) <- timedMS $ do
        vks <- atomically $ readTVar visitedKeys            
        rwTxT database $ \tx db -> DB.markAsValidated tx db vks worldVersion        
        pure $! Set.size vks
    
    liftIO $ reportCounters appContext topDownCounters        
    logDebug logger $ [i|Marked #{visitedSize} objects as used, took #{elapsed}ms.|]


-- This is to be able to print all counters as Int, not Identity Int
newtype IdenticalShow a = IdenticalShow a
    deriving stock (Generic)
    deriving (Functor)

instance Show a => Show (IdenticalShow a) where
    show (IdenticalShow a) = show a

reportCounters :: AppContext s -> TopDownCounters IORef -> IO ()
reportCounters AppContext {..} counters = do
    c <- btraverse (fmap IdenticalShow . readIORef) counters
    logDebug logger $ fmtGen c
                       

updateMftShortcut :: MonadIO m => TopDownContext -> AKI -> MftShortcut -> m ()
updateMftShortcut TopDownContext { allTas = AllTasTopDownContext {..} } aki MftShortcut {..} = 
    liftIO $ do 
        let !raw = Verbatim $ toStorable $ Compressed $ DB.MftShortcutMeta {..}
        atomically $ writeCQueue shortcutQueue (UpdateMftShortcut aki raw)        

updateMftShortcutChildren :: MonadIO m => TopDownContext -> AKI -> MftShortcut -> m ()
updateMftShortcutChildren TopDownContext { allTas = AllTasTopDownContext {..} } aki MftShortcut {..} = 
    liftIO $ do 
        -- Pre-serialise the object so that all the heavy-lifting happens in the thread 
        let !raw = Verbatim $ toStorable $ Compressed DB.MftShortcutChildren {..}        
        atomically $ writeCQueue shortcutQueue (UpdateMftShortcutChildren aki raw) 
    
storeShortcuts :: (Storage s, MonadIO m) => 
                AppContext s 
             -> ClosableQueue MftShortcutOp -> m ()
storeShortcuts AppContext {..} shortcutQueue = liftIO $   
    readQueueChunked shortcutQueue 1000 $ \mftShortcuts -> 
        rwTxT database $ \tx db -> 
            for_ mftShortcuts $ \case 
                UpdateMftShortcut aki s         -> DB.saveMftShorcutMeta tx db aki s                    
                UpdateMftShortcutChildren aki s -> DB.saveMftShorcutChildren tx db aki s
                 

data MftShortcutOp = UpdateMftShortcut AKI (Verbatim (Compressed DB.MftShortcutMeta))
                   | UpdateMftShortcutChildren AKI (Verbatim (Compressed DB.MftShortcutChildren))            

-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
markAsRead :: TopDownContext -> ObjectKey -> ValidatorT IO ()
markAsRead TopDownContext { allTas = AllTasTopDownContext {..} } k = 
    liftIO $ atomically $ modifyTVar' visitedKeys (Set.insert k)

markAsReadByHash :: Storage s => 
                    AppContext s -> TopDownContext -> Hash -> ValidatorT IO ()
markAsReadByHash AppContext {..} topDownContext hash = do
    key <- roTxT database $ \tx db -> DB.getKeyByHash tx db hash
    for_ key $ markAsRead topDownContext              

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl :: Monad m => ValidatorT m ()
oneMoreGbr, oneMoreAspa, oneMoreBgp, oneMoreSpl :: Monad m => ValidatorT m ()
oneMoreMftShort :: Monad m => ValidatorT m ()
oneMoreCert = updateMetric @ValidationMetric @_ (& #validCertNumber %~ (+1))
oneMoreRoa  = updateMetric @ValidationMetric @_ (& #validRoaNumber %~ (+1))
oneMoreSpl  = updateMetric @ValidationMetric @_ (& #validSplNumber %~ (+1))
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
addUniqueVRPCount !s = let
        vrpCountLens = typed @ValidationState . typed @RawMetric . #vrpCounts
        totalUnique = Count (fromIntegral $ uniqueVrpCount $ (s ^. #payloads) ^. #vrps)
        perTaUnique = MonoidalMap.map (Count . fromIntegral . V.length) (unVrps $ (s ^. #payloads) ^. #vrps)   
    in s & vrpCountLens . #totalUnique .~ totalUnique                
         & vrpCountLens . #perTaUnique .~ perTaUnique

extractPPAs :: Ca -> Either ValidationError PublicationPointAccess
extractPPAs = \case 
    CaShort (CaShortcut {..}) -> Right ppas 
    CaFull c                  -> getPublicationPointsFromCertObject $ c ^. #payload

getCaLocations :: Storage s => AppContext s -> Ca -> ValidatorT IO (Maybe Locations)
getCaLocations AppContext {..} = \case 
    CaShort (CaShortcut {..}) -> 
        roTxT database $ \tx db -> DB.getLocationsByKey tx db key
    CaFull c -> 
        pure $! Just $ getLocations c


data ManifestValidity e v = InvalidEntry e v 
                            | InvalidChild e v ObjectKey Text
                            | ValidEntry v (Maybe MftEntry) ObjectKey Text

makeChildWithIssues :: ObjectKey -> Text -> MftEntry
makeChildWithIssues childKey fileName = 
    MftEntry { child = TroubledChild childKey, .. }     


bumpCounterBy :: (MonadIO m, Num a) =>
                s -> Getting (IORef a) s (IORef a) -> a -> m ()
bumpCounterBy counters counterLens n = liftIO $     
    atomicModifyIORef' (counters ^. counterLens) $ \c -> (c + n, ())        

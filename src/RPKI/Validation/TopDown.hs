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
    validateMutlipleTAs
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
import qualified Data.Set.NonEmpty                as NESet
import           Data.Map.Strict                  (Map)
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
import qualified RPKI.Util.KeyMap as KeyMap


{-
This module implements the top-down validation algorithm.

Validation starts from the Trust Anchor (TA) certificate. The process of downloading 
and selecting the certificate implements the tie-breaking logic described 
in https://datatracker.ietf.org/doc/draft-spaghetti-sidrops-rpki-ta-tiebreaker/.

After that, validation is recursive for each CA:

- Manifests and manifest shortcuts (see below) are found.
- Depending on the freshness of the shortcut and the manifest, we either use 
  the shortcut data or re-validate using the new manifest.
- For new manifests, shortcuts are re-created and saved into a separate queue.

The idea behind shortcuts is as follows:

 - We store minimal representations of objects and their payloads to cache their 
   essential information. This avoids re-validating everything on each run.
 - Manifest shortcuts contain basic manifest metadata plus a list of their children. 
   Children are embedded into the manifest shortcut (TODO: They should also be referred 
   to by ObjectKey for big manifests). This structure avoids re-validating manifest 
   children that have already been validated.
 - This is why the logic in validateCa is very long and tedious.
 
 Validation is designed to be non-interfering with other processes, so it's safe to run 
 concurrently with fetching or cleanup operations (both of which are atomic).

-}

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
        interruptedByLimit      :: TVar Limited,
        payloadBuilder          :: PayloadBuilder,
        overclaimingHappened    :: Bool,
        fetcheables             :: TVar Fetcheables,
        earliestNotValidAfter   :: TVar EarliestToExpire
    }
    deriving stock (Generic)


data AllTasTopDownContext = AllTasTopDownContext {
        now                  :: Now,
        worldVersion         :: WorldVersion,
        visitedKeys          :: TVar (Set ObjectKey),        
        publicationPoints    :: PublicationPoints,
        shortcutQueue        :: ClosableQueue MftShortcutOp,
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
        payloads               :: Payloads,
        roas                   :: Roas,
        topDownValidations     :: ValidationState,
        discoveredRepositories :: Fetcheables,
        earliestNotValidAfter  :: EarliestToExpire
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving Semigroup via GenericSemigroup TopDownResult
    deriving Monoid    via GenericMonoid TopDownResult

fromValidations :: ValidationState -> TopDownResult
fromValidations vs = TopDownResult mempty mempty vs mempty mempty

newTopDownContext :: MonadIO m =>
                    TaName
                    -> AllTasTopDownContext
                    -> m TopDownContext
newTopDownContext taName allTas = 
    liftIO $ do 
        payloadBuilder <- newPayloadBuilder
        atomically $ do
            -- let verifiedResources = Just $ createVerifiedResources certificate
            let verifiedResources = Nothing
                currentPathDepth = 0
                overclaimingHappened = False       
            interruptedByLimit      <- newTVar CanProceed                 
            fetcheables             <- newTVar mempty                 
            earliestNotValidAfter   <- newTVar mempty
            pure $! TopDownContext {..}

newAllTasTopDownContext :: MonadIO m =>
                        WorldVersion
                        -> PublicationPoints 
                        -> ClosableQueue MftShortcutOp
                        -> m AllTasTopDownContext
newAllTasTopDownContext worldVersion publicationPoints shortcutQueue = liftIO $ do 
    let now = Now $ versionToInstant worldVersion
    topDownCounters <- newTopDownCounters
    atomically $ do        
        visitedKeys    <- newTVar mempty 
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



-- | It is the main entry point for the top-down validation. 
-- Validates a bunch of TAs starting from their TALs.  
validateMutlipleTAs :: Storage s =>
                    AppContext s
                    -> WorldVersion
                    -> [TAL]
                    -> IO (Map TaName TopDownResult)
validateMutlipleTAs appContext@AppContext {..} worldVersion tals = do

    fst <$> bracketChanClosable
                5000
                validateMutlipleTAs'
                (storeShortcuts appContext)
                (\_ -> pure ())
  where
    validateMutlipleTAs' queue = do 
        publicationPoints <- addRsyncPrefetchUrls <$> roTxT database DB.getPublicationPoints            
        allTas <- newAllTasTopDownContext worldVersion publicationPoints queue
        validateThem allTas
            `finally` 
            applyValidationSideEffects appContext allTas
    
    validateThem allTas =
        fmap Map.fromList $ 
            forConcurrently tals $ \tal -> do
                (r@TopDownResult{ payloads = Payloads {..}}, elapsed) <- timedMS $
                        validateTA appContext tal worldVersion allTas
                logInfo logger [i|Validated TA '#{getTaName tal}', got #{estimateVrpCountRoas roas} VRPs, took #{elapsed}ms|]
                pure (getTaName tal, r)
                 
    addRsyncPrefetchUrls pps =     
        foldr (mergePP . rsyncPP) pps (config ^. #rsyncConf . #rsyncPrefetchUrls)

--
validateTA :: Storage s =>
            AppContext s
            -> TAL
            -> WorldVersion
            -> AllTasTopDownContext
            -> IO TopDownResult
validateTA appContext@AppContext{..} tal worldVersion allTas = do
    let maxDuration = config ^. typed @ValidationConfig . #topDownTimeout
    topDownContext <- newTopDownContext taName allTas
    (r, topDownValidations) <- runValidatorT taContext $
            timeoutVT
                maxDuration
                (validateFromTAL topDownContext)
                (do
                    logError logger [i|Validation for TA #{taName} did not finish within #{maxDuration} and was interrupted.|]
                    appError $ ValidationE $ ValidationTimeout maxDuration)    

    (discoveredRepositories, earliestNotValidAfter) <- 
        atomically $ (,) <$>
            readTVar (topDownContext ^. #fetcheables) <*>
            readTVar (topDownContext ^. #earliestNotValidAfter)

    logDebug logger [i|Earliest #{taName}, earliestNotValidAfter=#{earliestNotValidAfter}|]

    case r of
        Left _ -> 
            pure $ fromValidations topDownValidations & #discoveredRepositories .~ discoveredRepositories
        Right _ -> do     
            let builder = topDownContext ^. #payloadBuilder
            vrps     <- readIORef $ builder ^. #vrps 
            aspas    <- fmap Set.fromList $ readIORef $ builder ^. #aspas 
            gbrs     <- fmap Set.fromList $ readIORef $ builder ^. #gbrs 
            bgpCerts <- fmap Set.fromList $ readIORef $ builder ^. #bgpCerts    

            splPayloads <- readIORef $ builder ^. #spls            
            let spls = Set.fromList [ SplN asn prefix | 
                                      SplPayload asn prefixes <- splPayloads, prefix <- prefixes ]

            let roas = Roas $ MonoidalMap.fromList $ 
                            map (\(T2 vrp k) -> (k, V.fromList vrp)) vrps 

            let payloads = Payloads {..}                    
            
            pure $ TopDownResult {..}

  where
    taName = getTaName tal
    taContext = newScopes' TAFocus $ unTaName taName

    validateFromTAL topDownContext = do
        timedMetric (Proxy :: Proxy ValidationMetric) $
            vFocusOn LocationFocus (getURL $ getTaCertURL tal) $ do
                (taCert, repos) <- validateTACertificateFromTAL appContext tal worldVersion    
                -- This clumsy code is to make it possible to construct topDownContext
                -- before getting and validating the TA certificate
                let topDownContext' = topDownContext & #verifiedResources ?~ createVerifiedResources (taCert ^. #payload)
                validateFromTACert appContext topDownContext' repos taCert

        

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
    let now = Now $ versionToInstant worldVersion
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
                (u, ro) <- fetchTACertificate appContext (newFetchConfig config) tal
                pure $ FetchedTA u ro)
            `catchError`
                tryToFallbackToCachedCopy

        case z of     
            FetchedTA actualUrl object -> do                                 
                certToUse <- case storableTa of
                    Nothing  -> vHoist $ validateTACert tal actualUrl object
                    Just StorableTA { taCert = cachedTaCert } -> 
                        vHoist (do 
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

    locatedTaCert url = Located (toLocations url)


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
    fromTryM
        (UnspecifiedE (unTaName taName) . fmtEx)
        (do
            -- TODO That might not be necessary
            let publicationPoints' = 
                    case filterPPAccess config initialRepos of 
                        Just filteredRepos -> foldr mergePP publicationPoints $ unPublicationPointAccess filteredRepos
                        Nothing            -> publicationPoints
            validateCa appContext 
                (topDownContext & #allTas . #publicationPoints .~ publicationPoints') 
                (CaFull taCert))


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
    repositoryCountLimit = let 
            maxRepositories = validationConfig ^. #maxTaRepositories in 
        (do
            (\fs -> repositoryCount fs > maxRepositories) <$> readTVar fetcheables,
        logCheck
            (TooManyRepositories maxRepositories)
            (\loc -> [i|Interrupting validation on #{fmtLocations loc}, maximum total new repository count per TA #{maxRepositories} is reached.|])
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
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    ca = 
    case extractPPAs ca of        
        Left e         -> vError e
        Right ppAccess -> do                
            let caFetcheables = getFetchables publicationPoints ppAccess

            -- Add these PPs to the validation-wide set of fetcheables, 
            -- i.e. all newly discovered publication points/repositories                        
            mergeFetcheables caFetcheables

            -- Do not validate if nothing was fetched for this CA
            -- otherwise we'll have a lot of useless errors about 
            -- missing manifests, so just don't go there
            unless (all ((== Pending) . snd) caFetcheables) $ do   
                let primaryUrl = getPrimaryRepositoryUrl publicationPoints ppAccess
                vFocusOn PPFocus primaryUrl $
                    metricFocusOn PPFocus primaryUrl $
                        validateCaNoFetch appContext topDownContext ca
  where
    mergeFetcheables caFetcheables =
        case map fst caFetcheables of 
            -- Expect either one of two PPs per CA
            primary : (listToMaybe -> fallback) -> do 
                liftIO $ atomically $ modifyTVar' fetcheables (<> newFetcheables primary fallback)
            weirdCaUrls -> do 
                logError logger [i|Found CA certificate with uncommon publication points: #{weirdCaUrls}.|]
                appError $ ValidationE $ WeirdCaPublicationPoints weirdCaUrls                        


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
        CaFull c -> 
            vFocusOn LocationFocus (getURL $ pickLocation $ getLocations c) $ do
                increment $ topDownCounters ^. #originalCa
                markAsUsedByHash appContext topDownContext (getHash c)                         
                validateObjectLocations c
                (_, notValidAfter) <- vHoist $ validateObjectValidityPeriod c now
                rememberNotValidAfter topDownContext notValidAfter
                oneMoreCert
                join $! nextAction $ toAKI $ getSKI c
        CaShort c -> 
            vFocusOn ObjectFocus (c ^. #key) $ do            
                increment $ topDownCounters ^. #shortcutCa 
                markAsUsed topDownContext (c ^. #key) 
                validateLocationForShortcut (c ^. #key)
                (_, notValidAfter) <- vHoist $ validateObjectValidityPeriod c now
                rememberNotValidAfter topDownContext notValidAfter
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
    makeNextFullValidationAction aki = do 
        mfts <- roTxT database $ \tx db -> DB.getMftsForAKI tx db aki
        pure $! processMfts aki mfts

    makeNextIncrementalAction :: AKI -> ValidatorT IO (ValidatorT IO ())
    makeNextIncrementalAction aki = do
        z <- roTxT database $ \tx db -> DB.getMftsForAKI tx db aki
        case z of
            []   -> pure $! vError $ NoMFT aki
            mfts -> actOnMfts mfts
      where
        actOnMfts mfts = do
            z <- roTxT database $ \tx db -> DB.getMftShorcut tx db aki
            case z of
                Nothing -> do
                    increment $ topDownCounters ^. #originalMft
                    pure $! processMfts aki mfts
                Just mftShortcut -> do
                    let mftShortcutKey = mftShortcut ^. #key
                    markAsUsed topDownContext mftShortcutKey
                    increment $ topDownCounters ^. #shortcutMft
                    action <- case mftsNotInFuture mfts of
                        [] -> vError $ NoMFT aki
                        mft_ : otherMfts 
                            | mft_ ^. #key == mftShortcutKey -> 
                                pure $! onlyCollectPayloads mftShortcut                                    
                            | otherwise -> pure $! do 
                                let mftKey = mft_ ^. #key
                                markAsUsed topDownContext mftKey
                                withMft mftKey $ \mft ->                                                                     
                                    tryOneMftWithShortcut mftShortcut mft
                                        `catchError` \e -> 
                                            if isWithinValidityPeriod now mftShortcut 
                                                then do
                                                    -- shortcut is still valid so fall back to it
                                                    vFocusOn ObjectFocus mftKey $ vWarn $ MftFallback e
                                                    let mftLocation = pickLocation $ getLocations $ mft ^. #object
                                                    logWarn logger [i|Falling back to the last valid manifest for #{mftLocation}, error: #{toMessage e}|]
                                                    onlyCollectPayloads mftShortcut                   
                                                else 
                                                    -- shortcut is too old, so continue with the other manifests
                                                    tryMfts aki otherMfts
                    pure $! action `andThen` 
                           (oneMoreMft >> oneMoreCrl >> oneMoreMftShort)
      
        tryOneMftWithShortcut mftShortcut mft = do
            fullCa <- getFullCa appContext topDownContext ca
            let crlKey = mftShortcut ^. #crlShortcut . #key
            markAsUsed topDownContext crlKey
            overlappingChildren <- manifestFullValidation fullCa mft (Just mftShortcut) aki
            collectPayloads mftShortcut (Just overlappingChildren) 
                        (pure fullCa)
                        (findAndValidateCrl fullCa mft aki)   
                        (getResources ca)

        onlyCollectPayloads mftShortcut = do 
            let crlKey = mftShortcut ^. #crlShortcut . #key                
            markAsUsed topDownContext crlKey
            collectPayloads mftShortcut Nothing 
                    (getFullCa appContext topDownContext ca)
                    (getCrlByKey appContext crlKey)
                    (getResources ca)             

    processMfts childrenAki mfts = do
        case (mftsNotInFuture mfts, mfts) of             
            ([], []) -> vError $ NoMFT childrenAki
            -- if there are only manifest(s) in the future, use them 
            -- anyway to have a meaningful error message
            ([], _) -> tryMfts childrenAki mfts
            -- If there're manifests that are not in the future, 
            -- use only them and skip the future ones
            (relevantMfts, _) -> tryMfts childrenAki relevantMfts
      

    tryMfts aki []              = vError $ NoMFT aki
    tryMfts aki (mftRef: mfts_) = 
        withMft (mftRef ^. #key) $ \mft -> do 
            tryOneMft mft `catchError` \e -> 
                case mfts_ of 
                    [] -> appError e
                    _  -> do 
                        vFocusOn ObjectFocus (mft ^. #key) $ vWarn $ MftFallback e
                        let mftLocation = pickLocation $ getLocations $ mft ^. #object
                        logWarn logger [i|Falling back to the previous manifest for #{mftLocation}, error: #{toMessage e}|]
                        tryMfts aki mfts_
      where
        tryOneMft mft = do                 
            markAsUsed topDownContext $ mft ^. #key                
            caFull <- getFullCa appContext topDownContext ca
            void $ manifestFullValidation caFull mft Nothing aki
            oneMoreMft >> oneMoreCrl         

    withMft key f = do 
        z <- roTxT database $ \tx db -> DB.getMftByKey tx db key
        case z of 
            Nothing  -> integrityError appContext [i|Referential integrity error, can't find a manifest by its key #{key}.|]
            Just mft -> f mft

    mftsNotInFuture = filter (\MftMeta {..} -> thisTime <= unNow now)


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

            keyedValidCrl@(Keyed validCrl crlKey) <- findAndValidateCrl fullCa keyedMft childrenAki                

            -- MFT can be revoked by the CRL that is on this MFT -- detect 
            -- revocation as well, this is clearly an error                               
            validMft <- vHoist $ validateMft (config ^. #validationConfig . typed) 
                                    now mft fullCa validCrl verifiedResources

            rememberNotValidAfter topDownContext (snd $ getValidityPeriod mft)
            rememberCrlNextUpdate topDownContext validCrl

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
                    --
                    -- * But the shortcut may have expired already and there will no be any options left,
                    --   so we need to be careful and just emit a warning in this case
                    --
                    -- * So in case there is nothing to fall back to, we emit a warning and still 
                    --   use the manifest
                    let (beforeMft, afterMft) = getValidityPeriod mftShort
                    let issue = ManifestNumberDecreased (mftShort ^. #manifestNumber) mftNumber
                    if beforeMft < unNow now && unNow now > afterMft
                        then vError issue
                        else vWarn issue

            -- Mark all manifest entries as used to avoid the situation
            -- when some of the children are garbage-collected from the cache 
            -- and some are still there. Do it both in case of successful 
            -- validation or a validation error.
            let markAllEntriesAsUsed = do
                    forM_ (newChildren <> overlappingChildren) $
                        \(T3 _ _ k) -> markAsUsed topDownContext k

            let processChildren = do                                              
                    -- Here we have the payloads for the fully validated MFT children
                    -- and the shortcut objects for these children
                    --                                            
                    childrenShortcuts <- 
                        fmap (\shortcuts -> [ (k, s) | T2 k (Just s) <- shortcuts ]) $
                            gatherMftEntryResults =<< 
                                gatherMftEntryValidations fullCa newChildren validCrl

                    let newEntries = makeEntriesWithMap newChildren (KeyMap.fromList childrenShortcuts)

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

                                -- If manifest key is not the same as the shortcut key,
                                -- we need to replace the shortcut with the new one      
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

            processChildren `recover` markAllEntriesAsUsed


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
                            markAsUsed topDownContext crlKey
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
            entry      <- maybeToList $ KeyMap.lookup key entryMap ]


    -- Check if shortcut children are revoked
    checkForRevokedChildren mftShortcut (Keyed (Located _ mft) _) children validCrl = do        
        when (isRevoked (getSerial mft) validCrl) $
            vWarn RevokedResourceCertificate   
        forM_ children $ \(T3 _ _ childKey) ->
            for_ (KeyMap.lookup childKey (mftShortcut ^. #nonCrlEntries)) $ \MftEntry {..} ->
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
                    Just key -> do 
                        validateMftFileName fileName
                        pure $! T3 fileName hash key        
        where
            longerThanOne = \case 
                _:_:_ -> True
                _     -> False

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
                                -- we expect objects to be stored in the parsed-serialised form                                                            
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
                    integrityError appContext 
                        [i|Referential integrity error, can't find locations for the object #{key} with positive location count #{count}.|]
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


    collectPayloads :: MftShortcut 
                    -> Maybe [T3 Text Hash ObjectKey] 
                    -> ValidatorT IO (Located CaCerObject)
                    -> ValidatorT IO (Keyed (Validated CrlObject))             
                    -> AllResources
                    -> ValidatorT IO ()
    collectPayloads mftShortcut childrenToCheck findFullCa findValidCrl parentCaResources = do      
        
        let nonCrlEntries = mftShortcut ^. #nonCrlEntries

        -- Filter children that we actually want to go through here
        let filteredChildren = 
                case childrenToCheck of 
                    Nothing -> KeyMap.toList nonCrlEntries
                    Just ch -> catMaybes [ (k,) <$> KeyMap.lookup k nonCrlEntries | T3 _ _ k <- ch ]

        let T3 caCount troubledCount totalCount = 
                foldr (\(_, MftEntry {..}) (T3 cas troubled total) -> 
                        case child of 
                            CaChild {}       -> T3 (cas + 1) troubled       (total + 1)
                            TroubledChild {} -> T3 cas       (troubled + 1) (total + 1)
                            _                -> T3 cas       troubled       (total + 1)
                    ) (T3 0 0 0 :: T3 Int Int Int) filteredChildren

        vFocusOn ObjectFocus (mftShortcut ^. #key) $ do
            validateLocationForShortcut (mftShortcut ^. #key)
            (_, notValidAfter) <- vHoist $ validateObjectValidityPeriod mftShortcut now
            rememberNotValidAfter topDownContext notValidAfter
            vFocusOn ObjectFocus (mftShortcut ^. #crlShortcut . #key) $ do                
                (_, notValidAfterCrl) <- vHoist $ validateObjectValidityPeriod (mftShortcut ^. #crlShortcut) now
                rememberNotValidAfter topDownContext notValidAfterCrl

            -- For children that are problematic we'll have to fall back 
            -- to full validation, for that we beed the parent CA and a valid CRL.
            -- Construct the validation function for such problematic children
            troubledValidation <-
                    case troubledCount of 
                        0 -> pure $ \_ _ -> 
                                -- Should never happen, there are no troubled children
                                integrityError appContext [i|Impossible happened!|]
                        _  -> do 
                            caFull   <- findFullCa
                            validCrl <- findValidCrl
                            pure $ \childKey fileName -> 
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
                        getLocatedOriginalUnknownType tx db childKey $ do
                            -- Something is wrong with the references in the database. Normally it should never happen,
                            -- but if it does, we have to delete the shortcut and report the error.
                            deleteMftShortcut topDownContext $ toAKI $ getSKI caFull
                            logError logger [i|Troubled child #{childKey} not found in the database, will delete manifest shortcut.|]
                            integrityError appContext 
                                [i|Referential integrity error, can't find a troubled child by its key #{childKey}.|]

            void $ validateChildObject caFull childObject fileName validCrl

        getChildPayloads troubledValidation (childKey, MftEntry {..}) = do 
            markAsUsed topDownContext childKey            
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
            (_, notValidAfter) <- vHoist $ validateObjectValidityPeriod r now
            rememberNotValidAfter topDownContext notValidAfter
            vHoist $ case validationRFC of
                StrictRFC       -> pure ()
                ReconsideredRFC 
                    | overclaimingHappened -> 
                        void $ validateChildParentResources validationRFC 
                            (r ^. #resources) parentCaResources verifiedResources
                    | otherwise -> pure ()
            


    -- TODO This is pretty bad, it's easy to forget to do it
    rememberPayloads :: forall m a . MonadIO m => Getting (IORef a) PayloadBuilder (IORef a) -> (a -> a) -> m ()
    rememberPayloads lens_ f = do
        let builder = topDownContext ^. #payloadBuilder . lens_        
        liftIO $! atomicModifyIORef' builder $ \b -> let !z = f b in (z, ())


-- Calculate difference bentween a manifest shortcut 
-- and the list of children of the new manifest object.
manifestDiff :: MftShortcut 
            -> [T3 Text a ObjectKey] 
            -> ([T3 Text a ObjectKey], [T3 Text a ObjectKey], Bool)
manifestDiff mftShortcut newMftChidlren =                
    (newOnes, overlapping, not $ KeyMap.null deletedEntries)
  where
    (newOnes, overlapping) = List.partition 
            (\(T3 fileName _  k) -> isNewEntry k fileName) newMftChidlren                

    -- it's not in the map of shortcut children or it has changed 
    -- its name (very unlikely but can happen in theory)                
    isNewEntry key_ fileName  = 
        case KeyMap.lookup key_ (mftShortcut ^. #nonCrlEntries) of
            Nothing -> True
            Just e  -> e ^. #fileName /= fileName 
                    
    deletedEntries = 
        -- If we delete everything from mftShortcut.nonCrlEntries that is present in 
        -- newMftChidlren, we only have the entries that are not present on the new manifest,
        -- i.e. the deleted ones.
        foldr (\(T3 fileName _ key_) entries -> 
                case KeyMap.lookup key_ (mftShortcut ^. #nonCrlEntries) of 
                    Nothing -> entries
                    Just e 
                        | e ^. #fileName == fileName -> KeyMap.delete key_ entries
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
                        integrityError appContext 
                            [i|Referential integrity error, can't find a CA by its key #{key}.|]            
            case z of 
                Keyed (Located locations (CerRO ca_)) _ -> pure $! Located locations ca_
                _ -> integrityError appContext 
                        [i|Referential integrity error, wrong type of the CA found by its key #{key}.|]            
    

getCrlByKey :: Storage s => AppContext s -> ObjectKey -> ValidatorT IO (Keyed (Validated CrlObject))
getCrlByKey appContext@AppContext {..} crlKey = do        
    z <- roTxT database $ \tx db -> DB.getObjectByKey tx db crlKey
    case z of 
        Just (CrlRO c) -> pure $! Keyed (Validated c) crlKey 
        _ -> integrityError appContext [i|Referential integrity error, can't find a CRL by its key #{crlKey}.|]
     
    
integrityError :: AppContext s -> Text -> ValidatorT IO a
integrityError AppContext {..} message = do     
    logError logger message
    appError $ ValidationE $ ReferentialIntegrityError message  

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
    (Validated mftObject) (KeyMap.fromList -> nonCrlEntries) 
    (Keyed (Validated validCrl) crlKey) = 
  let 
    (notValidBefore, notValidAfter) = getValidityPeriod mftObject        
    serial = getSerial mftObject
    manifestNumber = getCMSContent (cmsPayload mftObject) ^. #mftNumber
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
    when (focus `elem` vs) nonUniqueError
    vFocusOn c a f 
        

-- | Mark validated objects in the database, i.e.
applyValidationSideEffects :: (MonadIO m, Storage s) =>
                              AppContext s -> AllTasTopDownContext -> m ()
applyValidationSideEffects
    appContext@AppContext {..}
    AllTasTopDownContext {..} = liftIO $ do        
    (visitedSize, elapsed) <- timedMS $ do
        vks <- readTVarIO visitedKeys            
        rwTxT database $ \tx db -> DB.markAsValidated tx db vks worldVersion        
        pure $! Set.size vks
    
    liftIO $ reportCounters appContext topDownCounters        
    logDebug logger [i|Marked #{visitedSize} objects as used, took #{elapsed}ms.|]


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
        atomically $ writeCQueue shortcutQueue $ UpdateMftShortcut aki raw  

updateMftShortcutChildren :: MonadIO m => TopDownContext -> AKI -> MftShortcut -> m ()
updateMftShortcutChildren TopDownContext { allTas = AllTasTopDownContext {..} } aki MftShortcut {..} = 
    liftIO $ do 
        -- Pre-serialise the object so that all the heavy-lifting happens in the thread 
        let !raw = Verbatim $ toStorable $ Compressed DB.MftShortcutChildren {..}        
        atomically $ writeCQueue shortcutQueue $ UpdateMftShortcutChildren aki raw

deleteMftShortcut :: MonadIO m => TopDownContext -> AKI -> m ()
deleteMftShortcut TopDownContext { allTas = AllTasTopDownContext {..} } aki = 
    liftIO $ atomically $ writeCQueue shortcutQueue $ DeleteMftShortcut aki

storeShortcuts :: (Storage s, MonadIO m) => 
                AppContext s 
             -> ClosableQueue MftShortcutOp -> m ()
storeShortcuts AppContext {..} shortcutQueue = liftIO $   
    readQueueChunked shortcutQueue 1000 $ \mftShortcuts -> 
        rwTxT database $ \tx db -> 
            for_ mftShortcuts $ \case 
                UpdateMftShortcut aki s         -> DB.saveMftShorcutMeta tx db aki s                    
                UpdateMftShortcutChildren aki s -> DB.saveMftShorcutChildren tx db aki s
                DeleteMftShortcut aki           -> DB.deleteMftShortcut tx db aki
                 

data MftShortcutOp = UpdateMftShortcut AKI (Verbatim (Compressed DB.MftShortcutMeta))
                   | UpdateMftShortcutChildren AKI (Verbatim (Compressed DB.MftShortcutChildren))            
                   | DeleteMftShortcut AKI            

-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
markAsUsed :: TopDownContext -> ObjectKey -> ValidatorT IO ()
markAsUsed TopDownContext { allTas = AllTasTopDownContext {..} } k = 
    liftIO $ atomically $ modifyTVar' visitedKeys (Set.insert k)

markAsUsedByHash :: Storage s => 
                    AppContext s -> TopDownContext -> Hash -> ValidatorT IO ()
markAsUsedByHash AppContext {..} topDownContext hash = do
    key <- roTxT database $ \tx db -> DB.getKeyByHash tx db hash
    for_ key $ markAsUsed topDownContext              

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


rememberNotValidAfter :: MonadIO m => TopDownContext -> Instant -> m ()
rememberNotValidAfter TopDownContext {..} notValidAfter = 
    liftIO $ atomically $ modifyTVar' earliestNotValidAfter (<> EarliestToExpire notValidAfter)

rememberCrlNextUpdate :: MonadIO m => TopDownContext -> Validated CrlObject -> m ()
rememberCrlNextUpdate topDownContext (Validated (CrlObject { signCrl = SignCRL {..}})) = liftIO $ 
    for_ nextUpdateTime $ rememberNotValidAfter topDownContext    
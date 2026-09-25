-- Local validator helpers are used both at the enclosing effect stack and
-- under nested `runValidator` calls (which push fresh Reader/Error/State
-- handlers). GHC2024 implies MonoLocalBinds, which would pin the unsignatured
-- ones to the enclosing stack; turn it off so they generalise over `es`.
{-# LANGUAGE NoMonoLocalBinds     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Validation.TopDown (
    TopDownResult(..),
    validateMutlipleTAs,
    refreshTaCertificate,
    TroubledChildLoadPath(..),
    MftPlan(..),
    planManifests,
    resolveTroubledChildByKey,
    revokedShortcutChildren,
    manifestValidityPeriod
)
where

import           Effectful.Timeout (Timeout)
import           Effectful
import           Effectful.Concurrent.Async      (Concurrent)
import           Control.Concurrent.STM
import qualified Control.Concurrent.Async         as Async
import qualified Control.Exception                as IOExc
import           GHC.Conc                         (getNumCapabilities, setNumCapabilities)
import           System.Timeout                   (timeout)
import           Effectful.Error.Static           (catchError)
import           Control.Monad

import           Control.Lens hiding (children)

import           Barbies

import           Data.Generics.Product.Typed
import           GHC.Generics

import           Data.Foldable
import           Data.IORef
import           Data.Either
import           Data.Maybe
import qualified Data.Set.NonEmpty                as NESet
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import           Data.Monoid.Generic
import qualified Data.List                        as List
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy
import qualified Data.ByteString                  as BS


import           RPKI.AppContext
import           RPKI.AppState
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch.Fetch
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Messages
import           RPKI.Parallel
import           RPKI.Repository
import           RPKI.Resources.Types

import           RPKI.Store.Base.Storable
import           RPKI.Store.Database    (Tx, roTxT, rwTxT)
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util
import           RPKI.Validation.Common
import           RPKI.Validation.Types
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.ResourceValidation


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
        vrps     :: IORef [T2 VrpsPerAs ObjectKey],
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
        earliestNotValidAfter   :: TVar EarliestToExpire,        
        visitedAkis             :: TVar (Set AKI)
    }
    deriving stock (Generic)


data AllTasTopDownContext = AllTasTopDownContext {
        now                  :: Now,
        worldVersion         :: WorldVersion,
        visitedKeys          :: TVar (Set ObjectKey),        
        publicationPoints    :: PublicationPoints,
        shortcutQueue        :: ClosableQueue MftShortcutOp,
        topDownCounters      :: TopDownCounters IORef,
        -- | Objects published at more than one location, read once for the
        -- whole run. Validating a shortcut needs to know whether its object is
        -- one of these, and asking per object was ~440k queries per round to
        -- find the handful that are (4 of 793516 in a real cache).
        multiLocationKeys    :: Set ObjectKey,
        -- | Validating a CA with all its sub-tree, or a chunk of objects, 
        -- is a task in this pool, for all the TAs together
        workPool             :: WorkPool
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
        readParsed   :: f Int,
        repeatedAki  :: f Int
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

data TroubledChildLoadPath = TroubledFromParsed | TroubledFromOriginal
    deriving stock (Show, Eq, Ord, Generic)

newTopDownContext :: MonadIO m =>
                    TaName
                    -> AllTasTopDownContext
                    -> m TopDownContext
newTopDownContext taName allTas = 
    liftIO $ do 
        payloadBuilder <- newPayloadBuilder
        atomically $ do
            let verifiedResources = Nothing
                currentPathDepth = 0
                overclaimingHappened = False       
            interruptedByLimit      <- newTVar CanProceed                 
            fetcheables             <- newTVar mempty                 
            earliestNotValidAfter   <- newTVar mempty
            visitedAkis             <- newTVar mempty
            pure $! TopDownContext {..}

newAllTasTopDownContext :: MonadIO m =>
                        WorldVersion
                        -> PublicationPoints 
                        -> ClosableQueue MftShortcutOp
                        -> Set ObjectKey
                        -> WorkPool
                        -> m AllTasTopDownContext
newAllTasTopDownContext worldVersion publicationPoints shortcutQueue multiLocationKeys workPool = liftIO $ do 
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
    repeatedAki  <- newIORef 0
   
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
validateMutlipleTAs :: AppContext s
                    -> WorldVersion
                    -> [TAL]
                    -> IO (Map TaName TopDownResult)
validateMutlipleTAs appContext@AppContext {..} worldVersion tals = do
    -- All the TAs are validated by one pool of workers (see `WorkPool`), one on 
    -- every capability. The writer of manifest shortcuts, the only thread writing
    -- to the database here, gets an extra capability of its own. Every SQLite call
    -- is a safe foreign call, and on a capability shared with a worker it has to 
    -- win the capability back after each one. In a first validation, with hundreds 
    -- of thousands rows to write, the writer is the bottleneck. Taking a capability 
    -- away from the workers instead slows down every later validation, where there's 
    -- little to write, and doubles the time at -N2.    
    shortcutQueue <- newCQueueIO 5000
    let closeQueue = atomically $ closeCQueue shortcutQueue
    caps <- getNumCapabilities
    let writerCap  = caps
        workerCaps = [0 .. caps - 1]
    setNumCapabilities (caps + 1)
    (`IOExc.finally` setNumCapabilities caps) $
        Async.withAsyncOn writerCap (storeShortcuts appContext shortcutQueue `IOExc.finally` closeQueue) $ \writer ->
            Async.withAsync (validateAll shortcutQueue workerCaps `IOExc.finally` closeQueue) $ \validation ->
                fst <$> Async.waitBoth validation writer
  where
    validateAll shortcutQueue workerCaps = do 
        publicationPoints <- addprefetchUrls <$> roTxT database DB.getPublicationPoints            
        multiLocationKeys <- roTxT database DB.getMultiLocationShortcutChildren
        workPool <- newWorkPool
        allTas <- newAllTasTopDownContext worldVersion publicationPoints shortcutQueue multiLocationKeys workPool
        withWorkers workPool workerCaps (validateThem allTas)
            `IOExc.finally` 
            applyValidationSideEffects appContext allTas
    
    validateThem allTas = do
        tas <- forM tals $ \tal -> do
            topDownContext <- newTopDownContext (getTaName tal) allTas
            task <- submitTask (allTas ^. #workPool) $ do
                (r@TopDownResult{ payloads = Payloads {..}}, elapsed) <- timedMS $
                        validateTA appContext tal topDownContext
                logInfo logger [i|Validated TA '#{getTaName tal}', got #{estimateVrpCountRoas roas} VRPs, took #{elapsed}ms|]
                pure r
            pure (tal, topDownContext, task)

        -- One timeout for all of them: tasks of different TAs run on the same 
        -- threads, so there's no interrupting just one TA. 
        void $ timeout (toMicroseconds maxDuration) $ 
            forM_ tas $ \(_, _, task) -> waitTask task

        fmap Map.fromList $ forM tas $ \(tal, topDownContext, task) -> 
            fmap (getTaName tal, ) $ 
                pollTask task >>= \case
                    Just r  -> either IOExc.throwIO pure r
                    Nothing -> timedOut tal topDownContext

    timedOut tal topDownContext = do
        let taName = getTaName tal
        logError logger [i|Validation for TA #{taName} did not finish within #{maxDuration} and was interrupted.|]
        (_, validations) <- runValidatorIO (taScopes taName) $ 
                                (appError $ ValidationE $ ValidationTimeout maxDuration :: Eff AppEffects ())
        discoveredRepositories <- readTVarIO $ topDownContext ^. #fetcheables
        pure $ fromValidations validations & #discoveredRepositories .~ discoveredRepositories

    maxDuration = config ^. typed @SystemConfig . #validationWorkerLimits . #workerTimeout
                 
    addprefetchUrls pps =     
        foldr (mergePP . rsyncPP) pps (config ^. #rsyncConf . #prefetchUrls)

--
validateTA :: AppContext s
            -> TAL
            -> TopDownContext
            -> IO TopDownResult
validateTA appContext@AppContext{..} tal topDownContext = do
    (r, topDownValidations) <- runValidatorIO (taScopes taName) validateFromTAL

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
                                      SplPayload asn prefixes <- splPayloads, 
                                      prefix <- prefixes ]

            let roas = Roas $ MonoidalMap.fromList $ 
                            map (\(T2 roaPayload k) -> (k, roaPayload)) vrps 

            let payloads = Payloads {..}                    
            
            pure $ TopDownResult {..}

  where
    taName = getTaName tal

    validateFromTAL = do
        timedMetric (Proxy :: Proxy ValidationMetric) $
            vFocusOn LocationFocus (getURL $ getTaCertURL tal) $ do
                (taCert, repos) <- taCertificateFromCache appContext tal
                -- This clumsy code is to make it possible to construct topDownContext
                -- before getting and validating the TA certificate
                let topDownContext' = topDownContext & #verifiedResources ?~ createVerifiedResources (taCert ^. #payload)
                validateFromTACert appContext topDownContext' repos taCert

taScopes :: TaName -> Scopes
taScopes = newScopes' TAFocus . unTaName
        

data WhichTA = FetchedTA RpkiURL ParsedRpkiObject | CachedTA


-- | Refresh the TA certificate for the given TAL. Returns whether the
-- | certificate actually changed (a fresh download that differs from the
-- | cached copy, or there was no cached copy at all) -- callers use that to
-- | decide whether the TA needs to be revalidated.
refreshTaCertificate :: AppContext s
                        -> TAL
                        -> WorldVersion
                        -> IO (Either AppError Bool)
refreshTaCertificate appContext@AppContext {..} tal worldVersion = do
    (r, vs) <- runValidatorIO (newScopes' TAFocus (unTaName taName)) $
                    vFocusOn LocationFocus (getURL $ getTaCertURL tal) $ do
                        db <- liftIO $ readTVarIO database
                        storedTa <- DB.roAppTxEx db DB.storageError $ \tx -> DB.getTA tx taName
                        fetchValidateAndStoreTaCert appContext tal worldVersion storedTa

    -- The issues are stored no matter how it went: they are the only way for the
    -- top-down validation to find out what happened here.
    rwTxT database $ \tx -> DB.saveTaValidations tx taName (vs ^. typed)
    pure r
  where
    taName = getTaName tal


-- | Get the TA certificate to start the top-down validation from.
-- | This function doesn't throw exceptions.
taCertificateFromCache :: ValidatorIO es => AppContext s
                        -> TAL
                        -> Eff es (Located WellStructuredCaCert, PublicationPointAccess)
taCertificateFromCache AppContext {..} tal = do
    db <- liftIO $ readTVarIO database
    ta <- DB.roAppTxEx db DB.storageError $ \tx -> DB.getTA tx taName
    case ta of
        Nothing       -> taCertProblem "there's no TA certificate in the cache yet"
        Just storedTa -> do
            (taCert, taValidations) <-
                DB.roAppTxEx db DB.storageError $ \tx ->
                    (,) <$> DB.getTaCertByKey tx (storedTa ^. #taCertKey)
                        <*> DB.getTaValidations tx taName

            embedState $ mempty & typed .~ taValidations

            case taCert of
                -- The object is gone from the cache, the next run of the TA
                -- certificate job will download and store it again.
                Nothing   -> taCertProblem "TA certificate is not in the object cache"
                Just cert -> do
                    let locations = talCertLocations tal <> toLocations (storedTa ^. #actualUrl)
                    pure (locatedTaCert locations cert, storedTa ^. #initialRepositories)
  where
    taName = getTaName tal
    taCertProblem :: Validator es => Text -> Eff es a
    taCertProblem message = appError $ UnspecifiedE (unTaName taName) message


-- | Download the TA certificate using the locations from the TAL, validate it
-- | and store it together with the initial publication points.
-- |
-- | If the download fails, fall back to the cached copy, if there is one.
-- |
-- | This function doesn't throw exceptions.
-- | Download and validate the TA certificate, then store it (or keep the
-- | cached one, if that's what validation prefers). Returns whether the
-- | certificate on file after this call is different from the one that was
-- | cached before it: `True` for a first-ever download or a genuine change,
-- | `False` when the refresh reconfirmed the cached certificate (the common
-- | case, since these RRDP/rsync objects change rarely and are re-fetched
-- | on every refresh) or fell back to it after a download failure.
fetchValidateAndStoreTaCert :: (ValidatorIO es, Timeout :> es) => AppContext s
                        -> TAL
                        -> WorldVersion
                        -> Maybe StorableTA
                        -> Eff es Bool
fetchValidateAndStoreTaCert appContext@AppContext {..} tal worldVersion = go
  where
    go storableTa = do
        db <- liftIO $ readTVarIO database
        cachedTaCertM <- case storableTa of
            Nothing -> pure Nothing
            Just StorableTA { taCertKey } ->
                DB.roAppTxEx db DB.storageError $ \tx ->
                    DB.getTaCertByKey tx taCertKey

        z <- (do 
                (u, ro) <- fetchTACertificate appContext (newFetchConfig config) tal
                pure $ FetchedTA u ro)
            `catchError`
                (\_cs -> tryToFallbackToCachedCopy)

        case z of     
            FetchedTA actualUrl object -> do                                 
                fetchedCert <- validateTACert tal actualUrl object

                (certToUse, certToStore, changed) <- case cachedTaCertM of
                    Nothing  -> pure (fetchedCert, fetchedCert, True)
                    Just cachedTaCert ->
                        (do
                            cert <- chooseTaCert fetchedCert cachedTaCert
                            pure $ if cert == cachedTaCert
                                then (cachedTaCert, cachedTaCert, False)
                                else (fetchedCert, fetchedCert, True))
                        `catchError`
                            (\_cs (e :: AppError) -> do
                                logError logger [i|Fetched TA certificate is invalid with error #{e}, will use cached copy.|]
                                pure (cachedTaCert, cachedTaCert, False))

                case publicationPointsFromTAL tal certToUse of
                    Left e         -> appError $ ValidationE e
                    Right ppAccess -> do
                        DB.rwAppTxEx db DB.storageError $ \tx -> do
                            taCertKey <- DB.saveObject tx (WellStructuredRO (CerRO certToStore)) worldVersion
                            DB.linkObjectToUrl tx actualUrl taCertKey worldVersion
                            DB.saveTA tx (StorableTA tal taCertKey ppAccess actualUrl)
                        pure changed

            -- Nothing was downloaded, the cached copy stays as it is
            CachedTA ->
                case cachedTaCertM of
                    Nothing -> appError $ UnspecifiedE (unTaName $ getTaName tal) "Cached TA cert not found in objects store"
                    Just _  -> pure False

      where
        tryToFallbackToCachedCopy e =
            case storableTa of
                Nothing -> do 
                    logError logger $
                        [i|Could not download TA certiicate for #{getTaName tal}, error: #{e}|] <>
                        [i| and there is no cached copy of it.|]
                    appError e

                Just _ -> do  
                    logError logger $ 
                        [i|Could not download TA certiicate for #{getTaName tal}, error: #{e}|] <> 
                        [i| will use cached copy.|]                                        

                    pure CachedTA


locatedTaCert :: Locations -> WellStructuredCaCert -> Located WellStructuredCaCert
locatedTaCert locations cert = Located (Just locations) cert


-- | Do the validation starting from the TA certificate.
-- | 
-- | This function doesn't throw exceptions.
validateFromTACert :: (ValidatorIO es, Concurrent :> es) => AppContext s ->
                    TopDownContext ->
                    PublicationPointAccess ->
                    Located WellStructuredCaCert ->
                    Eff es ()
validateFromTACert
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    initialRepos
    taCert
  = do
    fromTryM
        (UnspecifiedE (unTaName taName) . fmtEx)
        (validateCa appContext 
            (topDownContext & #allTas . #publicationPoints .~ publicationPoints') 
            (CaFull taCert))
  where
    publicationPoints' = 
        case filterPPAccess config initialRepos of 
            Just filteredRepos -> foldr mergePP publicationPoints $ unPublicationPointAccess filteredRepos
            Nothing            -> publicationPoints


validateCa :: (ValidatorIO es, Concurrent :> es) => AppContext s ->
            TopDownContext ->
            Ca ->
            Eff es ()
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
    

validateCaNoLimitChecks :: (ValidatorIO es, Concurrent :> es) => AppContext s ->
                        TopDownContext ->
                        Ca ->
                        Eff es ()
validateCaNoLimitChecks
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    ca = 
    case extractPPAs ca of        
        Left e         -> vError e
        Right ppAccess -> do                
            let caFetcheables = getFetchables publicationPoints ppAccess

            -- Only the PPs of the protocols that are enabled can be fetched: with 
            -- '--no-rrdp'/'--no-rsync' the disabled protocol must neither get a fetcher 
            -- of its own nor be used as a fallback for the other one. If everything is 
            -- disabled, fall back to the full PP access, it is only used for reporting.
            let fetcheablePPs = fromMaybe ppAccess $ filterPPAccess config ppAccess

            -- Add these PPs to the validation-wide set of fetcheables, 
            -- i.e. all newly discovered publication points/repositories                        
            mergeFetcheables caFetcheables $ 
                map fst $ getFetchables publicationPoints fetcheablePPs

            -- Do not validate if nothing was fetched for this CA
            -- otherwise we'll have a lot of useless errors about 
            -- missing manifests, so just don't go there.
            -- Note that it is `caFetcheables`, i.e. all the PPs of the CA and not only 
            -- the fetcheable ones: the cache doesn't care which protocol filled it.
            unless (all ((== Pending) . snd) caFetcheables) $ do   
                let primaryUrl = getPrimaryRepositoryUrl publicationPoints fetcheablePPs
                let validateWithPpScope =
                        vFocusOn PPFocus primaryUrl $
                            metricFocusOn PPFocus primaryUrl $
                                validateCaNoFetch appContext topDownContext ca
                case ca of
                    CaFull c ->
                        vFocusOnLocated c validateWithPpScope
                    CaShort c ->
                        vFocusOn ObjectFocus (c ^. #key) validateWithPpScope
  where
    -- The sanity check is done on all the PPs of the CA, `caFetcheables`, and not on 
    -- the fetcheable ones, so that it doesn't depend on the configuration.
    mergeFetcheables caFetcheables fetcheableUrls =
        case map fst caFetcheables of 
            -- Expect either one of two PPs per CA
            _ : _ -> 
                case fetcheableUrls of 
                    -- Every PP of this CA uses a protocol that is disabled, nothing to fetch
                    []                                  -> pure ()
                    primary : (listToMaybe -> fallback) -> 
                        liftIO $ atomically $ modifyTVar' fetcheables (<> newFetcheables primary fallback)
            weirdCaUrls -> do 
                logError logger [i|Found CA certificate with uncommon publication points: #{weirdCaUrls}.|]
                appError $ ValidationE $ WeirdCaPublicationPoints weirdCaUrls                        


validateCaNoFetch :: (ValidatorIO es, Concurrent :> es) => AppContext s
                -> TopDownContext 
                -> Ca 
                -> Eff es ()
validateCaNoFetch
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    ca = do 
    
    case ca of 
        CaFull c ->
            vFocusOnLocated c $ do
                increment $ topDownCounters.originalCa
                markAsUsedByHash appContext topDownContext (getHash c)                         
                -- Do not validate locations of the TA certificates, these locations come from TAL
                -- so there is no point to warn about multiple locations.
                unless (currentPathDepth == 0) $ validateObjectLocations c
                ValidityPeriod {..} <- validateObjectValidityPeriod (c ^. #payload) now
                rememberNotValidAfter topDownContext notAfter
                oneMoreCert
                validateChildrenOf $ toAKI $ getSKI c
        CaShort c -> 
            vFocusOn ObjectFocus (c ^. #key) $ do            
                increment $ topDownCounters.shortcutCa 
                markAsUsed topDownContext (c ^. #key) 
                validateLocationForShortcut (c ^. #key)
                ValidityPeriod {..} <- validateObjectValidityPeriod c now
                rememberNotValidAfter topDownContext notAfter
                oneMoreCert
                validateChildrenOf $ toAKI (c ^. #ski)
  where    
    validationAlgorithm = config ^. typed @ValidationConfig . typed @ValidationAlgorithm
    validationRFC       = config ^. typed @ValidationConfig . typed @ValidationRFC

    -- Allow validating manifest only once per KI
    validateChildrenOf aki = 
        join $ liftIO $ atomically $ do
            visited <- readTVar visitedAkis
            if aki `Set.member` visited
                then pure $ do
                    increment topDownCounters.repeatedAki
                    vWarn $ MftAlreadyValidated aki
                else do
                    writeTVar visitedAkis $! Set.insert aki visited
                    pure $ validateManifests aki

    newShortcut = 
        case validationAlgorithm of 
            -- Do not create shortctus when validation algorithm is not incremental
            FullEveryIteration -> const Nothing
            Incremental        -> (Just $!)

    -- Validate the manifest of the CA with its children, the way `planManifests` says.
    validateManifests aki = do
        (mfts, shortcut) <- roTxT database $ \tx ->
            (,) <$> DB.getMftsForAKI tx aki
                <*> case validationAlgorithm of
                        FullEveryIteration -> pure Nothing
                        Incremental        -> DB.getMftShorcutMeta tx aki

        let (plan, premature) = planManifests now mfts shortcut

        -- A manifest from the future is a failed fetch (RFC 9286, 6.3) that
        -- has to be reported when an older one is used instead of it
        for_ premature $ \m ->
            withMft m.key $ reportMftFallback $
                ValidationE $ ThisUpdateTimeIsInTheFuture m.thisTime (unNow now)

        case plan of
            NoManifest ->
                vError $ NoMFT aki

            InFull mftMetas -> do
                increment topDownCounters.originalMft
                tryMfts aki mftMetas

            UseShortcut meta ->
                fromShortcut mfts meta $
                    onlyCollectPayloads meta

            OnlyShortcut meta -> do
                vWarn $ NoMFTButCachedMft aki
                fromShortcut mfts meta $
                    onlyCollectPayloads meta

            DiffWithShortcut meta mftMeta ->
                fromShortcut mfts meta $ do
                    markAsUsed topDownContext mftMeta.key
                    withMft mftMeta.key $ \mft ->
                        tryOneMftWithShortcut meta mft
                            `catchError` \_cs (e :: AppError) -> do
                                -- The shortcut is valid, so it is the latest
                                -- valid manifest to fall back to
                                reportMftFallback e mft
                                onlyCollectPayloads meta
      where
        fromShortcut mfts meta validate = do
            markAsUsed topDownContext meta.key
            for_ (shortcutMftNextUpdate mfts meta) $ rememberNotValidAfter topDownContext
            increment topDownCounters.shortcutMft
            r <- validate
            oneMoreMft >> oneMoreCrl >> oneMoreMftShort
            pure r

        -- The manifest changed since the last shortcut: run the full diff, which needs
        -- file_name (to detect renames), so fetch the full children map.
        tryOneMftWithShortcut meta@DB.MftShortcutMeta{..} mft = do
            fullCa <- getFullCa appContext topDownContext ca
            let crlKey = crlShortcut.key
            markAsUsed topDownContext crlKey
            fullChildren <- roTxT database $ \tx -> DB.getMftShorcutChildrenFull tx aki
            let mftShortcut = MftShortcut { nonCrlEntries = fullChildren, .. }
            overlappingChildren <- manifestFullValidation fullCa mft (Just mftShortcut) aki
            collectPayloads aki meta (Map.map ChildWithEntry fullChildren) (Just overlappingChildren)
                        (Left fullCa)
                        (findAndValidateCrl fullCa mft aki)
                        (getResources ca)

        -- The steady-state "nothing changed" path: fetch the light children map,
        -- which never selects file_name.
        onlyCollectPayloads meta@DB.MftShortcutMeta{..} = do
            let crlKey = crlShortcut.key
            markAsUsed topDownContext crlKey
            lightChildren <- roTxT database $ \tx -> DB.getMftShorcutChildrenLight tx aki
            -- A CA certificate validated in full is a new one (or a troubled one) and its
            -- resources may be different from the ones the shortcuts of its children were
            -- made with, e.g. it's re-issued with fewer resources. Then the resources of
            -- the children have to be checked again, which is what `Left` tells.
            let fullCa = case ca of
                    CaFull c  -> Left c
                    CaShort _ -> Right $ getFullCa appContext topDownContext ca
            collectPayloads aki meta (Map.map ChildLight lightChildren) Nothing
                    fullCa
                    (getCrlByKey appContext crlKey)
                    (getResources ca)

    -- Validate the manifests in full, falling back to the next one until one is valid
    tryMfts aki []              = vError $ NoMFT aki
    tryMfts aki (m : mftsMetas_) = 
        withMft (m ^. #key) $ \mft -> do 
            tryOneMft mft `catchError` \_cs (e :: AppError) -> 
                case mftsMetas_ of 
                    [] -> appError e
                    _  -> do 
                        reportMftFallback e mft
                        tryMfts aki mftsMetas_
      where
        tryOneMft mft = do                 
            markAsUsed topDownContext $ mft ^. #key                
            caFull <- getFullCa appContext topDownContext ca
            void $ manifestFullValidation caFull mft Nothing aki
            oneMoreMft >> oneMoreCrl         

    withMft key f = do 
        z <- roTxT database $ \tx -> DB.getMftByKey tx key
        case z of 
            Nothing  -> integrityError appContext [i|Referential integrity error, can't find a manifest by its key #{key}.|]
            Just mft -> f mft

    reportMftFallback e mft = do
        let mftLocation = describeLocated $ mft ^. #object
        let mftNumber = mft ^. #object . #payload . #content . #mftNumber
        vFocusOn ObjectFocus (mft ^. #key) $ vWarn $ MftFallback e mftNumber
        logWarn logger [i|Falling back to the previous manifest for #{mftLocation}, failed manifest number #{mftNumber}, error: #{toMessage e}|]        


    -- Proceed with full validation for children mentioned in the full manifest 
    -- and children mentioned in the manifest shortcut. Create a diff between them,
    -- run full validation only for new children and create a new manifest shortcut
    -- with updated set of children.
    manifestFullValidation :: (ValidatorIO es', Concurrent :> es') => Located WellStructuredCaCert
                        -> Keyed (Located WellStructuredMft)
                        -> Maybe MftShortcut 
                        -> AKI
                        -> Eff es' [T3 Text Hash ObjectKey]
    manifestFullValidation fullCa
        keyedMft@(Keyed locatedMft@(Located mftLocations mft) mftKey)
        mftShortcut childrenAki = do
        let uniqueFocusOn = case mftLocations of
                Just ls -> vUniqueFocusOn LocationFocus (getURL $ pickLocation ls)
                Nothing -> vUniqueFocusOn HashFocus (getHash mft)
        uniqueFocusOn
            doValidate
            (vError $ CircularReference $ KeyIdentity mftKey)
      where
        doValidate = do 
            -- General location validation
            validateObjectLocations locatedMft

            -- Manifest-specific location validation
            validateMftLocation locatedMft fullCa

            keyedValidCrl@(Keyed validCrl@(Validated validCrlObject) crlKey) <- findAndValidateCrl fullCa keyedMft childrenAki                

            -- MFT can be revoked by the CRL that is on this MFT -- detect 
            -- revocation as well, this is clearly an error                               
            validMft <- validateMft (config ^. #validationConfig . typed) 
                                    now mft (fullCa ^. #payload) validCrl verifiedResources

            let ValidityPeriod { notAfter = mftNotAfter } = manifestValidityPeriod mft
            rememberNotValidAfter topDownContext mftNotAfter
            rememberCrlNextUpdate topDownContext validCrl

            -- Validate entry list and filter out CRL itself
            nonCrlChildren <- validateMftEntries mft (getHash validCrlObject)

            -- If MFT shortcut is present, filter children that need validation, 
            -- children that are on the shortcut are already validated.
            let (newChildren, overlappingChildren0, deletedKeys) =
                    case mftShortcut of
                        Nothing       -> (nonCrlChildren, [], [])
                        Just mftShort -> manifestDiff mftShort nonCrlChildren

            -- If the CRL has changed, children that are not going to be re-validated 
            -- here (i.e. the overlapping ones) still have to be checked for revocation.
            -- New children are checked as a part of their full validation.
            revokedEntries <- 
                case mftShortcut of 
                    Just mftShort | crlKey /= mftShort.crlShortcut.key -> do 
                        increment topDownCounters.originalCrl
                        checkForRevokedChildren mftShort keyedMft overlappingChildren0 validCrl
                    _ -> pure []

            -- A revoked child must not contribute any payload (and, if it is a CA, 
            -- its sub-tree must not be traversed), so drop it from the overlapping set.            
            let revokedKeys = Set.fromList $ map fst revokedEntries
            let overlappingChildren = 
                    filter (\(T3 _ _ k) -> k `Set.notMember` revokedKeys) overlappingChildren0

            bumpCounterBy topDownCounters #newChildren (length newChildren)
            bumpCounterBy topDownCounters #overlappingChildren (length overlappingChildren)
            
            forM_ mftShortcut $ \mftShort -> do          
                -- manifest number must increase 
                -- https://www.rfc-editor.org/rfc/rfc9286.html#name-manifest
                let mftNumber = mft.content.mftNumber 
                when (mftNumber < mftShort.manifestNumber) $ do 
                    -- Here we have to do a bit of hackery: 
                    -- * Calling vError will interrupt this function and call for fall-back to 
                    --   the "latest valid manifest" which is the shortcut
                    --
                    -- * But the shortcut may have expired already and there will no be any options left,
                    --   so we need to be careful and just emit a warning in this case
                    --
                    -- * So in case there is nothing to fall back to, we emit a warning and still 
                    --   use the manifest
                    let issue = ManifestNumberDecreased mftShort.manifestNumber mftNumber
                    if isWithinValidityPeriod now mftShort
                        then vError issue
                        else vWarn issue

            -- Mark _all_ manifest entries as used to avoid the situation
            -- when some of the children are garbage-collected from the cache 
            -- and some are still there. Do it both in case of successful 
            -- validation or a validation error.
            let markAllEntriesAsUsed = do
                    forM_ (newChildren <> overlappingChildren0) $
                        \(T3 _ _ k) -> markAsUsed topDownContext k

            let processChildren = do                                              
                    -- Here we have the payloads for the fully validated MFT children
                    -- and the shortcut objects for these children
                    --                                            
                    childrenShortcuts <- 
                        fmap (\shortcuts -> [ (k, s) | T2 k (Just s) <- shortcuts ]) $
                            gatherMftEntryResults =<< 
                                gatherMftEntryValidations fullCa newChildren validCrl

                    let newEntries = makeEntriesWithMap newChildren (Map.fromList childrenShortcuts) 
                                        <> revokedEntries
                    
                    let nextMftShortcut = makeMftShortcut mftKey validMft newEntries keyedValidCrl

                    case validationAlgorithm of 
                        -- Only create shortcuts for case of incremental validation.
                        -- 
                        -- NOTE: That means that in case of full validation falling back to 
                        -- the previous valid manifest will not work, since there are no
                        -- shortcuts of previous manifests to fall back to.                  
                        Incremental -> do  
                            issues <- thisScopeIssues
                            -- Do no create shortcuts for manifests with warnings 
                            -- (or errors, obviously)
                            when (Set.null issues) $ do
                                let aki = toAKI $ getSKI fullCa

                                case mftShortcut of
                                    -- There's nothing to diff against, so all the children
                                    -- are new. Replace the whole shortcut: an expired one can
                                    -- still have children that are not on this manifest.
                                    Nothing -> do
                                        replaceMftShortcut topDownContext aki nextMftShortcut
                                        increment topDownCounters.updateMftMeta
                                        increment topDownCounters.updateMftChildren

                                    Just mftShort -> do
                                        -- If manifest key is not the same as the shortcut key,
                                        -- we need to replace the shortcut with the new one
                                        when (mftShort.key /= mftKey) $ do
                                            updateMftShortcut topDownContext aki nextMftShortcut
                                            increment topDownCounters.updateMftMeta

                                        -- Update manifest shortcut children in case there are new
                                        -- or deleted children in the new manifest.
                                        when (not (null newChildren)
                                            || not (null deletedKeys)
                                            || not (null revokedEntries)) $ do
                                                updateMftShortcutChildren topDownContext aki newEntries deletedKeys
                                                increment topDownCounters.updateMftChildren

                        _  -> pure ()

                    pure $! overlappingChildren

            processChildren `recover` markAllEntriesAsUsed


    findAndValidateCrl :: ValidatorIO es' => Located WellStructuredCaCert
                    -> Keyed (Located WellStructuredMft)
                    -> AKI
                    -> Eff es' (Keyed (Validated CrlObject))
    findAndValidateCrl fullCa (Keyed (Located _ mft) _) aki = do  
        MftPair _ crlHash <-
            case findCrlOnMft mft.content of
                []    -> vError $ NoCRLOnMFT aki 
                [crl] -> pure crl
                crls  -> vError $ MoreThanOneCRLOnMFT aki crls

        db <- liftIO $ readTVarIO database
        DB.roAppTx db $ \tx -> 
            DB.getKeyByHash tx crlHash >>= \case         
                Nothing  -> vError $ NoCRLExists aki crlHash
                Just key -> do           
                    increment $ topDownCounters.readParsed
                    z <- getStoredObject tx key
                    case z of 
                        Nothing -> 
                            vError $ NoCRLExists aki crlHash

                        Just (Keyed locatedCrl@(Located _ (WellStructuredRO (CrlRO crl))) crlKey) -> do
                            markAsUsed topDownContext crlKey
                            vFocusOnLocated locatedCrl $ do
                                validateObjectLocations locatedCrl
                                checkCrlLocation locatedCrl mft.eeCert
                                validatedCrl <- validateCrl now crl fullCa
                                pure $! Keyed validatedCrl crlKey
                        _ -> 
                            vError $ CRLHashPointsToAnotherObject crlHash   
            

    -- Utility for repeated peace of code
    makeEntriesWithMap childrenList entryMap = 
        [ (key, entry) | 
            T3 _ _ key <- childrenList,
            entry      <- maybeToList $ Map.lookup key entryMap ]


    -- Check which of the shortcut children are revoked by the (new) CRL, reporting
    -- a warning for each. Returns the replacement shortcut entries for them.
    checkForRevokedChildren :: ValidatorIO es' => MftShortcut 
                            -> Keyed (Located WellStructuredMft)
                            -> [T3 Text Hash ObjectKey]
                            -> Validated CrlObject
                            -> Eff es' [(ObjectKey, MftEntry)]
    checkForRevokedChildren mftShortcut (Keyed (Located _ mft) _) children validCrl = do        
        when (isRevoked (getSerial mft) validCrl) $
            vWarn RevokedResourceCertificate   
        let revoked = revokedShortcutChildren mftShortcut validCrl children
        forM_ revoked $ \(childKey, _) -> 
            vFocusOn ObjectFocus childKey $ vWarn RevokedResourceCertificate
        pure revoked


    -- this indicates the difference between RFC9286-bis 
    -- version 02 (strict) and version 03 and later (more loose).                                                                                            
    gatherMftEntryValidations =
        case config.validationConfig.manifestProcessing of
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
        forChildren
            nonCrlChildren
            $ \(T3 filename hash' key) -> do
                (z, vs) <- runValidator scopes $ do
                                ro <- getManifestEntry filename hash' key
                                -- if failed this one interrupts the whole MFT valdiation
                                validateMftChild fullCa ro filename validCrl
                pure $! case z of
                    -- In this case invalid child is considered invalid entry 
                    -- and the whole manifest is invalid
                    Left e              -> InvalidEntry e vs
                    Right childShortcut -> ValidEntry vs childShortcut key filename   
    
    independentMftChildrenResults fullCa nonCrlChildren validCrl = do
        scopes <- askScopes
        forChildren
            nonCrlChildren
            $ \(T3 filename hash key) -> do
                (r, vs) <- runValidator scopes $ getManifestEntry filename hash key
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
                        (z, vs') <- runValidator scopes $ validateMftChild fullCa ro filename validCrl
                        pure $! case z of
                                Left e              -> InvalidChild e vs' key filename
                                Right childShortcut -> ValidEntry vs' childShortcut key filename
    
    -- A child CA is a whole sub-tree to validate and a task of its own, 
    -- other objects are validated in chunks.
    forChildren = forInPool workPool 64 $ \(T3 fileName _ _) -> 
                    textObjectType fileName == Just CER

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
        let mftChildren = mft.content.mftEntries
        when (null mftChildren) $
            vError ZeroManifestEntries        

        let nonCrlChildren = filter (\(MftPair _ hash') -> crlHash /= hash') mftChildren

        -- Make sure all the entries are unique
        let entryMap = Map.fromListWith (<>) $ map (\(MftPair f h) -> (h, [f])) nonCrlChildren
        let nonUniqueEntries = Map.filter (`longerThan` 1) entryMap

        -- Don't crash here, it's just a warning, at the moment RFC doesn't say anything 
        -- about uniqueness of manifest entries. 
        -- TODO Or does it? Maybe something in ASN1 encoding as Set?
        unless (Map.null nonUniqueEntries) $
            vWarn $ NonUniqueManifestEntries $ Map.toList nonUniqueEntries

        db <- liftIO $ readTVarIO database
        DB.roAppTx db $ \tx ->
            forM nonCrlChildren $ \(MftPair fileName hash) -> do
                k <- DB.getKeyByHash tx hash
                case k of
                    Nothing  -> vError $ ManifestEntryDoesn'tExist hash fileName
                    Just key -> do
                        validateMftFileName fileName
                        pure $! T3 fileName hash key        

    -- Given MFT entry with hash, filename and its already-resolved key
    -- (from validateMftEntries, which already looked it up), get the
    -- object it refers to.
    getManifestEntry filename hash' key = do
        let objectType = textObjectType filename
        db <- liftIO $ readTVarIO database
        ro <- DB.roAppTx db $ \tx ->
            vFocusOn ObjectFocus key $ do
                increment topDownCounters.readParsed
                getStoredObject tx key >>= \case
                    Nothing -> vError $ ManifestEntryDoesn'tExist hash' filename
                    Just o  -> do
                        case o ^. #object . #payload of
                            OriginalRO _ vs_ _ _ -> do
                                increment topDownCounters.readOriginal
                                embedState vs_
                            WellStructuredRO _ ->
                                pure ()
                        pure $! o

        -- The type of the object that is deserialised must 
        -- correspond to the file extension on the manifest
        let realObjectType = getRpkiObjectType $ ro ^. #object

        let complain = vWarn $ ManifestEntryHasWrongFileType hash' filename realObjectType
        case objectType of 
            Nothing -> complain
            Just ot -> unless (realObjectType `isOfType` ot) complain

        pure ro                        


    validateMftChild caFull child@(Keyed (Located objectLocations _) _)
                     filename validCrl = do
        -- Warn about names on the manifest mismatching names in the object
        -- URLs -- skipped for objects with no location at all (Erik).
        for_ objectLocations $ \locations -> do
            let nameMatches = NESet.filter ((filename `Text.isSuffixOf`) . toText) $
                                unLocations locations
            when (null nameMatches) $
                vWarn $ ManifestLocationMismatch filename locations

        case child.object.payload of
            OriginalRO _ _ _ _ -> do
                pure $! newShortcut (makeChildWithIssues child.key filename)
            WellStructuredRO wellStructuredChild ->
                validateChildObject
                    caFull
                    (child & #object . #payload .~ wellStructuredChild)
                    filename
                    validCrl


    -- Location validation when all we have is a key.
    --
    -- Only objects with more than one location need anything done, and which
    -- objects those are was read once for the whole run, so the common case is
    -- a set lookup rather than a query and a transaction per object.
    validateLocationForShortcut key =
        when (key `Set.member` multiLocationKeys) $ do 
            z <- roTxT database $ \tx -> DB.getLocationsByKey tx key
            case z of 
                Nothing -> 
                    -- That's weird and it means DB inconsitency                                
                    integrityError appContext 
                        [i|Referential integrity error, can't find locations for the object #{key} known to have several.|]
                Just locations -> 
                    vFocusOn LocationFocus (getURL $ pickLocation locations) $
                        validateObjectLocations locations


    {-         
        Validate manifest child according to 
        https://datatracker.ietf.org/doc/rfc9286/

        And return shortcut created for it
    -}
    validateChildObject :: (ValidatorIO es', Concurrent :> es') => 
            Located WellStructuredCaCert
            -> Keyed (Located WellStructuredRpkiObject) 
            -> Text
            -> Validated CrlObject
            -> Eff es' (Maybe MftEntry)
    validateChildObject fullCa (Keyed child@(Located locations childRo) childKey) fileName validCrl = do
        case childRo of
            CerRO childCert -> do
                parentScope <- askScopes                
                {- 
                    Note that recursive validation of the child CA happens in the separate   
                    runValidator (...) call, it is to avoid short-circuit logic implemented by ExceptT:
                    otherwise an error in child validation would interrupt validation of the parent with
                    ExceptT's exception logic.
                -}
                (r, validationState) <- runValidator parentScope $
                    focusOnChild $ do
                        -- Check that AIA of the child points to the correct location of the parent
                        -- https://mailarchive.ietf.org/arch/msg/sidrops/wRa88GHsJ8NMvfpuxXsT2_JXQSU/
                        --                             
                        validateAIA childCert fullCa
 
                        (childVerifiedResources, overlclaiming) 
                            <- do
                                void $ validateResourceCert now childCert fullCa validCrl
                                validateResources (config ^. #validationConfig . typed) 
                                    verifiedResources childCert (fullCa ^. #payload)

                        let childTopDownContext = topDownContext
                                & #verifiedResources ?~ childVerifiedResources
                                & #currentPathDepth %~ (+ 1)
                                & #overclaimingHappened .~ isJust overlclaiming

                        validateCa appContext childTopDownContext (CaFull (Located locations childCert))

                embedState validationState
                case r of 
                    Left _  -> pure $! Just $! makeChildWithIssues childKey fileName
                    Right _  -> do 
                        case getPublicationPointsFromWellStructuredCert childCert of 
                            -- It's not going to happen?
                            Left e     -> vError e
                            Right ppas -> do 
                                -- Look at the issues for the child CA to decide if CA shortcut should be made
                                shortcut <- focusOnChild $
                                                shortcutIfNoIssues childKey fileName
                                                        (makeCaShortcut childKey (Validated childCert) ppas)
                                pure $! newShortcut shortcut

            RoaRO roa -> validLeaf $ do
                validRoa <- validateRoa validationRFC now roa fullCa.payload validCrl verifiedResources
                pure $! makeRoaShortcut childKey validRoa roa.content

            SplRO spl -> validLeaf $ do
                validSpl <- validateSpl validationRFC now spl fullCa.payload validCrl verifiedResources
                pure $! makeSplShortcut childKey validSpl spl.content

            AspaRO aspa -> validLeaf $ do
                validAspa <- validateAspa validationRFC now aspa fullCa.payload validCrl verifiedResources
                pure $! makeAspaShortcut childKey validAspa aspa.content

            BgpRO bgpCert -> validLeaf $ do
                (validBgpCert, bgpPayload) <- validateBgpCert now bgpCert fullCa.payload validCrl
                pure $! makeBgpSecShortcut childKey validBgpCert bgpPayload

            GbrRO gbr -> validLeaf $ do
                validGbr <- validateGbr validationRFC now gbr fullCa.payload validCrl verifiedResources
                pure $! makeGbrShortcut childKey validGbr (T2 (getHash gbr) gbr.content)

            -- Any new type of object should be added here, otherwise
            -- they will emit a warning.
            _somethingElse -> 
                focusOnChild $ do
                    logWarn logger [i|Unsupported type of object: #{locations}.|]                
                    pure $! newShortcut (makeChildWithIssues childKey fileName)

        where
            focusOnChild = vFocusOnLocated child

            -- Validate an object other than a CA certificate, which gives
            -- its shortcut, and take it the same way the shortcut is taken
            -- in the next rounds
            validLeaf validate =
                focusOnChild $ do
                    validateObjectLocations child
                    allowRevoked $ do
                        leaf <- validate
                        acceptLeaf topDownContext FromObject leaf
                        shortcut <- shortcutIfNoIssues childKey fileName leaf
                        pure $! newShortcut shortcut

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
    shortcutIfNoIssues key fileName child = do 
        issues <- thisScopeIssues
        pure $! if Set.null issues 
                    then MftEntry fileName child
                    else makeChildWithIssues key fileName

    thisScopeIssues :: Validator es' => Eff es' (Set VIssue)
    thisScopeIssues = 
        withCurrentScope $ \scopes vs -> 
            getIssues (scopes ^. typed) (vs ^. typed)


    collectPayloads :: (ValidatorIO es', Concurrent :> es') => AKI
                    -> DB.MftShortcutMeta
                    -> Map.Map ObjectKey ChildData
                    -> Maybe [T3 Text Hash ObjectKey]
                    -> Either (Located WellStructuredCaCert) (Eff es' (Located WellStructuredCaCert))
                    -> Eff es' (Keyed (Validated CrlObject))
                    -> AllResources
                    -> Eff es' ()
    collectPayloads childrenAki meta childrenMap childrenToCheck findFullCa findValidCrl parentCaResources = do

        -- Filter children that we actually want to go through here
        let filteredChildren =
                case childrenToCheck of
                    Nothing -> Map.toList childrenMap
                    Just ch -> catMaybes [ (k,) <$> Map.lookup k childrenMap | T3 _ _ k <- ch ]

        let anyTroubled = 
                or [ True | (_, childData) <- filteredChildren, 
                            TroubledChild {} <- [childOf childData] ]

        vFocusOn ObjectFocus meta.key $ do
            validateLocationForShortcut meta.key
            ValidityPeriod { notAfter } <- validateObjectValidityPeriod meta now
            rememberNotValidAfter topDownContext notAfter
            vFocusOn ObjectFocus meta.crlShortcut.key $ do
                ValidityPeriod { notAfter = notValidAfterCrl } <- validateObjectValidityPeriod meta.crlShortcut now
                rememberNotValidAfter topDownContext notValidAfterCrl

            -- For children that are problematic we'll have to fall back 
            -- to full validation, for that we beed the parent CA and a valid CRL.
            -- Construct the validation function for such problematic children
            troubledValidation <-
                    if anyTroubled 
                        then do 
                            caFull   <- either pure id findFullCa
                            validCrl <- findValidCrl
                            pure $ \childKey fileName -> 
                                    validateTroubledChild caFull fileName validCrl childKey                        
                        else pure $ \_ _ -> 
                                -- Should never happen, there are no troubled children
                                integrityError appContext [i|Impossible happened!|]

            collectResults filteredChildren (getChildPayloads troubledValidation)

      where
        -- A child CA is a whole sub-tree to validate and a task of its own, 
        -- other objects are cheap to go through and go in big chunks. 
        collectResults children f = do 
            scopes <- askScopes
            z <- forInPool workPool 500 isCa children $ runValidator scopes . f
            embedState $ mconcat $ map snd z                 
          where
            isCa (_, childData) = case childOf childData of
                                    CaChild {} -> True
                                    _          -> False

        validateTroubledChild caFull fileName (Keyed validCrl _) childKey = do  
            -- Troubled entries may point either to an original blob or to a
            -- well-structured object that previously produced issues (e.g. warnings).
            -- Handle both shapes to avoid stale-shortcut false positives.
            db <- liftIO $ readTVarIO database            
            resolved <-
                DB.roAppTx db $ \tx -> do
                    resolveTroubledChildByKey tx childKey

            childObject <-
                case resolved of
                    Just (TroubledFromParsed, objectByKey) -> do
                        increment topDownCounters.readParsed
                        pure objectByKey

                    Just (TroubledFromOriginal, objectByKey) -> do
                        increment topDownCounters.readOriginal
                        pure objectByKey

                    Nothing -> do
                        -- Something is wrong with the references in the database. Normally it should never happen,
                        -- but if it does, we have to delete the shortcut and report the error.
                        deleteMftShortcut topDownContext $ toAKI $ getSKI caFull
                        logError logger [i|Troubled child #{childKey} not found in the database, will delete manifest shortcut.|]
                        integrityError appContext
                            [i|Referential integrity error, can't find a troubled child by its key #{childKey}.|]

            validateChildObject caFull childObject fileName validCrl

        -- Rare fallback: the light (file_name-free) read path needs a file_name
        -- to write a child's entry. Look it up on demand instead of joining
        -- file_name into every bulk read.
        childFileName childKey = \case
            ChildWithEntry MftEntry {..} -> pure fileName
            ChildLight _ -> do
                mfn <- roTxT database $ \tx ->
                            DB.getMftShortcutChildFileName tx childrenAki childKey
                case mfn of
                    Just fn -> pure fn
                    Nothing -> integrityError appContext
                        [i|Referential integrity error, can't find file_name for child #{childKey}.|]

        storeChildIfChanged childKey childData newEntry =
            unless (newEntry.child == childOf childData) $
                updateMftShortcutChildren topDownContext childrenAki [(childKey, newEntry)] []

        getChildPayloads troubledValidation (childKey, childData) = do
            markAsUsed topDownContext childKey
            case childOf childData of
                CaChild caShortcut _ -> do
                    (childVerifiedResources, overlclaiming) <- 
                        validateChildParentResources 
                                    (config ^. #validationConfig . typed)                                 
                                    caShortcut.resources 
                                    parentCaResources 
                                    verifiedResources
                    
                    let childTopDownContext = topDownContext
                            & #currentPathDepth %~ (+ 1)                                        
                            & #verifiedResources ?~ childVerifiedResources
                            & #overclaimingHappened .~ isJust overlclaiming
                            
                    validateCa appContext childTopDownContext (CaShort caShortcut)
                        
                RoaChild r _    -> recheckLeaf r (validateRoaPrefixes verifiedResources r.roaPayload)
                SplChild s _    -> recheckLeaf s (validateSplAsn verifiedResources s.splPayload)
                AspaChild a _   -> recheckLeaf a (pure ())
                BgpSecChild b _ -> recheckLeaf b (pure ())
                GbrChild g _    -> recheckLeaf g (pure ())

                TroubledChild childKey_ -> do
                    increment topDownCounters.shortcutTroubled
                    fileName <- childFileName childKey_ childData
                    -- A troubled child can come out of re-validation clean, 
                    -- then it doesn't need to be validated in full anymore.
                    newEntry <- troubledValidation childKey_ fileName
                    for_ newEntry $ storeChildIfChanged childKey_ childData
          where
            -- Recheck the shortcut of an object other than a CA certificate, and
            -- take it the same way as the object is taken when validated in full
            recheckLeaf shortcut validatePayload =
                vFocusOn ObjectFocus childKey $ do
                    validateShortcut childData shortcut childKey validatePayload
                    acceptLeaf topDownContext FromShortcut (childOf childData)
    
        -- `validatePayload` is what full validation checks of the payload against 
        -- the resources of the CA, on top of the resources of the EE certificate.
        validateShortcut :: (ValidatorIO es', Concurrent :> es', WithValidityPeriod s, WithResources s) 
                         => ChildData -> s -> ObjectKey -> Eff es' () -> Eff es' ()
        validateShortcut childData shortcut key validatePayload = do
            validateLocationForShortcut key            
            ValidityPeriod {..} <- validateObjectValidityPeriod shortcut now
            rememberNotValidAfter topDownContext notAfter            
            {- We need to revalidate resources if either of the following happens:
                1) We came here from validating a new CA certificate, and not from a CA shortcut.
                   That can be determined by checking if `findFullCa` is `Left`.
                2) There were overclaiming resources on the way from the top to this CA
            -}
            let revalidateResources =
                    let potentiallyNewResources = isLeft findFullCa
                    in case validationRFC of 
                        StrictRFC       -> potentiallyNewResources
                        ReconsideredRFC -> potentiallyNewResources || overclaimingHappened
            when revalidateResources $ do            
                    void $ validateChildParentResources validationRFC 
                            (getResources shortcut) parentCaResources verifiedResources
                    -- With the reconsidered algorithm an EE certificate with resources 
                    -- the CA doesn't have anymore is only a warning, the payload isn't.
                    validatePayload
                `catchError` \_cs (e :: AppError) -> do 
                        -- The shortcut isn't valid anymore and later validations 
                        -- may not check its resources again (e.g. the CA is a shortcut 
                        -- by then), so it has to be validated in full from now on.
                        fileName <- childFileName key childData
                        storeChildIfChanged key childData (makeChildWithIssues key fileName)
                        appError e
            

-- | Where a valid child comes from: its object validated in full, or its shortcut.
data ChildSource = FromObject | FromShortcut

-- | Count a valid manifest child that is not a CA certificate and keep its
-- payload. It's done here only, the same for a child validated in full and
-- for a child taken from its shortcut.
acceptLeaf :: ValidatorIO es => TopDownContext -> ChildSource -> MftChild -> Eff es ()
acceptLeaf topDownContext source = \case
    RoaChild r _ -> do
        oneMoreRoa
        moreVrps $ Count $ fromIntegral $ length (roaV4 r.roaPayload) + length (roaV6 r.roaPayload)
        count (.originalRoa) (.shortcutRoa)
        keep #vrps $ T2 r.roaPayload r.key
    SplChild s _ -> do
        oneMoreSpl
        count (.originalSpl) (.shortcutSpl)
        keep #spls s.splPayload
    AspaChild a _ -> do
        oneMoreAspa
        count (.originalAspa) (.shortcutAspa)
        keep #aspas a.aspa
    BgpSecChild b _ -> do
        oneMoreBgp
        keep #bgpCerts b.bgpSec
    GbrChild g _ -> do
        oneMoreGbr
        keep #gbrs g.gbr
    -- A CA gives the payloads of its sub-tree
    CaChild {} -> pure ()
    -- A troubled child is validated in full and accepted as what that gives
    TroubledChild _ -> pure ()
  where
    counters = topDownContext.allTas.topDownCounters
    count original shortcut = increment $ case source of
        FromObject   -> original counters
        FromShortcut -> shortcut counters

    keep :: MonadIO m => Getting (IORef [a]) PayloadBuilder (IORef [a]) -> a -> m ()
    keep field a = liftIO $
        atomicModifyIORef' (topDownContext.payloadBuilder ^. field) $ \as -> (a : as, ())


-- | How to validate the manifest of a CA.
data MftPlan
    = NoManifest
    -- | There's no shortcut that can be used: validate the manifests in full,
    -- one after another until one of them is valid.
    | InFull [MftMeta]
    -- | The manifest of the shortcut is still the latest one, so the shortcut
    -- has everything.
    | UseShortcut DB.MftShortcutMeta
    -- | There's no manifest to use but the shortcut is still valid. It is the
    -- cached data of the last successful fetch, which is to be used until it
    -- becomes stale (RFC 9286, 6.6).
    | OnlyShortcut DB.MftShortcutMeta
    -- | There's a newer manifest than the one of the shortcut: validate only
    -- what changed, and fall back to the shortcut if the manifest is not valid.
    | DiffWithShortcut DB.MftShortcutMeta MftMeta
    deriving stock (Show, Eq)

-- | Given the manifests of a CA, newest first, and its manifest shortcut
-- (`Nothing` when shortcuts are not used), decide how to validate it.
--
-- Also returns the manifests from the future that are passed over for
-- older data, since they are failed fetches to report (RFC 9286, 6.3).
planManifests :: Now -> [MftMeta] -> Maybe DB.MftShortcutMeta -> (MftPlan, [MftMeta])
planManifests now mfts shortcut =
    case shortcut of
        Just meta | not (shortcutExpired meta) ->
            let plan = case current of
                    -- A shortcut is only made for a manifest that is not in the
                    -- future, so the manifest of this one is gone from the cache
                    []                        -> OnlyShortcut meta
                    m : _ | m.key == meta.key -> UseShortcut meta
                          | otherwise         -> DiffWithShortcut meta m
            in (plan, premature)
        _   | null mfts    -> (NoManifest, [])
            -- If there are only manifests from the future, validate
            -- them anyway to have a meaningful error message
            | null current -> (InFull premature, [])
            | otherwise    -> (InFull current, premature)
  where
    (current, premature) = List.partition (\m -> m.thisTime <= unNow now) mfts

    -- Shortcuts stored before `manifestValidityPeriod` only carry the validity
    -- of the manifest's EE certificate. A manifest that's past its nextUpdate
    -- stays unchanged, and so does its shortcut, so the manifest's own
    -- nextUpdate is checked here.
    shortcutExpired meta =
        not (isWithinValidityPeriod now meta) ||
        not (isWithinValidityPeriod now meta.crlShortcut) ||
        maybe False (< unNow now) (shortcutMftNextUpdate mfts meta)

-- | nextUpdate of the manifest of the shortcut, if it's still in the cache.
shortcutMftNextUpdate :: [MftMeta] -> DB.MftShortcutMeta -> Maybe Instant
shortcutMftNextUpdate mfts meta =
    (.nextTime) <$> List.find ((== meta.key) . (.key)) mfts


-- Either a full manifest entry (file_name known, from the diff-path's full read)
-- or just the child's shortcut payload (from the hot, file_name-free light read).
data ChildData = ChildWithEntry MftEntry | ChildLight MftChild

childOf :: ChildData -> MftChild
childOf (ChildWithEntry MftEntry {..}) = child
childOf (ChildLight c)                 = c


-- Calculate difference bentween a manifest shortcut
-- and the list of children of the new manifest object.
manifestDiff :: MftShortcut
            -> [T3 Text a ObjectKey]
            -> ([T3 Text a ObjectKey], [T3 Text a ObjectKey], [ObjectKey])
manifestDiff mftShortcut newMftChidlren =
    (List.reverse newOnes, List.reverse overlapping, Map.keys deletedEntries)
  where
    (newOnes, overlapping, deletedEntries) =
        foldl' go ([], [], mftShortcut.nonCrlEntries) newMftChidlren

    -- If we delete everything from mftShortcut.nonCrlEntries that is present in
    -- newMftChidlren, we only have the entries that are not present on the new manifest,
    -- i.e. the deleted ones.
    go (!newOnes_, !overlapping_, !remaining) t3@(T3 fileName _ key_) =
        case Map.lookup key_ mftShortcut.nonCrlEntries of
            -- it's not in the map of shortcut children -- new entry
            Nothing -> (t3 : newOnes_, overlapping_, remaining)
            Just e
                | e.fileName == fileName ->
                    (newOnes_, t3 : overlapping_, Map.delete key_ remaining)
                -- it has changed its name (very unlikely but can happen in theory)
                -- -- new entry, and the old one under the same key stays "deleted"
                | otherwise ->
                    (t3 : newOnes_, overlapping_, remaining)

revokedShortcutChildren :: MftShortcut 
                        -> Validated CrlObject
                        -> [T3 Text Hash ObjectKey]
                        -> [(ObjectKey, MftEntry)]
revokedShortcutChildren mftShortcut validCrl children = 
    [ (childKey, makeChildWithIssues childKey fileName)
    | T3 fileName _ childKey <- children
    , Just MftEntry { child } <- [ Map.lookup childKey mftShortcut.nonCrlEntries ]
    , Just childSerial        <- [ getMftChildSerial child ]
    , isRevoked childSerial validCrl ]


resolveTroubledChildByKey :: ValidatorIO es => Tx mode
                            -> ObjectKey
                            -> Eff es (Maybe (TroubledChildLoadPath, Keyed (Located WellStructuredRpkiObject)))
resolveTroubledChildByKey tx childKey =
    DB.getLocatedByKey tx childKey >>= \case
        Just (Located locations (WellStructuredRO vro)) ->
            pure $! Just (TroubledFromParsed, Keyed (Located locations vro) childKey)

        Just (Located locations (OriginalRO (ObjectOriginal blob) _ _ t)) -> 
            vFocusOn ObjectFocus childKey $ 
                -- Re-parsing a cached blob can raise a pure exception for a sufficiently 
                -- broken object. Turn it into a normal validation error so that it stays 
                -- contained to this child instead of failing the whole TA.
                fromTryM (\e -> 
                            parseErr $ "Failed to re-parse the cached object: " <> fmtEx e) $ do
                    validatedRo <- prevalidateObject =<< readObjectOfType t blob
                    pure $! Just (TroubledFromOriginal, Keyed (Located locations validatedRo) childKey)

        _ -> pure Nothing

getStoredObject :: ValidatorIO es => Tx mode
                    -> ObjectKey
                    -> Eff es (Maybe (Keyed (Located RpkiObjectLifecycle)))
getStoredObject tx key =
    fmap (`Keyed` key) <$> DB.getLocatedByKey tx key    

getFullCa :: ValidatorIO es => AppContext s -> TopDownContext -> Ca -> Eff es (Located WellStructuredCaCert)
getFullCa appContext@AppContext {..} topDownContext = \case    
    CaFull c -> pure c            
    CaShort CaShortcut {..} -> do   
        db <- liftIO $ readTVarIO database
        DB.roAppTx db $ \tx -> do 
            increment topDownContext.allTas.topDownCounters.readParsed
            z <- DB.getLocatedByKey tx key
            case z of 
                Just (Located locations (WellStructuredRO (CerRO ca_))) -> pure $! Located locations ca_
                _ -> integrityError appContext 
                        [i|Referential integrity error, wrong type of the CA found by its key #{key}.|]            
    

getCrlByKey :: ValidatorIO es => AppContext s -> ObjectKey -> Eff es (Keyed (Validated CrlObject))
getCrlByKey appContext@AppContext {..} crlKey = do        
    z <- roTxT database $ \tx -> DB.getObjectByKey tx crlKey
    case z of 
        Just (WellStructuredRO (CrlRO c)) -> pure $! Keyed (Validated c) crlKey
        _ -> integrityError appContext [i|Referential integrity error, can't find a CRL by its key #{crlKey}.|]
     
    
integrityError :: ValidatorIO es => AppContext s -> Text -> Eff es a
integrityError AppContext {..} message = do     
    logError logger message
    appError $ ValidationE $ ReferentialIntegrityError message  

makeCaShortcut :: ObjectKey -> Validated WellStructuredCaCert -> PublicationPointAccess -> MftChild
makeCaShortcut key (Validated certificate) ppas = let 
        ValidityPeriod {..} = getValidityPeriod certificate            
        ski = getSKI certificate
        serial = getSerial certificate
        resources = getResources certificate
    in CaChild (CaShortcut {..}) serial

makeRoaShortcut :: ObjectKey -> Validated WellStructuredRoa -> VrpsPerAs -> MftChild
makeRoaShortcut key (Validated roa) roaPayload = let
        ValidityPeriod {..} = getValidityPeriod roa    
        serial = getSerial roa
        resources = getResources roa
    in RoaChild (RoaShortcut {..}) serial

makeSplShortcut :: ObjectKey -> Validated WellStructuredSpl -> SplPayload -> MftChild
makeSplShortcut key (Validated spl) splPayload = let 
        ValidityPeriod {..} = getValidityPeriod spl
        serial = getSerial spl
        resources = getResources spl
    in SplChild (SplShortcut {..}) serial

makeAspaShortcut :: ObjectKey -> Validated WellStructuredAspa -> Aspa -> MftChild
makeAspaShortcut key (Validated aspaObject) aspa = let 
        ValidityPeriod {..} = getValidityPeriod aspaObject            
        serial = getSerial aspaObject
        resources = getResources aspaObject
    in AspaChild (AspaShortcut {..}) serial

makeGbrShortcut :: ObjectKey -> Validated WellStructuredGbr -> T2 Hash Gbr -> MftChild
makeGbrShortcut key (Validated gbrObject) gbr = let 
        ValidityPeriod {..} = getValidityPeriod gbrObject    
        serial = getSerial gbrObject
        resources = getResources gbrObject
    in GbrChild (GbrShortcut {..}) serial

makeBgpSecShortcut :: ObjectKey -> Validated WellStructuredBgpCert -> BGPSecPayload -> MftChild
makeBgpSecShortcut key (Validated bgpCert) bgpSec = let         
        ValidityPeriod {..} = getValidityPeriod bgpCert                  
        serial = getSerial bgpCert
        resources = getResources bgpCert
    in BgpSecChild (BgpSecShortcut {..}) serial

makeMftShortcut :: ObjectKey 
                -> Validated WellStructuredMft -> [(ObjectKey, MftEntry)] 
                -> Keyed (Validated CrlObject) 
                -> MftShortcut   
makeMftShortcut key 
    (Validated mftObject) (Map.fromList -> nonCrlEntries) 
    (Keyed (Validated validCrl) crlKey) = 
  let
    ValidityPeriod {..} = manifestValidityPeriod mftObject
    serial = getSerial mftObject
    manifestNumber = mftObject.content.mftNumber
    crlShortcut = let 
        SignCRL {..} = validCrl.signCrl
        in CrlShortcut {
            key = crlKey,
            notBefore = thisUpdateTime,
            notAfter = nextUpdateTime
        }            
    in MftShortcut { .. }


-- | The period in which a manifest can be used: its EE certificate has to be
-- valid and the manifest itself has to be current, i.e. between thisUpdate and
-- nextUpdate (https://www.rfc-editor.org/rfc/rfc9286.html#section-6.3).
-- `getValidityPeriod` of a manifest is only the EE certificate's.
manifestValidityPeriod :: WellStructuredMft -> ValidityPeriod
manifestValidityPeriod mft =
    let ValidityPeriod eeNotBefore eeNotAfter = getValidityPeriod mft
        Manifest { thisTime, nextTime } = mft.content
    in ValidityPeriod (max eeNotBefore thisTime) (min eeNotAfter nextTime)


-- Same as vFocusOn but it checks that there are no duplicates in the scope focuses, 
-- i.e. we are not returning to the same object again. That would mean we have detected
-- a loop in references.
vUniqueFocusOn :: Validator es => (a -> Focus) -> a -> Eff es r -> Eff es () -> Eff es r
vUniqueFocusOn c a f nonUniqueError = do
    Scopes { validationScope = Scope vs } <- withCurrentScope $ \scopes _ -> scopes
    let focus = c a
    when (focus `elem` vs) nonUniqueError
    vFocusOn c a f 
        

-- | Mark validated objects in the database, i.e.
applyValidationSideEffects :: (MonadIO m) =>
                              AppContext s -> AllTasTopDownContext -> m ()
applyValidationSideEffects
    appContext@AppContext {..}
    AllTasTopDownContext {..} = liftIO $ do        
    (visitedSize, elapsed) <- timedMS $ do
        vks <- readTVarIO visitedKeys            
        rwTxT database $ \tx -> DB.markAsValidated tx vks worldVersion        
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

-- Replace the whole shortcut, i.e. the meta and all the children, with this one.
replaceMftShortcut :: MonadIO m => TopDownContext -> AKI -> MftShortcut -> m ()
replaceMftShortcut TopDownContext { allTas = AllTasTopDownContext {..} } aki MftShortcut {..} =
    liftIO $ do
        let !raw = Verbatim $ toStorable $ Compressed $ DB.MftShortcutMeta {..}
        let !children = shortcutChildRows $ Map.toList nonCrlEntries
        atomically $ writeCQueue shortcutQueue $ ReplaceMftShortcut aki raw children

-- Only the new children get inserted (into `shortcuts` and `mft_shortcut_children`)
-- and only the deleted/revoked keys get removed.
updateMftShortcutChildren :: MonadIO m => TopDownContext -> AKI -> [(ObjectKey, MftEntry)] -> [ObjectKey] -> m ()
updateMftShortcutChildren TopDownContext { allTas = AllTasTopDownContext {..} } aki newEntries deletedKeys =
    liftIO $ do
        let !inserts = shortcutChildRows newEntries
        unless (null inserts && null deletedKeys) $
            atomically $ writeCQueue shortcutQueue $ UpdateMftShortcutChildren aki inserts deletedKeys

-- Pre-serialise each child's data so the heavy lifting happens on this
-- (validation) thread, not on the DB-writer thread.
shortcutChildRows :: [(ObjectKey, MftEntry)] -> [(ObjectKey, Text, BS.ByteString)]
shortcutChildRows entries =
    [ (k, fileName, unStorable $ toStorable $ Compressed child)
    | (k, MftEntry {..}) <- entries ]

deleteMftShortcut :: MonadIO m => TopDownContext -> AKI -> m ()
deleteMftShortcut TopDownContext { allTas = AllTasTopDownContext {..} } aki = 
    liftIO $ atomically $ writeCQueue shortcutQueue $ DeleteMftShortcut aki

storeShortcuts :: (MonadIO m) => 
                AppContext s 
             -> ClosableQueue MftShortcutOp -> m ()
storeShortcuts AppContext {..} shortcutQueue = liftIO $
    readQueueChunked shortcutQueue 1000 $ \shotcutOps ->
        rwTxT database $ \tx ->
            for_ shotcutOps $ \case
                UpdateMftShortcut aki s ->
                    DB.saveMftShorcutMeta tx aki s
                UpdateMftShortcutChildren aki inserts deletedKeys -> do
                    unless (null inserts)     $ DB.insertMftShortcutChildren tx aki inserts
                    unless (null deletedKeys) $ DB.deleteMftShortcutChildren tx aki deletedKeys
                ReplaceMftShortcut aki s children -> do
                    DB.deleteMftShortcut tx aki
                    DB.saveMftShorcutMeta tx aki s
                    DB.insertMftShortcutChildren tx aki children
                DeleteMftShortcut aki ->
                    DB.deleteMftShortcut tx aki


data MftShortcutOp = UpdateMftShortcut AKI (Verbatim (Compressed DB.MftShortcutMeta))
                   | UpdateMftShortcutChildren AKI [(ObjectKey, Text, BS.ByteString)] [ObjectKey]
                   | ReplaceMftShortcut AKI (Verbatim (Compressed DB.MftShortcutMeta)) [(ObjectKey, Text, BS.ByteString)]
                   | DeleteMftShortcut AKI

-- Do whatever is required to notify other subsystems that the object was touched 
-- during top-down validation. It doesn't mean that the object is valid, just that 
-- we read it from the database and looked at it. It will be used to decide when 
-- to GC this object from the cache -- if it's not visited for too long, it is 
-- removed.
markAsUsed :: ValidatorIO es => TopDownContext -> ObjectKey -> Eff es ()
markAsUsed TopDownContext { allTas = AllTasTopDownContext {..} } k = 
    liftIO $ atomically $ modifyTVar' visitedKeys (Set.insert k)

markAsUsedByHash :: ValidatorIO es => 
                    AppContext s -> TopDownContext -> Hash -> Eff es ()
markAsUsedByHash AppContext {..} topDownContext hash = do
    key <- roTxT database $ \tx -> DB.getKeyByHash tx hash
    for_ key $ markAsUsed topDownContext              

oneMoreCert, oneMoreRoa, oneMoreMft, oneMoreCrl :: Validator es => Eff es ()
oneMoreGbr, oneMoreAspa, oneMoreBgp, oneMoreSpl :: Validator es => Eff es ()
oneMoreMftShort :: Validator es => Eff es ()
oneMoreCert = updateMetric @ValidationMetric @_ (#validCertNumber %~ (+1))
oneMoreRoa  = updateMetric @ValidationMetric @_ (#validRoaNumber %~ (+1))
oneMoreSpl  = updateMetric @ValidationMetric @_ (#validSplNumber %~ (+1))
oneMoreMft  = updateMetric @ValidationMetric @_ (#validMftNumber %~ (+1))
oneMoreCrl  = updateMetric @ValidationMetric @_ (#validCrlNumber %~ (+1))
oneMoreGbr  = updateMetric @ValidationMetric @_ (#validGbrNumber %~ (+1))
oneMoreAspa = updateMetric @ValidationMetric @_ (#validAspaNumber %~ (+1))
oneMoreBgp  = updateMetric @ValidationMetric @_ (#validBgpNumber %~ (+1))
oneMoreMftShort = updateMetric @ValidationMetric @_ (#mftShortcutNumber %~ (+1))

moreVrps :: Validator es => Count -> Eff es ()
moreVrps n = updateMetric @ValidationMetric @_ (#vrpCounter %~ (+n))

extractPPAs :: Ca -> Either ValidationError PublicationPointAccess
extractPPAs = \case 
    CaShort (CaShortcut {..}) -> Right ppas 
    CaFull c                  -> getPublicationPointsFromWellStructuredCert c.payload

getCaLocations :: ValidatorIO es => AppContext s -> Ca -> Eff es (Maybe Locations)
getCaLocations AppContext {..} = \case 
    CaShort (CaShortcut {..}) -> 
        roTxT database $ \tx -> DB.getLocationsByKey tx key
    CaFull c ->
        pure $! getLocations c


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
rememberNotValidAfter TopDownContext {..} notAfter = 
    liftIO $ atomically $ modifyTVar' earliestNotValidAfter (<> EarliestToExpire notAfter)

rememberCrlNextUpdate :: MonadIO m => TopDownContext -> Validated CrlObject -> m ()
rememberCrlNextUpdate topDownContext (Validated (CrlObject { signCrl = SignCRL {..}})) = liftIO $ 
    rememberNotValidAfter topDownContext nextUpdateTime

longerThan :: [a] -> Int -> Bool
longerThan xs n = not $ null $ drop n xs
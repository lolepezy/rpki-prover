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
    resolveTroubledChildByKey
)
where

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
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import           Data.Proxy

import           RPKI.AppContext
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

import           RPKI.Store.Database    (Tx, roTxT)
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Util
import           RPKI.Validation.Common
import           RPKI.Validation.Types
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.ResourceValidation
import           RPKI.Validation.TopDown.Context
import           RPKI.Validation.TopDown.Shortcuts
import           RPKI.Validation.TopDown.TaCert


{-
This module implements the top-down validation algorithm.

Validation starts from the Trust Anchor (TA) certificate. The process of downloading 
and selecting the certificate (RPKI.Validation.TopDown.TaCert) implements the 
tie-breaking logic described in 
https://datatracker.ietf.org/doc/draft-spaghetti-sidrops-rpki-ta-tiebreaker/.

After that, validation is recursive for each CA:

- Manifests and the manifest shortcut (see below) are found, and `planManifests` 
  decides how to use them: the shortcut when the manifest hasn't changed, a diff 
  of a new manifest against the shortcut, or the manifests in full.
- Each child on the manifest is either validated in full (`validateChildObject`) 
  or taken from its shortcut (`collectPayloads`). A CA child is validated with 
  its sub-tree, any other child is taken in one place, `acceptLeaf`.
- For new manifests, shortcuts are re-created and saved into a separate queue.

The idea behind shortcuts is as follows:

 - We store minimal representations of objects and their payloads to cache their 
   essential information. This avoids re-validating everything on each run.
 - Manifest shortcuts contain basic manifest metadata plus the shortcuts of their 
   children, stored by ObjectKey. This avoids re-validating manifest children that 
   have already been validated.
 - A child that is invalid or has issues gets a troubled entry instead, so that it's 
   validated in full, and its issues reported, every time.
 - RPKI.Validation.TopDown.Shortcuts makes, uses and writes shortcuts, and 
   RPKI.Validation.TopDown.Context has the state of a validation run.
 
 Validation is designed to be non-interfering with other processes, so it's safe to run 
 concurrently with fetching or cleanup operations (both of which are atomic).

-}

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
    appContext
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
                validateLocationForShortcut appContext topDownContext (c ^. #key)
                ValidityPeriod {..} <- validateObjectValidityPeriod c now
                rememberNotValidAfter topDownContext notAfter
                oneMoreCert
                validateChildrenOf $ toAKI (c ^. #ski)
  where
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
                    pure $ validateManifests appContext topDownContext ca aki


-- Validate the manifest of the CA with its children, the way `planManifests` says.
validateManifests :: (ValidatorIO es, Concurrent :> es) =>
                    AppContext s -> TopDownContext -> Ca -> AKI -> Eff es ()
validateManifests
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..} }
    ca aki = do
    (mfts, shortcut) <- roTxT database $ \tx ->
        (,) <$> DB.getMftsForAKI tx aki
            <*> case validationAlgorithm of
                    FullEveryIteration -> pure Nothing
                    Incremental        -> DB.getMftShorcutMeta tx aki

    let (plan, premature) = planManifests now mfts shortcut

    -- A manifest from the future is a failed fetch (RFC 9286, 6.3) that
    -- has to be reported when an older one is used instead of it
    for_ premature $ \m ->
        withMft appContext m.key $ reportMftFallback appContext $
            ValidationE $ ThisUpdateTimeIsInTheFuture m.thisTime (unNow now)

    case plan of
        NoManifest ->
            vError $ NoMFT aki

        InFull mftMetas -> do
            increment topDownCounters.originalMft
            tryMfts appContext topDownContext ca aki mftMetas

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
                withMft appContext mftMeta.key $ \mft ->
                    tryOneMftWithShortcut meta mft
                        `catchError` \_cs (e :: AppError) -> do
                            -- The shortcut is valid, so it is the latest
                            -- valid manifest to fall back to
                            reportMftFallback appContext e mft
                            onlyCollectPayloads meta
  where
    validationAlgorithm = config.validationConfig.validationAlgorithm

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
        overlappingChildren <- manifestFullValidation appContext topDownContext fullCa mft (Just mftShortcut) aki
        collectPayloads appContext topDownContext aki meta (Map.map ChildWithEntry fullChildren) (Just overlappingChildren)
                    (Left fullCa)
                    (findAndValidateCrl appContext topDownContext fullCa mft aki)
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
        collectPayloads appContext topDownContext aki meta (Map.map ChildLight lightChildren) Nothing
                fullCa
                (getCrlByKey appContext crlKey)
                (getResources ca)


-- Validate the manifests in full, falling back to the next one until one is valid
tryMfts :: (ValidatorIO es, Concurrent :> es) =>
          AppContext s -> TopDownContext -> Ca -> AKI -> [MftMeta] -> Eff es ()
tryMfts _ _ _ aki []                                     = vError $ NoMFT aki
tryMfts appContext topDownContext ca aki (m : mftsMetas_) =
    withMft appContext (m ^. #key) $ \mft -> do
        tryOneMft mft `catchError` \_cs (e :: AppError) ->
            case mftsMetas_ of
                [] -> appError e
                _  -> do
                    reportMftFallback appContext e mft
                    tryMfts appContext topDownContext ca aki mftsMetas_
  where
    tryOneMft mft = do
        markAsUsed topDownContext $ mft ^. #key
        caFull <- getFullCa appContext topDownContext ca
        void $ manifestFullValidation appContext topDownContext caFull mft Nothing aki
        oneMoreMft >> oneMoreCrl


withMft :: ValidatorIO es =>
          AppContext s -> ObjectKey -> (Keyed (Located WellStructuredMft) -> Eff es a) -> Eff es a
withMft appContext@AppContext {..} key f = do
    z <- roTxT database $ \tx -> DB.getMftByKey tx key
    case z of
        Nothing  -> integrityError appContext [i|Referential integrity error, can't find a manifest by its key #{key}.|]
        Just mft -> f mft


reportMftFallback :: ValidatorIO es =>
                    AppContext s -> AppError -> Keyed (Located WellStructuredMft) -> Eff es ()
reportMftFallback AppContext {..} e mft = do
    let mftLocation = describeLocated $ mft ^. #object
    let mftNumber = mft ^. #object . #payload . #content . #mftNumber
    vFocusOn ObjectFocus (mft ^. #key) $ vWarn $ MftFallback e mftNumber
    logWarn logger [i|Falling back to the previous manifest for #{mftLocation}, failed manifest number #{mftNumber}, error: #{toMessage e}|]


-- Proceed with full validation for children mentioned in the full manifest
-- and children mentioned in the manifest shortcut. Create a diff between them,
-- run full validation only for new children and create a new manifest shortcut
-- with updated set of children.
manifestFullValidation :: (ValidatorIO es, Concurrent :> es) =>
                        AppContext s
                        -> TopDownContext
                        -> Located WellStructuredCaCert
                        -> Keyed (Located WellStructuredMft)
                        -> Maybe MftShortcut
                        -> AKI
                        -> Eff es [T3 Text Hash ObjectKey]
manifestFullValidation
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    fullCa
    keyedMft@(Keyed locatedMft@(Located mftLocations mft) mftKey)
    mftShortcut childrenAki = do
    let uniqueFocusOn = case mftLocations of
            Just ls -> vUniqueFocusOn LocationFocus (getURL $ pickLocation ls)
            Nothing -> vUniqueFocusOn HashFocus (getHash mft)
    uniqueFocusOn
        doValidate
        (vError $ CircularReference $ KeyIdentity mftKey)
  where
    validationAlgorithm = config.validationConfig.validationAlgorithm

    doValidate = do
        -- General location validation
        validateObjectLocations locatedMft

        -- Manifest-specific location validation
        validateMftLocation locatedMft fullCa

        keyedValidCrl@(Keyed validCrl@(Validated validCrlObject) crlKey) <- findAndValidateCrl appContext topDownContext fullCa keyedMft childrenAki

        -- MFT can be revoked by the CRL that is on this MFT -- detect
        -- revocation as well, this is clearly an error
        validMft <- validateMft (config ^. #validationConfig . typed)
                                now mft (fullCa ^. #payload) validCrl verifiedResources

        let ValidityPeriod { notAfter = mftNotAfter } = manifestValidityPeriod mft
        rememberNotValidAfter topDownContext mftNotAfter
        rememberCrlNextUpdate topDownContext validCrl

        -- Validate entry list and filter out CRL itself
        nonCrlChildren <- validateMftEntries appContext mft (getHash validCrlObject)

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
                    gatherMftEntryResults =<<
                        gatherMftEntryValidations appContext topDownContext fullCa newChildren validCrl

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
                                    replaceMftShortcut shortcutQueue aki nextMftShortcut
                                    increment topDownCounters.updateMftMeta
                                    increment topDownCounters.updateMftChildren

                                Just mftShort -> do
                                    -- If manifest key is not the same as the shortcut key,
                                    -- we need to replace the shortcut with the new one
                                    when (mftShort.key /= mftKey) $ do
                                        updateMftShortcut shortcutQueue aki nextMftShortcut
                                        increment topDownCounters.updateMftMeta

                                    -- Update manifest shortcut children in case there are new
                                    -- or deleted children in the new manifest.
                                    when (not (null newChildren)
                                        || not (null deletedKeys)
                                        || not (null revokedEntries)) $ do
                                            updateMftShortcutChildren shortcutQueue aki newEntries deletedKeys
                                            increment topDownCounters.updateMftChildren

                    _  -> pure ()

                pure $! overlappingChildren

        processChildren `recover` markAllEntriesAsUsed


findAndValidateCrl :: ValidatorIO es =>
                    AppContext s
                    -> TopDownContext
                    -> Located WellStructuredCaCert
                    -> Keyed (Located WellStructuredMft)
                    -> AKI
                    -> Eff es (Keyed (Validated CrlObject))
findAndValidateCrl
    AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..} }
    fullCa (Keyed (Located _ mft) _) aki = do
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
makeEntriesWithMap :: [T3 Text a ObjectKey] -> Map ObjectKey e -> [(ObjectKey, e)]
makeEntriesWithMap childrenList entryMap =
    [ (key, entry) |
        T3 _ _ key <- childrenList,
        entry      <- maybeToList $ Map.lookup key entryMap ]


-- Check which of the shortcut children are revoked by the (new) CRL, reporting
-- a warning for each. Returns the replacement shortcut entries for them.
checkForRevokedChildren :: ValidatorIO es => MftShortcut
                        -> Keyed (Located WellStructuredMft)
                        -> [T3 Text Hash ObjectKey]
                        -> Validated CrlObject
                        -> Eff es [(ObjectKey, MftEntry)]
checkForRevokedChildren mftShortcut (Keyed (Located _ mft) _) children validCrl = do
    when (isRevoked (getSerial mft) validCrl) $
        vWarn RevokedResourceCertificate
    let revoked = revokedShortcutChildren mftShortcut validCrl children
    forM_ revoked $ \(childKey, _) ->
        vFocusOn ObjectFocus childKey $ vWarn RevokedResourceCertificate
    pure revoked


-- this indicates the difference between RFC9286-bis
-- version 02 (strict) and version 03 and later (more loose).
gatherMftEntryValidations :: (ValidatorIO es, Concurrent :> es) =>
                            AppContext s
                            -> TopDownContext
                            -> Located WellStructuredCaCert
                            -> [T3 Text Hash ObjectKey]
                            -> Validated CrlObject
                            -> Eff es [ManifestValidity AppError ValidationState]
gatherMftEntryValidations appContext@AppContext {..} topDownContext =
    case config.validationConfig.manifestProcessing of
        {-
        https://datatracker.ietf.org/doc/rfc9286/
        item 6.4 says
            "If there are files listed in the manifest that cannot be retrieved
            from the publication point, the fetch has failed.."

        For that case validity of every object on the manifest is completely
        separate from each other and don't influence the manifest validity.
        -}
        RFC9286 -> independentMftChildrenResults appContext topDownContext

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
        RFC6486_Strict -> allOrNothingMftChildrenResults appContext topDownContext


allOrNothingMftChildrenResults :: (ValidatorIO es, Concurrent :> es) =>
                                AppContext s
                                -> TopDownContext
                                -> Located WellStructuredCaCert
                                -> [T3 Text Hash ObjectKey]
                                -> Validated CrlObject
                                -> Eff es [ManifestValidity AppError ValidationState]
allOrNothingMftChildrenResults appContext topDownContext fullCa nonCrlChildren validCrl = do
    scopes <- askScopes
    forChildren topDownContext.allTas.workPool
        nonCrlChildren
        $ \(T3 filename hash' key) -> do
            (z, vs) <- runValidator scopes $ do
                            ro <- getManifestEntry appContext topDownContext filename hash' key
                            -- if failed this one interrupts the whole MFT valdiation
                            validateMftChild appContext topDownContext fullCa ro filename validCrl
            pure $! case z of
                -- In this case invalid child is considered invalid entry
                -- and the whole manifest is invalid
                Left e              -> InvalidEntry e vs
                Right entry         -> ValidEntry vs key (keptEntry appContext entry)


independentMftChildrenResults :: (ValidatorIO es, Concurrent :> es) =>
                                AppContext s
                                -> TopDownContext
                                -> Located WellStructuredCaCert
                                -> [T3 Text Hash ObjectKey]
                                -> Validated CrlObject
                                -> Eff es [ManifestValidity AppError ValidationState]
independentMftChildrenResults appContext topDownContext fullCa nonCrlChildren validCrl = do
    scopes <- askScopes
    forChildren topDownContext.allTas.workPool
        nonCrlChildren
        $ \(T3 filename hash key) -> do
            (r, vs) <- runValidator scopes $ getManifestEntry appContext topDownContext filename hash key
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
                    (z, vs') <- runValidator scopes $ validateMftChild appContext topDownContext fullCa ro filename validCrl
                    -- What reading the object reported, e.g. the errors stored with
                    -- an object that failed to parse, is kept with what validating
                    -- it reports
                    let allVs = vs <> vs'
                    pure $! case z of
                            Left e              -> InvalidChild e allVs key filename
                            Right entry         -> ValidEntry allVs key (keptEntry appContext entry)


-- A child CA is a whole sub-tree to validate and a task of its own,
-- other objects are validated in chunks.
forChildren :: IOE :> es =>
            WorkPool -> [T3 Text Hash ObjectKey] -> (T3 Text Hash ObjectKey -> Eff es b) -> Eff es [b]
forChildren workPool = forInPool workPool 64 $ \(T3 fileName _ _) ->
                textObjectType fileName == Just CER


-- | Entries of children go into the manifest shortcut, i.e. only with incremental
-- validation, and without it they aren't kept at all. The results of all the
-- children of a manifest are kept until the sub-trees of its CA children are
-- validated, and so would be the entries of all its other children.
keptEntry :: AppContext s -> MftEntry -> Maybe MftEntry
keptEntry appContext entry =
    case appContext.config.validationConfig.validationAlgorithm of
        Incremental        -> Just entry
        FullEveryIteration -> Nothing


gatherMftEntryResults :: Validator es =>
                        [ManifestValidity AppError ValidationState] -> Eff es [(ObjectKey, MftEntry)]
gatherMftEntryResults =
    foldM (\childrenShortcuts r -> do
        case r of
            InvalidEntry e vs -> do
                embedState vs
                appError e
            InvalidChild _ vs key fileName -> do
                embedState vs
                let !entry = makeChildWithIssues key fileName
                pure $! (key, entry) : childrenShortcuts
            ValidEntry vs key entry -> do
                embedState vs
                -- Issues about the child in the scope of the manifest, e.g. its
                -- name not matching its location, don't make it troubled: they
                -- stop the manifest shortcut from being made at all.
                pure $! maybe childrenShortcuts (\e -> (key, e) : childrenShortcuts) entry
        ) mempty


-- Check manifest entries as a whole, without doing anything
-- with the objects they are pointing to.
validateMftEntries :: ValidatorIO es =>
                    AppContext s -> WellStructuredMft -> Hash -> Eff es [T3 Text Hash ObjectKey]
validateMftEntries AppContext {..} mft crlHash = do
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
getManifestEntry :: ValidatorIO es =>
                    AppContext s -> TopDownContext -> Text -> Hash -> ObjectKey
                    -> Eff es (Keyed (Located RpkiObjectLifecycle))
getManifestEntry
    AppContext {..}
    TopDownContext { allTas = AllTasTopDownContext {..} }
    filename hash' key = do
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


validateMftChild :: (ValidatorIO es, Concurrent :> es) =>
                    AppContext s
                    -> TopDownContext
                    -> Located WellStructuredCaCert
                    -> Keyed (Located RpkiObjectLifecycle)
                    -> Text
                    -> Validated CrlObject
                    -> Eff es MftEntry
validateMftChild appContext topDownContext caFull child@(Keyed (Located objectLocations _) _)
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
            pure $! makeChildWithIssues child.key filename
        WellStructuredRO wellStructuredChild ->
            validateChildObject appContext topDownContext
                caFull
                (child & #object . #payload .~ wellStructuredChild)
                filename
                validCrl


-- Location validation when all we have is a key.
--
-- Only objects with more than one location need anything done, and which
-- objects those are was read once for the whole run, so the common case is
-- a set lookup rather than a query and a transaction per object.
validateLocationForShortcut :: ValidatorIO es => AppContext s -> TopDownContext -> ObjectKey -> Eff es ()
validateLocationForShortcut
    appContext@AppContext {..}
    TopDownContext { allTas = AllTasTopDownContext {..} }
    key =
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

    And return the entry of the manifest shortcut for it
-}
validateChildObject :: (ValidatorIO es, Concurrent :> es) =>
                    AppContext s
                    -> TopDownContext
                    -> Located WellStructuredCaCert
                    -> Keyed (Located WellStructuredRpkiObject)
                    -> Text
                    -> Validated CrlObject
                    -> Eff es MftEntry
validateChildObject
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    fullCa (Keyed child@(Located locations childRo) childKey) fileName validCrl =
    vFocusOnLocated child $ entryFor =<< case childRo of
        CerRO childCert -> validCaChild childCert

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
        _somethingElse -> do
            logWarn logger [i|Unsupported type of object: #{locations}.|]
            pure Nothing

    where
        validationRFC = config.validationConfig.validationRFC

        -- What is stored for a child validated in full: its shortcut when it's
        -- valid and there are no issues in its scope, a troubled entry otherwise.
        -- A troubled child is validated in full again next time, so its issues
        -- are reported every time, and not only this once.
        entryFor valid = do
            issues <- thisScopeIssues
            pure $! case valid of
                Just shortcut | Set.null issues -> MftEntry fileName shortcut
                _                               -> makeChildWithIssues childKey fileName

        validCaChild childCert = do
            scopes <- askScopes
            {-
                Note that recursive validation of the child CA happens in the separate
                runValidator (...) call, it is to avoid short-circuit logic implemented by ExceptT:
                otherwise an error in child validation would interrupt validation of the parent with
                ExceptT's exception logic.
            -}
            (r, validationState) <- runValidator scopes $ do
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
                Left _  -> pure Nothing
                Right _ ->
                    case getPublicationPointsFromWellStructuredCert childCert of
                        -- It's not going to happen?
                        Left e     -> vError e
                        Right ppas -> pure $ Just $ makeCaShortcut childKey (Validated childCert) ppas

        -- Validate an object other than a CA certificate, which gives
        -- its shortcut, and take it the same way the shortcut is taken
        -- in the next rounds
        validLeaf validate = do
            validateObjectLocations child
            allowRevoked $ do
                leaf <- validate
                acceptLeaf topDownContext FromObject leaf
                pure $ Just leaf

        -- In case of RevokedResourceCertificate error, the whole manifest is not to be considered
        -- invalid, only the object with the revoked certificate is considered invalid.
        -- Replace RevokedResourceCertificate error with a warning and don't break the
        -- validation process.
        -- This is a hacky and ad-hoc, but it works fine.
        allowRevoked f =
            catchAndEraseError f isRevokedCertError $ do
                vWarn RevokedResourceCertificate
                pure Nothing
            where
                isRevokedCertError (ValidationE RevokedResourceCertificate) = True
                isRevokedCertError _ = False


thisScopeIssues :: Validator es => Eff es (Set VIssue)
thisScopeIssues =
    withCurrentScope $ \scopes vs ->
        getIssues (scopes ^. typed) (vs ^. typed)


collectPayloads :: (ValidatorIO es, Concurrent :> es) =>
                AppContext s
                -> TopDownContext
                -> AKI
                -> DB.MftShortcutMeta
                -> Map.Map ObjectKey ChildData
                -> Maybe [T3 Text Hash ObjectKey]
                -> Either (Located WellStructuredCaCert) (Eff es (Located WellStructuredCaCert))
                -> Eff es (Keyed (Validated CrlObject))
                -> AllResources
                -> Eff es ()
collectPayloads
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    childrenAki meta childrenMap childrenToCheck findFullCa findValidCrl parentCaResources = do

    -- Filter children that we actually want to go through here
    let filteredChildren =
            case childrenToCheck of
                Nothing -> Map.toList childrenMap
                Just ch -> catMaybes [ (k,) <$> Map.lookup k childrenMap | T3 _ _ k <- ch ]

    let anyTroubled =
            or [ True | (_, childData) <- filteredChildren,
                        TroubledChild {} <- [childOf childData] ]

    vFocusOn ObjectFocus meta.key $ do
        validateLocationForShortcut appContext topDownContext meta.key
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
    validationRFC = config.validationConfig.validationRFC

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
                    deleteMftShortcut shortcutQueue $ toAKI $ getSKI caFull
                    logError logger [i|Troubled child #{childKey} not found in the database, will delete manifest shortcut.|]
                    integrityError appContext
                        [i|Referential integrity error, can't find a troubled child by its key #{childKey}.|]

        validateChildObject appContext topDownContext caFull childObject fileName validCrl

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
            updateMftShortcutChildren shortcutQueue childrenAki [(childKey, newEntry)] []

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
                storeChildIfChanged childKey_ childData newEntry
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
        validateLocationForShortcut appContext topDownContext key
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

-- Same as vFocusOn but it checks that there are no duplicates in the scope focuses, 
-- i.e. we are not returning to the same object again. That would mean we have detected
-- a loop in references.
vUniqueFocusOn :: Validator es => (a -> Focus) -> a -> Eff es r -> Eff es () -> Eff es r
vUniqueFocusOn c a f nonUniqueError = do
    Scopes { validationScope = Scope vs } <- withCurrentScope $ \scopes _ -> scopes
    let focus = c a
    when (focus `elem` vs) nonUniqueError
    vFocusOn c a f 
        

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
                          | ValidEntry v ObjectKey (Maybe MftEntry)

longerThan :: [a] -> Int -> Bool
longerThan xs n = not $ null $ drop n xs
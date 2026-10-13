-- Local validator helpers are used both at the enclosing effect stack and
-- under nested `runValidator` calls (which push fresh Reader/Error/State
-- handlers). GHC2024 implies MonoLocalBinds, which would pin the unsignatured
-- ones to the enclosing stack; turn it off so they generalise over `es`.
{-# LANGUAGE NoMonoLocalBinds     #-}
{-# LANGUAGE StrictData           #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Validation.TopDown (
    TopDownResult(..),
    validateMultipleTAs,
    becomingValid,
    validateManifests,
    validateCa,
    refreshTaCertificate,
    TroubledChildLoadPath(..),
    resolveTroubledChildByKey,
    signedByCa
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
import           Data.List.NonEmpty               (NonEmpty (..))
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
import           Data.X509.Validation             (SignatureVerification (..))

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
import           RPKI.Validation.Crypto
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
        earliestValidityChange :: EarliestValidityChange,
        -- | RRDP repositories to fetch from a snapshot next time, with why
        snapshotCandidates     :: MonoidalMap.MonoidalMap RrdpURL (Set Text)
    }
    deriving stock (Show, Eq, Ord, Generic)    
    deriving Semigroup via GenericSemigroup TopDownResult
    deriving Monoid    via GenericMonoid TopDownResult

fromValidations :: ValidationState -> TopDownResult
fromValidations vs = TopDownResult mempty mempty vs mempty mempty mempty

data TroubledChildLoadPath = TroubledFromParsed | TroubledFromOriginal
    deriving stock (Show, Eq, Ord, Generic)

-- | It is the main entry point for the top-down validation. 
-- Validates a bunch of TAs starting from their TALs.  
validateMultipleTAs :: AppContext s
                    -> WorldVersion
                    -> [TAL]
                    -> IO (Map TaName TopDownResult)
validateMultipleTAs appContext@AppContext {..} worldVersion tals = do
    -- All the TAs are validated by one pool of workers (see `WorkPool`), one on 
    -- every capability. The writer of manifest shortcuts, the only thread writing
    -- to the database here, gets an extra capability of its own.
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
        (discoveredRepositories, snapshotCandidates) <- atomically $ (,) 
            <$> readTVar (topDownContext ^. #fetcheables)
            <*> readTVar (topDownContext ^. #snapshotCandidates)
        pure $ fromValidations validations 
                & #discoveredRepositories .~ discoveredRepositories
                & #snapshotCandidates .~ snapshotCandidates

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

    (discoveredRepositories, earliestNotValidAfter, snapshotCandidates) <- 
        atomically $ (,,) <$>
            readTVar (topDownContext ^. #fetcheables) <*>
            readTVar (topDownContext ^. #earliestNotValidAfter) <*>
            readTVar (topDownContext ^. #snapshotCandidates)

    -- An object that's not valid yet is used once it becomes valid, 
    -- that's as much of a change as an object that expires
    let earliestValidityChange = earliestNotValidAfter <> becomingValid (topDownValidations ^. typed)

    logDebug logger [i|Earliest #{taName}, earliestValidityChange=#{earliestValidityChange}|]

    case r of
        Left _ -> 
            pure $ fromValidations topDownValidations 
                    & #discoveredRepositories .~ discoveredRepositories
                    & #earliestValidityChange .~ earliestValidityChange
                    & #snapshotCandidates .~ snapshotCandidates
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

-- | The earliest moment when an object that isn't valid yet becomes valid.
becomingValid :: Validations -> EarliestValidityChange
becomingValid (Validations issues) =
    foldMapOf (folded . folded . issueValidationErrors) validFrom issues
  where
    validFrom = \case
        ObjectValidityIsInTheFuture notBefore _  -> EarliestValidityChange notBefore
        ThisUpdateTimeIsInTheFuture thisUpdate _ -> EarliestValidityChange thisUpdate
        _                                        -> mempty
        

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
    topDownContext@TopDownContext {..}
    ca 
    -- The depth is of one path, e.g. of a CA that keeps issuing CAs under 
    -- itself, so only this path is cut and the rest of the TA is validated
    | currentPathDepth > validationConfig ^. #maxCertificatePathDepth =
        logCheck 
            (CertificatePathTooDeep $ validationConfig ^. #maxCertificatePathDepth)
            (\loc -> [i|Interrupting validation on #{fmtLocations loc}, maximum tree depth is reached.|])
    | otherwise =
        checkAndReport visitedObjectCountLimit
            $ checkAndReport repositoryCountLimit
            $ validateCaNoLimitChecks appContext topDownContext ca        
  where
    -- Check and report for the maximal number of objects in the tree
    visitedObjectCountLimit = (
        (> validationConfig ^. #maxTotalTreeSize) <$> readTVar treeSize,
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

    -- The size of the tree and the number of repositories are for the whole TA:
    -- once one of them is reached, the rest of the TA isn't validated. The error 
    -- is reported only by the thread that first hits it.
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
    unless (null premature) $ do
        fullCa <- getFullCa appContext topDownContext ca
        for_ premature $ \m ->
            getSignedMft appContext topDownContext fullCa m >>= \case
                Just (SignedMft mft) -> reportMftFallback appContext
                    (ValidationE $ ThisUpdateTimeIsInTheFuture m.thisTime (unNow now)) mft
                _ -> pure ()

    -- The errors of the manifests that were tried and fallen back from
    fallbackErrors <- liftIO $ newIORef []
    let fallBackFrom e = liftIO $ modifyIORef' fallbackErrors (e :)

    onFailure shortcut fallbackErrors $ case plan of
        NoManifest ->
            vError $ NoMFT aki

        InFull mftMetas -> do
            increment topDownCounters.originalMft
            tryMfts appContext topDownContext fallBackFrom ca aki mftMetas

        UseShortcut meta ->
            fromShortcut mfts meta $
                useShortcut mfts meta

        OnlyShortcut meta -> do
            vWarn $ NoMFTButCachedMft aki
            fromShortcut mfts meta $
                onlyCollectPayloads meta

        DiffWithShortcut meta (newest :| newer) ->
            fromShortcut mfts meta $ do
                fullCa <- getFullCa appContext topDownContext ca
                tryNewerMfts fallBackFrom mfts meta fullCa (newest : newer)
  where
    validationAlgorithm = config.validationConfig.validationAlgorithm

    -- The manifests newer than the one of the shortcut, newest first: use the
    -- first one that the CA signed and that is valid. Each one that isn't valid,
    -- or is gone from the cache, is fallen back from and the next one is tried.
    -- When none of them is valid, the shortcut is, so it is the latest valid
    -- manifest to fall back to.
    tryNewerMfts fallBackFrom mfts meta fullCa mftMetas =
        nextSignedMft appContext topDownContext fullCa mftMetas >>= \case
            Nothing -> useShortcut mfts meta
            Just (candidate, olderMfts) ->
                tryManifest appContext candidate
                    (\e reportFallback -> do
                        void $ fallBackFrom e
                        reportFallback
                        tryNewerMfts fallBackFrom mfts meta fullCa olderMfts)
                    (\mft -> do
                        markAsUsed topDownContext $ mft ^. #key
                        tryOneMftWithShortcut meta fullCa mft)

    -- The CA ends up without an accepted manifest.
    --
    -- Shortcuts describe the tree as it was validated, so it mustn't keep one.
    --
    -- If the cache doesn't have what one of the manifests says, the repository 
    -- is to be fetched from a snapshot. Only now that there's nothing left to 
    -- fall back to: falling back is what the previous manifests are for, and 
    -- a newer manifest that refers to objects that aren't there yet is the 
    -- publisher's problem.
    onFailure shortcut fallbackErrors validate =
        validate `catchError` \_cs (e :: AppError) -> do
            for_ shortcut $ \_ -> deleteMftShortcut shortcutQueue aki
            errors <- (e :) <$> liftIO (readIORef fallbackErrors)
            let integrityErrors = filter isReferentialIntegrityError errors
            unless (null integrityErrors) $ do
                scope <- askScopes
                for_ (innermostRrdpPP $ scope ^. typed @VScope) $ \rrdpUrl ->
                    rememberSnapshotCandidate topDownContext rrdpUrl $
                        Set.fromList $ map toMessage integrityErrors
            appError e

    -- Use the manifest of the shortcut. One that was accepted with warnings is
    -- validated in full again, its children too, so that the warnings are
    -- reported every time, also the ones about its children. There's nothing
    -- to fall back to if that fails. If the manifest is gone from the cache,
    -- there's nothing to validate and only the shortcut is left.
    useShortcut mfts meta
        | meta.hasIssues && any ((== meta.key) . (.key)) mfts =
            withMft appContext meta.key $ validateAgain meta
        | otherwise =
            onlyCollectPayloads meta

    fromShortcut mfts meta validate = do
        markAsUsed topDownContext meta.key
        for_ (shortcutMftNextUpdate mfts meta) $ rememberNotValidAfter topDownContext
        increment topDownCounters.shortcutMft
        r <- validate
        oneMoreMft >> oneMoreCrl >> oneMoreMftShort
        pure r

    -- The manifest changed since the last shortcut: run the full diff, which needs
    -- file_name (to detect renames), so fetch the full children map.
    tryOneMftWithShortcut meta fullCa mft = do
        mftShortcut <- fullShortcut meta
        overlappingChildren <- manifestFullValidation appContext topDownContext fullCa mft
                                    (Just mftShortcut) OnlyNewChildren aki

        collectPayloads appContext topDownContext aki meta 
                    (Map.map ChildWithEntry mftShortcut.nonCrlEntries) 
                    (Just overlappingChildren)
                    (Left fullCa)
                    (findAndValidateCrl appContext topDownContext fullCa mft aki)
                    (getResources ca)

    -- The manifest of the shortcut was accepted with warnings: validate it again,
    -- all its children too, which gives all the payloads. There's nothing left
    -- to collect from the shortcut.
    validateAgain meta mft = do
        fullCa      <- getFullCa appContext topDownContext ca
        mftShortcut <- fullShortcut meta
        void $ manifestFullValidation appContext topDownContext fullCa mft
                    (Just mftShortcut) AllChildren aki

    fullShortcut DB.MftShortcutMeta{..} = do
        markAsUsed topDownContext crlShortcut.key
        fullChildren <- roTxT database $ \tx -> DB.getMftShorcutChildrenFull tx aki
        pure MftShortcut { nonCrlEntries = fullChildren, .. }

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
        collectPayloads appContext topDownContext aki meta (Map.map (uncurry ChildLight) lightChildren) Nothing
                fullCa
                (getCrlByKey appContext crlKey)
                (getResources ca)


-- Validate the manifests that the CA signed in full, falling back to the next one
-- until one is valid. A manifest that is gone from the cache is fallen back from too.
tryMfts :: (ValidatorIO es, Concurrent :> es) =>
          AppContext s -> TopDownContext -> (AppError -> Eff es ()) -> Ca -> AKI -> [MftMeta] -> Eff es ()
tryMfts _ _ _ _ aki [] = vError $ NoMFT aki
tryMfts appContext topDownContext fallBackFrom ca aki mftMetas = do
    caFull <- getFullCa appContext topDownContext ca
    nextSignedMft appContext topDownContext caFull mftMetas >>= \case
        Nothing                     -> vError $ NoMFT aki
        Just (candidate, mftMetas_) -> tryFrom caFull candidate mftMetas_
  where
    -- The error of the last one is the error of the CA, the ones before it are fallbacks
    tryFrom caFull candidate mftMetas_ =
        tryManifest appContext candidate (fallBack caFull mftMetas_) (tryOneMft caFull)

    fallBack caFull mftMetas_ e reportFallback =
        nextSignedMft appContext topDownContext caFull mftMetas_ >>= \case
            Nothing                  -> appError e
            Just (next, mftMetas__) -> do
                fallBackFrom e
                reportFallback
                tryFrom caFull next mftMetas__

    tryOneMft caFull mft = do
        markAsUsed topDownContext $ mft ^. #key
        void $ manifestFullValidation appContext topDownContext caFull mft Nothing OnlyNewChildren aki
        oneMoreMft >> oneMoreCrl


-- | A manifest of a CA to try: one that the CA signed, or one that is gone 
-- from the cache, so that it can't be told whether the CA signed it.
data MftCandidate = SignedMft (Keyed (Located WellStructuredMft))
                  | MissingMft MftMeta AppError

-- | Validate a manifest the given way, and fall back from it if that fails, or 
-- if the manifest is gone from the cache. Falling back gets the error, and how
-- to report it.
tryManifest :: ValidatorIO es =>
              AppContext s
              -> MftCandidate
              -> (AppError -> Eff es () -> Eff es a)
              -> (Keyed (Located WellStructuredMft) -> Eff es a)
              -> Eff es a
tryManifest appContext candidate fallBack validate =
    case candidate of
        MissingMft m e ->
            fallBack e $ vFocusOn ObjectFocus (m ^. #key) $ vWarn $ MftFallback e (m ^. #mftNumber)
        SignedMft mft ->
            validate mft `catchError` \_cs (e :: AppError) ->
                fallBack e $ reportMftFallback appContext e mft


withMft :: ValidatorIO es =>
          AppContext s -> ObjectKey -> (Keyed (Located WellStructuredMft) -> Eff es a) -> Eff es a
withMft appContext@AppContext {..} key f = do
    z <- roTxT database $ \tx -> DB.getMftByKey tx key
    case z of
        Nothing  -> integrityError appContext [i|Referential integrity error, can't find a manifest by its key #{key}.|]
        Just mft -> f mft


-- | Manifests are found by AKI, wherever they were published, and anyone can
-- publish one with the AKI of any CA. Only those with an EE certificate signed
-- by the key of the CA are its manifests (`validateCmsStructure` has checked
-- that the EE certificate signed the rest). The others are not tried or
-- reported for the CA, so that they can't change what is validated or reported
-- for it. The location of a manifest can't tell them apart: an RRDP repository
-- can publish an object at any URL, and Erik objects have none.
--
-- All the certificates with the SKI of the CA have the same key, since the SKI
-- is checked to be its hash (`validateSKIMatchesPublicKey`), so it doesn't
-- matter which of them is used.
signedByCa :: Located WellStructuredCaCert -> WellStructuredCms a -> Bool
signedByCa ca cms =
    case validateSignMaterial (cms ^. #eeCert) ca of
        SignaturePass     -> True
        SignatureFailed _ -> False

-- | The manifest, if the CA signed it, or if it is gone from the cache.
getSignedMft :: ValidatorIO es =>
                AppContext s -> TopDownContext -> Located WellStructuredCaCert -> MftMeta
                -> Eff es (Maybe MftCandidate)
getSignedMft AppContext {..} topDownContext ca m = do
    let mftKey = m ^. #key
    z <- roTxT database $ \tx -> DB.getMftByKey tx mftKey
    case z of
        Nothing -> do
            let message = [i|Referential integrity error, can't find a manifest by its key #{mftKey}.|]
            logError logger message
            pure $ Just $ MissingMft m $ ValidationE $ ReferentialIntegrityError message
        Just mft
            | signedByCa ca (mft ^. #object . #payload) -> pure $ Just $ SignedMft mft
            | otherwise -> do
                increment topDownContext.allTas.topDownCounters.foreignMft
                pure Nothing

-- | The first of the manifests that the CA signed, or that is gone from the cache,
-- with the ones after it.
nextSignedMft :: ValidatorIO es =>
                AppContext s -> TopDownContext -> Located WellStructuredCaCert -> [MftMeta]
                -> Eff es (Maybe (MftCandidate, [MftMeta]))
nextSignedMft _ _ _ [] = pure Nothing
nextSignedMft appContext topDownContext ca (m : ms) =
    getSignedMft appContext topDownContext ca m >>= \case
        Just mft -> pure $ Just (mft, ms)
        Nothing  -> nextSignedMft appContext topDownContext ca ms


reportMftFallback :: ValidatorIO es =>
                    AppContext s -> AppError -> Keyed (Located WellStructuredMft) -> Eff es ()
reportMftFallback AppContext {..} e mft = do
    let mftLocation = describeLocated $ mft ^. #object
    let mftNumber = mft ^. #object . #payload . #content . #mftNumber
    vFocusOn ObjectFocus (mft ^. #key) $ vWarn $ MftFallback e mftNumber
    logWarn logger [i|Falling back to the previous manifest for #{mftLocation}, failed manifest number #{mftNumber}, error: #{toMessage e}|]


-- Proceed with full validation for children mentioned in the full manifest
-- and children mentioned in the manifest shortcut. Create a diff between them,
-- run full validation only for new children (or for all of them, if asked to)
-- and create a new manifest shortcut with updated set of children.
manifestFullValidation :: (ValidatorIO es, Concurrent :> es) =>
                        AppContext s
                        -> TopDownContext
                        -> Located WellStructuredCaCert
                        -> Keyed (Located WellStructuredMft)
                        -> Maybe MftShortcut
                        -> ChildrenToValidate
                        -> AKI
                        -> Eff es [T3 Text Hash ObjectKey]
manifestFullValidation
    appContext@AppContext {..}
    topDownContext@TopDownContext { allTas = AllTasTopDownContext {..}, .. }
    fullCa
    keyedMft@(Keyed locatedMft@(Located mftLocations mft) mftKey)
    mftShortcut childrenToValidate childrenAki = do
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
                    Just mftShort ->
                        let diff@(_, _, deleted) = manifestDiff mftShort nonCrlChildren
                        in case childrenToValidate of
                            OnlyNewChildren -> diff
                            AllChildren     -> (nonCrlChildren, [], deleted)

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

                case validationAlgorithm of
                    -- Only create shortcuts for case of incremental validation.
                    --
                    -- NOTE: That means that in case of full validation falling back to
                    -- the previous valid manifest will not work, since there are no
                    -- shortcuts of previous manifests to fall back to.
                    Incremental -> do
                        -- A manifest accepted with warnings gets a shortcut as well,
                        -- shortcuts describe the tree as it was validated. It's
                        -- marked, so that it's validated in full every time and the
                        -- warnings are reported every time.
                        hasIssues <- not . Set.null <$> thisScopeIssues
                        let aki = toAKI $ getSKI fullCa
                        let nextMftShortcut = makeMftShortcut mftKey validMft newEntries keyedValidCrl hasIssues

                        case shortcutWrites mftShortcut nextMftShortcut deletedKeys of
                            ReplaceShortcut -> do
                                replaceMftShortcut shortcutQueue aki nextMftShortcut
                                increment topDownCounters.updateMftMeta
                                increment topDownCounters.updateMftChildren

                            UpdateShortcut {..} -> do
                                when writeMeta $ do
                                    updateMftShortcut shortcutQueue aki nextMftShortcut
                                    increment topDownCounters.updateMftMeta

                                unless (null changedChildren && null deletedChildren) $ do
                                    updateMftShortcutChildren shortcutQueue aki changedChildren deletedChildren
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
        case filter isCrlEntry mft.content.mftEntries of
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
                        then InvalidChild e vs key filename Nothing
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
                    let caCert = case ro ^. #object . #payload of
                                    WellStructuredRO (CerRO _) -> Just InvalidCaChild
                                    _                          -> Nothing
                    pure $! case z of
                            Left e              -> InvalidChild e allVs key filename caCert
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
            InvalidChild _ vs key fileName caCert -> do
                embedState vs
                let !entry = makeChildWithIssues key fileName caCert
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
            pure $! makeChildWithIssues child.key filename Nothing
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
    vFocusOnLocated child $ case childRo of
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
            (validBgpCert, bgpPayload) <- validateBgpCert validationRFC now bgpCert fullCa.payload validCrl verifiedResources
            pure $! makeBgpSecShortcut childKey validBgpCert bgpPayload

        GbrRO gbr -> validLeaf $ do
            validGbr <- validateGbr validationRFC now gbr fullCa.payload validCrl verifiedResources
            pure $! makeGbrShortcut childKey validGbr (T2 (getHash gbr) gbr.content)

        -- Any new type of object should be added here, otherwise
        -- they will emit a warning.
        _somethingElse -> do
            logWarn logger [i|Unsupported type of object: #{locations}.|]
            entryFor Nothing Nothing

    where
        validationRFC = config.validationConfig.validationRFC

        -- What is stored for a child validated in full: its shortcut when it's
        -- valid and there are no issues in its scope, a troubled entry otherwise.
        -- A troubled child is validated in full again next time, so its issues
        -- are reported every time, and not only this once. A CA certificate 
        -- stays what it is either way, e.g. a valid one with a warning somewhere
        -- in its sub-tree is still a valid one.
        entryFor caCert valid = do
            issues <- thisScopeIssues
            pure $! case valid of
                Just shortcut | Set.null issues -> MftEntry fileName shortcut caCert
                _                               -> makeChildWithIssues childKey fileName caCert

        validCaChild childCert = do
            scopes <- askScopes
            {-
                Note that recursive validation of the child CA happens in the separate
                runValidator (...) call, it is to avoid short-circuit logic implemented by ExceptT:
                otherwise an error in child validation would interrupt validation of the parent with
                ExceptT's exception logic.

                The certificate itself and its sub-tree are validated separately: a 
                valid certificate stays valid even if there's something wrong with 
                its publication point.
            -}
            (certResult, certState) <- runValidator scopes $ do
                -- Check that AIA of the child points to the correct location of the parent
                -- https://mailarchive.ietf.org/arch/msg/sidrops/wRa88GHsJ8NMvfpuxXsT2_JXQSU/
                --
                validateAIA childCert fullCa

                void $ validateResourceCert now childCert fullCa validCrl
                validateResources (config ^. #validationConfig . typed)
                    verifiedResources childCert (fullCa ^. #payload)

            case certResult of
                Left _ -> do
                    embedState certState
                    entryFor (Just InvalidCaChild) Nothing

                Right (childVerifiedResources, overlclaiming) -> do
                    let childTopDownContext = topDownContext
                            & #verifiedResources ?~ childVerifiedResources
                            & #currentPathDepth %~ (+ 1)
                            & #overclaimingHappened .~ isJust overlclaiming

                    (r, subTreeState) <- runValidator scopes $
                        validateCa appContext childTopDownContext (CaFull (Located locations childCert))

                    embedState $ certState <> subTreeState
                    entryFor (Just ValidCaChild) =<< case r of
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
            entryFor Nothing =<< allowRevoked (do
                leaf <- validate
                acceptLeaf topDownContext FromObject leaf
                pure $ Just leaf)

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
        ChildLight _ _ -> do
            mfn <- roTxT database $ \tx ->
                        DB.getMftShortcutChildFileName tx childrenAki childKey
            case mfn of
                Just fn -> pure fn
                Nothing -> integrityError appContext
                    [i|Referential integrity error, can't find file_name for child #{childKey}.|]

    -- The stored entry of a child follows what validation found, 
    -- otherwise the shortcuts wouldn't describe the tree as it was validated.
    storeChildIfChanged childKey childData newEntry =
        unless (newEntry.child == childOf childData && newEntry.caCert == caCertOf childData) $
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
                        `catchError` \_cs (e :: AppError) -> do
                            -- The parent has fewer resources now (under the same key),
                            -- the child isn't valid anymore. Expiry doesn't need this,
                            -- it's checked against the validity period every time.
                            fileName <- childFileName childKey childData
                            storeChildIfChanged childKey childData
                                (makeChildWithIssues childKey fileName (Just InvalidCaChild))
                            appError e

                let childTopDownContext = topDownContext
                        & #currentPathDepth %~ (+ 1)
                        & #verifiedResources ?~ childVerifiedResources
                        & #overclaimingHappened .~ isJust overlclaiming

                validateCa appContext childTopDownContext (CaShort caShortcut)

            RoaChild r _    -> recheckLeaf r (validateRoaPrefixes verifiedResources r.roaPayload)
            SplChild s _    -> recheckLeaf s (validateSplAsn verifiedResources s.splPayload)
            AspaChild a _   -> recheckLeaf a (pure ())
            BgpSecChild b _ -> recheckLeaf b (validateBgpSecAsns verifiedResources b.bgpSec.bgpSecAsns)
            GbrChild g _    -> recheckLeaf g (pure ())

            TroubledChild childKey_ -> do
                increment topDownCounters.shortcutTroubled
                fileName <- childFileName childKey_ childData
                -- A troubled child can come out of re-validation clean, or a
                -- valid CA certificate can become an invalid one, e.g. revoked.
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
                    storeChildIfChanged key childData (makeChildWithIssues key fileName Nothing)
                    appError e


resolveTroubledChildByKey :: ValidatorIO es => Tx mode
                            -> ObjectKey
                            -> Eff es (Maybe (TroubledChildLoadPath, Keyed (Located WellStructuredRpkiObject)))
resolveTroubledChildByKey tx childKey =
    DB.getLocatedByKey tx childKey >>= \case
        Just (Located locations (WellStructuredRO vro)) ->
            pure $! Just (TroubledFromParsed, Keyed (Located locations vro) childKey)

        Just (Located locations (OriginalRO (ObjectOriginal blob) _ hash t)) -> 
            vFocusOn ObjectFocus childKey $ 
                -- Re-parsing a cached blob can raise a pure exception for a sufficiently 
                -- broken object. Turn it into a normal validation error so that it stays 
                -- contained to this child instead of failing the whole TA.
                fromTryM (\e -> 
                            parseErr $ "Failed to re-parse the cached object: " <> fmtEx e) $ do
                    validatedRo <- prevalidateObject =<< readObjectOfType t (Hashed blob hash)
                    pure $! Just (TroubledFromOriginal, Keyed (Located locations validatedRo) childKey)

        _ -> pure Nothing

getStoredObject :: ValidatorIO es => Tx mode
                    -> ObjectKey
                    -> Eff es (Maybe (Keyed (Located RpkiObjectLifecycle)))
getStoredObject tx key =
    fmap (`Keyed` key) <$> DB.getLocatedByKey tx key    

getFullCa :: ValidatorIO es => AppContext s -> TopDownContext -> Ca -> Eff es (Located WellStructuredCaCert)
getFullCa appContext@AppContext {..} TopDownContext {..} = \case    
    CaFull c -> pure c            
    CaShort CaShortcut {..} -> do   
        db <- liftIO $ readTVarIO database
        DB.roAppTx db $ \tx -> do 
            increment allTas.topDownCounters.readParsed
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
                          -- The last field is what the child is if it's a CA certificate
                          | InvalidChild e v ObjectKey Text (Maybe CaChildValidity)
                          | ValidEntry v ObjectKey (Maybe MftEntry)

-- | Which children of a manifest validated in full are validated in full as well:
-- only the ones that are not on the shortcut, or all of them.
data ChildrenToValidate = OnlyNewChildren | AllChildren

longerThan :: [a] -> Int -> Bool
longerThan xs n = not $ null $ drop n xs

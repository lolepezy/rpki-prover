{-# LANGUAGE StrictData #-}

module RPKI.Validation.BottomUp where

import           Effectful
import           Effectful.Error.Static             (throwError, tryError)
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Lens

import           Data.Foldable                      (for_)
import           Data.IORef
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                        (catMaybes)
import           Data.X509.Validation              (SignatureVerification (..))

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.Time
import           RPKI.Validation.Common
import           RPKI.Validation.Crypto
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.TopDown            (signedByCa)

{-
    Given an object
     - find a path up to a TA certificate
     - validate the chain and the given object

    The certificates are found by SKI among all the cached objects, wherever
    they were published, and anyone can publish a certificate with any SKI and
    AKI. So the paths found this way are only candidates: they can be cyclic,
    there can be any number of them and any of them can be invalid. They are
    tried one by one, depth-first, and the first valid one is the result. The
    search is bounded by the path depth and by the number of certificates it
    looks at.
-}
validateBottomUp :: ValidatorIO es =>
                AppContext s
                -> ParsedRpkiObject
                -> Now
                -> Eff es (Validated ParsedRpkiObject, [Located WellStructuredCaCert])
validateBottomUp
    AppContext{..}
    object
    now = do
    db <- liftIO $ readTVarIO database
    validObject <- vFocusOn TextFocus "rpki-object" $ prevalidateObject object
    case getAKI object of
        Nothing  -> vError NoAKI
        Just aki -> do
            taCerts <- getTaCerts db
            path    <- findValidPath db taCerts validObject aki
            pure (Validated object, path)
  where
    validationRFC = config ^. #validationConfig . #validationRFC
    maxDepth      = config ^. #validationConfig . #maxCertificatePathDepth

    findValidPath db taCerts validObject aki = do
        budget <- liftIO $ newIORef maxCertificatesToLookAt
        scopes <- askScopes
        let
            -- `path` is the part of the path found so far, from the last certificate found
            -- down to the issuer of the object, and `aki` is what the next one must have as SKI.
            climb path (AKI ki)
                | Just c <- List.find ((== SKI ki) . getSKI) path =
                    vError $ CircularReference $ HashIdentity $ getHash c
                | length path >= maxDepth =
                    vError $ CertificatePathTooDeep maxDepth
                | otherwise =
                    case Map.lookup (SKI ki) taCerts of
                        -- TA certificates are cached as any other certificate, but only
                        -- the ones from the TALs can end a path. The path is validated in
                        -- the scope where the search started, it has its own scopes.
                        Just tas ->
                            firstValid [ localScopes (const scopes) $
                                            validateTopDownAlongPath db validObject (ta : path)
                                       | ta <- issuersOf path tas ]
                        Nothing -> do
                            issuers <- issuersOf path <$> findCertificates (SKI ki)
                            firstValid [ vFocusOnLocated c $
                                            case getAKI c of
                                                Nothing   -> vError NoAKI
                                                Just aki' -> climb (c : path) aki'
                                       | c <- issuers ]

            -- Spend the budget on the candidates, it bounds the whole search.
            findCertificates ski = do
                left <- liftIO $ readIORef budget
                when (left <= 0) $
                    vError $ TreeIsTooBig maxCertificatesToLookAt
                candidates <- DB.roAppTx db $ \tx -> DB.getBySKIUpTo tx ski left
                liftIO $ writeIORef budget $ left - length candidates
                pure candidates

        climb [] aki

    -- Leave only the certificates that can have issued the last certificate found.
    -- All the certificates with the same SKI have the same key, so it's all or none
    -- of them, but it's cheap and stops a search that is going nowhere.
    issuersOf path candidates =
        case path of
            []        -> candidates
            child : _ -> filter (signed child) candidates
      where
        signed child parent =
            case validateSignMaterial (child ^. #payload) parent of
                SignaturePass     -> True
                SignatureFailed _ -> False

    getTaCerts db =
        DB.roAppTx db $ \tx -> do
            tas <- DB.getTAs tx
            fmap (Map.fromListWith (<>) . catMaybes) $ forM tas $ \StorableTA {..} -> do
                mcert <- DB.getTaCertByKey tx taCertKey
                pure $ fmap (\cert -> (getSKI cert, [Located (Just $ talCertLocations tal) cert])) mcert

    {- Given a chain of certificatees from a TA to the object,
       proceed with top-down validation along this chain only.
    -}
    validateTopDownAlongPath _ _ [] = vError ParentCertificateNotFound
    validateTopDownAlongPath db validObject certPath@(taCert : _) = do
        vFocusOnLocated taCert $ do
            for_ (getLocations taCert) $ \locs ->
                validateTaCertAKI taCert (pickLocation locs)
            void $ validateObjectValidityPeriod (taCert ^. #payload) now
        let verifiedResources = createVerifiedResources $ taCert ^. #payload
        go verifiedResources certPath
        pure certPath
      where
        go _ [] = pure ()

        go verifiedResources [bottomCert] = do
            vFocusOnLocated bottomCert $
                case validObject of
                    -- A manifest is not on a manifest and it is checked with the CRL
                    -- that is on it, the same as in the top-down validation
                    MftRO mft -> do
                        crl <- vFocusOn TextFocus "rpki-object" $ findMftCrl db bottomCert mft
                        validateObjectItself validObject bottomCert crl verifiedResources
                        notSuperseded db bottomCert verifiedResources mft

                    _ -> do
                        (mft, crl) <- validateManifest db bottomCert verifiedResources

                        -- RSC objects are not supposed to be on a manifest
                        case object of
                            RscRO _ -> pure ()
                            _       -> validateOnMft mft object

                        validateObjectItself validObject bottomCert crl verifiedResources

        go verifiedResources (cert : childCert : certs) = do
            vFocusOnLocated cert $ do
                (mft, crl) <- validateManifest db cert verifiedResources
                validateOnMft mft childCert
                Validated validCert <- validateResourceCert
                                                now
                                                (childCert ^. #payload)
                                                (cert ^. #payload)
                                                crl
                (childVerifiedResources, _) <- validateResources
                                                    validationRFC
                                                    (Just verifiedResources)
                                                    validCert
                                                    (cert ^. #payload)
                go childVerifiedResources (childCert : certs)

        validateOnMft mft o = do
            let mftChildren = mftEntries $ mft ^. #content
            case filter (\(MftPair _ h) -> h == getHash o) mftChildren of
                [] -> vError ObjectNotOnManifest
                _  -> pure ()


    validateObjectItself validObject bottomCert crl verifiedResources =
        vFocusOn TextFocus "rpki-object" $ do
            let parentCert = bottomCert ^. #payload
            case validObject of
                CerRO child -> do
                    void $ validateResourceCert now child parentCert crl
                    void $ validateResources validationRFC (Just verifiedResources) child parentCert
                MftRO mft ->
                    void $ validateMft validationRFC now mft parentCert crl (Just verifiedResources)
                RoaRO roa ->
                    void $ validateRoa validationRFC now roa parentCert crl (Just verifiedResources)
                SplRO spl ->
                    void $ validateSpl validationRFC now spl parentCert crl (Just verifiedResources)
                GbrRO gbr ->
                    void $ validateGbr validationRFC now gbr parentCert crl (Just verifiedResources)
                RscRO rsc ->
                    void $ validateRsc validationRFC now rsc parentCert crl (Just verifiedResources)
                AspaRO aspa ->
                    void $ validateAspa validationRFC now aspa parentCert crl (Just verifiedResources)
                BgpRO bgp -> do
                    void $ validateBgpCert now bgp parentCert crl
                    void $ validateResources validationRFC (Just verifiedResources) bgp parentCert
                CrlRO childCrl ->
                    void $ validateCrl now childCrl parentCert


    {- This resembles `tryMfts` from TopDown.hs but the difference is that
       we don't do any descent down the tree and don't track visited object
       or metrics.

       Manifests are found by AKI wherever they were published, so only the
       ones that the CA signed are tried (see `signedByCa`), newest first,
       until one of them is valid.
    -}
    validateManifest db certificate verifiedResources = do
        let childrenAki = toAKI $ getSKI certificate
        mftMetas <- DB.roAppTx db $ \tx -> DB.getMftsForAKI tx childrenAki
        nextSignedMft mftMetas >>= \case
            Nothing               -> vError $ NoMFT childrenAki
            Just (mft, mftMetas') -> tryFrom mft mftMetas'
      where
        tryFrom keyedMft mftMetas = do
            (z, vs) <- isolated $ validateOneMft keyedMft
            case z of
                Right r -> embedState vs >> pure r
                Left e  ->
                    nextSignedMft mftMetas >>= \case
                        -- The error of the last one is the error
                        Nothing -> embedState vs >> throwError e
                        Just (nextMft, mftMetas') -> do
                            let mftNumber = keyedMft ^. #object . #payload . #content . #mftNumber
                            vFocusOnLocated (keyedMft ^. #object) $ vWarn $ MftFallback e mftNumber
                            tryFrom nextMft mftMetas'

        nextSignedMft [] = pure Nothing
        nextSignedMft (m : ms) = do
            z <- DB.roAppTx db $ \tx -> DB.getMftByKey tx (m ^. #key)
            case z of
                Just mft | signedByCa certificate (mft ^. #object . #payload) ->
                    pure $ Just (mft, ms)
                _ ->
                    nextSignedMft ms

        validateOneMft (Keyed locatedMft@(Located _ mft) _) =
            vFocusOnLocated locatedMft $ do
                validateObjectLocations locatedMft
                validateMftLocation locatedMft certificate
                validCrl <- findMftCrl db certificate mft
                void $ validateMft validationRFC now mft (certificate ^. #payload) validCrl (Just verifiedResources)
                pure (mft, validCrl)

    -- The CRL that is on the manifest, validated
    findMftCrl db certificate mft = do
        let childrenAki = toAKI $ getSKI certificate
        MftPair _ crlHash <-
                case findCrlOnMft $ mft ^. #content of
                    []    -> vError $ NoCRLOnMFT childrenAki
                    [crl] -> pure crl
                    crls  -> vError $ MoreThanOneCRLOnMFT childrenAki crls

        crlObject <- DB.roAppTx db $ \tx -> DB.getByHash tx crlHash
        case crlObject of
            Nothing ->
                vError $ NoCRLExists childrenAki crlHash

            Just foundCrl@(Located _ (WellStructuredRO (CrlRO crl))) ->
                vFocusOnLocated foundCrl $ do
                    validateObjectLocations foundCrl
                    checkCrlLocation foundCrl $ eeCert mft
                    validateCrl now crl (certificate ^. #payload)

            Just _ ->
                vError $ CRLHashPointsToAnotherObject crlHash

    -- For a manifest, the analogue of being on the current manifest is to be
    -- the current manifest: there must be no newer valid manifest of the CA in
    -- the cache. If there's no valid one there at all, that is not a problem
    -- of this manifest, so the issues of the cached ones are not kept.
    notSuperseded db certificate verifiedResources mft = do
        (z, _) <- isolated $ validateManifest db certificate verifiedResources
        case z of
            Right (latest, _)
                | latestNumber > mftNumber ->
                    vFocusOn TextFocus "rpki-object" $
                        vError $ NewerManifestExists mftNumber latestNumber
              where
                mftNumber    = mft ^. #content . #mftNumber
                latestNumber = latest ^. #content . #mftNumber
            _ ->
                pure ()


-- | An upper bound for the number of certificates the search for a path looks at.
-- Real paths are a handful of certificates with one or two certificates per SKI,
-- so it is only reached when someone publishes certificates to make it big.
maxCertificatesToLookAt :: Int
maxCertificatesToLookAt = 1000


-- | The first of the alternatives that is valid. Only its issues are kept: the
-- others are dead ends of the search, not problems of the object. When none of
-- them is valid, the issues of all of them are kept and the error of the first
-- one is the error.
firstValid :: Validator es => [Eff es a] -> Eff es a
firstValid = go []
  where
    go failures = \case
        [] -> do
            let failures' = reverse failures
            for_ failures' $ embedState . snd
            case failures' of
                []         -> vError ParentCertificateNotFound
                (e, _) : _ -> throwError e
        alternative : alternatives -> do
            (z, vs) <- isolated alternative
            case z of
                Right r -> embedState vs >> pure r
                Left e  -> go ((e, vs) : failures) alternatives


-- | Run the validator and return its result and its issues
-- instead of adding them to the issues of the caller.
isolated :: Validator es => Eff es a -> Eff es (Either AppError a, ValidationState)
isolated f = do
    before <- getVState
    putVState mempty
    z <- tryError @AppError f
    vs <- getVState
    putVState before
    pure (either (Left . snd) Right z, vs)

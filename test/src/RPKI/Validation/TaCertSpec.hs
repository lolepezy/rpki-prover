module RPKI.Validation.TaCertSpec where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad                    (void)
import           Effectful                        (Eff)

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C8
import qualified Data.ByteString.Lazy             as LBS
import           Data.Generics.Product.Typed
import           Data.Hourglass
import           Data.IORef
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import           Data.Semigroup                   (sconcat)
import qualified Data.Text                        as Text

import qualified Network.HTTP.Types               as HTTP
import qualified Network.Wai                      as Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState                    (instantToVersion)
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Repository                  (publicationPointsFromTAL)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (hashed, parseRpkiURL)
import           RPKI.Validation.ObjectValidation
import           RPKI.Validation.TopDown.TaCert


{-
    RFC 10097, section 2 (replacing RFC 8630, section 3): the RP MUST

    1. Retrieve the TA certificate from (one of) the TAL URIs, if that fails
       use the cached copy.
    2. Check that it is a current, validly self-signed RPKI CA certificate,
       if not, use the cached copy.
    3. Check that its public key is the one from the TAL, if not, use the
       cached copy. If there is no cached copy, use the retrieved one.
    4. Use the one with the more recent notBefore.
    5. With equal notBefore, use the one with the shorter validity period.
    6. With equal validity periods, use the newly retrieved one.

    The certificates are in test/data/ta-tiebreak, see generate.sh there.
    All of them are of the same key, except for ta-other-key.cer.
-}
taCertGroup :: TestTree
taCertGroup =
    testGroup "TA certificate (RFC 10097)"
        [ testGroup "Validation of the retrieved certificate (steps 2 and 3)"
            [ HU.testCase "Accepts a current certificate with the TAL key" shouldAcceptValidTaCert
            , HU.testCase "Rejects an expired certificate" shouldRejectExpiredTaCert
            , HU.testCase "Rejects a certificate that is not valid yet" shouldRejectFutureTaCert
            , HU.testCase "Rejects a certificate with a key that is not in the TAL" shouldRejectOtherKey
            , HU.testCase "Rejects an object that is not a certificate" shouldRejectNonCertificate
            ]
        , testGroup "Tiebreak between the retrieved and the cached certificate (steps 4-6)"
            [ HU.testCase "Step 4: retrieved with more recent notBefore wins" shouldPreferMoreRecentNotBefore
            , HU.testCase "Step 4: cached with more recent notBefore wins" shouldKeepCachedWithMoreRecentNotBefore
            , HU.testCase "Step 5: retrieved with shorter validity wins" shouldPreferShorterValidity
            , HU.testCase "Step 5: cached with shorter validity wins" shouldKeepCachedWithShorterValidity
            , HU.testCase "Step 6: retrieved wins with equal validity" shouldPreferRetrievedWithEqualValidity
            , HU.testCase "Cached that is not valid yet doesn't win" shouldNotKeepFutureCachedCert
            , HU.testCase "Cached that is expired doesn't win" shouldNotKeepExpiredCachedCert
            ]
        , testGroup "Refreshing the TA certificate"
            [ HU.testCase "Step 1: stores the retrieved certificate when nothing is cached" shouldStoreFirstCert
            , HU.testCase "Step 1: keeps the cached copy when retrieval fails" shouldKeepCachedWhenRetrievalFails
            , HU.testCase "Step 1: fails when retrieval fails and nothing is cached" shouldFailWithoutCacheWhenRetrievalFails
            , HU.testCase "Step 2: keeps the cached copy when the retrieved one is expired" shouldKeepCachedInsteadOfExpired
            , HU.testCase "Step 2: keeps the cached copy when the retrieved one is not valid yet" shouldKeepCachedInsteadOfFuture
            , HU.testCase "Step 2: fails when the retrieved one is expired and nothing is cached" shouldFailWithoutCacheWhenExpired
            , HU.testCase "Step 2: uses the next URI when the first gives an expired certificate" shouldUseNextUriAfterExpired
            , HU.testCase "Step 3: keeps the cached copy when the key differs from the TAL" shouldKeepCachedInsteadOfOtherKey
            , HU.testCase "Step 3: uses the next URI when the first gives a wrong key" shouldUseNextUriAfterOtherKey
            , HU.testCase "Step 4: replaces the cached copy with a more recent one" shouldReplaceWithMoreRecent
            , HU.testCase "Step 4: keeps the cached copy over an older one" shouldKeepCachedOverOlder
            , HU.testCase "Step 5: replaces the cached copy with a shorter one" shouldReplaceWithShorter
            , HU.testCase "Step 6: replaces the cached copy with a reissued one" shouldReplaceWithReissued
            , HU.testCase "Reports no change when the same certificate is retrieved" shouldReportNoChangeForSameCert
            , HU.testCase "Replaces a cached copy that is not valid yet" shouldReplaceFutureCachedCopy
            ]
        ]


-- Validation of the retrieved certificate ------------------------------------------

shouldAcceptValidTaCert :: HU.Assertion
shouldAcceptValidTaCert = do
    base <- loadCert "ta-2020-2100"
    (r, _) <- runV $ validateTACert (talWith base [someUrl]) (inYear 2050) someUrl base.parsed
    case r of
        Right c -> HU.assertEqual "Must return the certificate" (getHash base.cert) (getHash c)
        Left e  -> HU.assertFailure $ "Expected the certificate to be valid, got " <> show e

shouldRejectExpiredTaCert :: HU.Assertion
shouldRejectExpiredTaCert = do
    base    <- loadCert "ta-2020-2100"
    expired <- loadCert "ta-2000-2010"
    (r, _) <- runV $ validateTACert (talWith base [someUrl]) (inYear 2050) someUrl expired.parsed
    expectError "ObjectIsExpired" r $ \case
        ValidationE (ObjectIsExpired _ _) -> True
        _                                 -> False

shouldRejectFutureTaCert :: HU.Assertion
shouldRejectFutureTaCert = do
    base   <- loadCert "ta-2020-2100"
    future <- loadCert "ta-2090-2100"
    (r, _) <- runV $ validateTACert (talWith base [someUrl]) (inYear 2050) someUrl future.parsed
    expectError "ObjectValidityIsInTheFuture" r $ \case
        ValidationE (ObjectValidityIsInTheFuture _ _) -> True
        _                                             -> False

shouldRejectOtherKey :: HU.Assertion
shouldRejectOtherKey = do
    base  <- loadCert "ta-2020-2100"
    other <- loadCert "ta-other-key"
    (r, _) <- runV $ validateTACert (talWith base [someUrl]) (inYear 2050) someUrl other.parsed
    expectError "SPKIMismatch" r $ \case
        ValidationE (SPKIMismatch _ _) -> True
        _                              -> False

shouldRejectNonCertificate :: HU.Assertion
shouldRejectNonCertificate = do
    base <- loadCert "ta-2020-2100"
    mft  <- parseFile "ripe-ncc-ta.mft"
    (r, _) <- runV $ validateTACert (talWith base [someUrl]) (inYear 2050) someUrl mft
    expectError "UnknownObjectAsTACert" r $ \case
        ValidationE UnknownObjectAsTACert -> True
        _                                 -> False


-- Tiebreak --------------------------------------------------------------------------

shouldPreferMoreRecentNotBefore :: HU.Assertion
shouldPreferMoreRecentNotBefore =
    expectChosen (inYear 2050) "ta-2021-2100" "ta-2020-2100" Retrieved

shouldKeepCachedWithMoreRecentNotBefore :: HU.Assertion
shouldKeepCachedWithMoreRecentNotBefore =
    expectChosen (inYear 2050) "ta-2020-2100" "ta-2021-2100" Cached

shouldPreferShorterValidity :: HU.Assertion
shouldPreferShorterValidity =
    expectChosen (inYear 2050) "ta-2020-2090" "ta-2020-2100" Retrieved

shouldKeepCachedWithShorterValidity :: HU.Assertion
shouldKeepCachedWithShorterValidity =
    expectChosen (inYear 2050) "ta-2020-2100" "ta-2020-2090" Cached

shouldPreferRetrievedWithEqualValidity :: HU.Assertion
shouldPreferRetrievedWithEqualValidity =
    expectChosen (inYear 2050) "ta-2020-2100-reissued" "ta-2020-2100" Retrieved

-- The cached one would win at step 4, but it is not valid yet
shouldNotKeepFutureCachedCert :: HU.Assertion
shouldNotKeepFutureCachedCert =
    expectChosen (inYear 2050) "ta-2020-2100" "ta-2090-2100" Retrieved

-- The cached one would win at step 5, but it has already expired
shouldNotKeepExpiredCachedCert :: HU.Assertion
shouldNotKeepExpiredCachedCert =
    expectChosen (inYear 2095) "ta-2020-2100" "ta-2020-2090" Retrieved


data Winner = Retrieved | Cached
    deriving (Show, Eq)

expectChosen :: Now -> String -> String -> Winner -> HU.Assertion
expectChosen now retrievedName cachedName winner = do
    retrieved <- loadCert retrievedName
    cached    <- loadCert cachedName
    (r, vs) <- runV $ chooseTaCert now retrieved.cert cached.cert
    case r of
        Left e  -> HU.assertFailure $ "chooseTaCert must not fail, got " <> show e
        Right c -> do
            let expected = case winner of
                    Retrieved -> retrieved
                    Cached    -> cached
            HU.assertEqual ("Must choose the " <> show winner <> " certificate")
                (getHash expected.cert) (getHash c)
            -- Choosing the cached copy must be visible to the operator
            HU.assertEqual "Must warn when the cached copy is chosen"
                (winner == Cached) (any isPreferCached $ allWarnings $ vs ^. typed)


-- Refreshing ------------------------------------------------------------------------

shouldStoreFirstCert :: HU.Assertion
shouldStoreFirstCert =
    withServer $ \env -> do
        base <- loadCert "ta-2020-2100"
        let tal = talWith base [urlOf env "ta.cer"]
        serve env [("ta.cer", base)]
        refresh env tal >>= HU.assertEqual "Must report a change" (Right True)
        void $ expectStored env "the retrieved certificate" base (urlOf env "ta.cer")

shouldKeepCachedWhenRetrievalFails :: HU.Assertion
shouldKeepCachedWhenRetrievalFails =
    withServer $ \env -> do
        base <- loadCert "ta-2020-2100"
        let tal = talWith base [urlOf env "ta.cer"]
        cacheVia env tal base
        serve env []
        refresh env tal >>= HU.assertEqual "Must use the cached copy" (Right False)
        void $ expectStored env "the cached certificate" base (urlOf env "ta.cer")

shouldFailWithoutCacheWhenRetrievalFails :: HU.Assertion
shouldFailWithoutCacheWhenRetrievalFails =
    withServer $ \env -> do
        base <- loadCert "ta-2020-2100"
        let tal = talWith base [urlOf env "ta.cer"]
        serve env []
        refresh env tal >>= expectLeft
        expectNothingStored env

shouldKeepCachedInsteadOfExpired :: HU.Assertion
shouldKeepCachedInsteadOfExpired =
    expectCachedKept "ta-2000-2010" $ \case
        ValidationE (ObjectIsExpired _ _) -> True
        _                                 -> False

-- This is the case that used to break the TA: the certificate that is not
-- valid yet has the more recent notBefore, so it won the tiebreak.
shouldKeepCachedInsteadOfFuture :: HU.Assertion
shouldKeepCachedInsteadOfFuture =
    expectCachedKept "ta-2090-2100" $ \case
        ValidationE (ObjectValidityIsInTheFuture _ _) -> True
        _                                             -> False

shouldKeepCachedInsteadOfOtherKey :: HU.Assertion
shouldKeepCachedInsteadOfOtherKey =
    expectCachedKept "ta-other-key" $ \case
        ValidationE (SPKIMismatch _ _) -> True
        _                              -> False

shouldFailWithoutCacheWhenExpired :: HU.Assertion
shouldFailWithoutCacheWhenExpired =
    withServer $ \env -> do
        base    <- loadCert "ta-2020-2100"
        expired <- loadCert "ta-2000-2010"
        let tal = talWith base [urlOf env "ta.cer"]
        serve env [("ta.cer", expired)]
        refresh env tal >>= expectLeft
        expectNothingStored env

shouldUseNextUriAfterExpired :: HU.Assertion
shouldUseNextUriAfterExpired =
    expectNextUriUsed "ta-2000-2010" $ \case
        ValidationE (ObjectIsExpired _ _) -> True
        _                                 -> False

shouldUseNextUriAfterOtherKey :: HU.Assertion
shouldUseNextUriAfterOtherKey =
    expectNextUriUsed "ta-other-key" $ \case
        ValidationE (SPKIMismatch _ _) -> True
        _                              -> False

shouldReplaceWithMoreRecent :: HU.Assertion
shouldReplaceWithMoreRecent = expectRefreshOutcome "ta-2020-2100" "ta-2021-2100" Retrieved

shouldKeepCachedOverOlder :: HU.Assertion
shouldKeepCachedOverOlder = expectRefreshOutcome "ta-2021-2100" "ta-2020-2100" Cached

shouldReplaceWithShorter :: HU.Assertion
shouldReplaceWithShorter = expectRefreshOutcome "ta-2020-2100" "ta-2020-2090" Retrieved

shouldReplaceWithReissued :: HU.Assertion
shouldReplaceWithReissued = expectRefreshOutcome "ta-2020-2100" "ta-2020-2100-reissued" Retrieved

shouldReportNoChangeForSameCert :: HU.Assertion
shouldReportNoChangeForSameCert =
    withServer $ \env -> do
        base <- loadCert "ta-2020-2100"
        let tal = talWith base [urlOf env "ta.cer"]
        cacheVia env tal base
        refresh env tal >>= HU.assertEqual "Must report no change" (Right False)
        void $ expectStored env "the same certificate" base (urlOf env "ta.cer")

-- An older version didn't check the validity period of the retrieved
-- certificate, so a certificate that is not valid yet could be in the cache.
shouldReplaceFutureCachedCopy :: HU.Assertion
shouldReplaceFutureCachedCopy =
    withServer $ \env -> do
        base   <- loadCert "ta-2020-2100"
        future <- loadCert "ta-2090-2100"
        let tal = talWith base [urlOf env "ta.cer"]
        storeDirectly env tal future (urlOf env "ta.cer")
        serve env [("ta.cer", base)]
        refresh env tal >>= HU.assertEqual "Must report a change" (Right True)
        void $ expectStored env "the retrieved certificate" base (urlOf env "ta.cer")


-- First cache `base`, then retrieve an invalid certificate:
-- the cached copy must stay, the problem must be reported.
expectCachedKept :: String -> (AppError -> Bool) -> HU.Assertion
expectCachedKept invalidName isExpected =
    withServer $ \env -> do
        base    <- loadCert "ta-2020-2100"
        invalid <- loadCert invalidName
        let tal = talWith base [urlOf env "ta.cer"]
        cacheVia env tal base
        serve env [("ta.cer", invalid)]
        refresh env tal >>= HU.assertEqual "Must use the cached copy" (Right False)
        (_, _, vs) <- expectStored env "the cached certificate" base (urlOf env "ta.cer")
        HU.assertBool ("Must report the problem with the retrieved certificate, got " <> show vs) $
            any isExpected (allWarnings vs <> allErrors vs)

-- The first URI gives an invalid certificate, the second a valid one:
-- the valid one must be used, and the first one is only a warning.
expectNextUriUsed :: String -> (AppError -> Bool) -> HU.Assertion
expectNextUriUsed invalidName isExpected =
    withServer $ \env -> do
        base    <- loadCert "ta-2020-2100"
        invalid <- loadCert invalidName
        let first  = urlOf env "a.cer"
        let second = urlOf env "b.cer"
        let tal = talWith base [first, second]
        serve env [("a.cer", invalid), ("b.cer", base)]
        refresh env tal >>= HU.assertEqual "Must report a change" (Right True)
        (_, _, vs) <- expectStored env "the certificate from the second URI" base second
        HU.assertBool ("Must try the first URI and warn about it, got " <> show vs) $
            any isExpected $ allWarnings vs
        HU.assertEqual "Must not report errors" [] (allErrors vs)

expectRefreshOutcome :: String -> String -> Winner -> HU.Assertion
expectRefreshOutcome cachedName retrievedName winner =
    withServer $ \env -> do
        cached    <- loadCert cachedName
        retrieved <- loadCert retrievedName
        let tal = talWith cached [urlOf env "ta.cer"]
        cacheVia env tal cached
        serve env [("ta.cer", retrieved)]
        refresh env tal >>= HU.assertEqual "Must report whether it changed" (Right $ winner == Retrieved)
        let expected = case winner of
                Retrieved -> retrieved
                Cached    -> cached
        (_, _, vs) <- expectStored env (show winner <> " certificate") expected (urlOf env "ta.cer")
        HU.assertEqual "Must not report errors" [] (allErrors vs)


-- Fixtures --------------------------------------------------------------------------

data CertFixture = CertFixture {
        bytes  :: BS.ByteString,
        parsed :: ParsedRpkiObject,
        cert   :: WellStructuredCaCert
    }

loadCert :: String -> IO CertFixture
loadCert name = do
    bytes  <- BS.readFile $ "test/data/ta-tiebreak/" <> name <> ".cer"
    parsed <- parseBytes (name <> ".cer") bytes
    case parsed of
        CerRO c -> pure CertFixture { cert = extractCert c, .. }
        _       -> HU.assertFailure $ name <> " is not a certificate"

parseFile :: String -> IO ParsedRpkiObject
parseFile name = BS.readFile ("test/data/" <> name) >>= parseBytes name

parseBytes :: String -> BS.ByteString -> IO ParsedRpkiObject
parseBytes name bytes =
    runV (readObject (rsyncUrl name) (hashed bytes)) >>= \case
        (Right o, _) -> pure o
        (Left e, _)  -> HU.assertFailure $ "Can't parse " <> name <> ": " <> show e

testTaName :: TaName
testTaName = TaName "rfc10097"

-- A TAL with the key of the given certificate
talWith :: CertFixture -> [RpkiURL] -> TAL
talWith fx urls = RFC_TAL {
        certificateLocations = sconcat $ NonEmpty.fromList $ map toLocations urls,
        publicKeyInfo        = unSPKI $ getSubjectPublicKeyInfo fx.cert,
        taName               = testTaName
    }

someUrl :: RpkiURL
someUrl = rsyncUrl "ta.cer"

rsyncUrl :: String -> RpkiURL
rsyncUrl name = either (error . show) id $ parseRpkiURL $ "rsync://rpki.example.net/repo/" <> Text.pack name

inYear :: Int -> Now
inYear year = Now $ newInstant $ DateTime (Date year January 1) (TimeOfDay 0 0 0 0)

runV :: Eff AppEffects a -> IO (Either AppError a, ValidationState)
runV = runValidatorIO (newScopes "ta-cert")

expectError :: Show a => String -> Either AppError a -> (AppError -> Bool) -> HU.Assertion
expectError name r isExpected =
    case r of
        Left e | isExpected e -> pure ()
        other                 -> HU.assertFailure $ "Expected " <> name <> ", got " <> show other

expectLeft :: Either AppError Bool -> HU.Assertion
expectLeft = \case
    Left _  -> pure ()
    Right c -> HU.assertFailure $ "Expected an error, got Right " <> show c

isPreferCached :: AppError -> Bool
isPreferCached = \case
    ValidationE (TACertPreferCachedCopy _) -> True
    _                                      -> False

allErrors :: Validations -> [AppError]
allErrors (Validations m) = [ e | issues <- Map.elems m, VErr e <- Set.toList issues ]

allWarnings :: Validations -> [AppError]
allWarnings (Validations m) = [ e | issues <- Map.elems m, VWarn (VWarning e) <- Set.toList issues ]


-- A local HTTP server for the TA certificates ---------------------------------------

data Env = Env {
        appContext :: AppContext,
        served     :: IORef (Map.Map BS.ByteString LBS.ByteString),
        port       :: Int
    }

withServer :: (Env -> IO a) -> IO a
withServer f =
    withTestContext $ \appContext -> do
        served <- newIORef mempty
        Warp.testWithApplication (pure $ app served) $ \port -> f Env {..}
  where
    app served req respond = do
        files <- readIORef served
        respond $ case Map.lookup (Wai.rawPathInfo req) files of
            Just body -> Wai.responseLBS HTTP.status200 [] body
            Nothing   -> Wai.responseLBS HTTP.status404 [] "not found"

-- Replace everything the server serves
serve :: Env -> [(String, CertFixture)] -> IO ()
serve env files =
    writeIORef env.served $ Map.fromList
        [ (C8.pack $ "/" <> path, LBS.fromStrict fx.bytes) | (path, fx) <- files ]

urlOf :: Env -> String -> RpkiURL
urlOf env path =
    either (error . show) id $ parseRpkiURL $ Text.pack $
        "http://127.0.0.1:" <> show env.port <> "/" <> path

refresh :: Env -> TAL -> IO (Either AppError Bool)
refresh env tal = do
    worldVersion <- instantToVersion . unNow <$> thisInstant
    refreshTaCertificate env.appContext tal worldVersion

-- Make the certificate the cached one the regular way, by retrieving it
cacheVia :: Env -> TAL -> CertFixture -> IO ()
cacheVia env tal fx = do
    serve env [("ta.cer", fx)]
    refresh env tal >>= HU.assertEqual "Must cache the certificate" (Right True)

-- Put the certificate in the cache bypassing any validation
storeDirectly :: Env -> TAL -> CertFixture -> RpkiURL -> IO ()
storeDirectly env tal fx url = do
    ppAccess <- either (HU.assertFailure . show) pure $ publicationPointsFromTAL tal fx.cert
    db <- readTVarIO $ env.appContext ^. #database
    worldVersion <- instantToVersion . unNow <$> thisInstant
    DB.rwTx db $ \tx -> do
        key <- DB.saveObject tx (WellStructuredRO (CerRO fx.cert)) worldVersion
        DB.saveTA tx $ StorableTA tal key ppAccess url

readStored :: Env -> IO (Maybe (WellStructuredCaCert, RpkiURL, Validations))
readStored env = do
    db <- readTVarIO $ env.appContext ^. #database
    DB.roTx db $ \tx ->
        DB.getTA tx testTaName >>= \case
            Nothing -> pure Nothing
            Just ta -> do
                cert <- DB.getTaCertByKey tx ta.taCertKey
                vs   <- DB.getTaValidations tx testTaName
                pure $ (, ta.actualUrl, vs) <$> cert

expectStored :: Env -> String -> CertFixture -> RpkiURL
            -> IO (WellStructuredCaCert, RpkiURL, Validations)
expectStored env what expected expectedUrl =
    readStored env >>= \case
        Nothing -> HU.assertFailure "No TA certificate in the cache"
        Just s@(cert, url, _) -> do
            HU.assertEqual ("Must store " <> what) (getHash expected.cert) (getHash cert)
            HU.assertEqual "Must store the URL it was retrieved from" expectedUrl url
            pure s

expectNothingStored :: Env -> HU.Assertion
expectNothingStored env =
    readStored env >>= \case
        Nothing -> pure ()
        Just (c, _, _) -> HU.assertFailure $ "Expected nothing in the cache, got " <> show (getHash c)

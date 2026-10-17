module RPKI.Validation.BottomUpSpec where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import qualified Crypto.PubKey.RSA                as RSA
import qualified Crypto.PubKey.RSA.PKCS15         as PKCS15
import           Crypto.Hash.Algorithms           (SHA256 (..))

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C8
import qualified Data.ByteString.Short            as BSS
import           Data.Generics.Product.Typed
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import qualified Data.X509                        as X509

import           System.Timeout                   (timeout)

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState                    (instantToVersion)
import           RPKI.AppTypes                    (WorldVersion)
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting
import           RPKI.Repository
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.TAL
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (hashed, parseRpkiURL, sha256s)
import           RPKI.Validation.BottomUp
import           RPKI.Validation.ObjectValidation (extractCert)


{-
    The RSC is a real one, but its CA is not in the cache. The certificates
    above it are made up, with the SKIs, AKIs and signatures that build the
    paths of each test, which is all that the search for a path looks at.
    There are no manifests, so no path is valid and the search has to end
    with an error, rather than with a vacuous success or never.
-}
bottomUpGroup :: TestTree
bottomUpGroup =
    testGroup "BottomUp validation"
        [ HU.testCase "Validates the path to the TA before accepting the object" shouldValidateThePath
        , HU.testCase "Stops at a certificate that is its own issuer" shouldStopAtSelfIssuedCert
        , HU.testCase "Stops at a TA certificate with an AKI" shouldStopAtTaCertWithAki
        , HU.testCase "Limits the depth of the path" shouldLimitPathDepth
        , HU.testCase "Limits the number of certificates looked at" shouldLimitCertificateCount
        ]


shouldValidateThePath :: HU.Assertion
shouldValidateThePath =
    withFixture $ \fx -> do
        ta <- newKey "ta"
        taCert <- storeTaCert fx ta Nothing
        storeCerts fx [issue fx ta (keyLike fx.rscAki) (Just $ toAKI ta.keyId) 1]

        -- It used to take the TA certificate found by SKI for a path of zero
        -- certificates, and accept the object without any validation
        bottomUp fx >>= \case
            (Left (ValidationE (NoMFT aki)), _) ->
                HU.assertEqual "Must fail on the manifest of the TA" (toAKI $ getSKI taCert) aki
            (other, _) ->
                HU.assertFailure $ "Expected NoMFT of the TA, got " <> show (void other)


shouldStopAtSelfIssuedCert :: HU.Assertion
shouldStopAtSelfIssuedCert =
    withFixture $ \fx -> do
        ta <- newKey "ta"
        void $ storeTaCert fx ta Nothing
        let subject = keyLike fx.rscAki
        storeCerts fx [issue fx subject subject (Just fx.rscAki) 1]

        bottomUp fx >>= \case
            (Left (ValidationE (CircularReference _)), _) -> pure ()
            (other, _) -> HU.assertFailure $ "Expected CircularReference, got " <> show (void other)


shouldStopAtTaCertWithAki :: HU.Assertion
shouldStopAtTaCertWithAki =
    withFixture $ \fx -> do
        -- RFC 6487 allows a self-signed certificate to have an AKI, the same as its SKI
        ta <- newKey "ta"
        void $ storeTaCert fx ta (Just $ toAKI ta.keyId)
        storeCerts fx [issue fx ta (keyLike fx.rscAki) (Just $ toAKI ta.keyId) 1]

        bottomUp fx >>= \case
            (Left (ValidationE (NoMFT _)), _) -> pure ()
            (other, _) -> HU.assertFailure $ "Expected NoMFT of the TA, got " <> show (void other)


shouldLimitPathDepth :: HU.Assertion
shouldLimitPathDepth =
    withFixture $ \fx -> do
        ta <- newKey "ta"
        void $ storeTaCert fx ta Nothing
        keys <- forM [1 .. 40 :: Int] $ \n -> newKey $ "level-" <> show n
        storeCerts fx $ chain fx ta keys 1

        bottomUp fx >>= \case
            (Left (ValidationE (CertificatePathTooDeep _)), _) -> pure ()
            (other, _) -> HU.assertFailure $ "Expected CertificatePathTooDeep, got " <> show (void other)


-- There are 4 ^ 11 paths with valid signatures, far too many to try them all
shouldLimitCertificateCount :: HU.Assertion
shouldLimitCertificateCount =
    withFixture $ \fx -> do
        ta <- newKey "ta"
        void $ storeTaCert fx ta Nothing
        keys <- forM [1 .. 10 :: Int] $ \n -> newKey $ "level-" <> show n
        storeCerts fx $ concat [ chain fx ta keys n | n <- [1 .. 4] ]

        bottomUp fx >>= \case
            (Left _, vs) ->
                HU.assertBool "Must stop because there are too many certificates" $
                    ValidationE (TreeIsTooBig maxCertificatesToLookAt) `elem` allErrors vs
            (Right _, _) ->
                HU.assertFailure "Expected an error"


data Fixture = Fixture {
        appContext :: AppContext,
        rsc        :: ParsedRpkiObject,
        rscAki     :: AKI,
        template   :: WellStructuredCaCert
    }

withFixture :: (Fixture -> IO a) -> IO a
withFixture f =
    withTestContext $ \appContext -> do
        rsc <- readFixture "checklist.sig"
        template <- readFixture "ripe-ncc-ta.cer" >>= \case
            CerRO c -> pure $ extractCert c
            _       -> HU.assertFailure "Expected a certificate"
        case getAKI rsc of
            Nothing     -> HU.assertFailure "The RSC must have an AKI"
            Just rscAki -> f Fixture {..}
  where
    readFixture name = do
        blob <- BS.readFile $ "test/data/" <> name
        runValidatorIO (newScopes "parse") (readObject (testUrl name) (hashed blob)) >>= \case
            (Right o, _) -> pure o
            (Left e, _)  -> HU.assertFailure $ "Can't parse " <> name <> ": " <> show e


-- Run the validation with a timeout, it used to never end for some of these paths
bottomUp :: Fixture -> IO (Either AppError [Located WellStructuredCaCert], ValidationState)
bottomUp fx = do
    now <- thisInstant
    z <- timeout (60 * 1_000_000) $
            runValidatorIO (newScopes "bottom-up") $
                snd <$> validateBottomUp fx.appContext fx.rsc now
    case z of
        Nothing -> HU.assertFailure "Bottom-up validation didn't finish in time"
        Just r  -> pure r


data TestKey = TestKey {
        keyId      :: SKI,
        publicKey  :: X509.PubKey,
        privateKey :: Maybe RSA.PrivateKey
    }

newKey :: String -> IO TestKey
newKey name = do
    (publicKey, privateKey) <- RSA.generate 128 65537
    let Hash h = sha256s $ C8.pack name
    pure TestKey {
            keyId      = SKI $ mkKI $ BS.take 20 $ BSS.fromShort h,
            publicKey  = X509.PubKeyRSA publicKey,
            privateKey = Just privateKey
        }

-- A key that is only an SKI, nothing can be signed with it
keyLike :: AKI -> TestKey
keyLike (AKI ki) = TestKey {
        keyId      = SKI ki,
        publicKey  = X509.PubKeyUnknown [] BS.empty,
        privateKey = Nothing
    }

-- The n-th certificates of every level of the path, from the one
-- with the SKI that is the AKI of the RSC, up to the TA
chain :: Fixture -> TestKey -> [TestKey] -> Int -> [WellStructuredCaCert]
chain fx ta keys n =
    zipWith3 (\subject issuer k -> issue fx issuer subject (Just $ toAKI issuer.keyId) k)
        (keyLike fx.rscAki : keys)
        (keys <> [ta])
        [n * 1000 ..]

-- A certificate of the subject key signed by the issuer key. Only what the search
-- for a path looks at is real, the rest is from the RIPE TA certificate.
issue :: Fixture -> TestKey -> TestKey -> Maybe AKI -> Int -> WellStructuredCaCert
issue fx issuer subject aki n =
    fx.template
        & #hash      .~ sha256s encoded
        & #ski       .~ subject.keyId
        & #aki       .~ aki
        & #pubKey    .~ subject.publicKey
        & #validity  .~ ValidityPeriod (Instant 0) (Instant maxBound)
        & #encoded   .~ BSS.toShort encoded
        & #signature .~ SignatureValue (BSS.toShort signature)
        & #sigAlg    .~ SignatureAlgorithmIdentifier (X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA)
  where
    encoded = C8.pack $ "certificate " <> show n
    signature =
        case issuer.privateKey of
            Nothing -> "not a signature"
            Just pk -> either (error . show) id $ PKCS15.sign Nothing (Just SHA256) pk encoded


storeTaCert :: Fixture -> TestKey -> Maybe AKI -> IO WellStructuredCaCert
storeTaCert fx ta aki = do
    let taCert = issue fx ta ta aki 0
    db <- readTVarIO $ fx.appContext ^. #database
    wv <- worldVersion
    DB.rwTx db $ \tx -> do
        key <- DB.saveObject tx (WellStructuredRO (CerRO taCert)) wv
        DB.saveTA tx $ StorableTA tal key ppAccess url
    pure taCert
  where
    url = testUrl "ta.cer"
    tal = RFC_TAL {
            certificateLocations = toLocations url,
            publicKeyInfo        = EncodedBase64 "",
            taName               = TaName "test"
        }
    ppAccess =
        case url of
            RsyncU u -> PublicationPointAccess (rsyncPP u :| [])
            RrdpU _  -> error "Must be an rsync URL"

storeCerts :: Fixture -> [WellStructuredCaCert] -> IO ()
storeCerts fx certs = do
    db <- readTVarIO $ fx.appContext ^. #database
    wv <- worldVersion
    DB.rwTx db $ \tx ->
        forM_ certs $ \c -> DB.saveObject tx (WellStructuredRO (CerRO c)) wv


worldVersion :: IO WorldVersion
worldVersion = instantToVersion . unNow <$> thisInstant

testUrl :: String -> RpkiURL
testUrl name =
    either (error . show) id $ parseRpkiURL $ "rsync://host/repo/" <> Text.pack name

allErrors :: ValidationState -> [AppError]
allErrors vs =
    let Validations m = vs ^. typed
    in [ e | issues <- Map.elems m, VErr e <- Set.toList issues ]

{-
    Objects for testing validation, made up with only what top-down validation
    looks at being real: keys, signatures, resources and the times. They have
    no locations, so the checks of locations don't apply.
-}
module RPKI.Validation.Fixtures where

import           Control.Concurrent.STM
import           Control.Lens hiding (children)
import           Control.Monad

import qualified Crypto.PubKey.RSA                as RSA
import qualified Crypto.PubKey.RSA.PKCS15         as PKCS15
import           Crypto.Hash.Algorithms           (SHA256 (..))

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as C8
import qualified Data.ByteString.Short            as BSS
import           Data.Foldable                    (toList)
import           Data.Generics.Product.Typed      (typed)
import           Data.IORef
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import qualified Data.X509                        as X509

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState                    (instantToVersion)
import           RPKI.Domain
import           RPKI.Parallel                    (ClosableQueue, closeCQueue, newCQueueIO, newWorkPool)
import           RPKI.Reporting
import           RPKI.Repository
import qualified RPKI.Resources.IntervalContainers as IS
import           RPKI.Resources.Resources         (readIp4)
import           RPKI.Resources.Types
import           RPKI.Store.AppSqliteStorage      (SqliteBackend)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types                 (RpkiObjectLifecycle (..))
import           RPKI.Time
import           RPKI.Util                        (parseRpkiURL, sha256s)
import           RPKI.Validation.Common           (createVerifiedResources)
import           RPKI.Validation.TopDown          (validateCa)
import           RPKI.Validation.TopDown.Context
import           RPKI.Validation.TopDown.Shortcuts (MftShortcutOp, storeShortcuts)
import           RPKI.Validation.Types            (Ca (..))


data Run = Run {
        vrps    :: Set VrpsPerAs,
        state   :: ValidationState,
        context :: TopDownContext
    }

-- | Validate the CA certificates one after another with the same context,
-- each one as if under a parent with all its resources, and give the
-- payloads of the ROAs that are valid. The shortcuts are written when 
-- it's done, as validation does it.
validateInOrder :: AppContext SqliteBackend -> Now -> [WellStructuredCaCert] -> IO Run
validateInOrder appContext now cas = do
    (context, shortcutQueue) <- newContext now
    states <- forM cas $ \ca ->
        fmap snd $ runValidatorIO (newScopes "in-order") $
            validateCa appContext
                (context & #verifiedResources ?~ createVerifiedResources ca
                         & #currentPathDepth .~ 1)
                (CaFull $ Located Nothing ca)
    atomically $ closeCQueue shortcutQueue
    storeShortcuts appContext shortcutQueue
    payloads <- readIORef $ context ^. #payloadBuilder . #vrps
    pure Run { vrps = Set.fromList [ p | T2 p _ <- payloads ], state = mconcat states, .. }

counter :: TopDownContext -> (TopDownCounters IORef -> IORef Int) -> IO Int
counter context c = readIORef $ c $ context ^. #allTas . #topDownCounters

issues :: ValidationState -> [ValidationError]
issues state = 
    let Validations byScope = state ^. typed 
    in toListOf (folded . folded . issueValidationErrors) byScope

validationMetric :: ValidationState -> ValidationMetric
validationMetric state = 
    mconcat $ toList $ unMetricMap $ state ^. #topDownMetric . #validationMetrics


-- | The context of validating a TA, with the repository of every CA fetched,
-- otherwise there's nothing to validate
newContext :: Now -> IO (TopDownContext, ClosableQueue MftShortcutOp)
newContext (Now now) = do
    shortcutQueue <- newCQueueIO 1000
    workPool      <- newWorkPool
    allTas <- newAllTasTopDownContext (instantToVersion now) fetched shortcutQueue mempty workPool
    (, shortcutQueue) <$> newTopDownContext (TaName "test") allTas
  where
    fetched = PublicationPoints (RrdpMap mempty) $
                toRsyncForest repositoryRoot (RepositoryMeta (FetchedAt now) Nothing) newRsyncForest
    repositoryRoot = case parseRpkiURL "rsync://host/repo/" of
                        Right (RsyncU u) -> u
                        other            -> error $ "Not an rsync URL: " <> show other


-- | Store the manifest of the key, its CRL and the children it lists
storePublicationPoint :: AppContext SqliteBackend -> Now -> TestKey -> Integer
                      -> [(Text, WellStructuredRpkiObject)] -> IO ()
storePublicationPoint appContext now key mftNumber children = do
    let theCrl  = makeCrl now key
        entries = ("ca.crl", theCrl.hash) : [ (name, objectHash o) | (name, o) <- children ]
        mft     = makeManifest now key mftNumber entries
    storeObjects appContext now (CrlRO theCrl : MftRO mft : map snd children)
  where
    objectHash = \case
        CerRO c -> c.hash
        RoaRO r -> r.hash
        MftRO m -> m.hash
        CrlRO c -> c.hash
        other   -> error $ "Not used in these tests: " <> show other


-- | CA certificates validated not as children of a manifest, so that 
-- they are known by their keys as well
storeCertificates :: AppContext SqliteBackend -> Now -> [WellStructuredCaCert] -> IO ()
storeCertificates appContext now = storeObjects appContext now . map CerRO

storeObjects :: AppContext SqliteBackend -> Now -> [WellStructuredRpkiObject] -> IO ()
storeObjects appContext now objects = do
    db <- readTVarIO $ appContext ^. #database
    DB.rwTx db $ \tx ->
        forM_ objects $ \o ->
            void $ DB.saveObject tx (WellStructuredRO o) (instantToVersion $ unNow now)


data TestKey = TestKey {
        name       :: String,
        keyId      :: SKI,
        publicKey  :: X509.PubKey,
        privateKey :: RSA.PrivateKey
    }

newKey :: String -> IO TestKey
newKey name = do
    (publicKey, privateKey) <- RSA.generate 128 65537
    let Hash h = sha256s $ C8.pack name
    pure TestKey {
            name       = name,
            keyId      = SKI $ mkKI $ BS.take 20 $ BSS.fromShort h,
            publicKey  = X509.PubKeyRSA publicKey,
            privateKey = privateKey
        }

sign :: TestKey -> BS.ByteString -> SignatureValue
sign key bytes =
    SignatureValue $ BSS.toShort $ either (error . show) id $
        PKCS15.sign Nothing (Just SHA256) key.privateKey bytes

sha256WithRSA :: SignatureAlgorithmIdentifier
sha256WithRSA = SignatureAlgorithmIdentifier (X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA)

-- | Every key has its own directory in the repository
publishedAt :: TestKey -> Text -> URI
publishedAt key file = URI $ "rsync://host/repo/" <> Text.pack key.name <> "/" <> file

-- | A CA certificate of the subject key, signed by the issuer key
makeCaCert :: Now -> TestKey -> TestKey -> Integer -> AllResources -> WellStructuredCaCert
makeCaCert now issuer subject serial resources = WellStructuredCert {
        hash      = sha256s encoded,
        ski       = subject.keyId,
        aki       = Just $ toAKI issuer.keyId,
        resources = resources,
        pubKey    = subject.publicKey,
        serial    = Serial serial,
        validity  = aroundNow now,
        certUris  = CertUris {
                aiaCaIssuersUri = Just $ publishedAt issuer "ca.cer",
                crlDPUri        = Just $ publishedAt issuer "ca.crl",
                repositoryUri   = Just $ publishedAt subject "",
                manifestUri     = Just $ publishedAt subject "ca.mft",
                rrdpNotifyUri   = Nothing
            },
        encoded   = BSS.toShort encoded,
        signature = sign issuer encoded,
        sigAlg    = sha256WithRSA
    }
  where
    encoded = C8.pack $ "certificate " <> issuer.name <> " " <> show serial

-- | The same CA certificate with the SIA of another publication point
movedTo :: Text -> WellStructuredCaCert -> WellStructuredCaCert
movedTo directory ca = ca 
    & #certUris . #repositoryUri ?~ URI ("rsync://host/repo/" <> directory <> "/")
    & #certUris . #manifestUri   ?~ URI ("rsync://host/repo/" <> directory <> "/ca.mft")

-- | An EE certificate of a signed object, signed by the issuer key
makeEeCert :: Now -> TestKey -> Integer -> AllResources -> WellStructuredEECert
makeEeCert now issuer serial resources = WellStructuredEECert {
        ski       = SKI $ mkKI $ BS.take 20 $ BSS.fromShort h,
        aki       = toAKI issuer.keyId,
        resources = resources,
        pubKey    = issuer.publicKey,
        serial    = Serial serial,
        validity  = aroundNow now,
        certUris  = CertUris {
                aiaCaIssuersUri = Just $ publishedAt issuer "ca.cer",
                crlDPUri        = Just $ publishedAt issuer "ca.crl",
                repositoryUri   = Nothing,
                manifestUri     = Nothing,
                rrdpNotifyUri   = Nothing
            },
        encoded   = BSS.toShort encoded,
        signature = sign issuer encoded,
        sigAlg    = sha256WithRSA
    }
  where
    encoded = C8.pack $ "EE certificate " <> issuer.name <> " " <> show serial
    Hash h  = sha256s encoded

-- | Only the EE certificate of a signed object is checked by top-down
-- validation, the CMS signature is checked when the object is parsed
makeSignedObject :: Now -> TestKey -> Integer -> AllResources -> a -> WellStructuredCms a
makeSignedObject now issuer serial resources content = WellStructuredCms {
        hash          = sha256s $ C8.pack $ "signed object " <> issuer.name <> " " <> show serial,
        content       = content,
        eeCert        = makeEeCert now issuer serial resources,
        signingTime   = unNow now,
        cmsSignature  = SignatureValue BSS.empty,
        signedAttrsBS = BSS.empty
    }

makeManifest :: Now -> TestKey -> Integer -> [(Text, Hash)] -> WellStructuredMft
makeManifest now issuer mftNumber entries =
    makeSignedObject now issuer (1000 + mftNumber) (AllResources Inherit Inherit Inherit)
        Manifest {
            mftNumber   = Serial mftNumber,
            fileHashAlg = X509.HashSHA256,
            thisTime    = momentAfter (unNow now) (-3600),
            nextTime    = momentAfter (unNow now) 86400,
            mftEntries  = [ MftPair name hash | (name, hash) <- entries ]
        }

makeRoa :: Now -> TestKey -> Integer -> ASN -> String -> WellStructuredRoa
makeRoa now issuer serial asn prefix =
    makeSignedObject now issuer serial (ipv4s [prefix]) $
        VrpsPerAs asn [Vrp4 (readIp4 prefix) (PrefixLength 24)] []

makeCrl :: Now -> TestKey -> CrlObject
makeCrl now issuer = CrlObject {
        hash    = sha256s encoded,
        aki     = toAKI issuer.keyId,
        signCrl = SignCRL {
            thisUpdateTime     = momentAfter (unNow now) (-3600),
            nextUpdateTime     = momentAfter (unNow now) 86400,
            signatureAlgorithm = sha256WithRSA,
            signatureValue     = sign issuer encoded,
            encodedValue       = BSS.toShort encoded,
            crlNumber          = Serial 1,
            revokedSerials     = mempty
        }
    }
  where
    encoded = C8.pack $ "CRL " <> issuer.name

ipv4s :: [String] -> AllResources
ipv4s prefixes = AllResources (RS $ IS.fromList $ map readIp4 prefixes) (RS IS.empty) (RS IS.empty)

aroundNow :: Now -> ValidityPeriod
aroundNow (Now now) = ValidityPeriod (momentAfter now (-86400)) (momentAfter now (365 * 86400))

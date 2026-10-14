module RPKI.Validation.SharedKeySpec where

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

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes                    (Count (..))
import           RPKI.AppState                    (instantToVersion)
import           RPKI.Config                      (ValidationAlgorithm (..))
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
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (parseRpkiURL, sha256s)
import           RPKI.Validation.Common           (createVerifiedResources)
import           RPKI.Validation.ObjectValidation (validateCrl, validateRoa)
import           RPKI.Validation.TopDown          (validateCa)
import           RPKI.Validation.TopDown.Context
import           RPKI.Validation.TopDown.Shortcuts (MftShortcutOp, storeShortcuts)
import           RPKI.Validation.Types            (Ca (..))


{-
    Nothing stops two CA certificates from having the same key, e.g. issued by
    different parents with different resources. Manifests, CRLs and the other
    children of a CA are found by its key, so they are children of all of these
    certificates, and valid under any one of them that has their resources.

    Whichever of them validation gets to first must not decide which of the 
    children are valid. Several certificates with one key also make many 
    paths to the same objects, see `claimKeyPass`. 

    The objects are made up here, with only what top-down validation looks at
    being real: keys, signatures, resources and the times. They have no
    locations, so the checks of locations don't apply.
-}
sharedKeyGroup :: TestTree
sharedKeyGroup =
    testGroup "Certificates with the same key"
        [ HU.testCase "Validates the children of a key under every certificate of the key"
            shouldValidateUnderEveryCertificate
        , HU.testCase "Uses the shortcut of a key only under what its children were validated under"
            shouldUseShortcutOnlyUnderItsContext
        , HU.testCase "Tells about the certificates with the same key"
            shouldReportCertificatesWithSameKey
        , HU.testCase "Validates the manifest of a key once under duplicates of a certificate"
            shouldValidateOnceUnderDuplicates
        , HU.testCase "Validates the manifest of every key once under levels of duplicates"
            shouldValidateOnceUnderLevelsOfDuplicates
        , HU.testCase "Counts every pass of a key towards the size of the tree"
            shouldCountEveryPassOfKey
        , HU.testCase "Stops at a cycle of keys"
            shouldStopAtCycle
        , HU.testCase "Checks a signature under a key once more at most"
            shouldCheckSignaturesOnce
        ]

validationWorkGroup :: TestTree
validationWorkGroup =
    testGroup "Work of validating a CA"
        [ HU.testCase "Checks what doesn't need cryptography before signatures"
            shouldCheckBeforeSignatures
        , HU.testCase "Falls back from so many manifests at most"
            shouldLimitManifestFallbacks
        ]


-- | Each of the certificates has the resources of one of the ROAs.
-- Both ROAs are valid, whichever of the certificates comes first.
shouldValidateUnderEveryCertificate :: HU.Assertion
shouldValidateUnderEveryCertificate =
    withTestContext $ \appContext -> do
        TwoCertificates {..} <- twoCertificates appContext
        forM_ [[ca1, ca2], [ca2, ca1]] $ \order -> do
            Run {..} <- validateInOrder appContext now order
            HU.assertEqual "Both ROAs must be valid"
                (Set.fromList [roa1.content, roa2.content]) vrps


-- | The shortcut of a key is written under the certificate validated first, 
-- and the next time it can be another one that comes first, with other 
-- resources. Then the shortcut is not used, otherwise the ROA of the other
-- certificate would be valid without its resources.
shouldUseShortcutOnlyUnderItsContext :: HU.Assertion
shouldUseShortcutOnlyUnderItsContext =
    withTestContext $ \appContext_ -> do
        let appContext = appContext_ & #config . #validationConfig . #validationAlgorithm .~ Incremental
        TwoCertificates {..} <- twoCertificates appContext
        let validate cas expected message = do
                run <- validateInOrder appContext now cas
                HU.assertEqual message (Set.fromList $ map (.content) expected) run.vrps
                pure run

        void $ validate [ca1, ca2] [roa1, roa2] "Both, the shortcut is written under the first one"
        void $ validate [ca2, ca1] [roa1, roa2] "Both, with the shortcut of the other one"
        void $ validate [ca1] [roa1] "Only the first one, with the shortcut of the other one"
        Run {..} <- validate [ca1] [roa1] "Only the first one, with its own shortcut"
        counter context (.shortcutMft) >>= HU.assertEqual "The shortcut is used" 1
        void $ validate [ca2] [roa2] "Only the other one"


-- | Every certificate with the key but the first one is told about, with the 
-- others, and they are counted.
shouldReportCertificatesWithSameKey :: HU.Assertion
shouldReportCertificatesWithSameKey =
    withTestContext $ \appContext -> do
        TwoCertificates {..} <- twoCertificates appContext
        Run {..} <- validateInOrder appContext now [ca1, ca2]
        HU.assertEqual "The second certificate is told about"
            [CertificatesWithSameKey shared.keyId 1 []] 
            [ e | e@CertificatesWithSameKey {} <- issues state ]
        HU.assertEqual "Both of them are counted" 
            (Count 2) (validationMetric state).sharedKeyCertNumber
        HU.assertEqual "There's no other warning" 
            [] [ e | e@MftAlreadyValidated {} <- issues state ]


-- | Certificates with the key that are the same but for e.g. the serial number,
-- and that are on the manifests of several CAs, or on one manifest many times:
-- nothing can be valid under one of them that isn't under another one.
shouldValidateOnceUnderDuplicates :: HU.Assertion
shouldValidateOnceUnderDuplicates =
    withTestContext $ \appContext -> do
        now    <- thisInstant
        issuer <- newKey "issuer"
        shared <- newKey "shared"
        let duplicates = [ makeCaCert now issuer shared n (ipv4s ["10.0.1.0/24"]) | n <- [1 .. 8] ]
            roa = makeRoa now shared 10 (ASN 1) "10.0.1.0/24"
        storeCertificates appContext now duplicates
        storePublicationPoint appContext now shared 1 [("r10.roa", RoaRO roa)]

        Run {..} <- validateInOrder appContext now duplicates
        HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) vrps
        counter context (.originalMft) >>= HU.assertEqual "The manifest is validated once" 1
        counter context (.anotherKeyPass) >>= HU.assertEqual "Under one certificate" 0
        HU.assertEqual "All of them are counted" 
            (Count 8) (validationMetric state).sharedKeyCertNumber


-- | Every key has several certificates, issued by the key before it, so that 
-- there are (certificates per key) ^ (number of keys) paths to the last key. 
-- The manifest of every key is validated once.
shouldValidateOnceUnderLevelsOfDuplicates :: HU.Assertion
shouldValidateOnceUnderLevelsOfDuplicates =
    withTestContext $ \appContext -> do
        now  <- thisInstant
        keys@(first : _) <- forM [0 .. levelCount] $ \n -> newKey $ "level-" <> show n
        let resources = ipv4s ["10.0.0.0/16"]
            duplicatesOf issuer subject = 
                [ makeCaCert now issuer subject n resources | n <- [1 .. perKey] ]
            roa = makeRoa now (last keys) 10 (ASN 1) "10.0.1.0/24"
        top <- newKey "top"
        forM_ (zip3 [1 ..] keys (drop 1 keys)) $ \(n, issuer, subject) ->
            storePublicationPoint appContext now issuer n
                [ (Text.pack $ "c" <> show k <> ".cer", CerRO c) 
                | (k, c) <- zip [1 :: Int ..] (duplicatesOf issuer subject) ]
        storePublicationPoint appContext now (last keys) 1000 [("r10.roa", RoaRO roa)]

        let tops = duplicatesOf top first
        storeCertificates appContext now tops
        Run {..} <- validateInOrder appContext now tops
        HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) vrps
        counter context (.originalMft) >>= HU.assertEqual 
            "The manifest of every key is validated once" (levelCount + 1)
  where
    levelCount = 6 :: Int
    perKey = 4 :: Integer


-- | Validating a manifest under one more certificate with its key looks at
-- the same objects once more, it counts as much as the first time.
shouldCountEveryPassOfKey :: HU.Assertion
shouldCountEveryPassOfKey =
    withTestContext $ \appContext -> do
        TwoCertificates {..} <- twoCertificates appContext
        once  <- validateInOrder appContext now [ca1]
        twice <- validateInOrder appContext now [ca1, ca2]
        onceSize  <- readTVarIO $ once.context ^. #treeSize
        twiceSize <- readTVarIO $ twice.context ^. #treeSize
        HU.assertEqual "Twice as much" (2 * onceSize) twiceSize


-- | The key of a CA has a certificate issued by its child.
-- The second one has the same location of the manifest, or another one, and 
-- then the manifest is validated under it once more.
shouldStopAtCycle :: HU.Assertion
shouldStopAtCycle =
    forM_ [(id, 0), (movedTo "elsewhere", 1)] $ \(moved, anotherPass) -> 
        withTestContext $ \appContext -> do
            now    <- thisInstant
            root   <- newKey "root"
            parent <- newKey "parent"
            child  <- newKey "child"
            let resources = ipv4s ["10.0.0.0/16"]
                parentCa = makeCaCert now root parent 1 resources
                childCa  = makeCaCert now parent child 2 resources
                -- The key of the parent, issued by the child
                cycleCa  = moved $ makeCaCert now child parent 3 resources
                roa = makeRoa now parent 10 (ASN 1) "10.0.1.0/24"
            storeCertificates appContext now [parentCa]
            storePublicationPoint appContext now parent 1 
                [("child.cer", CerRO childCa), ("r10.roa", RoaRO roa)]
            storePublicationPoint appContext now child 2 [("cycle.cer", CerRO cycleCa)]

            Run {..} <- validateInOrder appContext now [parentCa]
            HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) vrps
            counter context (.anotherKeyPass) >>= HU.assertEqual "Passes of the key" anotherPass


-- | A signature doesn't depend on which certificate with the key the object is 
-- validated under. It's checked the first time and once more, and known after. 
shouldCheckSignaturesOnce :: HU.Assertion
shouldCheckSignaturesOnce =
    withTestContext $ \appContext -> do
        now     <- thisInstant
        shared  <- newKey "shared"
        issuers <- forM [1 .. 3 :: Int] $ \n -> newKey $ "issuer-" <> show n
        -- Each one with an SIA of its own, so each one is a pass of its own
        let cas = [ movedTo (Text.pack $ "pp-" <> show n) $ 
                        makeCaCert now issuer shared n (ipv4s ["10.0.0.0/16"])
                  | (n, issuer) <- zip [1 ..] issuers ]
            roa1 = makeRoa now shared 10 (ASN 1) "10.0.1.0/24"
            roa2 = makeRoa now shared 11 (ASN 2) "10.0.2.0/24"
        storeCertificates appContext now cas
        storePublicationPoint appContext now shared 1
            [("r10.roa", RoaRO roa1), ("r11.roa", RoaRO roa2)]

        Run {..} <- validateInOrder appContext now cas
        HU.assertEqual "Both ROAs are valid" (Set.fromList [roa1.content, roa2.content]) vrps
        counter context (.anotherKeyPass) >>= HU.assertEqual "Three passes" 2
        counter context (.rememberedSignature) >>= HU.assertEqual 
            "The manifest, the CRL and the ROAs are known in the third one" 4


-- | An object that is not valid for other reasons doesn't cost a signature
-- check, the signature is checked last. Here the signatures are not valid.
shouldCheckBeforeSignatures :: HU.Assertion
shouldCheckBeforeSignatures = do
    now   <- thisInstant
    key   <- newKey "ca"
    other <- newKey "other"
    let ca  = makeCaCert now other key 1 (ipv4s ["10.0.0.0/16"])
        crl = makeCrl now key & #signCrl . #revokedSerials .~ Set.singleton (Serial 11)
        forged :: WellStructuredRoa -> WellStructuredRoa
        forged roa = roa & #eeCert . #signature .~ sign other (BSS.fromShort roa.eeCert.encoded)
        validate roa = fst <$> runValidatorIO (newScopes "order")
                        (validateRoa StrictRFC now roa ca (Validated crl) 
                            (Just $ createVerifiedResources ca))

    validate (forged $ makeRoa now key 10 (ASN 1) "10.0.1.0/24") >>= \case
        Left (ValidationE (InvalidSignature _)) -> pure ()
        other_ -> HU.assertFailure $ "The signature is not valid: " <> show other_
    validate (forged $ makeRoa now key 12 (ASN 1) "10.1.0.0/24") >>= \case
        Left (ValidationE (OverclaimedResources _)) -> pure ()
        other_ -> HU.assertFailure $ "Resources go before the signature: " <> show other_
    validate (forged $ makeRoa now key 11 (ASN 1) "10.0.1.0/24") >>= 
        HU.assertEqual "Revocation goes before the signature" 
            (Left $ ValidationE RevokedResourceCertificate)

    let expired = makeCrl now key 
                    & #signCrl . #nextUpdateTime .~ momentAfter (unNow now) (-60)
                    & #signCrl . #signatureValue .~ sign other "CRL ca"
    fst <$> runValidatorIO (newScopes "crl") (validateCrl now expired ca) >>= \case
        Left (ValidationE NextUpdateTimeIsInThePast {}) -> pure ()
        other_ -> HU.assertFailure $ "The times go before the signature: " <> show other_


-- | The key of a CA can sign any number of manifests that are not valid, 
-- and every one of them that is tried validates its children
shouldLimitManifestFallbacks :: HU.Assertion
shouldLimitManifestFallbacks =
    withTestContext $ \appContext -> do
        now    <- thisInstant
        issuer <- newKey "issuer"
        key    <- newKey "key"
        let ca     = makeCaCert now issuer key 1 (ipv4s ["10.0.0.0/16"])
            crl    = makeCrl now key
            broken n = makeManifest now key n 
                        [("ca.crl", crl.hash), ("missing.roa", sha256s "not in the cache")]
        storeCertificates appContext now [ca]
        storeObjects appContext now $ CrlRO crl : [ MftRO (broken n) | n <- [1 .. 15] ]

        Run {..} <- validateInOrder appContext now [ca]
        let maxFallbacks = appContext ^. #config . #validationConfig . #maxManifestFallbacks
        HU.assertEqual "Falls back as many times as it's allowed" 
            maxFallbacks (length [ () | MftFallback {} <- issues state ])
        HU.assertEqual "Tells about it" 
            [TooManyManifestFallbacks maxFallbacks] [ e | e@TooManyManifestFallbacks {} <- issues state ]


data TwoCertificates = TwoCertificates {
        now     :: Now,
        shared  :: TestKey,
        ca1     :: WellStructuredCaCert,
        ca2     :: WellStructuredCaCert,
        roa1    :: WellStructuredRoa,
        roa2    :: WellStructuredRoa
    }

-- | Two certificates with one key issued by two other keys, each 
-- with the resources of one of the ROAs of the key
twoCertificates :: AppContext SqliteBackend -> IO TwoCertificates
twoCertificates appContext = do
    now     <- thisInstant
    issuer1 <- newKey "issuer-1"
    issuer2 <- newKey "issuer-2"
    shared  <- newKey "shared"
    let ca1  = makeCaCert now issuer1 shared 1 (ipv4s ["10.0.1.0/24"])
        ca2  = makeCaCert now issuer2 shared 2 (ipv4s ["10.0.2.0/24"])
        roa1 = makeRoa now shared 10 (ASN 1) "10.0.1.0/24"
        roa2 = makeRoa now shared 11 (ASN 2) "10.0.2.0/24"
    storeCertificates appContext now [ca1, ca2]
    storePublicationPoint appContext now shared 1
        [("r10.roa", RoaRO roa1), ("r11.roa", RoaRO roa2)]
    pure TwoCertificates {..}


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
        fmap snd $ runValidatorIO (newScopes "shared-key") $
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

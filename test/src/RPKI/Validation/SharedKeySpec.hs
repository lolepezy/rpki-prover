module RPKI.Validation.SharedKeySpec where

import           Control.Lens hiding (children)
import           Control.Monad

import qualified Data.Set                         as Set
import qualified Data.Text                        as Text

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppContext
import           RPKI.AppTypes                    (Count (..))
import           RPKI.Config                      (ValidationAlgorithm (..))
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Resources.Types
import           RPKI.Store.AppSqliteStorage      (SqliteBackend)
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Validation.Fixtures
import           RPKI.Validation.TopDown.Context


{-
    Nothing stops two CA certificates from having the same key, e.g. issued by
    different parents with different resources. Manifests, CRLs and the other
    children of a CA are found by its key, so they are children of all of these
    certificates, and valid under any one of them that has their resources.

    Whichever of them validation gets to first must not decide which of the 
    children are valid. But anyone can issue a certificate with the key of 
    anyone else, with their own resources, as many of them as they like. So 
    the children are validated under the ones that have resources the children 
    need, and only under so many of them, see `claimKeyPass`.
-}
sharedKeyGroup :: TestTree
sharedKeyGroup =
    testGroup "Certificates with the same key"
        [ HU.testCase "Validates the children of a key under every certificate of the key with resources they need"
            shouldValidateUnderEveryCertificate
        , HU.testCase "Uses the shortcut of a key only under what its children were validated under"
            shouldUseShortcutOnlyUnderItsContext
        , HU.testCase "Tells about the certificates with the same key"
            shouldReportCertificatesWithSameKey
        , HU.testCase "Validates the manifest of a key once under duplicates of a certificate"
            shouldValidateOnceUnderDuplicates
        , HU.testCase "Validates the manifest of every key once under levels of duplicates"
            shouldValidateOnceUnderLevelsOfDuplicates
        , HU.testCase "Validates the manifest of every key under so many certificates at most"
            shouldValidateUnderSoManyCertificates
        , HU.testCase "Doesn't validate the children of a key under a certificate without resources they need"
            shouldSkipCertificateWithoutResourcesOfChildren
        , HU.testCase "Copies of a certificate with other resources don't take the size of the tree"
            shouldNotCountCopies
        , HU.testCase "Checks the first certificate of a key that is not what the key was validated under"
            shouldCheckFirstCertificateNotAsBefore
        , HU.testCase "Checks the first certificate of a key that had more than one"
            shouldCheckFirstCertificateOfSharedKey
        , HU.testCase "Looks for what the children of a key need through CA certificates that inherit"
            shouldLookThroughInheritingCertificates
        , HU.testCase "Stops at a cycle of keys"
            shouldStopAtCycle
        , HU.testCase "Checks a signature under a key once more at most"
            shouldCheckSignaturesOnce
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


-- | The same, but the certificates of a key have different resources, each
-- within those of every certificate of the key before it, and they have 
-- what the children of the key need. So the manifest of every key is validated
-- under every one of its certificates, but only under so many of them, and 
-- that's once for each of them: what it's validated under doesn't depend on 
-- which certificate of the key before it the certificate is validated under.
shouldValidateUnderSoManyCertificates :: HU.Assertion
shouldValidateUnderSoManyCertificates =
    withTestContext $ \appContext -> do
        now  <- thisInstant
        keys@(first : _) <- forM [0 .. levelCount] $ \n -> newKey $ "level-" <> show n
        let certificatesOf level issuer subject = 
                [ makeCaCert now issuer subject (fromIntegral k) (resources level k) | k <- [1 .. perKey] ]
            roa = makeRoa now (last keys) 10 (ASN 1) "10.0.0.0/24"
        top <- newKey "top"
        forM_ (zip3 [1 ..] keys (drop 1 keys)) $ \(level, issuer, subject) ->
            storePublicationPoint appContext now issuer (fromIntegral level)
                [ (Text.pack $ "c" <> show k <> ".cer", CerRO c) 
                | (k, c) <- zip [1 :: Int ..] (certificatesOf level issuer subject) ]
        storePublicationPoint appContext now (last keys) 1000 [("r10.roa", RoaRO roa)]

        let tops = certificatesOf 0 top first
        storeCertificates appContext now tops
        Run {..} <- validateInOrder appContext now tops
        let maxPasses = appContext ^. #config . #validationConfig . #maxCertificatesPerKey
        HU.assertBool "There are more certificates of a key" (maxPasses < perKey)
        HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) vrps
        counter context (.originalMft) >>= HU.assertEqual 
            "The manifest of every key is validated under so many certificates of it" 
            ((levelCount + 1) * maxPasses)
        HU.assertBool "The other ones are told about" $ 
            not $ null [ () | TooManyCertificatesOfKey {} <- issues state ]
  where
    levelCount = 6 :: Int
    perKey = 4 :: Int
    -- 10.0.0.0/17 at the first level, half of it at every next one, 
    -- and some of the other half of the level before, different for 
    -- every certificate
    resources level k = ipv4s [ prefix 0 (17 + level), 
                                prefix (2 ^ (15 - level) + 16 * k) 28 ]
    prefix offset len = 
        "10.0." <> show (offset `div` 256 :: Int) <> "." <> show (offset `mod` 256) <> "/" <> show (len :: Int)


-- | Copies of the certificate of a key, issued by someone else with their 
-- resources. Nothing the key has can be valid under them, so its manifest is 
-- not validated under them. Unless one comes first: then there's nothing to 
-- tell it's not the one, it is validated under it once.
shouldSkipCertificateWithoutResourcesOfChildren :: HU.Assertion
shouldSkipCertificateWithoutResourcesOfChildren =
    withTestContext $ \appContext -> do
        Victim {..} <- victimWithCopies appContext 3
        forM_ [(real : copies, 1), (copies <> [real], 2)] $ \(order, passes) -> do
            Run {..} <- validateInOrder appContext now order
            HU.assertEqual "The ROAs are valid" (Set.fromList $ map (.content) roas) vrps
            counter context (.originalMft) >>= HU.assertEqual "Validated under so many" passes
            HU.assertEqual "The copies that are not validated under are told about" 
                (replicate (4 - passes) $ CertificateCoversNothingOfKey key.keyId) 
                [ e | e@CertificateCoversNothingOfKey {} <- issues state ]


-- | Copies of a certificate with many children don't take more of the size of 
-- the tree than the copies themselves: the size is for the whole TA, and when 
-- it's reached the rest of it is not validated.
shouldNotCountCopies :: HU.Assertion
shouldNotCountCopies =
    withTestContext $ \appContext_ -> do
        let appContext = appContext_ & #config . #validationConfig . #maxTotalTreeSize .~ 100
        Victim {..} <- victimWithCopies appContext 6
        other  <- newKey "other"
        issuer <- newKey "other-issuer"
        let otherCa  = makeCaCert now issuer other 100 (ipv4s ["10.100.0.0/16"])
            otherRoa = makeRoa now other 100 (ASN 100) "10.100.0.0/24"
        storeCertificates appContext now [otherCa]
        storePublicationPoint appContext now other 1 [("r100.roa", RoaRO otherRoa)]

        Run {..} <- validateInOrder appContext now (copies <> [real, otherCa])
        HU.assertEqual "All the ROAs are valid" 
            (Set.fromList $ map (.content) (otherRoa : roas)) vrps
        HU.assertEqual "The size of the tree is not reached" 
            [] [ e | e@TreeIsTooBig {} <- issues state ]


-- | The key was validated under its certificate before. The first certificate 
-- with it now is a copy with other resources, it's checked as one after another.
shouldCheckFirstCertificateNotAsBefore :: HU.Assertion
shouldCheckFirstCertificateNotAsBefore =
    withTestContext $ \appContext_ -> do
        let appContext = appContext_ & #config . #validationConfig . #validationAlgorithm .~ Incremental
        Victim {..} <- victimWithCopies appContext 1
        void $ validateInOrder appContext now [real]
        Run {..} <- validateInOrder appContext now (copies <> [real])
        HU.assertEqual "The ROAs are valid" (Set.fromList $ map (.content) roas) vrps
        counter context (.skippedKeyPass) >>= HU.assertEqual "Not under the copy" 1
        counter context (.shortcutMft) >>= HU.assertEqual "Under the certificate as before" 1
        counter context (.originalRoa) >>= HU.assertEqual "Nothing is validated in full" 0


-- | The first time, the copy comes first and nothing tells it's a copy, the 
-- shortcut of the key is written under it. The next time it's known that 
-- the key had two certificates, the copy is checked and the certificate is
-- the one to validate the key under first, and to write the shortcut.
-- The certificates are children of manifests here, that's what tells the
-- key had two of them.
shouldCheckFirstCertificateOfSharedKey :: HU.Assertion
shouldCheckFirstCertificateOfSharedKey =
    withTestContext $ \appContext_ -> do
        let appContext = appContext_ & #config . #validationConfig . #validationAlgorithm .~ Incremental
        now      <- thisInstant
        top      <- newKey "top"
        parent   <- newKey "parent"
        copier   <- newKey "copier"
        key      <- newKey "key"
        let parentCa   = makeCaCert now top parent 1 (ipv4s ["10.0.0.0/16"])
            copierCa   = makeCaCert now top copier 2 (ipv4s ["10.1.0.0/16"])
            real = makeCaCert now parent key 3 (ipv4s ["10.0.0.0/24"])
            copy = makeCaCert now copier key 4 (ipv4s ["10.1.0.0/24"])
            roa  = makeRoa now key 10 (ASN 1) "10.0.0.0/24"
        storeCertificates appContext now [parentCa, copierCa]
        storePublicationPoint appContext now parent 1 [("real.cer", CerRO real)]
        storePublicationPoint appContext now copier 1 [("copy.cer", CerRO copy)]
        storePublicationPoint appContext now key 1 [("r10.roa", RoaRO roa)]

        let validate = validateInOrder appContext now [copierCa, parentCa]
        first <- validate
        HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) first.vrps
        counter first.context (.anotherKeyPass) >>= HU.assertEqual "Under both" 1

        second <- validate
        HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) second.vrps
        counter second.context (.skippedKeyPass) >>= HU.assertEqual "Not under the copy" 1
        counter second.context (.anotherKeyPass) >>= HU.assertEqual "Under the certificate" 0
        counter second.context (.originalRoa) >>= HU.assertEqual 
            "The ROA is validated in full, the shortcut was not under the certificate" 1

        third <- validate
        HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) third.vrps
        counter third.context (.skippedKeyPass) >>= HU.assertEqual "Not under the copy" 1
        counter third.context (.originalRoa) >>= HU.assertEqual 
            "The shortcut is under the certificate now" 0


-- | The children of the key are CA certificates that inherit their resources, 
-- the way some TAs have it. What they need is what their children need.
shouldLookThroughInheritingCertificates :: HU.Assertion
shouldLookThroughInheritingCertificates =
    withTestContext $ \appContext -> do
        now        <- thisInstant
        issuer     <- newKey "issuer"
        copier     <- newKey "copier"
        key        <- newKey "key"
        inheriting <- newKey "inheriting"
        member     <- newKey "member"
        let real   = makeCaCert now issuer key 1 (ipv4s ["10.0.0.0/16"])
            other  = makeCaCert now copier key 2 (ipv4s ["10.1.0.0/16"])
            within = makeCaCert now copier key 3 (ipv4s ["10.0.1.0/24", "10.1.0.0/16"])
            inheritingCa = makeCaCert now key inheriting 4 (AllResources Inherit Inherit Inherit)
            memberCa = makeCaCert now inheriting member 5 (ipv4s ["10.0.1.0/24"])
            roa = makeRoa now member 10 (ASN 1) "10.0.1.0/24"
        storeCertificates appContext now [real, other, within]
        storePublicationPoint appContext now key 1 [("inheriting.cer", CerRO inheritingCa)]
        storePublicationPoint appContext now inheriting 1 [("member.cer", CerRO memberCa)]
        storePublicationPoint appContext now member 1 [("r10.roa", RoaRO roa)]

        -- Under the one with resources the member needs, the certificate that
        -- inherits is validated under them as well, that's a pass of its key
        forM_ [(other, 0, 1), (within, 2, 0)] $ \(second, anotherPasses, skipped) -> do
            Run {..} <- validateInOrder appContext now [real, second]
            HU.assertEqual "The ROA is valid" (Set.fromList [roa.content]) vrps
            counter context (.anotherKeyPass) >>= HU.assertEqual "Under the second one" anotherPasses
            counter context (.skippedKeyPass) >>= HU.assertEqual "Not under the second one" skipped


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
-- The signature of the manifest is known in the second time already, it's 
-- checked to find what the children need.
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
            "The manifest in the second one, and the manifest, the CRL and the ROAs in the third one" 5


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


data Victim = Victim {
        now    :: Now,
        key    :: TestKey,
        real   :: WellStructuredCaCert,
        copies :: [WellStructuredCaCert],
        roas   :: [WellStructuredRoa]
    }

-- | A certificate of a key with 20 ROAs, and copies of it issued by others, 
-- each with resources of their own
victimWithCopies :: AppContext SqliteBackend -> Int -> IO Victim
victimWithCopies appContext copyCount = do
    now    <- thisInstant
    issuer <- newKey "issuer"
    key    <- newKey "key"
    let real = makeCaCert now issuer key 1 (ipv4s ["10.0.0.0/16"])
        roas = [ makeRoa now key (100 + n) (ASN 1) ("10.0." <> show n <> ".0/24") | n <- [0 .. 19] ]
    copies <- forM [1 .. copyCount] $ \n -> do
        copier <- newKey $ "copier-" <> show n
        pure $ makeCaCert now copier key (fromIntegral $ 1 + n) (ipv4s ["10." <> show n <> ".0.0/16"])
    storeCertificates appContext now (real : copies)
    storePublicationPoint appContext now key 1 
        [ (Text.pack $ "r" <> show n <> ".roa", RoaRO roa) | (n, roa) <- zip [0 :: Int ..] roas ]
    pure Victim {..}

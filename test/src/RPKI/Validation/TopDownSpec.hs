module RPKI.Validation.TopDownSpec where

import           Effectful
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad                    (void)

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Short            as BSS
import           Data.Either                      (isLeft, isRight)
import           Data.Int                         (Int64)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Map.Monoidal.Strict         as MonoidalMap
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import qualified Data.X509                        as X509
import           Data.Generics.Product.Typed      (typed)

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppState                    (instantToVersion)
import           RPKI.AppMonad
import           RPKI.AppTypes                    (WorldVersion)
import           RPKI.Parallel                    (ClosableQueue, closeCQueue, newCQueueIO, newWorkPool)
import           RPKI.Config                      (ValidationAlgorithm (..))
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.AppContext                  (AppContext)
import           RPKI.Reporting
import           RPKI.Repository                  (PublicationPointAccess (..), rsyncPP)
import qualified RPKI.Resources.IntervalContainers as IS
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable         (Compressed (..), Verbatim (..), serialiseField, toStorable)
import qualified RPKI.Store.SQLite                as SQLite
import           RPKI.Store.Database              (DB)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (hashed, parseRpkiURL)
import           RPKI.Validation.ObjectValidation (prevalidateObject, validateBgpSecAsns)
import           RPKI.Validation.Types
import           RPKI.Validation.TopDown
                ( TroubledChildLoadPath (..)
                , becomingValid
                , resolveTroubledChildByKey
                , signedByCa
                , validateCa
                , validateManifests
                )
import           RPKI.Validation.TopDown.Context
                ( ChildSource (..)
                , Limited (..)
                , TopDownContext
                , acceptLeaf
                , markAsUsed
                , newAllTasTopDownContext
                , newTopDownContext
                )
import           RPKI.Validation.TopDown.Shortcuts
                ( MftPlan (..)
                , MftShortcutOp
                , ShortcutWrites (..)
                , makeChildWithIssues
                , manifestValidityPeriod
                , planManifests
                , revokedShortcutChildren
                , shortcutWrites
                , storeShortcuts
                )


topDownRegressionGroup :: TestTree
topDownRegressionGroup =
    testGroup "TopDown regressions"
        [ HU.testCase "Resolves troubled child key from well-structured object" shouldResolveTroubledFromWellStructured
        , HU.testCase "Resolves troubled child key from original object" shouldResolveTroubledFromOriginal
        , HU.testCase "Replaces revoked shortcut children with troubled entries" shouldReplaceRevokedShortcutChildren
        , HU.testCase "Bounds manifest validity by thisUpdate and nextUpdate" shouldBoundManifestValidityByUpdateTimes
        , HU.testCase "Picks how to validate the manifest of a CA" shouldPlanManifests
        , HU.testCase "Tells whether the CA signed an EE certificate" shouldTellWhetherCaSignedEECert
        , HU.testCase "Finds when objects from the future become valid" shouldFindWhenObjectsBecomeValid
        , HU.testCase "Remembers when a leaf validated in full expires" shouldRememberWhenLeavesExpire
        , HU.testCase "Checks the ASNs of a router certificate against its CA" shouldCheckRouterCertificateAsns
        , HU.testCase "Cuts only the path that is too deep" shouldCutOnlyThePathThatIsTooDeep
        , HU.testCase "Counts the size of the tree of every TA on its own" shouldCountTreeSizePerTa
        , HU.testCase "Writes the shortcut of an accepted manifest, with warnings or not" shouldWriteShortcutOfAcceptedManifest
        , HU.testCase "Deletes the shortcut of a CA without an accepted manifest" shouldDeleteShortcutOfCaWithoutManifest
        , HU.testCase "Forces a snapshot only when no manifest can be used" shouldForceSnapshotOnlyWithoutFallback
        , HU.testCase "Tells integrity errors and the repository they are in" shouldTellIntegrityErrors
        ]


shouldResolveTroubledFromWellStructured :: HU.Assertion
shouldResolveTroubledFromWellStructured =
    withTestContext $ \appContext -> do
        db <- readTVarIO $ appContext ^. #database
        worldVersion <- instantToVersion . unNow <$> thisInstant

        (Right (url, _, parsedObject), _) <- runValidatorIO (newScopes "fixture-ws") $ readFixtureObject fixturePath
        (Right expectedObject, _) <- runValidatorIO (newScopes "prevalidate-ws") $ prevalidateObject parsedObject

        key <- storeLifecycle db worldVersion (WellStructuredRO expectedObject) url

        (resolved, _) <- runValidatorIO (newScopes "resolve-ws") $
            DB.roAppTx db $ \tx -> resolveTroubledChildByKey tx key

        case resolved of
            Right (Just (TroubledFromParsed, Keyed (Located _ actualObject) actualKey)) -> do
                HU.assertEqual "Resolved key mismatch" key actualKey
                HU.assertEqual "Resolved object mismatch" expectedObject actualObject
            other ->
                HU.assertFailure $ "Expected TroubledFromParsed resolution, got: " <> show other


shouldResolveTroubledFromOriginal :: HU.Assertion
shouldResolveTroubledFromOriginal =
    withTestContext $ \appContext -> do
        db <- readTVarIO $ appContext ^. #database
        worldVersion <- instantToVersion . unNow <$> thisInstant

        (Right (url, blob, parsedObject), _) <- runValidatorIO (newScopes "fixture-orig") $ readFixtureObject fixturePath
        (Right expectedObject, _) <- runValidatorIO (newScopes "prevalidate-orig") $ prevalidateObject parsedObject

        let lifecycle =
                OriginalRO
                    (ObjectOriginal blob)
                    mempty
                    (getHash parsedObject)
                    (getRpkiObjectType parsedObject)

        key <- storeLifecycle db worldVersion lifecycle url

        (resolved, _) <- runValidatorIO (newScopes "resolve-orig") $
            DB.roAppTx db $ \tx -> resolveTroubledChildByKey tx key

        case resolved of
            Right (Just (TroubledFromOriginal, Keyed (Located _ actualObject) actualKey)) -> do
                HU.assertEqual "Resolved key mismatch" key actualKey
                HU.assertEqual "Resolved object mismatch" expectedObject actualObject
            other ->
                HU.assertFailure $ "Expected TroubledFromOriginal resolution, got: " <> show other


storeLifecycle :: DB -> WorldVersion -> RpkiObjectLifecycle -> RpkiURL -> IO ObjectKey
storeLifecycle db worldVersion lifecycle url =
    DB.rwTx db $ \tx -> do
        key <- DB.saveObject tx lifecycle worldVersion
        DB.linkObjectToUrl tx url key worldVersion
        pure key


readFixtureObject :: ValidatorIO es => FilePath -> Eff es (RpkiURL, BS.ByteString, ParsedRpkiObject)
readFixtureObject path = do
    blob <- liftIO $ BS.readFile path
    -- Drop the "./" prefix of the fixture path: `parseRpkiURL` (rightly) rejects 
    -- dot-segments, since rsync URLs are mapped onto local filesystem paths.
    let urlPath = Text.dropWhile (== '/') $ Text.replace "./" "" $ Text.pack path
    let url =
            case parseRpkiURL $ "rsync://host/" <> urlPath of
                Right parsedUrl -> parsedUrl
                Left err -> error $ "Failed to parse fixture URL: " <> Text.unpack err
    object <- readObject url (hashed blob)
    pure (url, blob, object)


fixturePath :: FilePath
fixturePath = "./test/data/afrinic_mft1.mft"


-- | A child that is covered by the manifest shortcut (i.e. is not going to be
-- re-validated in this round) and whose serial appears on the new CRL must be
-- reported back, so that its shortcut entry is replaced with a troubled one and it
-- stops contributing payloads. See `revokedShortcutChildren` for why replacing the
-- entry (rather than just skipping the child for this round) is what is needed.
shouldReplaceRevokedShortcutChildren :: HU.Assertion
shouldReplaceRevokedShortcutChildren = do
    let revokedRoaKey = objectKey 1
        liveRoaKey    = objectKey 2
        revokedGbrKey = objectKey 3
        troubledKey   = objectKey 4
        notOnShortcut = objectKey 5
        revokedCaKey  = objectKey 6

    let mftShortcut = testMftShortcut
            [ (revokedRoaKey, MftEntry "revoked.roa"  (RoaChild (testRoaShortcut revokedRoaKey) (Serial 100)) Nothing)
            , (liveRoaKey,    MftEntry "live.roa"     (RoaChild (testRoaShortcut liveRoaKey)    (Serial 200)) Nothing)
            , (revokedGbrKey, MftEntry "revoked.gbr"  (GbrChild (testGbrShortcut revokedGbrKey) (Serial 300)) Nothing)
            , (revokedCaKey,  MftEntry "revoked.cer"  (CaChild (testCaShortcut revokedCaKey)    (Serial 400)) (Just ValidCaChild))
            -- A troubled child carries no serial, it is re-validated in full anyway
            , (troubledKey,   MftEntry "troubled.roa" (TroubledChild troubledKey) Nothing)
            ]

    let mftChildren =
            [ T3 "revoked.roa"  (testHash "h1") revokedRoaKey
            , T3 "live.roa"     (testHash "h2") liveRoaKey
            , T3 "revoked.gbr"  (testHash "h3") revokedGbrKey
            , T3 "troubled.roa" (testHash "h4") troubledKey
            -- Not on the shortcut at all, so not this function's business
            , T3 "new.roa"      (testHash "h5") notOnShortcut
            , T3 "revoked.cer"  (testHash "h6") revokedCaKey
            ]

    -- The new CRL revokes the ROA, the GBR and the CA, plus a serial that belongs to
    -- nothing here. A revoked CA certificate stays a CA certificate, an invalid one.
    HU.assertEqual "Wrong set of revoked children"
        [ (revokedRoaKey, MftEntry "revoked.roa" (TroubledChild revokedRoaKey) Nothing)
        , (revokedGbrKey, MftEntry "revoked.gbr" (TroubledChild revokedGbrKey) Nothing)
        , (revokedCaKey,  MftEntry "revoked.cer" (TroubledChild revokedCaKey)  (Just InvalidCaChild))
        ]
        (revokedShortcutChildren mftShortcut (testCrl [Serial 100, Serial 300, Serial 400, Serial 999]) mftChildren)

    HU.assertEqual "Nothing may be revoked by a CRL that lists none of these serials"
        []
        (revokedShortcutChildren mftShortcut (testCrl [Serial 999]) mftChildren)


-- | Manifests are found by AKI, and anyone can publish one with the AKI of any
-- CA. Only the ones whose EE certificate the CA signed are its manifests.
shouldTellWhetherCaSignedEECert :: HU.Assertion
shouldTellWhetherCaSignedEECert = do
    (Right objects, _) <- runValidatorIO (newScopes "signed-by-ca") $
        (,,) <$> prevalidated "./test/data/overcleaiming/2R93viIBHX4dV12fmttfjhYOX9k.cer"
             <*> prevalidated "./test/data/overcleaiming/EBA158C223CE11EBA804DD64C4F9AE02.roa"
             <*> prevalidated fixturePath
    case objects of
        (CerRO ca, RoaRO roa, MftRO mft) -> do
            let locatedCa = Located Nothing ca
            HU.assertBool "The CA signed the EE certificate of its ROA" $
                signedByCa locatedCa roa
            -- The same AKI, but not the signature of the CA
            let spoilt = roa & #eeCert . #signature %~
                    \(SignatureValue s) -> SignatureValue $ BSS.pack $ reverse $ BSS.unpack s
            HU.assertBool "An EE certificate with the AKI of the CA, but not signed by it" $
                not $ signedByCa locatedCa spoilt
            HU.assertBool "A manifest of another CA" $
                not $ signedByCa locatedCa mft
        other ->
            HU.assertFailure $ "Unexpected fixture objects: " <> show other
  where
    prevalidated path = do
        (_, _, parsedObject) <- readFixtureObject path
        prevalidateObject parsedObject


-- | A manifest can be used only while its EE certificate is valid and the
-- manifest is current, whichever ends first. Manifest shortcuts used to take
-- only the EE certificate's validity, so the fast path accepted a manifest past
-- its nextUpdate when its EE certificate lived longer.
shouldBoundManifestValidityByUpdateTimes :: HU.Assertion
shouldBoundManifestValidityByUpdateTimes = do
    (Right (_, _, parsedObject), _) <- runValidatorIO (newScopes "fixture-mft") $ readFixtureObject fixturePath
    (Right wellStructured, _) <- runValidatorIO (newScopes "prevalidate-mft") $ prevalidateObject parsedObject
    mft <- case wellStructured of
                MftRO m -> pure m
                other   -> HU.assertFailure $ "Expected a manifest, got: " <> show other

    let ValidityPeriod eeNotBefore eeNotAfter = getValidityPeriod mft
    let withUpdateTimes thisTime nextTime =
            mft & #content . #thisTime .~ thisTime
                & #content . #nextTime .~ nextTime

    -- Current for a shorter time than the EE certificate is valid
    let current = ValidityPeriod (shift eeNotBefore hour) (shift eeNotAfter (-hour))
    HU.assertEqual "thisUpdate and nextUpdate must bound the period"
        current
        (manifestValidityPeriod $ withUpdateTimes current.notBefore current.notAfter)

    -- The EE certificate is valid for a shorter time than the manifest is current
    HU.assertEqual "The EE certificate's validity must bound the period"
        (ValidityPeriod eeNotBefore eeNotAfter)
        (manifestValidityPeriod $ withUpdateTimes (shift eeNotBefore (-hour)) (shift eeNotAfter hour))
  where
    hour = 3600
    shift (Instant t) seconds = Instant (t + seconds * nanosPerSecond)


-- | The moment to revalidate at for the objects that aren't valid yet.
shouldFindWhenObjectsBecomeValid :: HU.Assertion
shouldFindWhenObjectsBecomeValid = do
    let atHour hours = Instant (hours * 3600 * nanosPerSecond)
        now = atHour 100
        issuesOf = Validations . Map.fromList
        manifest = (newScope "manifest", Set.fromList [
                        VWarn $ VWarning $ ValidationE $ ThisUpdateTimeIsInTheFuture (atHour 103) now ])
        roa      = (newScope "roa", Set.fromList [
                        VErr $ ValidationE $ ObjectValidityIsInTheFuture (atHour 102) (atHour 200) ])
        aspa     = (newScope "aspa", Set.fromList [
                        VErr $ ValidationE $ ObjectIsExpired (atHour 1) (atHour 50) ])
        futureMft = ValidationE $ ThisUpdateTimeIsInTheFuture (atHour 101) now
        -- How a manifest from the future is reported when there's an older one to use
        fallback = (newScope "fallback", Set.fromList [
                        VWarn $ VWarning $ ValidationE $ MftFallback futureMft (Serial 7) ])

    HU.assertEqual "The first of them to become valid"
        (EarliestValidityChange $ atHour 102)
        (becomingValid $ issuesOf [manifest, roa, aspa])

    HU.assertEqual "A manifest from the future with an older one to fall back to"
        (EarliestValidityChange $ atHour 101)
        (becomingValid $ issuesOf [manifest, roa, aspa, fallback])

    HU.assertEqual "An expired object never becomes valid"
        mempty
        (becomingValid $ issuesOf [aspa])


-- | When a leaf expires is a reason to revalidate. For one taken from its 
-- shortcut it's remembered where the shortcut is checked, not here.
shouldRememberWhenLeavesExpire :: HU.Assertion
shouldRememberWhenLeavesExpire =
    withTestContext $ \appContext -> do
        (topDownContext, _) <- newTestTopDownContext appContext
        let roa notAfter = RoaChild (testRoaShortcut (objectKey 1) & #notAfter .~ Instant notAfter) (Serial 1)
            accept source child = void $ runValidatorIO (newScopes "accept") $
                                    acceptLeaf topDownContext source child
            earliest = readTVarIO $ topDownContext ^. #earliestNotValidAfter

        accept FromShortcut (roa 100)
        earliest >>= HU.assertEqual "Not for a leaf from its shortcut" mempty

        accept FromObject (roa 200)
        accept FromObject (TroubledChild $ objectKey 2)
        earliest >>= HU.assertEqual "For a leaf validated in full" (EarliestValidityChange $ Instant 200)

        accept FromObject (roa 150)
        earliest >>= HU.assertEqual "The first of them" (EarliestValidityChange $ Instant 150)


-- | A router certificate is only good for the ASNs that its CA has. With 
-- the reconsidered algorithm, resources it overclaims are only a warning.
shouldCheckRouterCertificateAsns :: HU.Assertion
shouldCheckRouterCertificateAsns = do
    let caAsns = IS.fromList [AS (ASN 64496), ASRange (ASN 65000) (ASN 65010)]
        verified = Just $ VerifiedRS $ PrefixesAndAsns IS.empty IS.empty caAsns
        checkAsns verifiedResources asns = fst <$> runValidatorIO (newScopes "bgpsec")
                                                (validateBgpSecAsns verifiedResources asns)

    checkAsns verified [ASN 64496, ASN 65005] >>= HU.assertEqual
        "All its ASNs are the CA's" (Right ())

    checkAsns verified [ASN 64496, ASN 64497, ASN 65011] >>= HU.assertEqual
        "Some of them aren't"
        (Left $ ValidationE $ BGPCertAsnsNotInResourceSet [ASN 64497, ASN 65011] (IS.toList caAsns))

    checkAsns Nothing [ASN 64497] >>= HU.assertEqual
        "Without verified resources it's the resources of the issuer that count" (Right ())


-- | Too deep is a matter of one path, e.g. of a CA that keeps issuing CAs
-- under itself. Only that path is cut, the rest of the TA is validated.
shouldCutOnlyThePathThatIsTooDeep :: HU.Assertion
shouldCutOnlyThePathThatIsTooDeep =
    withTestContext $ \appContext -> do
        (topDownContext, _) <- newTestTopDownContext appContext
        ca <- fixtureCa
        let maxDepth = appContext ^. #config . #validationConfig . #maxCertificatePathDepth
        (r, _) <- runValidatorIO (newScopes "deep") $
                    validateCa appContext (topDownContext & #currentPathDepth .~ maxDepth + 1) ca
        HU.assertEqual "The path is cut" (Left $ ValidationE $ CertificatePathTooDeep maxDepth) r
        readTVarIO (topDownContext ^. #interruptedByLimit) >>= HU.assertEqual
            "The rest of the TA goes on" CanProceed


-- | The size of the tree is for one TA, while the objects that validation 
-- has looked at are for all of them.
shouldCountTreeSizePerTa :: HU.Assertion
shouldCountTreeSizePerTa =
    withTestContext $ \appContext -> do
        (ripe, _) <- newTestTopDownContext appContext
        arin <- newTopDownContext (TaName "arin") (ripe ^. #allTas)
        let mark ta keys = void $ runValidatorIO (newScopes "mark") $
                            mapM_ (markAsUsed ta . objectKey) keys
        mark ripe [1, 2, 2, 3]
        mark arin [3, 4]
        readTVarIO (ripe ^. #treeSize) >>= HU.assertEqual "Every object once" 3
        readTVarIO (arin ^. #treeSize) >>= HU.assertEqual
            "Only its own, an object of both is of the TA that got to it first" 1


-- | Which manifest of a CA is validated, and how.
shouldPlanManifests :: HU.Assertion
shouldPlanManifests = do
    let now = Now (atHour 100)
        -- As the cache gives them, newest first
        future = mftMeta 3 (atHour 101) (atHour 125)
        newer  = mftMeta 2 (atHour 99)  (atHour 123)
        older  = mftMeta 1 (atHour 90)  (atHour 114)
        shortcut = shortcutMeta 1 (atHour 90) (atHour 114)

    -- The plan, and the manifests from the future passed over for older data
    HU.assertEqual "There are no manifests"
        (NoManifest, [])
        (planManifests now [] Nothing)

    HU.assertEqual "Without a shortcut, the manifests not in the future are validated in full"
        (InFull [newer, older], [future])
        (planManifests now [future, newer, older] Nothing)

    HU.assertEqual "When all of them are in the future, all of them are tried, for the error"
        (InFull [future], [])
        (planManifests now [future] Nothing)

    HU.assertEqual "The manifest of the shortcut is the latest one not in the future"
        (UseShortcut shortcut, [future])
        (planManifests now [future, older] (Just shortcut))

    HU.assertEqual "There's a newer manifest than the one of the shortcut"
        (DiffWithShortcut shortcut (newer :| []), [future])
        (planManifests now [future, newer, older] (Just shortcut))

    -- Manifests are found by AKI wherever they were published, so a manifest
    -- with this CA's AKI from somewhere else can be the newest one. It must not
    -- hide the CA's own manifest that is newer than the shortcut.
    let elsewhere = mftMeta 4 (atHour 99) (atHour 124)
    HU.assertEqual "All manifests newer than the one of the shortcut are kept, newest first"
        (DiffWithShortcut shortcut (elsewhere :| [newer]), [future])
        (planManifests now [future, elsewhere, newer, older] (Just shortcut))

    HU.assertEqual "The manifests older than the one of the shortcut are not kept"
        (DiffWithShortcut shortcut (newer :| []), [])
        (planManifests now [newer, older, mftMeta 0 (atHour 80) (atHour 104)] (Just shortcut))

    HU.assertEqual "When the manifest of the shortcut is gone from the cache, all are kept"
        (DiffWithShortcut shortcut (elsewhere :| [newer]), [])
        (planManifests now [elsewhere, newer] (Just shortcut))

    HU.assertEqual "An expired shortcut is not used"
        (InFull [newer, older], [])
        (planManifests now [newer, older] (Just $ shortcut & #notAfter .~ atHour 99))

    HU.assertEqual "A shortcut with an expired CRL is not used"
        (InFull [newer, older], [])
        (planManifests now [newer, older] (Just $ shortcut & #crlShortcut . #notAfter .~ atHour 99))

    -- Shortcuts made before `manifestValidityPeriod` have only the 
    -- validity of the EE certificate of the manifest
    let stale = mftMeta 1 (atHour 90) (atHour 99)
    HU.assertEqual "A shortcut of a manifest past its nextUpdate is not used"
        (InFull [stale], [])
        (planManifests now [stale] (Just $ shortcut & #notAfter .~ atHour 200))

    -- The manifest of a shortcut can be gone from the cache when validation 
    -- didn't get to the CA for longer than the cache keeps manifests
    HU.assertEqual "A valid shortcut is used when there are no manifests"
        (OnlyShortcut shortcut, [])
        (planManifests now [] (Just shortcut))

    HU.assertEqual "A valid shortcut is used when there are only manifests from the future"
        (OnlyShortcut shortcut, [future])
        (planManifests now [future] (Just shortcut))

    HU.assertEqual "An expired shortcut is not used when there are no manifests"
        (NoManifest, [])
        (planManifests now [] (Just $ shortcut & #notAfter .~ atHour 99))
  where
    atHour hours = Instant (hours * 3600 * nanosPerSecond)
    mftMeta k thisTime_ nextTime_ = MftMeta {
            key       = objectKey k,
            mftNumber = Serial (fromIntegral k),
            thisTime  = thisTime_,
            nextTime  = nextTime_,
            contentDigest = testHash "digest"
        }
    shortcutMeta k notBefore_ notAfter_ = DB.MftShortcutMeta {
            key            = objectKey k,
            notBefore      = notBefore_,
            notAfter       = notAfter_,
            serial         = Serial 1,
            manifestNumber = Serial (fromIntegral k),
            crlShortcut    = CrlShortcut (objectKey 1000) notBefore_ notAfter_,
            hasIssues      = False
        }


-- | A manifest accepted with warnings gets a shortcut too, marked, so that the
-- shortcut names the manifest that is in the tree. Only what changed is written.
shouldWriteShortcutOfAcceptedManifest :: HU.Assertion
shouldWriteShortcutOfAcceptedManifest = do
    let roaKey   = objectKey 1
        newKey   = objectKey 2
        roa      = (roaKey, MftEntry "a.roa" (RoaChild (testRoaShortcut roaKey) (Serial 100)) Nothing)
        newRoa   = (newKey, MftEntry "b.roa" (RoaChild (testRoaShortcut newKey) (Serial 200)) Nothing)
        olderMft = testMftShortcut [roa] & #key .~ objectKey 100
        -- With the entries of the children that were validated in full
        accepted entries hasIssues = testMftShortcut entries
                                        & #key       .~ objectKey 110
                                        & #hasIssues .~ hasIssues

    HU.assertEqual "Without a shortcut to validate against, the whole one is replaced"
        ReplaceShortcut
        (shortcutWrites Nothing (accepted [roa] True) [])

    HU.assertEqual "A newer manifest with warnings takes the shortcut over"
        (UpdateShortcut True [newRoa] [roaKey])
        (shortcutWrites (Just olderMft) (accepted [newRoa] True) [roaKey])

    -- The manifest of a shortcut with issues is validated with all its children again
    let withIssues = testMftShortcut [roa, newRoa] & #key .~ objectKey 110 & #hasIssues .~ True
    HU.assertEqual "Nothing changed, nothing is written"
        (UpdateShortcut False [] [])
        (shortcutWrites (Just withIssues) (accepted [roa, newRoa] True) [])

    HU.assertEqual "The warnings are gone"
        (UpdateShortcut True [] [])
        (shortcutWrites (Just withIssues) (accepted [roa, newRoa] False) [])

    let troubled = (newKey, makeChildWithIssues newKey "b.roa" Nothing)
    HU.assertEqual "Only the child that is validated to something else is written"
        (UpdateShortcut False [troubled] [])
        (shortcutWrites (Just withIssues) (accepted [roa, troubled] True) [])


-- | Shortcuts describe the tree as it was validated, so a CA that ends up without 
-- an accepted manifest loses its shortcut. A shortcut that is used stays.
shouldDeleteShortcutOfCaWithoutManifest :: HU.Assertion
shouldDeleteShortcutOfCaWithoutManifest =
    withIncrementalContext $ \appContext -> do
        Now now <- thisInstant
        let aki = AKI (mkKI "98765432109876543210")

        -- No manifests and the shortcut is expired: there's nothing to accept
        let expired = shortcutMetaUntil now (momentAfter now (-60))
        saveShortcut appContext aki expired
        (r, stored, _) <- validateCaManifests appContext testRrdpPP aki
        HU.assertBool "Validation must fail" (isLeft r)
        HU.assertEqual "The shortcut must be gone" Nothing stored

        -- No manifests, but the shortcut is still valid, so it's used (RFC 9286, 6.6)
        let valid = shortcutMetaUntil now (momentAfter now 3600)
        saveShortcut appContext aki valid
        (r', stored', _) <- validateCaManifests appContext testRrdpPP aki
        HU.assertBool "Validation must succeed" (isRight r')
        HU.assertEqual "The shortcut must stay" (Just valid) stored'


-- | A manifest that refers to what the cache doesn't have makes the repository
-- fetched from a snapshot, but only when there's nothing left to fall back to.
shouldForceSnapshotOnlyWithoutFallback :: HU.Assertion
shouldForceSnapshotOnlyWithoutFallback =
    withIncrementalContext $ \appContext -> do
        Now now <- thisInstant
        let rrdpUrl = RrdpURL $ URI "https://rrdp.example.net/notification.xml"

        -- Neither of the manifests is in the cache, there's no shortcut
        let noFallback = AKI (mkKI "11111111111111111111")
        storeBrokenManifest appContext noFallback 1 (momentAfter now (-7200))
        storeBrokenManifest appContext noFallback 2 (momentAfter now (-3600))
        (r, _, candidates) <- validateCaManifests appContext (RrdpU rrdpUrl) noFallback
        HU.assertBool "Validation must fail" (isLeft r)
        HU.assertEqual "The repository must be a snapshot candidate"
            [rrdpUrl] (MonoidalMap.keys candidates)
        HU.assertBool "With why" (all (not . Set.null) candidates)

        -- The same, under a publication point that has no RRDP repository
        (_, _, rsyncCandidates) <- validateCaManifests appContext testRsyncPP noFallback
        HU.assertEqual "There's no RRDP repository to fetch a snapshot of" mempty rsyncCandidates

        -- The newer manifest is not in the cache, but the shortcut is valid
        let withFallback = AKI (mkKI "22222222222222222222")
        let valid = shortcutMetaUntil now (momentAfter now 3600)
        saveShortcut appContext withFallback valid
        storeBrokenManifest appContext withFallback 3 (momentAfter now (-60))
        (r', stored', candidates') <- validateCaManifests appContext (RrdpU rrdpUrl) withFallback
        HU.assertBool "Validation must succeed with the shortcut" (isRight r')
        HU.assertEqual "The shortcut must stay" (Just valid) stored'
        HU.assertEqual "Nothing is forced when falling back works" mempty candidates'

        -- No manifest at all is not an integrity problem
        let noManifests = AKI (mkKI "33333333333333333333")
        (r'', _, candidates'') <- validateCaManifests appContext (RrdpU rrdpUrl) noManifests
        HU.assertBool "Validation must fail" (isLeft r'')
        HU.assertEqual "Nothing to force for a CA without manifests" mempty candidates''


shouldTellIntegrityErrors :: HU.Assertion
shouldTellIntegrityErrors = do
    let missing = ValidationE $ ManifestEntryDoesn'tExist (testHash "h") "a.roa"
        other   = ValidationE $ NoMFT (AKI (mkKI "11111111111111111111"))
    HU.assertBool "A missing object" (isReferentialIntegrityError missing)
    HU.assertBool "A missing object of a manifest fallen back from"
        (isReferentialIntegrityError $ ValidationE $ MftFallback missing (Serial 1))
    HU.assertBool "Not a missing manifest" (not $ isReferentialIntegrityError other)

    let rrdpUrl = RrdpURL $ URI "https://rrdp.example.net/notification.xml"
        scope = newScopes' PPFocus (RrdpU rrdpUrl) ^. typed @VScope
    HU.assertEqual "The RRDP repository of the publication point"
        (Just rrdpUrl) (innermostRrdpPP scope)
    HU.assertEqual "Not the one of an outer publication point"
        Nothing (innermostRrdpPP $ subScope PPFocus testRsyncPP scope)


-- | Shortcuts are only used by incremental validation
withIncrementalContext :: (AppContext -> IO a) -> IO a
withIncrementalContext f =
    withTestContext $ \appContext ->
        f $ appContext & #config . #validationConfig . #validationAlgorithm .~ Incremental

-- | Run `validateManifests` for a CA in the scope of its publication point, 
-- the way validation does. Gives back the result, the shortcut of the CA once 
-- the shortcut writes are done, and the repositories to fetch from a snapshot.
validateCaManifests :: AppContext -> RpkiURL -> AKI
                    -> IO (Either AppError (), Maybe DB.MftShortcutMeta, MonoidalMap.MonoidalMap RrdpURL (Set.Set Text.Text))
validateCaManifests appContext ppUrl aki = do
    db <- readTVarIO $ appContext ^. #database
    (topDownContext, shortcutQueue) <- newTestTopDownContext appContext
    -- Any CA certificate does, it's only needed to tell which manifests the CA signed
    ca <- fixtureCa
    (r, _) <- runValidatorIO (newScopes "validate-manifests") $
                vFocusOn PPFocus ppUrl $
                    validateManifests appContext topDownContext ca aki
    atomically $ closeCQueue shortcutQueue
    storeShortcuts appContext shortcutQueue
    stored <- DB.roTx db $ \tx -> DB.getMftShorcutMeta tx aki
    candidates <- readTVarIO $ topDownContext ^. #snapshotCandidates
    pure (r, stored, candidates)

-- | The context of validating a TA, with the queue of its shortcut writes
newTestTopDownContext :: AppContext -> IO (TopDownContext, ClosableQueue MftShortcutOp)
newTestTopDownContext appContext = do
    db <- readTVarIO $ appContext ^. #database
    Now now <- thisInstant
    shortcutQueue <- newCQueueIO 10
    workPool      <- newWorkPool
    publicationPoints <- DB.roTx db DB.getPublicationPoints
    allTas <- newAllTasTopDownContext (instantToVersion now) publicationPoints shortcutQueue mempty workPool
    topDownContext <- newTopDownContext (TaName "test") allTas
    pure (topDownContext, shortcutQueue)

-- | A CA certificate, with no locations
fixtureCa :: IO Ca
fixtureCa = do
    (Right ca, _) <- runValidatorIO (newScopes "ca") $ do
        (_, _, parsed) <- readFixtureObject "./test/data/overcleaiming/2R93viIBHX4dV12fmttfjhYOX9k.cer"
        prevalidateObject parsed >>= \case
            CerRO c -> pure $ CaFull $ Located Nothing c
            other   -> error $ "Not a CA certificate: " <> show other
    pure ca

saveShortcut :: AppContext -> AKI -> DB.MftShortcutMeta -> IO ()
saveShortcut appContext aki meta = do
    db <- readTVarIO $ appContext ^. #database
    DB.rwTx db $ \tx -> DB.saveMftShorcutMeta tx aki (Verbatim $ toStorable $ Compressed meta)

-- | A shortcut that is valid from an hour before `now` until `notAfter_`
shortcutMetaUntil :: Instant -> Instant -> DB.MftShortcutMeta
shortcutMetaUntil now notAfter_ = DB.MftShortcutMeta {
        key            = objectKey 2,
        notBefore      = momentAfter now (-3600),
        notAfter       = notAfter_,
        serial         = Serial 1,
        manifestNumber = Serial 1,
        crlShortcut    = CrlShortcut (objectKey 3) (momentAfter now (-3600)) notAfter_,
        hasIssues      = False
    }

-- | A manifest of the AKI that is known, but whose object is not a manifest,
-- as if it was gone from the cache.
storeBrokenManifest :: AppContext -> AKI -> Int64 -> Instant -> IO ()
storeBrokenManifest appContext aki n thisTime_ = do
    db <- readTVarIO $ appContext ^. #database
    worldVersion <- instantToVersion . unNow <$> thisInstant
    let blob = BS.pack $ map (fromIntegral . fromEnum) $ "not a manifest " <> show n
    DB.rwTx db $ \tx@(DB.Tx conn) -> do
        key <- DB.saveObject tx (OriginalRO (ObjectOriginal blob) mempty (testHash blob) MFT) worldVersion
        let meta = MftMeta {
                key           = key,
                mftNumber     = Serial (fromIntegral n),
                thisTime      = thisTime_,
                nextTime      = momentAfter thisTime_ 86400,
                contentDigest = testHash blob
            }
        SQLite.execute conn
            "INSERT INTO manifest_meta(object_key, aki, manifest_number, meta) VALUES (?, ?, ?, ?)"
            (key, aki, serialiseField meta.mftNumber, serialiseField meta)

testRrdpPP, testRsyncPP :: RpkiURL
testRrdpPP  = RrdpU $ RrdpURL $ URI "https://rrdp.example.net/other.xml"
testRsyncPP = case parseRpkiURL "rsync://rsync.example.net/repo/" of
                Right u -> u
                Left e  -> error $ "Not an rsync URL: " <> show e


objectKey :: Int64 -> ObjectKey
objectKey = ObjectKey . asKey

testHash :: BS.ByteString -> Hash
testHash = Hash . BSS.toShort

testMftShortcut :: [(ObjectKey, MftEntry)] -> MftShortcut
testMftShortcut entries = MftShortcut {
        key            = objectKey 100,
        nonCrlEntries  = Map.fromList entries,
        notBefore      = Instant 0,
        notAfter       = Instant 1,
        serial         = Serial 1,
        manifestNumber = Serial 1,
        crlShortcut    = CrlShortcut (objectKey 101) (Instant 0) (Instant 1),
        hasIssues      = False
    }

testCaShortcut :: ObjectKey -> CaShortcut
testCaShortcut key = CaShortcut {
        key       = key,
        ski       = SKI (mkKI "01234567890123456789"),
        ppas      = PublicationPointAccess $ NonEmpty.singleton $ rsyncPP testRsyncUrl,
        notBefore = Instant 0,
        notAfter  = Instant 1,
        resources = AllResources Inherit Inherit Inherit
    }
  where
    testRsyncUrl = case parseRpkiURL "rsync://host/repo/" of
                        Right (RsyncU u) -> u
                        other            -> error $ "Not an rsync URL: " <> show other

testRoaShortcut :: ObjectKey -> RoaShortcut
testRoaShortcut key = RoaShortcut {
        key        = key,
        roaPayload = VrpsPerAs (ASN 64496) [] [],
        notBefore  = Instant 0,
        notAfter   = Instant 1,
        resources  = AllResources Inherit Inherit Inherit
    }

testGbrShortcut :: ObjectKey -> GbrShortcut
testGbrShortcut key = GbrShortcut {
        key       = key,
        gbr       = T2 (testHash "gbr") (Gbr $ BSS.toShort "gbr"),
        notBefore = Instant 0,
        notAfter  = Instant 1,
        resources = AllResources Inherit Inherit Inherit
    }

-- Only `revokedSerials` matters for the revocation check, the rest is filler.
testCrl :: [Serial] -> Validated CrlObject
testCrl serials = Validated CrlObject {
        hash    = testHash "crl",
        aki     = AKI (mkKI "0123456789012345678"),
        signCrl = SignCRL {
            thisUpdateTime     = Instant 0,
            nextUpdateTime     = Instant 1,
            signatureAlgorithm = SignatureAlgorithmIdentifier
                                    (X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA),
            signatureValue     = SignatureValue (BSS.toShort "signature"),
            encodedValue       = BSS.toShort "encoded",
            crlNumber          = Serial 1,
            revokedSerials     = Set.fromList serials
        }
    }

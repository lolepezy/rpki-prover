module RPKI.Validation.TopDownSpec where

import           Effectful
import           Control.Concurrent.STM
import           Control.Lens

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Short            as BSS
import           Data.Either                      (isLeft, isRight)
import           Data.Int                         (Int64)
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import qualified Data.X509                        as X509

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppState                    (instantToVersion)
import           RPKI.AppMonad
import           RPKI.AppTypes                    (WorldVersion)
import           RPKI.Parallel                    (closeCQueue, newCQueueIO, newWorkPool)
import           RPKI.Config                      (ValidationAlgorithm (..))
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting                   (newScopes)
import           RPKI.Repository                  (PublicationPointAccess (..), rsyncPP)
import           RPKI.Resources.Types
import           RPKI.Store.Base.Storable         (Compressed (..), Verbatim (..), toStorable)
import           RPKI.Store.Database              (DB)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (hashed, parseRpkiURL)
import           RPKI.Validation.ObjectValidation (prevalidateObject)
import           RPKI.Validation.Types
import           RPKI.Validation.TopDown
                ( TroubledChildLoadPath (..)
                , resolveTroubledChildByKey
                , signedByCa
                , validateManifests
                )
import           RPKI.Validation.TopDown.Context
                ( newAllTasTopDownContext
                , newTopDownContext
                )
import           RPKI.Validation.TopDown.Shortcuts
                ( MftPlan (..)
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
        , HU.testCase "Writes the shortcut of an accepted manifest, with warnings or not" shouldWriteShortcutOfAcceptedManifest
        , HU.testCase "Deletes the shortcut of a CA without an accepted manifest" shouldDeleteShortcutOfCaWithoutManifest
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
            nextTime  = nextTime_
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
    withTestContext $ \appContext_ -> do
        -- Shortcuts are only used by incremental validation
        let appContext = appContext_ & #config . #validationConfig . #validationAlgorithm .~ Incremental
        db <- readTVarIO $ appContext ^. #database
        Now now <- thisInstant
        let aki = AKI (mkKI "98765432109876543210")
            ca  = CaShort $ testCaShortcut (objectKey 1)
            shortcut notAfter_ = DB.MftShortcutMeta {
                    key            = objectKey 2,
                    notBefore      = momentAfter now (-3600),
                    notAfter       = notAfter_,
                    serial         = Serial 1,
                    manifestNumber = Serial 1,
                    crlShortcut    = CrlShortcut (objectKey 3) (momentAfter now (-3600)) notAfter_,
                    hasIssues      = False
                }

        let validateWith meta = do
                DB.rwTx db $ \tx ->
                    DB.saveMftShorcutMeta tx aki (Verbatim $ toStorable $ Compressed meta)
                shortcutQueue <- newCQueueIO 10
                workPool      <- newWorkPool
                publicationPoints <- DB.roTx db DB.getPublicationPoints
                allTas <- newAllTasTopDownContext (instantToVersion now) publicationPoints shortcutQueue mempty workPool
                topDownContext <- newTopDownContext (TaName "test") allTas
                (r, _) <- runValidatorIO (newScopes "validate-manifests") $
                            validateManifests appContext topDownContext ca aki
                atomically $ closeCQueue shortcutQueue
                storeShortcuts appContext shortcutQueue
                stored <- DB.roTx db $ \tx -> DB.getMftShorcutMeta tx aki
                pure (r, stored)

        -- No manifests and the shortcut is expired: there's nothing to accept
        let expired = shortcut (momentAfter now (-60))
        (r, stored) <- validateWith expired
        HU.assertBool "Validation must fail" (isLeft r)
        HU.assertEqual "The shortcut must be gone" Nothing stored

        -- No manifests, but the shortcut is still valid, so it's used (RFC 9286, 6.6)
        let valid = shortcut (momentAfter now 3600)
        (r', stored') <- validateWith valid
        HU.assertBool "Validation must succeed" (isRight r')
        HU.assertEqual "The shortcut must stay" (Just valid) stored'


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

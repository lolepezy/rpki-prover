module RPKI.Workflow.RevalidationSpec where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import qualified Data.ByteString.Short            as BSS
import           Data.Int                         (Int64)
import qualified Data.Set                         as Set

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppMonad
import           RPKI.AppState                    (instantToVersion)
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting                   (newScopes)
import           RPKI.Store.Database              (Tx (..))
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.DatabaseSpec          (readObjectFromFile, toValidatedRpkiObject)
import qualified RPKI.Store.SQLite                as SQLite
import           RPKI.Store.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Validation.Types
import           RPKI.Workflow.Revalidation


revalidationSpec :: TestTree
revalidationSpec = testGroup "Revalidation on new manifests"
    [ HU.testCase "Compares a new manifest with the shortcut of its CA" shouldCompareWithShortcut
    , HU.testCase "Decides for a whole fetch" shouldDecideForFetch
    , HU.testCase "Tells which new manifests validation won't use" shouldTellUnusableManifests
    ]


shouldCompareWithShortcut :: HU.Assertion
shouldCompareWithShortcut = do
    let current = mft "D1"
        usable  = ShortcutView (shortcutMeta (atHour 90) (atHour 114)) (Just $ digestOf "D1")
        compareWith = manifestRevalidation Incremental now

    HU.assertEqual "Without shortcuts every manifest counts"
        FullValidation
        (manifestRevalidation FullEveryIteration now current (WithShortcut usable))

    HU.assertEqual "A CA without a shortcut"
        NoShortcut
        (compareWith current WithoutShortcut)

    HU.assertEqual "A CA that no certificate in the cache has the key of"
        UnknownCa
        (compareWith current NoCertificate)

    HU.assertEqual "... with or without shortcuts"
        UnknownCa
        (manifestRevalidation FullEveryIteration now current NoCertificate)

    HU.assertEqual "A shortcut past its validity"
        ShortcutUnusable
        (compareWith current $ WithShortcut $ usable & #shortcut . #notAfter .~ atHour 99)

    HU.assertEqual "A shortcut with its CRL past its validity"
        ShortcutUnusable
        (compareWith current $ WithShortcut $ usable & #shortcut . #crlShortcut . #notAfter .~ atHour 99)

    HU.assertEqual "A shortcut whose manifest is gone from the cache"
        ShortcutManifestGone
        (compareWith current $ WithShortcut $ usable & #digest .~ Nothing)

    HU.assertEqual "Another content"
        ContentChanged
        (compareWith (mft "D2") (WithShortcut usable))

    HU.assertEqual "The same content"
        SameContent
        (compareWith current (WithShortcut usable))

    HU.assertEqual "The same content as a shortcut accepted with warnings"
        SameContent
        (compareWith current $ WithShortcut $ usable & #shortcut . #hasIssues .~ True)

    HU.assertEqual "Only an unknown CA and the same content are no reason"
        [ShortcutUnusable, ShortcutManifestGone, ContentChanged, NoShortcut, FullValidation]
        (filter isReason [UnknownCa, ShortcutUnusable, ShortcutManifestGone, SameContent, ContentChanged, NoShortcut, FullValidation])


shouldDecideForFetch :: HU.Assertion
shouldDecideForFetch = do
    let some = FetchUpdates (StoredManifests [mft "D1"]) False
        decide = fetchRevalidation ServerMode

    HU.assertEqual "In one-off mode every fetch counts"
        RevalidateNow
        (fetchRevalidation (OneOffMode "vrps.csv") mempty [])

    HU.assertEqual "Too many manifests to compare"
        RevalidateNow
        (decide (FetchUpdates ManyManifests False) [])

    HU.assertEqual "A new object other than a manifest or a CRL"
        RevalidateNow
        (decide (FetchUpdates mempty True) [])

    HU.assertEqual "A new object, with manifests that change nothing"
        RevalidateNow
        (decide (some & #newObjects .~ True) [SameContent])

    HU.assertEqual "Nothing to revalidate for"
        NoRevalidation
        (decide some [SameContent, UnknownCa])

    HU.assertEqual "Nothing stored"
        NoRevalidation
        (decide mempty [])

    HU.assertEqual "One manifest that changes something is enough"
        RevalidateNow
        (decide some [SameContent, ContentChanged, UnknownCa])


-- | A manifest that its CA didn't sign, or one that lists objects the cache
-- doesn't have yet, is no reason to revalidate.
shouldTellUnusableManifests :: HU.Assertion
shouldTellUnusableManifests =
    withTestContext $ \appContext -> do
        db <- readTVarIO $ appContext ^. #database
        version <- instantToVersion . unNow <$> thisInstant
        (read_, _) <- runValidatorIO (newScopes "read") $
            forM [ "./test/data/ripe-ncc-ta.cer"
                 , "./test/data/overcleaiming/2R93viIBHX4dV12fmttfjhYOX9k.cer"
                 , "./test/data/ripe-ncc-ta.mft"
                 , "./test/data/afrinic_mft1.mft" ] $ fmap snd . readObjectFromFile
        (taCert, otherCa, taMft, afrinicMft) <- case read_ of
            Right [a, b, c, d] -> pure (a, b, c, d)
            other              -> HU.assertFailure $ "Can't read the fixtures: " <> show other

        let store o = DB.rwTx db $ \tx -> DB.saveObject tx (WellStructuredRO $ toValidatedRpkiObject o) version
        void $ store taCert
        otherCaKey <- store otherCa
        taMftKey      <- store taMft
        afrinicMftKey <- store afrinicMft

        let akiOf o = maybe (HU.assertFailure $ "No AKI: " <> show o) pure $ getAKI o
        taAki      <- akiOf taMft
        afrinicAki <- akiOf afrinicMft
        let unusable ms = DB.roTx db $ \tx -> unusableManifests tx ms
            stored key aki = NewManifest key aki (Hash mempty)
            otherAki = case otherCa of
                CerRO c -> toAKI $ getSKI c
                o       -> error $ "Not a CA certificate: " <> show o

        unusable [stored taMftKey taAki] >>= HU.assertEqual
            "Signed by its CA, waiting for its objects" (UnusableManifests mempty (Set.singleton taMftKey))

        unusable [stored taMftKey otherAki] >>= HU.assertEqual
            "With the AKI of a CA that didn't sign it" (UnusableManifests (Set.singleton taMftKey) mempty)

        unusable [stored afrinicMftKey afrinicAki] >>= HU.assertEqual
            "No certificate of its CA to check it with, waiting for its objects"
            (UnusableManifests mempty (Set.singleton afrinicMftKey))

        let entries = case taMft of
                MftRO m -> getCMSContent (cmsPayload m) ^. #mftEntries
                o       -> error $ "Not a manifest: " <> show o
        forM_ entries $ \MftPair {..} -> DB.rwTx db $ \tx -> void $ DB.saveObject tx
            (OriginalRO (ObjectOriginal "entry") mempty hash ROA) version
        unusable [stored taMftKey taAki] >>= HU.assertEqual "Signed by its CA and complete" mempty

        -- A certificate with the SKI of the CA but another key, which prevalidation
        -- doesn't let into the cache, and found before the one of the CA: the most
        -- recently stored ones come first
        DB.rwTx db $ \(Tx conn) ->
            SQLite.execute conn "UPDATE certificates SET ski = ? WHERE object_key = ?" (SKI $ unAKI taAki, otherCaKey)
        unusable [stored taMftKey taAki] >>= HU.assertEqual
            "Signed by one of the certificates with the SKI of its CA" mempty


now :: Instant
now = atHour 100

atHour :: Int64 -> Instant
atHour hours = Instant (hours * 3600 * nanosPerSecond)

digestOf :: String -> Hash
digestOf = Hash . BSS.pack . map (fromIntegral . fromEnum)

mft :: String -> NewManifest
mft d = NewManifest (ObjectKey $ asKey 1) (AKI $ mkKI "01234567890123456789") (digestOf d)

shortcutMeta :: Instant -> Instant -> DB.MftShortcutMeta
shortcutMeta notBefore notAfter = DB.MftShortcutMeta {
        key            = ObjectKey $ asKey 1,
        notBefore      = notBefore,
        notAfter       = notAfter,
        serial         = Serial 1,
        manifestNumber = Serial 1,
        crlShortcut    = CrlShortcut (ObjectKey $ asKey 2) notBefore notAfter,
        hasIssues      = False
    }

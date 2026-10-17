module RPKI.Workflow.RevalidationSpec where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import qualified Data.ByteString.Short            as BSS
import           Data.Int                         (Int64)

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppMonad
import           RPKI.AppState                    (instantToVersion)
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting                   (newScopes)
import           RPKI.Store.Base.Storable         (Compressed (..), Verbatim (..), toStorable)
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
    , HU.testCase "Tells whether a new manifest is a reason to revalidate" shouldGiveVerdicts
    ]


shouldCompareWithShortcut :: HU.Assertion
shouldCompareWithShortcut = do
    let usable = shortcutMeta (atHour 90) (atHour 114)
        same   = sameAsShortcut (Now now) (mft "D1")

    HU.assertBool "The same content" $
        same usable (Just $ digestOf "D1")

    HU.assertBool "The same content as a shortcut accepted with warnings" $
        same (usable & #hasIssues .~ True) (Just $ digestOf "D1")

    HU.assertBool "Another content" $
        not $ same usable (Just $ digestOf "D2")

    HU.assertBool "A shortcut whose manifest is gone from the cache" $
        not $ same usable Nothing

    HU.assertBool "A shortcut past its validity" $
        not $ same (usable & #notAfter .~ atHour 99) (Just $ digestOf "D1")

    HU.assertBool "A shortcut with its CRL past its validity" $
        not $ same (usable & #crlShortcut . #notAfter .~ atHour 99) (Just $ digestOf "D1")


shouldDecideForFetch :: HU.Assertion
shouldDecideForFetch = do
    let some = FetchUpdates (StoredManifests [mft "D1"]) False
        decide = fetchRevalidation ServerMode

    HU.assertEqual "In one-off mode every fetch counts"
        RevalidateNow
        (fetchRevalidation (OneOffMode "vrps.csv") nothingStored [])

    HU.assertEqual "Too many manifests to compare"
        RevalidateNow
        (decide (FetchUpdates ManyManifests False) [])

    HU.assertEqual "A new object other than a manifest or a CRL"
        RevalidateNow
        (decide (nothingStored & #newObjects .~ True) [])

    HU.assertEqual "A new object, with manifests that change nothing"
        RevalidateNow
        (decide (some & #newObjects .~ True) [SameAsShortcut])

    HU.assertEqual "Nothing to revalidate for"
        NoRevalidation
        (decide some [SameAsShortcut, UnknownCa, NotSignedByCa, WaitingForObjects])

    HU.assertEqual "Nothing stored"
        NoRevalidation
        (decide nothingStored [])

    HU.assertEqual "One manifest that changes something is enough"
        RevalidateNow
        (decide some [SameAsShortcut, Revalidate, UnknownCa])


-- | A manifest is a reason to revalidate unless it lists the same as the
-- shortcut of its CA, there's no certificate of its CA, its CA didn't sign 
-- it, or it lists objects the cache doesn't have yet.
shouldGiveVerdicts :: HU.Assertion
shouldGiveVerdicts =
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
        Just taMftMeta <- DB.roTx db $ \tx -> DB.getMftMetaByKey tx taMftKey
        let verdict algorithm m = DB.roTx db $ \tx -> manifestVerdict tx algorithm (Now now) m
            stored key aki = NewManifest key aki (digestOf "another content")
            sameAsStored   = NewManifest taMftKey taAki taMftMeta.contentDigest
            otherAki = case otherCa of
                CerRO c -> toAKI $ getSKI c
                o       -> error $ "Not a CA certificate: " <> show o
            saveShortcut aki key = DB.rwTx db $ \tx -> DB.saveMftShorcutMeta tx aki
                (Verbatim $ toStorable $ Compressed $ shortcutMeta (atHour 90) (atHour 114) & #key .~ key)

        verdict Incremental (stored taMftKey taAki) >>= HU.assertEqual
            "Signed by its CA, waiting for its objects" WaitingForObjects

        verdict Incremental (stored taMftKey otherAki) >>= HU.assertEqual
            "With the AKI of a CA that didn't sign it" NotSignedByCa

        verdict Incremental (stored afrinicMftKey afrinicAki) >>= HU.assertEqual
            "No certificate of its CA" UnknownCa

        let entries = case taMft of
                MftRO m -> getCMSContent (cmsPayload m) ^. #mftEntries
                o       -> error $ "Not a manifest: " <> show o
        forM_ entries $ \MftPair {..} -> DB.rwTx db $ \tx -> void $ DB.saveObject tx
            (OriginalRO (ObjectOriginal "entry") mempty hash ROA) version
        verdict Incremental (stored taMftKey taAki) >>= HU.assertEqual
            "Signed by its CA and complete" Revalidate

        saveShortcut taAki taMftKey
        verdict Incremental sameAsStored >>= HU.assertEqual
            "The same content as the shortcut of its CA" SameAsShortcut
        verdict Incremental (stored taMftKey taAki) >>= HU.assertEqual
            "Another content than the shortcut of its CA" Revalidate
        verdict FullEveryIteration sameAsStored >>= HU.assertEqual
            "Without shortcuts every manifest counts" Revalidate

        saveShortcut afrinicAki afrinicMftKey
        verdict Incremental (stored afrinicMftKey afrinicAki) >>= HU.assertEqual
            "A shortcut, but no certificate of its CA" UnknownCa

        -- A certificate with the SKI of the CA but another key, which prevalidation
        -- doesn't let into the cache, and found before the one of the CA: the most
        -- recently stored ones come first
        DB.rwTx db $ \(Tx conn) ->
            SQLite.execute conn "UPDATE certificates SET ski = ? WHERE object_key = ?" (SKI $ unAKI taAki, otherCaKey)
        verdict Incremental (stored taMftKey taAki) >>= HU.assertEqual
            "Signed by one of the certificates with the SKI of its CA" Revalidate


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
        hasIssues      = False,
        validatedUnder = digestOf "context"
    }

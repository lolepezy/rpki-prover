module RPKI.Workflow.RevalidationSpec where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad

import qualified Data.ByteString.Short            as BSS
import           Data.Int                         (Int64)
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import           System.Timeout                   (timeout)

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.Config
import           RPKI.Domain
import           RPKI.Logging
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Validation.Types
import           RPKI.Workflow.Revalidation
import           RPKI.Workflow.Shared             (scheduleRevalidationAt)


revalidationSpec :: TestTree
revalidationSpec = testGroup "Revalidation on new manifests"
    [ HU.testCase "Compares a new manifest with the shortcut of its CA" shouldCompareWithShortcut
    , HU.testCase "Decides for a whole fetch" shouldDecideForFetch
    , HU.testCase "Revalidates at every moment it is asked to" shouldRevalidateAtScheduledMoments
    ]


shouldCompareWithShortcut :: HU.Assertion
shouldCompareWithShortcut = do
    let current = mft (atHour 99) "D1"
        future  = mft (atHour 101) "D1"
        usable  = ShortcutView (shortcutMeta (atHour 90) (atHour 114)) (Just $ digestOf "D1")
        compareWith = manifestRevalidation Incremental now

    HU.assertEqual "Without shortcuts every manifest counts"
        (FullValidation, Just now)
        (manifestRevalidation FullEveryIteration now current (Just usable))

    HU.assertEqual "A CA without a shortcut"
        (NoShortcut, Just now)
        (compareWith current Nothing)

    HU.assertEqual "A shortcut past its validity"
        (ShortcutUnusable, Just now)
        (compareWith current $ Just $ usable & #shortcut . #notAfter .~ atHour 99)

    HU.assertEqual "A shortcut with its CRL past its validity"
        (ShortcutUnusable, Just now)
        (compareWith current $ Just $ usable & #shortcut . #crlShortcut . #notAfter .~ atHour 99)

    HU.assertEqual "A shortcut whose manifest is gone from the cache"
        (ShortcutManifestGone, Just now)
        (compareWith current $ Just $ usable & #digest .~ Nothing)

    HU.assertEqual "Another content"
        (ContentChanged, Just now)
        (compareWith (mft (atHour 99) "D2") (Just usable))

    HU.assertEqual "Another content, of a manifest that is current only later"
        (ContentChanged, Just $ atHour 101)
        (compareWith (mft (atHour 101) "D2") (Just usable))

    HU.assertEqual "The same content"
        (SameContent, Nothing)
        (compareWith future (Just usable))

    HU.assertEqual "The same content as a shortcut accepted with warnings"
        (SameContent, Nothing)
        (compareWith current $ Just $ usable & #shortcut . #hasIssues .~ True)


shouldDecideForFetch :: HU.Assertion
shouldDecideForFetch = do
    let some = StoredManifests [mft (atHour 99) "D1"]
        decide = fetchRevalidation ServerMode now

    HU.assertEqual "In one-off mode every fetch counts"
        RevalidateNow
        (fetchRevalidation (OneOffMode "vrps.csv") now mempty [])

    HU.assertEqual "Too many manifests to compare"
        RevalidateNow
        (decide ManyManifests [])

    HU.assertEqual "Nothing to revalidate for"
        NoRevalidation
        (decide some [(SameContent, Nothing)])

    HU.assertEqual "Nothing stored"
        NoRevalidation
        (decide mempty [])

    HU.assertEqual "The earliest moment counts"
        RevalidateNow
        (decide some [(ContentChanged, Just $ atHour 101), (SameContent, Nothing), (NoShortcut, Just now)])

    HU.assertEqual "Only later"
        (RevalidateAt $ atHour 101)
        (decide some [(ContentChanged, Just $ atHour 102), (ContentChanged, Just $ atHour 101)])


-- | Times are generous, only the order of things is checked.
shouldRevalidateAtScheduledMoments :: HU.Assertion
shouldRevalidateAtScheduledMoments =
    withLogger (newLogConfig ErrorL MainLog) $ \logger -> do
        scheduled     <- newTVarIO mempty
        tasToValidate <- newTVarIO mempty
        Now start <- thisInstant
        let ta1 = TaName "ta1"
            ta2 = TaName "ta2"
            schedule tas millis = scheduleRevalidationAt logger scheduled tasToValidate
                                    (Set.fromList tas) (afterMillis start millis)

        schedule [ta1] 1000
        schedule [ta1, ta2] 1500
        -- Earlier than what the TA waits for already
        schedule [ta1] 100

        let requested expected = do
                r <- timeout 5_000_000 $ atomically $ do
                        tas <- readTVar tasToValidate
                        unless (expected `Set.isSubsetOf` tas) retry
                        writeTVar tasToValidate mempty
                        pure tas
                Now t <- thisInstant
                case r of
                    Nothing  -> HU.assertFailure $ "Never revalidated " <> show expected
                    Just tas -> pure (tas, millisBetween start t)

        (tas, elapsed) <- requested (Set.singleton ta1)
        HU.assertEqual "The earlier moment wakes the TA up earlier" (Set.singleton ta1) tas
        HU.assertBool ("Revalidated " <> show elapsed <> "ms after, expected about 100ms") (elapsed < 700)

        (tas', elapsed') <- requested (Set.singleton ta1)
        HU.assertEqual "The later moment is kept" (Set.singleton ta1) tas'
        HU.assertBool ("Revalidated " <> show elapsed' <> "ms after, expected about 1000ms") 
            (elapsed' >= 1000 && elapsed' < 1400)

        (tas'', elapsed'') <- requested (Set.fromList [ta1, ta2])
        HU.assertBool ("Revalidated " <> show elapsed'' <> "ms after, expected about 1500ms") (elapsed'' >= 1500)
        HU.assertEqual "Both TAs at the last moment" (Set.fromList [ta1, ta2]) tas''

        leftOver <- readTVarIO scheduled
        HU.assertEqual "Nothing is waiting anymore" Map.empty leftOver
  where
    afterMillis (Instant t) millis = Instant $ t + millis * 1_000_000
    millisBetween (Instant from_) (Instant to_) = (to_ - from_) `div` 1_000_000


now :: Instant
now = atHour 100

atHour :: Int64 -> Instant
atHour hours = Instant (hours * 3600 * nanosPerSecond)

digestOf :: String -> Hash
digestOf = Hash . BSS.pack . map (fromIntegral . fromEnum)

mft :: Instant -> String -> NewManifest
mft thisTime d = NewManifest (AKI $ mkKI "01234567890123456789") thisTime (digestOf d)

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

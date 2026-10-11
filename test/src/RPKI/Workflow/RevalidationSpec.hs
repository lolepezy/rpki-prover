module RPKI.Workflow.RevalidationSpec where

import           Control.Lens

import qualified Data.ByteString.Short            as BSS
import           Data.Int                         (Int64)

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.Config
import           RPKI.Domain
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Validation.Types
import           RPKI.Workflow.Revalidation


revalidationSpec :: TestTree
revalidationSpec = testGroup "Revalidation on new manifests"
    [ HU.testCase "Compares a new manifest with the shortcut of its CA" shouldCompareWithShortcut
    , HU.testCase "Decides for a whole fetch" shouldDecideForFetch
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

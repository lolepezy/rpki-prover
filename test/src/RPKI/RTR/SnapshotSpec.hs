module RPKI.RTR.SnapshotSpec where

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import           Data.Hourglass                    (Seconds (..))

import           System.FilePath                   ((</>))
import           System.IO.Temp                    (withSystemTempDirectory)

import           RPKI.AppState                     (versionToInstant)
import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Orphans                      ()
import           RPKI.RTR.Protocol
import           RPKI.RTR.RtrServer                (servedPayloads)
import           RPKI.RTR.RtrState
import           RPKI.RTR.Snapshot
import           RPKI.RTR.Types
import           RPKI.Resources.Types
import           RPKI.Time
import           RPKI.Util                         (encodeBase64)

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC


rtrSnapshotGroup :: TestTree
rtrSnapshotGroup = testGroup "Persisted RTR state" [
        QC.testProperty "Should read back the payloads" $
            \vrps aspas ->
                let payloads = makePayloads vrps (routerKeys 3) aspas
                in case decodeSnapshot (encodeSnapshot (Instant 1) (stateOf payloads)) of
                    Left _ -> False
                    Right RtrSnapshot { payloads = restored } ->
                        uniqueVrps restored == uniqueVrps payloads
                            && aspas_ restored == aspas_ payloads
                            && emptyDiffs (evalDiffs payloads restored),

        HU.testCase "Should read back the session" $ do
            case decodeSnapshot (encodeSnapshot (Instant 123) (stateOf $ makePayloads [] (routerKeys 2) [])) of
                Left e -> HU.assertFailure $ Text.unpack e
                Right RtrSnapshot {..} -> do
                    HU.assertEqual "Session" (RtrSessionId 7) sessionId
                    HU.assertEqual "Serial" (SerialNumber 42) serial
                    HU.assertEqual "Version" version worldVersion
                    HU.assertEqual "Written at" (Instant 123) writtenAt,

        HU.testCase "Should not see a change in router keys grouped differently" $ do
            let (k1, k2) = (Set.findMin $ routerKeys 2, Set.findMax $ routerKeys 2)
                -- The same key for two ASNs, as one certificate and as two
                together = k1 { bgpSecAsns = [ASN 1, ASN 2] }
                apart    = Set.fromList [k1 { bgpSecAsns = [ASN 1] }, k1 { bgpSecAsns = [ASN 2] }]
            HU.assertBool "Should be the same" $ emptyDiffs $
                evalDiffs (makePayloads [] (Set.fromList [together, k2]) [])
                          (makePayloads [] (Set.insert k2 apart) []),

        HU.testCase "Should reject damaged state" $ do
            let bytes = LBS.toStrict $ encodeSnapshot (Instant 1)
                            $ stateOf $ makePayloads [] (routerKeys 5) []
                decode = decodeSnapshot . LBS.fromStrict
                flipByte n bs = BS.take n bs <> BS.singleton (BS.index bs n + 1) <> BS.drop (n + 1) bs
            assertFailsWith "not an RTR state file" $ decode $ flipByte 0 bytes
            assertFailsWith "format version" $ decode $ flipByte 9 bytes
            assertFailsWith "Broken RTR state" $ decode $ BS.take (BS.length bytes - 1) bytes
            -- In the compressed PDUs
            case decode $ flipByte (BS.length bytes - 1) bytes of
                Left _  -> pure ()
                Right _ -> HU.assertFailure "Damaged payloads should not be read",

        HU.testCase "Should write and read the file" $
            withSystemTempDirectory "rtr-state" $ \dir -> do
                let path = dir </> snapshotFileName
                HU.assertEqual "No file" Nothing . fmap (const ()) =<< readSnapshot path
                let state = stateOf $ makePayloads [] (routerKeys 2) []
                writeSnapshot path (Instant 1) state
                -- Again, over the existing one
                writeSnapshot path (Instant 2) state { currentSerial = SerialNumber 43 }
                readSnapshot path >>= \case
                    Just (Right RtrSnapshot {..}) -> do
                        HU.assertEqual "Serial" (SerialNumber 43) serial
                        HU.assertEqual "Written at" (Instant 2) writtenAt
                    other -> HU.assertFailure $ "Should be read: " <> show other,

        HU.testCase "Should only use the state within the expire interval" $ do
            let snapshot = RtrSnapshot version (RtrSessionId 1) (SerialNumber 1) (Instant 0) mempty
                Instant v = versionToInstant version
                secondsLater s = Instant $ v + s * nanosPerSecond
            HU.assertBool "Fresh" $ freshEnough (Seconds 7200) (secondsLater 7199) snapshot
            HU.assertBool "Expired" $ not $ freshEnough (Seconds 7200) (secondsLater 7200) snapshot,

        HU.testCase "Should not reuse the session ID of the previous session" $ do
            let sameAsBefore = worldVersionToRtrSessionId version
            HU.assertEqual "Without a previous session" sameAsBefore $ newSessionId version Nothing
            HU.assertBool "With the same previous session" $
                newSessionId version (Just sameAsBefore) /= sameAsBefore,

        QC.testProperty "Should have the VRPs and ASPAs of both in the union" $
            \vrps1 vrps2 aspas1 aspas2 ->
                let before    = makePayloads vrps1 mempty aspas1
                    validated = makePayloads vrps2 mempty aspas2
                    union     = unionPayloads before validated
                    vrpSet p  = Set.fromList $ vrpsToList p.uniqueVrps
                    unionVrps = vrpsToList union.uniqueVrps
                    -- AS0 alone says there are no providers, any provider replaces it
                    providersOf p = [ (c, Set.delete (ASN 0) ps) | Aspa c ps <- Set.toList p.aspas ]
                    hasAspasOf p  = all (\(c, ps) -> any (\(c', ps') -> c == c' && ps `Set.isSubsetOf` ps')
                                                         (providersOf union))
                                        (providersOf p)
                in Set.fromList unionVrps == vrpSet before <> vrpSet validated
                    && length unionVrps == Set.size (Set.fromList unionVrps)
                    && hasAspasOf before && hasAspasOf validated
                    && Set.size (Set.map (.customer) union.aspas) == Set.size union.aspas,

        HU.testCase "Should have a router key in the union once for every ASN" $ do
            let together = routerKeys 2
                apart    = routerKeysPerAsn together
                union    = unionPayloads (makePayloads [] together []) (makePayloads [] apart [])
            HU.assertEqual "One for every ASN" apart union.bgpSec,

        HU.testCase "Should serve the carried over payloads until the validation is settled" $ do
            let before    = makePayloads [] (routerKeys 2) []
                validated = makePayloads [] (routerKeys 3) []
                carried   = CarriedOver (servedPayloads before) version
                Instant v = versionToInstant version
                secondsLater s = Instant $ v + s * nanosPerSecond
                carryOver seconds settled maybeCarried =
                    withCarriedOver (Seconds 7200) (secondsLater seconds) settled maybeCarried validated
            HU.assertEqual "Not settled" (unionPayloads before validated, Just carried) $
                carryOver 60 False (Just carried)
            HU.assertEqual "Settled" (validated, Nothing) $ carryOver 60 True (Just carried)
            HU.assertEqual "Too old" (validated, Nothing) $ carryOver 7200 False (Just carried)
            HU.assertEqual "Nothing carried over" (validated, Nothing) $ carryOver 60 False Nothing,

        HU.testCase "Should persist the version of the carried over payloads" $ do
            let restored = makePayloads [] (routerKeys 2) []
                older    = WorldVersion 1_600_000_000_000_000_000
                state    = (stateOf restored) { carriedOver = Just $ CarriedOver (servedPayloads restored) older }
            case decodeSnapshot (encodeSnapshot (Instant 1) state) of
                Left e -> HU.assertFailure $ Text.unpack e
                Right RtrSnapshot {..} -> HU.assertEqual "Version" older worldVersion
    ]
  where
    version = WorldVersion 1_700_000_000_000_000_000

    stateOf payloads =
        newRtrState version (RtrSessionId 7) (SerialNumber 42) defIntervals (servedPayloads payloads)

    assertFailsWith expected = \case
        Right _ -> HU.assertFailure "Should fail"
        Left e  -> HU.assertBool (Text.unpack e) $ expected `Text.isInfixOf` e

    aspas_ RtrPayloads {..} = aspas


makePayloads :: [Vrp] -> Set.Set BGPSecPayload -> [Aspa] -> RtrPayloads
makePayloads vrps bgpSec aspas =
    RtrPayloads {
        vrps       = mempty,
        uniqueVrps = uniqVrpsPackedBy cmpPacked4 cmpPacked6 $ createVrps vrps,
        bgpSec     = bgpSec,
        aspas      = mergeAspasByCustomer $ Set.fromList aspas
    }

-- | Router keys with SKIs and SPKIs that can be put into PDUs,
-- each of them for two ASNs.
routerKeys :: Int -> Set.Set BGPSecPayload
routerKeys n = Set.fromList [
        BGPSecPayload {
            bgpSecSki  = SKI $ KI $ toShortBS $ BS.replicate 20 (fromIntegral k),
            bgpSecAsns = [ASN $ fromIntegral k, ASN $ fromIntegral k + 1000],
            bgpSecSpki = SPKI $ encodeBase64 $ DecodedBase64 $ BS.replicate 91 (fromIntegral k)
        }
        | k <- [1 .. n] ]

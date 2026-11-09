module RPKI.RTR.SnapshotSpec where

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import           Control.Lens                      ((^.))
import           Data.Foldable                     (toList)
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text
import           Data.Hourglass                    (Seconds (..))

import           System.FilePath                   ((</>))
import           System.IO.Temp                    (withSystemTempDirectory)

import           RPKI.AppState                     (versionToInstant)
import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Domain.Vrps
import           RPKI.Orphans                      ()
import           RPKI.RTR.Pdus
import           RPKI.RTR.Protocol
import           RPKI.RTR.RtrServer                (servedPayloads)
import           RPKI.RTR.RtrState
import           RPKI.RTR.Snapshot
import           RPKI.RTR.Types
import           RPKI.RTR.UniqueVrps
import           RPKI.Resources.Resources          (readIp4, readIp6)
import           RPKI.Resources.Types
import           RPKI.Time
import           RPKI.Util                         (encodeBase64)

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC


rtrSnapshotGroup :: TestTree
rtrSnapshotGroup = testGroup "Persisted RTR state" [
        QC.testProperty "Should render the same bytes as serialising every PDU" $
            \vrps aspas ->
                -- Few ASNs, to have IPv4 and IPv6 VRPs with the same one
                let fewAsns = [ Vrp (ASN $ asn `mod` 5) prefix maxLength
                              | Vrp (ASN asn) prefix maxLength <- vrps ]
                    payloads = makePayloads fewAsns (routerKeys 3) aspas
                in all (\version -> currentCachePayloadBS version payloads == onePduAtATime version payloads)
                       [V0, V1, V2],

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

        HU.testCase "Should read the state of format version 1" $ do
            bytes <- LBS.readFile "test/data/rtr-state/format-1"
            case decodeSnapshot bytes of
                Left e -> HU.assertFailure $ Text.unpack e
                Right RtrSnapshot {..} -> do
                    HU.assertEqual "Session" (RtrSessionId 7) sessionId
                    HU.assertEqual "Serial" (SerialNumber 42) serial
                    HU.assertEqual "Version" version worldVersion
                    HU.assertEqual "Written at" (Instant 1_700_000_060_000_000_000) writtenAt
                    HU.assertEqual "Payloads" format1Payloads payloads
                    HU.assertEqual "Unknown PDUs" 0 unknownPdus
                    HU.assertEqual "No diffs" (Right []) storedDiffs,

        HU.testCase "Should read the state of format version 1 with diffs" $ do
            bytes <- LBS.readFile "test/data/rtr-state/format-1-diffs"
            case decodeSnapshot bytes of
                Left e -> HU.assertFailure $ Text.unpack e
                Right RtrSnapshot {..} -> do
                    HU.assertEqual "Payloads" format1Payloads payloads
                    HU.assertEqual "Diffs" (Right format1Diffs) storedDiffs,

        QC.testProperty "Should read back the stored diffs" $
            \vrps1 vrps2 vrps3 aspas1 aspas2 aspas3 ->
                let state = updatedStates [
                            makePayloads vrps1 (routerKeys 2) aspas1,
                            makePayloads vrps2 (routerKeys 3) aspas2,
                            -- Removing keys in the latest diff, which is always kept
                            makePayloads vrps3 (routerKeys 1) aspas3
                        ]
                in case decodeSnapshot (encodeSnapshot (Instant 1) state) of
                    Left _ -> False
                    Right RtrSnapshot {..} ->
                        storedDiffs == Right (toList $ diffs state),

        HU.testCase "Should give the routers that are behind the diffs after a restart" $ do
            let vrp asn = Vrp (ASN asn) (Ipv4P $ readIp4 "192.0.2.0/24") (PrefixLength 24)
                -- Enough VRPs for both diffs to be kept
                state = updatedStates [
                        makePayloads (map vrp [1])          (routerKeys 1) [],
                        makePayloads (map vrp [1, 2])       (routerKeys 1) [],
                        makePayloads (map vrp [1, 2, 3, 4]) (routerKeys 1) []
                    ]
                expected = diffsFromSerial state (SerialNumber 42)
            HU.assertBool "Has them before the restart" $ expected /= Nothing
            case decodeSnapshot (encodeSnapshot (Instant 1) state) of
                Left e -> HU.assertFailure $ Text.unpack e
                Right RtrSnapshot {..} -> do
                    let restored = withStoredDiffs (either (const []) id storedDiffs) $
                            newRtrState worldVersion sessionId serial defIntervals (servedPayloads payloads)
                    HU.assertEqual "The same diffs" expected $ diffsFromSerial restored (SerialNumber 42)
                    HU.assertEqual "The same size" (totalDiffSize state) (totalDiffSize restored),

        HU.testCase "Should keep the payloads if the diffs are damaged" $ do
            let state = updatedStates [makePayloads [] (routerKeys 1) [], makePayloads [] (routerKeys 2) []]
                bytes = LBS.toStrict $ encodeSnapshot (Instant 1) state
                lastByteChanged = BS.init bytes <> BS.singleton (BS.last bytes + 1)
                useful = \case
                    Right RtrSnapshot {..} -> (emptyDiffs $ evalDiffs (state ^. #served . #payloads) payloads,
                                               either (const False) (const True) storedDiffs)
                    Left _                 -> (False, True)
            HU.assertEqual "Damaged" (True, False) $ useful $ decodeSnapshot $ LBS.fromStrict lastByteChanged
            HU.assertEqual "Cut" (True, False) $ useful $ decodeSnapshot $ LBS.fromStrict $ BS.init bytes,

        HU.testCase "Should not use diffs with unknown PDUs" $ do
            let withUnknown = BS.concat [
                        -- serial
                        BS.pack [0, 0, 0, 43],
                        -- added: one unknown PDU
                        BS.pack [0, 0, 0, 0, 0, 0, 0, 8], unknownPdu 3 99 mempty,
                        -- deleted: nothing
                        BS.replicate 8 0
                    ]
                bytes = LBS.toStrict (encodeSnapshot (Instant 1) (stateOf format1Payloads))
                            <> compressedBlock withUnknown
            case decodeSnapshot (LBS.fromStrict bytes) of
                Left e -> HU.assertFailure $ Text.unpack e
                Right RtrSnapshot {..} -> do
                    HU.assertEqual "Payloads" format1Payloads payloads
                    assertFailsWith "unknown types" storedDiffs,

        HU.testCase "Should skip PDUs of unknown versions and types" $ do
            let rendered = renderedV2 . servedPayloads
                pdus = BS.concat [
                        -- A known type of an unknown version
                        unknownPdu 3 4 (BS.replicate 12 0),
                        rendered format1Payloads { bgpSec = mempty, aspas = mempty },
                        unknownPdu 2 99 mempty,
                        rendered format1Payloads { uniqueVrps = mempty },
                        unknownPdu 3 12 (BS.pack [1, 2, 3, 4])
                    ]
            HU.assertEqual "Payloads" (Right (format1Payloads, 3)) $ payloadsFromPdus pdus,

        HU.testCase "Should not skip PDUs with a broken length" $ do
            let assertFails what bytes = HU.assertBool what $ either (const True) (const False) $ payloadsFromPdus bytes
                ipv4Pdu = renderedV2 $ servedPayloads $ makePayloads [Vrp (ASN 1) (Ipv4P $ readIp4 "192.0.2.0/24") (PrefixLength 24)] mempty []
            -- It would be skipped forever
            assertFails "Shorter than the header" $ BS.pack [3, 12, 0, 0, 0, 0, 0, 0]
            assertFails "Longer than the rest" $ BS.pack [3, 12, 0, 0, 0, 0, 0, 9]
            assertFails "Known one longer than it is" $
                BS.take 7 ipv4Pdu <> BS.singleton 24 <> BS.drop 8 ipv4Pdu <> BS.replicate 4 0,

        HU.testCase "Should only use the state within the expire interval" $ do
            let snapshot = RtrSnapshot version (RtrSessionId 1) (SerialNumber 1) (Instant 0) emptyRtrPayloads 0 (Right [])
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
                    vrpSet p  = Set.fromList $ vrpsToList $ fromUniqueVrps p.uniqueVrps
                    unionVrps = vrpsToList $ fromUniqueVrps union.uniqueVrps
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

    -- A state that went through these payloads, with a diff for every change
    updatedStates = \case
        [] -> error "Needs payloads"
        first : rest -> foldl
            (\state next -> updatedRtrState state version
                                (evalDiffs (state ^. #served . #payloads) next) (servedPayloads next))
            (stateOf first) rest

    assertFailsWith expected = \case
        Right _ -> HU.assertFailure "Should fail"
        Left e  -> HU.assertBool (Text.unpack e) $ expected `Text.isInfixOf` e

    aspas_ RtrPayloads {..} = aspas

    onePduAtATime version RtrPayloads {..} =
        BS.concat
            $ map (\pdu -> LBS.toStrict $ pduToBytes $ VersionedPdu pdu version)
            $ filter (`compatibleWith` version)
            $ map (vrpToPdu Announcement) (rtrOrderedVrps uniqueVrps)
                <> concatMap (bgpSecToPdu Announcement) (Set.toList bgpSec)
                <> map aspaToPdu (Set.toAscList aspas)

    unknownPdu protocolVersion code body = BS.pack [protocolVersion, code, 0, 0, 0, 0, 0, fromIntegral $ 8 + BS.length body] <> body


-- | What test/data/rtr-state/format-1 has. Neither of them may ever change,
-- since files of older versions of rpki-prover must stay readable.
format1Payloads :: RtrPayloads
format1Payloads = makePayloads
    [ Vrp (ASN 64496) (Ipv4P $ readIp4 "192.0.2.0/24") (PrefixLength 24)
    , Vrp (ASN 64496) (Ipv4P $ readIp4 "198.51.100.0/24") (PrefixLength 28)
    , Vrp (ASN 0) (Ipv4P $ readIp4 "203.0.113.0/24") (PrefixLength 24)
    , Vrp (ASN 64497) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 48) ]
    -- The first key is for two ASNs, i.e. in two PDUs
    (Set.fromList [routerKey 1 64496, routerKey 1 64497, routerKey 2 64498])
    [ Aspa (ASN 64496) (Set.fromList [ASN 64497, ASN 64498])
    , Aspa (ASN 64499) (Set.fromList [ASN 0]) ]
  where
    routerKey k asn = BGPSecPayload {
            bgpSecSki  = SKI $ KI $ toShortBS $ BS.replicate 20 k,
            bgpSecAsns = [ASN asn],
            bgpSecSpki = SPKI $ encodeBase64 $ DecodedBase64 $ BS.replicate 91 k
        }

-- | What test/data/rtr-state/format-1-diffs has on top of 'format1Payloads'.
-- Neither of them may ever change either.
format1Diffs :: [(SerialNumber, RtrDiffs)]
format1Diffs = [
        (SerialNumber 42, GenDiffs {
            vrpDiff    = Diff (Set.singleton $ Vrp (ASN 64496) (Ipv4P $ readIp4 "192.0.2.0/24") (PrefixLength 24))
                              (Set.singleton $ Vrp (ASN 64500) (Ipv6P $ readIp6 "2001:db8:1::/48") (PrefixLength 48)),
            bgpSecDiff = Diff mempty
                              (Set.singleton BGPSecPayload {
                                    bgpSecSki  = SKI $ KI $ toShortBS $ BS.replicate 20 3,
                                    bgpSecAsns = [ASN 64500],
                                    bgpSecSpki = SPKI $ encodeBase64 $ DecodedBase64 $ BS.replicate 91 3
                                }),
            aspaDiff   = Diff (Set.singleton $ Aspa (ASN 64496) (Set.fromList [ASN 64497, ASN 64498]))
                              (Set.singleton $ Aspa (ASN 64496) (Set.fromList [ASN 64497]))
        }),
        (SerialNumber 41, GenDiffs {
            vrpDiff    = Diff mempty mempty,
            bgpSecDiff = Diff (Set.singleton BGPSecPayload {
                                    bgpSecSki  = SKI $ KI $ toShortBS $ BS.replicate 20 2,
                                    bgpSecAsns = [ASN 64498],
                                    bgpSecSpki = SPKI $ encodeBase64 $ DecodedBase64 $ BS.replicate 91 2
                                }) mempty,
            aspaDiff   = Diff mempty mempty
        })
    ]

makePayloads :: [Vrp] -> Set.Set BGPSecPayload -> [Aspa] -> RtrPayloads
makePayloads vrps bgpSec aspas =
    RtrPayloads {
        vrps       = mempty,
        uniqueVrps = toUniqueVrps $ createVrps vrps,
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

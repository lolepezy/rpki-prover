module RPKI.RTR.RtrSpec where

import           Control.Monad

import qualified Data.ByteString.Lazy              as LBS
import qualified Data.List                         as List
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import qualified Data.Text                         as Text

import           RPKI.Domain

import           Test.QuickCheck.Arbitrary.Generic
import           Test.Tasty

import           RPKI.AppState
import           RPKI.Orphans
import           RPKI.RTR.Pdus
import           RPKI.RTR.RtrState
import           RPKI.RTR.Protocol

import           RPKI.RTR.RtrServer                (PduLike (..), compatiblePduLike, diffPayloadPdus)
import           RPKI.RTR.Types
import           RPKI.Resources.Types

import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC


rtrGroup :: TestTree
rtrGroup = testGroup "RTR tests" [
        rtrPduParseGroup,
        rtrDiffsGroup,
        rtrStateGroup
    ]


rtrDiffsGroup :: TestTree
rtrDiffsGroup = testGroup "RTR diff unit tests" [
        testEmptyDiff,
        testOneDiff,
        testTwoIndependentDiffs,
        testTwoDependentDiffs,
        testThreeDiffs,
        testGenerateDiffs,
        testMergeAspasByCustomer,
        testAspaDiffPdus
    ]

rtrPduParseGroup :: TestTree 
rtrPduParseGroup = testGroup "RTR PDU parser tests" [
        testParseErrorPdu,
        
        QC.testProperty "Should create, serialise and parse back SerialQueryPdu" 
            $ \session serial protocol -> serialiseAndParseBack protocol 
                $ SerialQueryPdu session serial,
    
        QC.testProperty "Should create, serialise and parse back NotifyPdu" 
            $ \session serial protocol -> serialiseAndParseBack protocol 
                $ NotifyPdu session serial,

        QC.testProperty "Should create, serialise and parse back ResetQueryPdu" 
            $ \protocol -> serialiseAndParseBack protocol ResetQueryPdu,

        QC.testProperty "Should create, serialise and parse back CacheResponsePdu" 
            $ \session protocol -> serialiseAndParseBack protocol $ CacheResponsePdu session,

        QC.testProperty "Should create, serialise and parse back IPv4PrefixPdu" 
            $ \flags prefix asn prefixLength protocol -> serialiseAndParseBack protocol
                $ IPv4PrefixPdu flags prefix asn prefixLength,

        QC.testProperty "Should create, serialise and parse back IPv6PrefixPdu" 
            $ \flags prefix asn prefixLength protocol -> serialiseAndParseBack protocol
                $ IPv6PrefixPdu flags prefix asn prefixLength,

        QC.testProperty "Should create, serialise and parse back EndOfDataPdu in V0" 
            $ \session serial -> serialiseAndParseBack V0
                $ EndOfDataPdu session serial defIntervals,

        QC.testProperty "Should create, serialise and parse back EndOfDataPdu in V1" 
            $ \session serial intervals -> serialiseAndParseBack V1
                $ EndOfDataPdu session serial intervals,

        QC.testProperty "Should create, serialise and parse back CacheResetPdu" 
            $ \protocol -> serialiseAndParseBack protocol CacheResetPdu,                

        QC.testProperty "Should create, serialise and parse back RouterKeyPdu" 
            $ \asn flags ski bs -> serialiseAndParseBack V1
                $ RouterKeyPdu asn flags ski bs,

        QC.testProperty "Should create, serialise and parse back ASPA announcement PDU in V2" 
            $ QC.forAll genValidAspaAnnouncement $ serialiseAndParseBack V2,

        QC.testProperty "Should create, serialise and parse back ASPA withdrawal PDU in V2" 
            $ \customer -> serialiseAndParseBack V2 $ AspaPdu Withdrawal customer [],

        testSerialiseAspaPdu,
        testAspaPduOnlyInV2,
        testRejectInvalidAspaPdus,

        QC.testProperty "Should create, serialise and parse back ErrorPdu" 
            $ \code message brokenPdu protocol -> let 
                message' = if Text.null message then Nothing else Just message
                errorPdu = ErrorPdu code (Just $ pduToBytes $ VersionedPdu brokenPdu protocol) message'
                in serialiseAndParseBack protocol errorPdu
    ]


rtrStateGroup :: TestTree
rtrStateGroup = testGroup "RTR state unit tests" [
        testRtrStateUpdates
    ]

testEmptyDiff :: TestTree
testEmptyDiff = HU.testCase "Should squash one diff properly" $    
    HU.assertEqual "It's a bummer" newRtrDiff $ squashDiffs []

testOneDiff :: TestTree
testOneDiff = HU.testCase "Should squash one diff" $ do
    let diff :: GenDiffs Int Int Int = mkNewDiff [1,2] [3,4]
    HU.assertEqual "It's a bummer"                 
        diff
        $ squash [diff]

testTwoIndependentDiffs :: TestTree
testTwoIndependentDiffs = HU.testCase "Should squash two unrelated diffs" $
    HU.assertEqual "It's a bummer"                 
        (mkNewDiff [1, 2, 10, 20] [3, 4, 30] :: GenDiffs Int Int Int)
        $ squash [
            mkNewDiff [1,2] [3,4],
            mkNewDiff [10,20] [30]
        ]     

testTwoDependentDiffs :: TestTree
testTwoDependentDiffs = HU.testCase "Should squash two related diffs" $
    HU.assertEqual "It's a bummer"                             
        (mkNewDiff [1, 4, 5] [2, 3] :: GenDiffs Int Int Int)
        $ squash [
            mkNewDiff [1,2] [3,4],
            mkNewDiff [4,5] [2]            
        ]        

testThreeDiffs :: TestTree
testThreeDiffs = HU.testCase "Should squash three diffs properly" $     
    HU.assertEqual "It's a bummer"                 
        (mkNewDiff [2, 3, 4, 5] [1, 6] :: GenDiffs Int Int Int)
        $ squash [
            mkNewDiff [1,2] [3,4],
            mkNewDiff [4,5] [2],            
            mkNewDiff [2,3,4] [6,1]    
        ]


squash :: (Ord a, Ord b, Ord c) => [GenDiffs a b c] -> GenDiffs a b c
squash diffs = squashDiffs $ map (\(i, d) -> (SerialNumber i, d)) $ zip [1..] diffs

mkNewDiff :: (Ord a, Ord b, Ord c) => [a] -> [a] -> GenDiffs a b c
mkNewDiff added deleted = 
    GenDiffs {
        vrpDiff = Diff { 
                added = Set.fromList added, 
                deleted = Set.fromList deleted
            },
        bgpSecDiff = newDiff,
        aspaDiff = newDiff
    }


mkNewGenDiff :: Ord a => [a] -> [a] -> Diff a
mkNewGenDiff added deleted = Diff { 
        added = Set.fromList added, 
        deleted = Set.fromList deleted
    }



testGenerateDiffs :: TestTree
testGenerateDiffs = HU.testCase "Should generate correct VRP diffs" $ do                
    vrps1 <- generateVrps 10
    vrps2 <- generateVrps 5
    vrps3 <- generateVrps 15

    let diff1 = setDiff (vrps1 <> vrps2) vrps1

    HU.assertEqual "Wrong deleted diff" (added diff1) Set.empty
    HU.assertEqual "Wrong deleted diff 2" (deleted diff1) vrps2

    let diff2 = setDiff (vrps1 <> vrps2) (vrps1 <> vrps3)

    HU.assertEqual "Wrong mixed diff" (added diff2) vrps3
    HU.assertEqual "Wrong mixed diff 2" (deleted diff2) vrps2
    


testParseErrorPdu :: TestTree
testParseErrorPdu = HU.testCase "Should parse Error PDU from rtrclient program" $ do    
    let bytes = "\SOH\n\NUL\ACK\NUL\NUL\NUL$\NUL\NUL\NUL\DC4\SOH\EOT\NUL\NUL\NUL\NUL\NUL\DC4\NUL\NAK\NAK\NUL\199\253\128\NUL\NUL\NUL\NUL\209\NUL\NUL\NUL\NUL"
    HU.assertEqual 
        "Couldn't parse Error PDU properly"
        (Right (VersionedPdu (ErrorPdu WithdrawalOfUnknownRecord (Just "\SOH\EOT\NUL\NUL\NUL\NUL\NUL\DC4\NUL\NAK\NAK\NUL\199\253\128\NUL\NUL\NUL\NUL\209") Nothing) V1))
        (bytesToVersionedPdu bytes)    


genValidAspaAnnouncement :: QC.Gen Pdu
genValidAspaAnnouncement = do 
    customer  <- arbitrary
    providers <- QC.listOf1 arbitrary
    let providers' = Set.toAscList $ Set.fromList providers
    -- AS0 is only allowed as the sole provider
    pure $ AspaPdu Announcement customer $ 
        case providers' of 
            [_] -> providers'
            _   -> filter (/= ASN 0) providers'
                    <> [ASN 1 | all (== ASN 0) providers']

testSerialiseAspaPdu :: TestTree
testSerialiseAspaPdu = HU.testCase "Should serialise ASPA PDUs exactly as in the draft" $ do
    HU.assertEqual "Wrong announcement bytes"
        (LBS.pack [2, 11, 1, 0,  0, 0, 0, 20,  0, 0, 0xFD, 0xE8,  0, 0, 0, 1,  0, 0, 0, 2])
        (pduToBytes $ VersionedPdu (AspaPdu Announcement (ASN 65000) [ASN 1, ASN 2]) V2)
    HU.assertEqual "Wrong withdrawal bytes"
        (LBS.pack [2, 11, 0, 0,  0, 0, 0, 12,  0, 0, 0xFD, 0xE8])
        (pduToBytes $ VersionedPdu (AspaPdu Withdrawal (ASN 65000) []) V2)

testAspaPduOnlyInV2 :: TestTree
testAspaPduOnlyInV2 = HU.testCase "ASPA PDU should only be sent to V2 clients" $ do
    let pdu = AspaPdu Announcement (ASN 1) [ASN 2]
    HU.assertBool "V0" $ not $ compatibleWith pdu V0
    HU.assertBool "V1" $ not $ compatibleWith pdu V1
    HU.assertBool "V2" $ compatibleWith pdu V2
    -- ... and the same decision taken from the version a PDU is framed with,
    -- which is how the server filters what it puts on a connection
    HU.assertBool "Framed V0" $ not $ compatiblePduLike $ TruePdu $ VersionedPdu pdu V0
    HU.assertBool "Framed V1" $ not $ compatiblePduLike $ TruePdu $ VersionedPdu pdu V1
    HU.assertBool "Framed V2" $ compatiblePduLike $ TruePdu $ VersionedPdu pdu V2
    -- and it's not accepted if it comes in V1 
    HU.assertBool "Parsed in V1" $ 
        either (const True) (const False) $ bytesToVersionedPdu $ 
            LBS.pack [1, 11, 1, 0,  0, 0, 0, 16,  0, 0, 0, 1,  0, 0, 0, 2]

testRejectInvalidAspaPdus :: TestTree
testRejectInvalidAspaPdus = HU.testCase "Should not parse invalid ASPA PDUs" $ do
    let parses pdu = either (const False) (const True) $ 
                        bytesToVersionedPdu $ pduToBytes $ VersionedPdu pdu V2
    HU.assertBool "Announcement without providers" $ 
        not $ parses $ AspaPdu Announcement (ASN 1) []
    HU.assertBool "Withdrawal with providers" $ 
        not $ parses $ AspaPdu Withdrawal (ASN 1) [ASN 2]
    HU.assertBool "AS0 with other providers" $ 
        not $ parses $ AspaPdu Announcement (ASN 1) [ASN 0, ASN 2]
    HU.assertBool "Not ascending providers" $ 
        not $ parses $ AspaPdu Announcement (ASN 1) [ASN 3, ASN 2]
    HU.assertBool "Duplicate providers" $ 
        not $ parses $ AspaPdu Announcement (ASN 1) [ASN 2, ASN 2]
    HU.assertBool "AS0 alone is fine" $ 
        parses $ AspaPdu Announcement (ASN 1) [ASN 0]

testMergeAspasByCustomer :: TestTree
testMergeAspasByCustomer = HU.testCase "Should have one ASPA per customer" $ do
    let aspa c ps = Aspa (ASN c) (Set.fromList $ map ASN ps)
    HU.assertEqual "Wrong merge" 
        (Set.fromList [aspa 1 [2, 3, 4], aspa 5 [0], aspa 6 [7]])
        (mergeAspasByCustomer $ Set.fromList [
            aspa 1 [2, 3], aspa 1 [3, 4], 
            aspa 5 [0], 
            aspa 6 [0, 7], 
            aspa 8 []])

testAspaDiffPdus :: TestTree
testAspaDiffPdus = HU.testCase "Should generate ASPA PDUs for a diff" $ do
    let aspa c ps = Aspa (ASN c) (Set.fromList $ map ASN ps)
    let diff = newRtrDiff { 
            aspaDiff = Diff {
                -- 1 has changed providers, 2 is new, 3 and 4 are gone
                added   = Set.fromList [aspa 2 [10], aspa 1 [5, 6]],
                deleted = Set.fromList [aspa 4 [7], aspa 1 [5], aspa 3 [8], aspa 3 [9]]
            }
        }
    HU.assertEqual "Wrong PDUs" 
        [ TruePdu $ VersionedPdu (AspaPdu Announcement (ASN 1) [ASN 5, ASN 6]) V2
        , TruePdu $ VersionedPdu (AspaPdu Announcement (ASN 2) [ASN 10]) V2
        , TruePdu $ VersionedPdu (AspaPdu Withdrawal (ASN 3) []) V2
        , TruePdu $ VersionedPdu (AspaPdu Withdrawal (ASN 4) []) V2
        ]
        (diffPayloadPdus V2 diff)

    -- The same diff for a V1 session yields PDUs the send loop drops
    HU.assertEqual "V1 should not get ASPA PDUs"
        []
        (filter compatiblePduLike $ diffPayloadPdus V1 diff)

serialiseAndParseBack :: ProtocolVersion -> Pdu -> Bool
serialiseAndParseBack protocolVersion pdu =     
    let versionedPdu = VersionedPdu pdu protocolVersion
        parsed = bytesToVersionedPdu $ pduToBytes versionedPdu
        in parsed == Right versionedPdu


testRtrStateUpdates :: TestTree
testRtrStateUpdates = HU.testCase "Should update RTR state and shrink it when needed" $ do    
    appState <- newAppState

    let update rtrState n m = do 
            newVersion <- getOrCreateWorldVerion appState
            vrpDiff <- Diff <$> generateVrps n <*> generateVrps m
            bgpSecDiff <- Diff <$> generateBgpSecs n <*> generateBgpSecs m
            pure $! updatedRtrState rtrState newVersion GenDiffs {aspaDiff = newDiff, ..}
    
    worldVersion <- getOrCreateWorldVerion appState
    let z = newRtrState worldVersion 10
    let rtrState = z { maxSerialsPerSession = 2, maxTotalDiffSize = 80 }

    rtrState1 <- update rtrState 10 1
    HU.assertEqual "There should be one diff" 1 (List.length $ diffs rtrState1)    

    rtrState2 <- update rtrState1 15 1
    HU.assertEqual "There should be two diffs" 2 (List.length $ diffs rtrState2)

    rtrState3 <- update rtrState2 12 1
    HU.assertEqual "There should be still two diffs" 2 (List.length $ diffs rtrState3)

    -- Add a big one to force eviction of everything that was already there
    rtrState4 <- update rtrState3 50 2

    HU.assertEqual "There should be only one big diff" 1 (List.length $ diffs rtrState4)

    -- ASPAs count towards the size of the diffs as well
    aspas1 <- generateAspas 10
    aspas2 <- generateAspas 5
    version <- getOrCreateWorldVerion appState
    let withAspas = updatedRtrState (newRtrState version 10) version 
                        newRtrDiff { aspaDiff = Diff aspas1 aspas2 }
    HU.assertEqual "Wrong total size" (Set.size aspas1 + Set.size aspas2) (totalDiffSize withAspas)


-- rtrToStr RtrState {..} = 
--     "[currentSerial = " <> show currentSerial 
--         <> ", earliestSerial = " <> show earliestSerial
--         <> ", lastKnownWorldVersion = " <> show lastKnownWorldVersion
--         <> ", currentSessionId = " <> show currentSessionId
--         <> ", maxSerialsPerSession = " <> show maxSerialsPerSession <> "]"

generateVrps :: Int -> IO (Set Vrp)
generateVrps n = Set.fromList <$> replicateM n (QC.generate arbitrary)

generateBgpSecs :: Int -> IO (Set BGPSecPayload)
generateBgpSecs n = Set.fromList <$> replicateM n (QC.generate arbitrary)

generateAspas :: Int -> IO (Set Aspa)
generateAspas n = Set.fromList <$> replicateM n (QC.generate arbitrary)

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrSpec where

import           Control.Monad

import qualified Data.ByteString.Lazy     as LBS

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

import           RPKI.Resources.Types

import           Test.QuickCheck.Monadic
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
        testGenerateDiffs                
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

        QC.testProperty "Should create, serialise and parse back ErrorPdu" 
            $ \code message brokenPdu protocol -> let 
                message' = if Text.null message then Nothing else Just message
                errorPdu = ErrorPdu code (Just $ pduToBytes brokenPdu protocol) message'
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
    let diff :: GenDiffs Int Int = mkNewDiff [1,2] [3,4]
    HU.assertEqual "It's a bummer"                 
        diff
        $ squash [diff]

testTwoIndependentDiffs :: TestTree
testTwoIndependentDiffs = HU.testCase "Should squash two unrelated diffs" $
    HU.assertEqual "It's a bummer"                 
        (mkNewDiff [1, 2, 10, 20] [3, 4, 30] :: GenDiffs Int Int)
        $ squash [
            mkNewDiff [1,2] [3,4],
            mkNewDiff [10,20] [30]
        ]     

testTwoDependentDiffs :: TestTree
testTwoDependentDiffs = HU.testCase "Should squash two related diffs" $
    HU.assertEqual "It's a bummer"                             
        (mkNewDiff [1, 4, 5] [2, 3] :: GenDiffs Int Int)
        $ squash [
            mkNewDiff [1,2] [3,4],
            mkNewDiff [4,5] [2]            
        ]        

testThreeDiffs :: TestTree
testThreeDiffs = HU.testCase "Should squash three diffs properly" $     
    HU.assertEqual "It's a bummer"                 
        (mkNewDiff [2, 3, 4, 5] [1, 6] :: GenDiffs Int Int)
        $ squash [
            mkNewDiff [1,2] [3,4],
            mkNewDiff [4,5] [2],            
            mkNewDiff [2,3,4] [6,1]    
        ]


squash :: (Ord a, Ord b) => [GenDiffs a b] -> GenDiffs a b
squash diffs = squashDiffs $ map (\(i, d) -> (SerialNumber i, d)) $ zip [1..] diffs

mkNewDiff :: (Ord a, Ord b) => [a] -> [a] -> GenDiffs a b
mkNewDiff added deleted = 
    GenDiffs {
        vrpDiff = Diff { 
                added = Set.fromList added, 
                deleted = Set.fromList deleted
            },
        bgpSecDiff = newDiff
    }


mkNewGenDiff :: Ord a => [a] -> [a] -> Diff a
mkNewGenDiff added deleted = Diff { 
        added = Set.fromList added, 
        deleted = Set.fromList deleted
    }


-- testRespond :: TestTree
-- testRespond = HU.testCase "Should insert and get a repository" $ do    
--     appState <- newAppState
--     z <- newRtrState =<< getWorldVerionIO appState

--     let rtrState = z
    
--     vrps :: [Vrp] <- replicateM 5 $ QC.generate arbitrary

--     let pdu = SerialQueryPdu (currentSessionId rtrState) (currentSerial rtrState)

--     let response = respondToPdu 
--                         (Just rtrState) 
--                         vrps 
--                         (VersionedPdu pdu V1)
--                         (pduToBytes pdu V1)
--                         (Session V1)

--     putStrLn $ "response = " <> show response 

--     pure ()


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


serialiseAndParseBack :: ProtocolVersion -> Pdu -> Bool
serialiseAndParseBack protocolVersion pdu =     
    let bytes = pduToBytes pdu protocolVersion
        parsed = bytesToVersionedPdu bytes
        in parsed == Right (VersionedPdu pdu protocolVersion)


testRtrStateUpdates :: TestTree
testRtrStateUpdates = HU.testCase "Should update RTR state and shrink it when needed" $ do    
    appState <- newAppState

    let update rtrState n m = do 
            newVersion <- getOrCreateWorldVerion appState
            vrpDiff <- Diff <$> generateVrps n <*> generateVrps m
            bgpSecDiff <- Diff <$> generateBgpSecs n <*> generateBgpSecs m
            pure $! updatedRtrState rtrState newVersion GenDiffs {..}
    
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
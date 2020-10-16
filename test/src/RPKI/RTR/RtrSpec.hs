{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrSpec where

import           Control.Monad
import           Data.ByteString.Short             as BSS
import           Data.ByteString.Base16 as Hex

import qualified Data.Set                          as Set
import           RPKI.Domain
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.Tasty

import           Data.String.Interpolate

import           RPKI.Util                         (convert)

import           RPKI.AppState
import           RPKI.Orphans
import           RPKI.RTR.Pdus
import           RPKI.RTR.RtrState
import           RPKI.RTR.RtrServer
import           RPKI.RTR.Types

import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC


rtrGroup :: TestTree
rtrGroup = testGroup "RTR tests" [
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
        testParseErrorPdu
    ]

rtrStateGroup :: TestTree
rtrStateGroup = testGroup "RTR state unit tests" [
        
    ]

testEmptyDiff :: TestTree
testEmptyDiff = HU.testCase "Should squash one diff properly" $    
    HU.assertEqual "It's a bummer"                 
        (Diff mempty mempty)
        $ squashDiffs ([] :: [(SerialNumber, Diff Int)])

testOneDiff :: TestTree
testOneDiff = HU.testCase "Should squash one diff" $ do
    let diff = newDiff [1,2] [3,4]        
    HU.assertEqual "It's a bummer"                 
        diff
        $ squash [diff]

testTwoIndependentDiffs :: TestTree
testTwoIndependentDiffs = HU.testCase "Should squash two unrelated diffs" $
    HU.assertEqual "It's a bummer"                 
        (newDiff [1, 2, 10, 20] [3, 4, 30])
        $ squash [
            newDiff [1,2] [3,4],
            newDiff [10,20] [30]
        ]     

testTwoDependentDiffs :: TestTree
testTwoDependentDiffs = HU.testCase "Should squash two related diffs" $
    HU.assertEqual "It's a bummer"                             
        (newDiff [1, 4, 5] [2, 3])
        $ squash [
            newDiff [1,2] [3,4],
            newDiff [4,5] [2]            
        ]        

testThreeDiffs :: TestTree
testThreeDiffs = HU.testCase "Should squash three diffs properly" $     
    HU.assertEqual "It's a bummer"                 
        (newDiff [2, 3, 4, 5] [1, 6])
        $ squash [
            newDiff [1,2] [3,4],
            newDiff [4,5] [2],            
            newDiff [2,3,4] [6,1]    
        ]


squash :: Ord a => [Diff a] -> Diff a
squash diffs = squashDiffs $ map (\(i, d) -> (SerialNumber i, d)) $ zip [1..] diffs

newDiff :: Ord a => [a] -> [a] -> Diff a
newDiff added deleted = Diff { 
        added = added, 
        deleted = deleted
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
    vrps1 :: [Vrp] <- replicateM 10 $ QC.generate arbitrary    
    vrps2 :: [Vrp] <- replicateM 5 $ QC.generate arbitrary        
    vrps3 :: [Vrp] <- replicateM 15 $ QC.generate arbitrary

    let diff1 = evalVrpDiff (vrps1 <> vrps2) vrps1

    HU.assertEqual "Wrong deleted diff" (Set.fromList $ added diff1) Set.empty
    HU.assertEqual "Wrong deleted diff 2" (Set.fromList $ deleted diff1) (Set.fromList vrps2)    

    let diff2 = evalVrpDiff (vrps1 <> vrps2) (vrps1 <> vrps3)

    HU.assertEqual "Wrong mixed diff" (Set.fromList $ added diff2) (Set.fromList vrps3)
    HU.assertEqual "Wrong mixed diff 2" (Set.fromList $ deleted diff2) (Set.fromList vrps2)    
    

testParseErrorPdu :: TestTree
testParseErrorPdu = HU.testCase "Should parse Error PDU from rtrclient program" $ do    
    let bytes = "\SOH\n\NUL\ACK\NUL\NUL\NUL$\NUL\NUL\NUL\DC4\SOH\EOT\NUL\NUL\NUL\NUL\NUL\DC4\NUL\NAK\NAK\NUL\199\253\128\NUL\NUL\NUL\NUL\209\NUL\NUL\NUL\NUL"
    HU.assertEqual 
        "Couldn't parse Error PDU properly"
        (Right (VersionedPdu (ErrorPdu WithdrawalOfUnknownRecord (Just "\SOH\EOT\NUL\NUL\NUL\NUL\NUL\DC4\NUL\NAK\NAK\NUL\199\253\128\NUL\NUL\NUL\NUL\209") Nothing) V1))
        (bytesToVersionedPdu bytes)    



-- testRtrStateUpdates :: TestTree
-- testRtrStateUpdates = HU.testCase "Should insert and get a repository" $ do    
--     appState <- newAppState
--     z <- newRtrState =<< getWorldVerionIO appState
--     let rtrState = z { maxSerialsPerSession = 2 }

--     newVersion <- updateWorldVerion appState

--     diffs :: [VrpDiff] <- replicateM 5 $ QC.generate arbitrary

--     let serial0 = currentSerial rtrState

--     let rtrState1 = updatedRtrState rtrState newVersion (head diffs)
--     HU.assertEqual "It's a bummer 1" (nextSerial serial0) (currentSerial rtrState1)
--     HU.assertEqual "It's a bummer 2" serial0 (earliestSerial rtrState1)

--     let rtrState2 = updatedRtrState rtrState1 newVersion (diffs !! 1)
--     HU.assertEqual "It's a bummer 3" (nextSerial $ nextSerial serial0) (currentSerial rtrState2)
--     HU.assertEqual "It's a bummer 4" serial0 (earliestSerial rtrState2)


--     let rtrState3 = updatedRtrState rtrState2 newVersion (diffs !! 2)

--     putStrLn $ "rtrState = " <> rtrToStr rtrState 
--                 <> ", rtrState1 = " <> rtrToStr rtrState1 
--                 <> ", rtrState2 = " <> rtrToStr rtrState2
--                 <> ", rtrState3 = " <> rtrToStr rtrState3

--     HU.assertEqual "It's a bummer 5" (nextSerial $ nextSerial $ nextSerial serial0) (currentSerial rtrState3)
--     HU.assertEqual "It's a bummer 6" serial0 (earliestSerial rtrState3)
    


    

--     HU.assertEqual "It's a bummer" 1 1


-- rtrToStr RtrState {..} = 
--     "[currentSerial = " <> show currentSerial 
--         <> ", earliestSerial = " <> show earliestSerial
--         <> ", lastKnownWorldVersion = " <> show lastKnownWorldVersion
--         <> ", currentSessionId = " <> show currentSessionId
--         <> ", maxSerialsPerSession = " <> show maxSerialsPerSession <> "]"

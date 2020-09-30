{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.RTR.RtrSpec where

import           Data.ByteString.Short             as BSS
import           Data.Hex                          (hex)

import qualified Data.Set as Set
import           RPKI.Domain
import           RPKI.RRDP.Parse
import           RPKI.RRDP.Types

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.Tasty

import           Data.String.Interpolate

import           RPKI.Util                         (convert)

import           RPKI.Orphans
import           RPKI.RTR.Types
import           RPKI.RTR.RtrState
import           RPKI.RTR.Pdus

import qualified Test.Tasty.HUnit        as HU


rtrGroup :: TestTree
rtrGroup = testGroup "RTR tests" [
        rtrContextGroup
    ]


rtrContextGroup :: TestTree
rtrContextGroup = testGroup "RTR Context unit tests" [
        testEmptyDiff,
        testOneDiff,
        testTwoIndependentDiffs,
        testTwoDependentDiffs,
        testThreeDiffs
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
        added = Set.fromList added, 
        deleted = Set.fromList deleted
    }

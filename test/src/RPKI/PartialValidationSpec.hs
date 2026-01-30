{-# LANGUAGE OverloadedStrings #-}

module RPKI.PartialValidationSpec where

import Control.Monad (replicateM)

import Data.Text (Text)
import Data.Maybe (catMaybes, isJust)
import Data.List (sort, isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import GHC.Generics

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.QuickCheck             as QC
import qualified Test.Tasty.HUnit                  as HU

import           Test.QuickCheck.Gen

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Util
import           RPKI.Orphans
import           RPKI.Validation.Partial


partialValidationSpec :: TestTree
partialValidationSpec = testGroup "Partial validation" [
        HU.testCase "Find parent simple" shouldFindParent,
        HU.testCase "Find only parent" shouldFindOnlyParent,
        HU.testCase "Find parent in longer chain" shouldFindParentLongerChains,
        HU.testCase "Should find start CAs" shouldFindStartCasSimple
    ]


shouldFindParent :: HU.Assertion
shouldFindParent = do    
    let cache = newCache [ ("a", "parent", 1), ("parent", "parent", 2) ]
    let Just kimA = Map.lookup "a" cache

    let testIt = findPathUp (\ki -> pure $ Map.lookup ki cache) (const True) ("a", kimA)

    Just (paths, ignored) <- testIt (Set.fromList [1])
    HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2]) paths
    HU.assertBool "Nothing is ignored" (Set.null ignored)


shouldFindOnlyParent :: HU.Assertion
shouldFindOnlyParent = do
    let cache = newCache [ ("a", "parent", 1), ("parent", "parent", 2) ]
    let Just kimA = Map.lookup "a" cache
                     
    let testIt = findPathUp (\ki -> pure $ Map.lookup ki cache) (const True) ("a", kimA)

    Just (paths, ignored) <- testIt (Set.fromList [1, 2])
    HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2]) paths
    HU.assertEqual "Child should be ignored" (Set.fromList [1]) ignored    


shouldFindParentLongerChains :: HU.Assertion
shouldFindParentLongerChains = do
    let cache = newCache [ ("a", "b", 1), ("b", "parent", 2), ("parent", "parent", 3) ]
    let Just kimA = Map.lookup "a" cache

    let testIt = findPathUp (\ki -> pure $ Map.lookup ki cache) (const True) ("a", kimA)

    do 
        Just (paths, ignored) <- testIt (Set.fromList [1])
        HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2, 3]) paths
        HU.assertEqual "Children should be ignored" mempty ignored    

    do 
        Just (paths, ignored) <- testIt (Set.fromList [1, 2])
        HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2, 3]) paths
        HU.assertEqual "Children should be ignored" (Set.fromList [1]) ignored    

    do 
        Just (paths, ignored) <- testIt (Set.fromList [1, 2, 3])
        HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2, 3]) paths
        HU.assertEqual "Children should be ignored" (Set.fromList [1, 2]) ignored    


shouldFindStartCasSimple :: HU.Assertion
shouldFindStartCasSimple = do
    let cache = newCache [ ("a", "b", 1), ("b", "parent", 2), ("parent", "parent", 3) ]    
    let testIt = findStartCas (\ki -> pure $ Map.lookup ki cache) (const True)

    do 
        (startCas, paths) <- testIt [TestAdded 5 "a"]
        HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2, 3, 5]) paths
        HU.assertEqual "CAs to validate" (Set.fromList [1]) startCas

    do
        (startCas, paths) <- testIt [TestAdded 5 "a", TestAdded 10 "b"]
        HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2, 3, 5, 10]) paths
        HU.assertEqual "CAs to validate" (Set.fromList [2]) startCas        

    do
        (startCas, paths) <- testIt [TestAdded 5 "a", TestAdded 10 "b", TestAdded 20 "parent"]
        HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2, 3, 5, 10, 20]) paths
        HU.assertEqual "CAs to validate" (Set.fromList [3]) startCas                    



newCache :: [(Text, Text, Int)] -> Map.Map Text TestKIMeta
newCache metas = Map.fromList [ (ki, TestKIMeta { parentKI = pki, caCertificate = ca }) | (ki, pki, ca) <- metas ]


data TestKIMeta = TestKIMeta {
    parentKI      :: Text,
    caCertificate :: Int
} deriving (Eq, Show, Generic)

data TestAdded = TestAdded {
    objectKey :: Int,
    ki        :: Text  
} deriving (Eq, Show, Generic)

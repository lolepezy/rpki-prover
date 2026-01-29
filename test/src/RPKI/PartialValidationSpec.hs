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
        HU.testCase "Find only parent" shouldFindOnlyParent
    ]

data TestKIMeta = TestKIMeta {
    parentKI      :: Text,
    caCertificate :: Int
} deriving (Eq, Show, Generic)

shouldFindParent :: HU.Assertion
shouldFindParent = do    
    let cache = newCache [ ("a", "p", 1), ("p", "p", 2) ]
    let Just kimA = Map.lookup "a" cache

    let readFromCache ki = pure $ Map.lookup ki cache
    let accept _ = True
    let startCas = Set.fromList [1]

    Just (paths, ignored) <- findPathUp readFromCache accept ("a", kimA) startCas    
    HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2]) paths
    HU.assertBool "Nothing is ignored" (Set.null ignored)


shouldFindOnlyParent :: HU.Assertion
shouldFindOnlyParent = do
    let cache = newCache [ ("a", "p", 1), ("p", "p", 2) ]
    let Just kimA = Map.lookup "a" cache
                     
    let readFromCache ki = pure $ Map.lookup ki cache
    let accept _ = True
    let startCas = Set.fromList [1, 2]

    Just (paths, ignored) <- findPathUp readFromCache accept ("a", kimA) startCas    
    HU.assertEqual "paths should contain lead to the TA" (Set.fromList [1, 2]) paths
    HU.assertEqual "Child should be ignored" (Set.fromList [1]) ignored    


newCache :: [(Text, Text, Int)] -> Map.Map Text TestKIMeta
newCache metas = Map.fromList [ (ki, TestKIMeta { parentKI = pki, caCertificate = ca }) | (ki, pki, ca) <- metas ]
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
    caCertificate :: Text
} deriving (Eq, Show, Generic)

shouldFindParent :: HU.Assertion
shouldFindParent = do
    let kimA = TestKIMeta { parentKI = "p", caCertificate = "certC" }
    let kimP = TestKIMeta { parentKI = "p", caCertificate = "certP" }
    let cache = Map.fromList [ ("a", kimA), ("p", kimP) ]
                     
    let readFromCache ki = pure $ Map.lookup ki cache
    let accept _ = True
    let startCas = Set.fromList ["certC"]

    Just (involved, ignored) <- findPathUp readFromCache accept ("a", kimA) startCas    
    HU.assertEqual "Involved should contain child only" (Set.fromList ["certC"]) involved
    HU.assertBool "Nothing is ignored" (Set.null ignored)


shouldFindOnlyParent :: HU.Assertion
shouldFindOnlyParent = do
    let kimA = TestKIMeta { parentKI = "p", caCertificate = "certC" }
    let kimP = TestKIMeta { parentKI = "p", caCertificate = "certP" }
    let cache = Map.fromList [ ("a", kimA), ("p", kimP) ]
                     
    let readFromCache ki = pure $ Map.lookup ki cache
    let accept _ = True
    let startCas = Set.fromList ["certC", "certP"]

    Just (involved, ignored) <- findPathUp readFromCache accept ("a", kimA) startCas    
    HU.assertEqual "Involved should contain both parent and child" (Set.fromList ["certC", "certP"]) involved    
    HU.assertEqual "Child should be ignored" (Set.fromList ["certC"]) ignored    


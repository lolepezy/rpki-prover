{-# LANGUAGE OverloadedStrings #-}

module RPKI.PartialValidationSpec where

import Control.Monad (replicateM)

import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.List (sort, isPrefixOf, sortOn)
import qualified Data.Map as Map

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.QuickCheck             as QC
import qualified Test.Tasty.HUnit                  as HU

import           Test.QuickCheck.Gen

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Util
import           RPKI.Orphans


partialValidationGroup :: TestTree
partialValidationGroup = testGroup "Partial validation" [
        HU.testCase "Find parent simple" shouldFindSimpleParent
    ]

data TestKIMeta = TestKIMeta {
    parentKI      :: Text,
    caCertificate :: Text
} deriving (Eq, Show)

shouldFindSimpleParent :: HU.Assertion
shouldFindSimpleParent = do
    let cache = Map.fromList [
            ("child", TestKIMeta { parentKI = "parent", caCertificate = "cert1" }),
            ("parent", TestKIMeta { parentKI = "grandparent", caCertificate = "cert2" })
            ]
                     
    -- let readFromCache ki = 

    -- findPathUp readFromCache accept (ki, kiMeta) startCas
    pure ()

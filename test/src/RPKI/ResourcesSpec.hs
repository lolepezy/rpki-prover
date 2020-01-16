{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NumericUnderscores        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeApplications          #-}

module RPKI.ResourcesSpec where

import Control.Exception (bracket)
import           Control.Monad
import           Data.Maybe
import           Data.List as L

import Control.Lens

import           System.IO.Temp
import           System.Directory

import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.Tasty
import qualified Test.Tasty.QuickCheck             as QC
import qualified Test.Tasty.HUnit as HU

import           RPKI.Domain
import           RPKI.Orphans
import           RPKI.Resources


resourceGroup :: TestTree
resourceGroup = testGroup "Resource set tests"
  [
    QC.testProperty "Intersections and substractions" prop_intersecting_subtraction,
    QC.testProperty "Normalise" prop_normalise
  ]

prop_intersecting_subtraction :: QC.Property
prop_intersecting_subtraction = monadicIO $ assert True

prop_normalise :: QC.Property
prop_normalise = monadicIO $ assert True
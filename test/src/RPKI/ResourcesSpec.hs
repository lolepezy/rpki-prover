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
    QC.testProperty "Normalise V4" $
      \(ips :: [Ipv4Prefix]) -> normalise ips == (normalise $ normalise ips),
    QC.testProperty "Normalise V6" $
      \(ips :: [Ipv6Prefix]) -> normalise ips == (normalise $ normalise ips),
    -- QC.testProperty "Intersections and substractions V4" prop_intersecting_subtraction_v6,
    QC.testProperty "Intersection + subtraction" prop_intersecting_subtraction_v4
    -- QC.testProperty "Normalise" prop_normalise_v6
  ]

prop_intersecting_subtraction_v4 = do  
  True

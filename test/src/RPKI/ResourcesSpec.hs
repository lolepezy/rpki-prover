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

import           Control.Monad
import           Data.Maybe
import           Data.List as L
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Gen
import           Test.Tasty
import qualified Test.Tasty.QuickCheck             as QC
import qualified Test.Tasty.HUnit as HU

import           RPKI.Orphans
import           RPKI.Resources


resourceGroup :: TestTree
resourceGroup = testGroup "Resource set tests"
  [
    QC.testProperty "Normalise V4" $
      \(ips :: [Ipv4Prefix]) -> normalise ips == (normalise $ normalise ips),
    QC.testProperty "Normalise V6" $
      \(ips :: [Ipv6Prefix]) -> normalise ips == (normalise $ normalise ips),
    QC.testProperty "toRange and toPrefix V4" $
      \(ip :: Ipv4Prefix) -> (normalise $ toPrefixes $ toRange ip) == [ip],
    QC.testProperty "toRange and toPrefix V6" $
      \(ip :: Ipv6Prefix) -> (normalise $ toPrefixes $ toRange ip) == [ip]
    -- QC.testProperty "Intersections and substractions V4" prop_intersecting_subtraction_v6,
    -- QC.testProperty "Intersection + subtraction" prop_intersecting_subtraction_v4
    -- QC.testProperty "Normalise" prop_normalise_v6
  ]


-- prop_intersecting_subtraction_v4 :: Ipv4Prefix -> Ipv4Prefix -> Bool
-- prop_intersecting_subtraction_v4 ip1 ip2 = 


-- prop_intersecting_subtraction_v4 :: [Ipv4Prefix] -> QC.Property
-- prop_intersecting_subtraction_v4 ips = QC.forAll (sublistOf ips) $ 
--   \sub -> do
--     let nips = normalise ips
--     let nsub = normalise sub
    
--     True

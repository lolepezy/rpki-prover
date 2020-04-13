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
import           Data.List                         as List
import           Data.Maybe
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.HUnit                  ((@=?), (@?=))
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           RPKI.Orphans
import qualified RPKI.Resources.IntervalSet        as IS
import           RPKI.Resources.Resources
import           RPKI.Resources.Types



resourceGroup :: TestTree
resourceGroup = testGroup "Resources" [
    prefixPropertyGroup,
    resourcesUnitTests,
    intervalSetUnitTests
  ]
  
prefixPropertyGroup :: TestTree
prefixPropertyGroup = testGroup "Prefix properties tests"
  [
    QC.testProperty "Normalise V4" $
      \(ips :: [Ipv4Prefix]) -> normalise ips == (normalise $ normalise ips),
    QC.testProperty "Normalise V6" $
      \(ips :: [Ipv6Prefix]) -> normalise ips == (normalise $ normalise ips),
    QC.testProperty "toRange and toPrefix V4" $
      \(ip :: Ipv4Prefix) -> (normalise $ toPrefixes $ toRange ip) == [ip],
    QC.testProperty "toRange and toPrefix V6" $
      \(ip :: Ipv6Prefix) -> (normalise $ toPrefixes $ toRange ip) == [ip],
    QC.testProperty "Every AS interval intersects with every of its element" $
      \(asns :: [AsResource]) -> 
          QC.forAll (sublistOf asns) $ \sub ->
              let intervalSet = IS.fromList asns
                  check as = normalise (IS.findIntersections as intervalSet) == [as]
                in List.all check sub
  ]  


resourcesUnitTests :: TestTree
resourcesUnitTests = testGroup "AS resource unit tests" [
  HU.testCase "Should calculate intersection for ASN resource" $ do             
        intersection (AS (ASN 10)) (AS (ASN 15)) @?= []
        intersection (AS (ASN 10)) (AS (ASN 10)) @?= [AS (ASN 10)]
        intersection (ASRange (ASN 10) (ASN 20)) (AS (ASN 10)) @?= [AS (ASN 10)]
        intersection (ASRange (ASN 10) (ASN 20)) (AS (ASN 15)) @?= [AS (ASN 15)]
        intersection (ASRange (ASN 10) (ASN 20)) (AS (ASN 27)) @?= []
        intersection (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 15) (ASN 30)) @?= [ASRange (ASN 15) (ASN 20)]
        intersection (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 21) (ASN 30)) @?= []
        intersection (ASRange (ASN 20) (ASN 30)) (ASRange (ASN 1) (ASN 10)) @?= []        
        intersection (ASRange (ASN 10) (ASN 30)) (ASRange (ASN 20) (ASN 25)) @?= [ASRange (ASN 20) (ASN 25)]
        intersection (ASRange (ASN 10) (ASN 30)) (ASRange (ASN 20) (ASN 35)) @?= [ASRange (ASN 20) (ASN 30)],        

    HU.testCase "Should subtract ASN resource" $ do     
        checkAnsSub (10, 20) (17, 22) [ASRange (ASN 10) (ASN 16)]
        checkAnsSub (1, 10) (5, 15) [ASRange (ASN 1) (ASN 4)]
        checkAnsSub (10, 20) (5, 20) []
        checkAnsSub (10, 20) (12, 18) [ASRange (ASN 10) (ASN 11), ASRange (ASN 19) (ASN 20)]
        subtractAsn (ASRange (ASN 10) (ASN 20)) (AS (ASN 10)) @?= [ASRange (ASN 11) (ASN 20)]
        subtractAsn (ASRange (ASN 10) (ASN 11)) (AS (ASN 10)) @?= [AS (ASN 11)],

    HU.testCase "Should get intersection and overclaiming for ASNs" $ do
      let
        asChild  = IS.fromList [ASRange (ASN 10) (ASN 20)]
        asParent = IS.fromList [ASRange (ASN 15) (ASN 30)]
        (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals asChild asParent
        in do
          n @?= IS.fromList [ASRange (ASN 15) (ASN 20)]
          o @?= (IS.fromList $ normalise [ASRange (ASN 10) (ASN 14)])
      let
        asChild  = IS.fromList [
            ASRange (ASN 10) (ASN 20), 
            ASRange (ASN 16) (ASN 26), 
            AS (ASN 27), 
            AS (ASN 32),
            AS (ASN 37) 
          ]
        asParent = IS.fromList [ASRange (ASN 15) (ASN 30), AS (ASN 37)]
        (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals asChild asParent
        in do
          n @?= IS.fromList [ASRange (ASN 15) (ASN 27), AS (ASN 37)]
          o @?= (IS.fromList $ normalise [ASRange (ASN 10) (ASN 14), AS (ASN 32)])
  ]
  where 
    checkAnsSub (a0, a1) (b0, b1) expected = 
      (subtractAsn (ASRange (ASN a0) (ASN a1)) (ASRange (ASN b0) (ASN b1))) @?= expected


intervalSetUnitTests :: TestTree
intervalSetUnitTests = testGroup "AS interval sets unit tests" [
      HU.testCase "Should calculate intersection for ASN resources" $ do        
        IS.findIntersections (AS (ASN 10)) (mkIS [AS (ASN 15)]) @?= []      
        IS.findIntersections (AS (ASN 10)) (mkIS [AS (ASN 10)]) @?= [AS (ASN 10)]      
        IS.findIntersections (AS (ASN 10)) (mkIS [AS (ASN 10), AS (ASN 15)]) @?= [AS (ASN 10)]      
        IS.findIntersections (AS (ASN 10)) (mkIS [AS (ASN 10), AS (ASN 15), AS (ASN 20)]) @?= [AS (ASN 10)]      
        IS.findIntersections (AS (ASN 30)) (mkIS [AS (ASN 10), AS (ASN 15), AS (ASN 20)]) @?= []
        IS.findIntersections (AS (ASN 10)) (mkIS [ASRange (ASN 10) (ASN 15), AS (ASN 19), AS (ASN 20)]) @?= [AS (ASN 10)]
  ]
  where 
    mkIS = IS.fromList 

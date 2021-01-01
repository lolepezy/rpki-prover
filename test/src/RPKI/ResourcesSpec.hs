{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module RPKI.ResourcesSpec where

import           Data.List                  as List
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.HUnit           ((@?=))
import qualified Test.Tasty.HUnit           as HU
import qualified Test.Tasty.QuickCheck      as QC

import           RPKI.Orphans

import qualified RPKI.Resources.IntervalSet as IS
import           RPKI.Resources.Resources
import           RPKI.Resources.Types


resourceGroup :: TestTree
resourceGroup = testGroup "Resources" [
        prefixPropertyGroup,
        resourcesUnitTests,
        intervalSetUnitTests,
        ipOverlapTests
    ]

prefixPropertyGroup :: TestTree
prefixPropertyGroup = testGroup "Prefix properties tests"
    [
        QC.testProperty "Normalise V4" $
            \(ips :: [Ipv4Prefix]) -> normalise ips == normalise (normalise ips),
        QC.testProperty "Normalise V6" $
            \(ips :: [Ipv6Prefix]) -> normalise ips == normalise (normalise ips),
        QC.testProperty "toRange and toPrefix V4" $
            \(ip :: Ipv4Prefix) -> normalise (toPrefixes $ toRange ip) == [ip],
        QC.testProperty "toRange and toPrefix V6" $
            \(ip :: Ipv6Prefix) -> normalise (toPrefixes $ toRange ip) == [ip],

        QC.testProperty "Every AS interval intersects with every of its element" $
            \(asns :: [AsResource]) -> intersectsWithItsElements asns,

        QC.testProperty "Every IPv4 interval intersects with every of its element" $
            \(ips :: [Ipv4Prefix]) -> intersectsWithItsElements ips,

        QC.testProperty "Every IPv6 interval intersects with every of its element" $
            \(ips :: [Ipv6Prefix]) -> intersectsWithItsElements ips,

        QC.testProperty "Intersection and over-claming IPv4 must be complimentary" $
            \(ips :: [Ipv4Prefix]) -> intersectionAndOverclaimedAreComplimentary ips,

        QC.testProperty "Intersection and over-claming IPv6 must be complimentary" $
            \(ips :: [Ipv6Prefix]) -> intersectionAndOverclaimedAreComplimentary ips,   

        QC.testProperty "Intersection and over-claming ASNs must be complimentary" $
            \(asns :: [AsResource]) -> intersectionAndOverclaimedAreComplimentary asns                        
    ]  
    where 
        intersectsWithItsElements xs = 
            QC.forAll (sublistOf xs) $ \sub ->
                let intervalSet = IS.fromList xs
                    check as = normalise (IS.findIntersections as intervalSet) == [as]
                    in List.all check sub

        intersectionAndOverclaimedAreComplimentary xs = 
            QC.forAll (sublistOf xs) $ \subXs ->
                let biggerSet  = IS.fromList xs
                    smallerSet = IS.fromList subXs                    
                    (Nested is, Overclaiming os)   = IS.intersectionAndOverclaimedIntervals smallerSet biggerSet
                    (Nested is1, Overclaiming os1) = IS.intersectionAndOverclaimedIntervals biggerSet smallerSet 
                    in 
                        is == smallerSet && os == IS.empty 
                        && is1 == smallerSet
                        && is <> os == smallerSet
                        && is1 <> os1 == biggerSet

resourcesUnitTests :: TestTree
resourcesUnitTests = testGroup "AS resource unit tests" [
    HU.testCase "Should subtract ASN resource" $ do         
        let check a b = HU.assertBool (show a <> " doesn't contain " <> show b) $ contains a b
        let checkNot a b = HU.assertBool (show a <> " contains " <> show b) $ not (contains a b)

        check (ASRange (ASN 10) (ASN 16)) (AS $ ASN 10)
        checkNot (ASRange (ASN 10) (ASN 16)) (AS $ ASN 20)
        checkNot (ASRange (ASN 10) (ASN 16)) (AS $ ASN 6)

        check (AS $ ASN 10) (AS $ ASN 10)
        checkNot (AS $ ASN 10) (AS $ ASN 111)

        check (AS $ ASN 10) (ASRange (ASN 10) (ASN 10)) 
        checkNot (AS $ ASN 10) (ASRange (ASN 10) (ASN 11)) 

        check (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 14) (ASN 17)) 
        check (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 10) (ASN 17)) 
        check (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 18) (ASN 20)) 
        check (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 20) (ASN 20)) 
        check (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 10) (ASN 10)) 
        check (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 10) (ASN 20)) 

        checkNot (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 8) (ASN 18)) 
        checkNot (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 12) (ASN 22)) 
        checkNot (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 8) (ASN 22)) 
        checkNot (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 10) (ASN 22)) 
        checkNot (ASRange (ASN 10) (ASN 20)) (ASRange (ASN 8) (ASN 20)),
        

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
        checkAsnSub (10, 20) (17, 22) [ASRange (ASN 10) (ASN 16)]
        checkAsnSub (1, 10) (5, 15) [ASRange (ASN 1) (ASN 4)]
        checkAsnSub (10, 20) (5, 20) []
        checkAsnSub (10, 20) (12, 18) [ASRange (ASN 10) (ASN 11), ASRange (ASN 19) (ASN 20)]
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
                o @?= IS.fromList (normalise [ASRange (ASN 10) (ASN 14), AS (ASN 32)])
    ]
    where 
        checkAsnSub (a0, a1) (b0, b1) expected = 
            subtractAsn (ASRange (ASN a0) (ASN a1)) (ASRange (ASN b0) (ASN b1)) @?= expected


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

ipOverlapTests :: TestTree
ipOverlapTests = testGroup "IP interval sets unit tests" [
    HU.testCase "Should calculate intersection IPs with itself" $ do
        let p1 = readIp4 "103.101.176.0/32"
        HU.assertEqual "" (intersection p1 p1) [p1],

    HU.testCase "Should calculate intersection for nested IPs" $ do
        let p1 = readIp4 "103.101.176.0/32"
        let p2 = readIp4 "103.101.176.0/22"
        HU.assertEqual "" (intersection p1 p2) [p1]
        HU.assertEqual "" (intersection p2 p1) [p1],

    HU.testCase "Should calculate intersection for overlapping IPs" $ do
        let p1 = readIp4 "103.100.0.0/15"
        let p2 = readIp4 "103.101.0.0/16"
        HU.assertEqual "" (intersection p1 p2) [p2]
        HU.assertEqual "" (intersection p2 p1) [p2],

    HU.testCase "Should calculate intersection for overlapping IPs 2" $ do
        let p1 = readIp4 "103.100.0.0/15"
        let p2 = readIp4 "103.100.0.0/16"
        HU.assertEqual "" (intersection p1 p2) [p2]
        HU.assertEqual "" (intersection p2 p1) [p2],

    HU.testCase "Should calculate intersection for non-overlapping IPs" $ do
        let p1 = readIp4 "103.101.176.0/24"
        let p2 = readIp4 "103.101.177.0/24"
        HU.assertEqual "" (intersection p1 p2) []
        HU.assertEqual "" (intersection p2 p1) [],

    HU.testCase "Should calculate nested and over-claiming part for IPs resources, one address" $ do
        let p1 = readIp4 "103.101.176.0/32"
        let p2 = readIp4 "103.101.176.0/22"
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        HU.assertEqual "" is (IS.fromList [p1])
        HU.assertEqual "" os IS.empty,

    HU.testCase "Should calculate nested and over-claiming part for IPs resources, equal prefixes" $ do
        let p1 = readIp4 "103.101.176.0/22"
        let p2 = readIp4 "103.101.176.0/22"
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        HU.assertEqual "" is (IS.fromList [p1])
        HU.assertEqual "" os IS.empty,

    HU.testCase "Should calculate nested and over-claiming part for IPs resources, nested prefixes" $ do
        let p1 = readIp4 "103.101.176.0/22"
        let p2 = readIp4 "103.101.176.0/22"
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        HU.assertEqual "" is (IS.fromList [p1])
        HU.assertEqual "" os IS.empty,

    HU.testCase "Should calculate nested and over-claiming part for non-overlapping IPs" $ do
        let p1 = readIp4 "103.100.0.0/16"
        let p2 = readIp4 "103.100.100.0/24"        
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        HU.assertEqual "" is (IS.fromList [p2])
        let r = [
                    readIp4 "103.100.0.0/18",
                    readIp4 "103.100.64.0/19",
                    readIp4 "103.100.96.0/22",
                    readIp4 "103.100.101.0/24",
                    readIp4 "103.100.102.0/23",
                    readIp4 "103.100.104.0/21",
                    readIp4 "103.100.112.0/20",
                    readIp4 "103.100.128.0/17"
                ]
        HU.assertEqual "" os (IS.fromList r)
    ]    
    where
        mkIS ip = IS.fromList [ip]

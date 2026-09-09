{-# LANGUAGE OverloadedStrings #-}

module RPKI.Resources.ResourcesSpec where

import           Prelude                    hiding (subtract)

import           Data.List                  as List
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.HUnit           ((@?=))
import qualified Test.Tasty.HUnit           as HU
import qualified Test.Tasty.QuickCheck      as QC

import           RPKI.Orphans ()

import qualified RPKI.Resources.IntervalContainers as IS
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.TestCommons           (readIp4, readIp6)


-- | All the intervals of the set that `a` intersects with.
findIntersections :: Interval a => a -> IntervalSet a -> [a]
findIntersections a as = concatMap fst $ IS.findFullIntersections a as


resourceGroup :: TestTree
resourceGroup = testGroup "Resources" [
        prefixPropertyGroup,
        resourcesUnitTests,
        intervalSetUnitTests,
        ipOverlapTests,
        boundaryValueTests
    ]

{- | `ASN` and the hw-ip address types derive `Enum` from `Word32`/`Word128`, so 
   `succ maxBound` and `pred minBound` throw. These used to be reachable from 
   parsing alone: an AS block of {0-4294967295, 10-20} crashed `normaliseAsns` 
   before any signature was checked.

   Everything here must merely not throw; the expected values are the 
   mathematically correct ones.
-}
boundaryValueTests :: TestTree
boundaryValueTests = testGroup "Boundary values must not throw" [
    HU.testCase "normalise of AS ranges touching maxBound" $ do
        let maxAsn = ASN maxBound
        -- The original crash: "all ASNs" plus any other range
        normalise [ASRange (ASN 0) maxAsn, ASRange (ASN 10) (ASN 20)]
            @?= [ASRange (ASN 0) maxAsn]
        normalise [ASRange (ASN 10) (ASN 20), ASRange (ASN 0) maxAsn]
            @?= [ASRange (ASN 0) maxAsn]
        normalise [AS maxAsn, AS maxAsn] @?= [AS maxAsn]
        normalise [ASRange (ASN 10) maxAsn, AS maxAsn] @?= [ASRange (ASN 10) maxAsn]
        -- Inverted ranges can reach normalisation via a malformed certificate
        normalise [AS maxAsn, ASRange maxAsn (ASN 0)] `seq` pure ()
        normalise [ASRange maxAsn (ASN 0), AS (ASN 5)] `seq` pure (),

    HU.testCase "subtractAsn at the ends of the range" $ do
        let maxAsn = ASN maxBound
        subtractAsn (ASRange (ASN 0) maxAsn) (AS (ASN 0))
            @?= [ASRange (ASN 1) maxAsn]
        subtractAsn (ASRange (ASN 0) maxAsn) (AS maxAsn)
            @?= [ASRange (ASN 0) (ASN (maxBound - 1))]
        subtractAsn (AS (ASN 0)) (AS (ASN 0)) @?= []
        subtractAsn (ASRange maxAsn maxAsn) (AS maxAsn) @?= []
        subtractAsn (ASRange (ASN 0) (ASN 0)) (AS (ASN 0)) @?= []
        subtractAsn (ASRange (ASN 0) maxAsn) (ASRange (ASN 0) maxAsn) @?= [],

    HU.testCase "subtract of IPv4 prefixes at 0.0.0.0 and 255.255.255.255" $ do
        -- `subtract` uses pred/succ on the range ends, which throw at the 
        -- boundaries of the address space
        let all4    = readIp4 "0.0.0.0/0"
        let lower   = readIp4 "0.0.0.0/1"
        let upper   = readIp4 "128.0.0.0/1"
        subtract all4 lower  @?= [upper]
        subtract all4 upper  @?= [lower]
        subtract all4 all4   @?= []
        subtract lower lower @?= []
        subtract lower all4  @?= [],

    HU.testCase "subtract of IPv6 prefixes at the ends of the address space" $ do
        let all6  = readIp6 "::/0"
        let lower = readIp6 "::/1"
        let upper = readIp6 "8000::/1"
        subtract all6 lower @?= [upper]
        subtract all6 upper @?= [lower]
        subtract all6 all6  @?= [],

    HU.testCase "overclaiming against a parent holding the whole address space" $ do
        let child  = IS.fromList [readIp4 "0.0.0.0/0"]
        let parent = IS.fromList [readIp4 "0.0.0.0/1"]
        let (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals child parent
        n @?= IS.fromList [readIp4 "0.0.0.0/1"]
        o @?= IS.fromList [readIp4 "128.0.0.0/1"],

    HU.testCase "countAsns does not materialise the ASNs" $ do
        countAsns [ASRange (ASN 0) (ASN maxBound)] @?= 4294967296
        countAsns [AS (ASN 7)] @?= 1
        countAsns [ASRange (ASN 10) (ASN 20)] @?= 11
        countAsns [ASRange (ASN 20) (ASN 10)] @?= 0
        countAsns [] @?= 0,

    HU.testCase "mkAsn rejects out-of-range AS numbers" $ do
        mkAsn 0 @?= Right (ASN 0)
        mkAsn 4294967295 @?= Right (ASN maxBound)
        HU.assertBool "2^32 must be rejected"     $ isLeft $ mkAsn 4294967296
        HU.assertBool "2^32 + 7 must be rejected" $ isLeft $ mkAsn 4294967303
        HU.assertBool "negative must be rejected" $ isLeft $ mkAsn (-1),

    QC.testProperty "normalise never throws for arbitrary AS resources including the bounds" $
        QC.forAll (listOf extremeAsResource) $ \asns -> 
            length (normalise asns) >= 0,

    QC.testProperty "subtractAsn never throws for arbitrary AS resources including the bounds" $
        QC.forAll ((,) <$> extremeAsResource <*> extremeAsResource) $ \(a, b) -> 
            length (subtractAsn a b) >= 0
    ]
  where
    isLeft = \case
        Left _  -> True
        Right _ -> False

    -- Generator biased towards the ends of the ASN space, which the default 
    -- `Arbitrary AsResource` instance essentially never produces.
    extremeAsResource = do 
        let edges = [0, 1, 2, maxBound, maxBound - 1, maxBound - 2, 65535, 65536]
        a <- elements edges
        b <- elements edges
        elements [AS (ASN a), ASRange (ASN a) (ASN b)]

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
                    check as = normalise (findIntersections as intervalSet) == [as]
                    in List.all check sub

        intersectionAndOverclaimedAreComplimentary xs = 
            QC.forAll (sublistOf xs) $ \subXs ->
                let biggerSet  = IS.fromList xs
                    smallerSet = IS.fromList subXs                    
                    (Nested is, Overclaiming os)   = IS.intersectionAndOverclaimedIntervals smallerSet biggerSet
                    (Nested is1, Overclaiming os1) = IS.intersectionAndOverclaimedIntervals biggerSet smallerSet 
                    in 
                        is == smallerSet && os == IS.empty 
                        && is <> os == smallerSet
                        && is1 == smallerSet                        
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
        findIntersections (AS (ASN 10)) (mkIS [AS (ASN 15)]) @?= []      
        findIntersections (AS (ASN 10)) (mkIS [AS (ASN 10)]) @?= [AS (ASN 10)]      
        findIntersections (AS (ASN 10)) (mkIS [AS (ASN 10), AS (ASN 15)]) @?= [AS (ASN 10)]      
        findIntersections (AS (ASN 10)) (mkIS [AS (ASN 10), AS (ASN 15), AS (ASN 20)]) @?= [AS (ASN 10)]      
        findIntersections (AS (ASN 30)) (mkIS [AS (ASN 10), AS (ASN 15), AS (ASN 20)]) @?= []
        findIntersections (AS (ASN 10)) (mkIS [ASRange (ASN 10) (ASN 15), AS (ASN 19), AS (ASN 20)]) @?= [AS (ASN 10)]
    ]
    where 
        mkIS = IS.fromList 

ipOverlapTests :: TestTree
ipOverlapTests = testGroup "IP interval sets unit tests" [
    HU.testCase "Should calculate intersection IPs with itself" $ do
        let p1 = readIp4 "103.101.176.0/32"
        checkEq (intersection p1 p1) [p1],

    HU.testCase "Should calculate intersection for nested IPs" $ do
        let p1 = readIp4 "103.101.176.0/32"
        let p2 = readIp4 "103.101.176.0/22"
        checkEq (intersection p1 p2) [p1]
        checkEq (intersection p2 p1) [p1],

    HU.testCase "Should calculate intersection for overlapping IPs" $ do
        let p1 = readIp4 "103.100.0.0/15"
        let p2 = readIp4 "103.101.0.0/16"
        checkEq (intersection p1 p2) [p2]
        checkEq (intersection p2 p1) [p2],

    HU.testCase "Should calculate intersection for overlapping IPs 2" $ do
        let p1 = readIp4 "103.100.0.0/15"
        let p2 = readIp4 "103.100.0.0/16"
        checkEq (intersection p1 p2) [p2]
        checkEq (intersection p2 p1) [p2],

    HU.testCase "Should calculate intersection for non-overlapping IPs" $ do
        let p1 = readIp4 "103.101.176.0/24"
        let p2 = readIp4 "103.101.177.0/24"
        checkEq (intersection p1 p2) []
        checkEq (intersection p2 p1) [],

    HU.testCase "Should calculate nested and over-claiming part for IPs resources, one address" $ do
        let p1 = readIp4 "103.101.176.0/32"
        let p2 = readIp4 "103.101.176.0/22"
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        checkEq is $ IS.fromList [p1]
        checkEq os IS.empty,

    HU.testCase "Should calculate nested and over-claiming part for IPs resources, equal prefixes" $ do
        let p1 = readIp4 "103.101.176.0/22"
        let p2 = readIp4 "103.101.176.0/22"
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        checkEq is $ IS.fromList [p1]
        checkEq os IS.empty,

    HU.testCase "Should calculate nested and over-claiming part for IPs resources, nested prefixes" $ do
        let p1 = readIp4 "103.101.176.0/22"
        let p2 = readIp4 "103.101.176.0/22"
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        checkEq is $ IS.fromList [p1]
        checkEq os IS.empty,

    HU.testCase "Should calculate nested and over-claiming part for non-overlapping IPs" $ do
        let p1 = readIp4 "103.100.0.0/16"
        let p2 = readIp4 "103.100.100.0/24"        
        let (Nested is, Overclaiming os) = 
                IS.intersectionAndOverclaimedIntervals (mkIS p1) (mkIS p2)
        checkEq is $ IS.fromList [p2]
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
        checkEq os $ IS.fromList r
    ]    
    where
        mkIS ip = IS.fromList [ip]
        checkEq = HU.assertEqual ""

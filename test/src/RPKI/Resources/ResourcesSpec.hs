module RPKI.Resources.ResourcesSpec where

import           Prelude                    hiding (subtract)

import           Data.ASN1.BitArray
import           Data.ASN1.Parse
import           Data.ASN1.Types
import qualified Data.ByteString            as BS
import           Data.List                  as List
import           Data.Word                  (Word8)
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.HUnit           ((@?=))
import qualified Test.Tasty.HUnit           as HU
import qualified Test.Tasty.QuickCheck      as QC

import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import           HaskellWorks.Data.Network.Ip.Range (Range (..))

import           RPKI.Orphans

import           RPKI.Parse.Internal.Common (ipv4Address, ipv6Address)
import qualified RPKI.Resources.IntervalContainers as IS
import           RPKI.Resources.Resources
import           RPKI.Resources.Types


resourceGroup :: TestTree
resourceGroup = testGroup "Resources" [
        prefixPropertyGroup,
        resourcesUnitTests,
        intervalSetUnitTests,
        ipOverlapTests,
        boundaryValueTests,
        exactSetOperationTests,
        canonicalPrefixTests
    ]

{- | Nested and overclaimed parts must be exact, not only add up to the child:
   overclaimed must not contain anything the parent has. The overclaimed part
   used to be computed against every intersecting parent interval separately,
   so a child interval spanning several of them was reported as overclaimed
   whole.
-}
exactSetOperationTests :: TestTree
exactSetOperationTests = testGroup "Set operations are exact" [
    HU.testCase "overclaiming of an AS range spanning several parent ranges" $ do
        let child  = IS.fromList [ASRange (ASN 1) (ASN 100)]
        let parent = IS.fromList [ASRange (ASN 10) (ASN 20), ASRange (ASN 30) (ASN 40)]
        let (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals child parent
        n @?= parent
        o @?= IS.fromList [ASRange (ASN 1) (ASN 9), ASRange (ASN 21) (ASN 29), ASRange (ASN 41) (ASN 100)],

    HU.testCase "overclaiming of an IPv4 prefix spanning several parent prefixes" $ do
        let child  = IS.fromList [readIp4 "10.0.0.0/22"]
        let parent = IS.fromList [readIp4 "10.0.1.0/24", readIp4 "10.0.2.0/23"]
        let (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals child parent
        n @?= parent
        o @?= IS.fromList [readIp4 "10.0.0.0/24"],

    HU.testCase "overclaiming of an IPv6 prefix spanning several parent prefixes" $ do
        let child  = IS.fromList [readIp6 "2001:db8::/32"]
        let parent = IS.fromList [readIp6 "2001:db8:1::/48", readIp6 "2001:db8:8000::/33"]
        let (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals child parent
        n @?= parent
        o @?= IS.fromList [readIp6 "2001:db8::/48", readIp6 "2001:db8:2::/47",
                           readIp6 "2001:db8:4::/46", readIp6 "2001:db8:8::/45",
                           readIp6 "2001:db8:10::/44", readIp6 "2001:db8:20::/43",
                           readIp6 "2001:db8:40::/42", readIp6 "2001:db8:80::/41",
                           readIp6 "2001:db8:100::/40", readIp6 "2001:db8:200::/39",
                           readIp6 "2001:db8:400::/38", readIp6 "2001:db8:800::/37",
                           readIp6 "2001:db8:1000::/36", readIp6 "2001:db8:2000::/35",
                           readIp6 "2001:db8:4000::/34"],

    -- A prefix that starts exactly where the subtracted one ends was taken
    -- for not overlapping with it at all.
    HU.testCase "subtract of prefixes touching at one address" $ do
        subtract (readIp4 "10.0.0.0/24") (readIp4 "10.0.0.0/32")
            @?= ipv4RangeToPrefixes 0x0A000001 0x0A0000FF
        subtract (readIp4 "10.0.0.255/32") (readIp4 "10.0.0.0/24") @?= []
        subtract (readIp4 "10.0.0.0/24") (readIp4 "10.0.0.255/32")
            @?= ipv4RangeToPrefixes 0x0A000000 0x0A0000FE
        subtract (readIp6 "2001:db8::/48") (readIp6 "2001:db8::/128")
            @?= ipv6RangeToPrefixes (0x20010db8, 0, 0, 1) (0x20010db8, 0xFFFF, 0xFFFFFFFF, 0xFFFFFFFF)
        subtract (readIp6 "2001:db8::/128") (readIp6 "2001:db8::/48") @?= [],

    HU.testCase "overclaiming against a parent holding the first address only" $ do
        let child  = IS.fromList [readIp4 "10.0.0.0/24"]
        let parent = IS.fromList [readIp4 "10.0.0.0/32"]
        let (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals child parent
        n @?= parent
        o @?= IS.fromList (ipv4RangeToPrefixes 0x0A000001 0x0A0000FF),

    QC.testProperty "subtract and intersection of IPv4 prefixes are exact" $
        \(p :: Ipv4Prefix) q -> subtractIsExact p q,
    QC.testProperty "subtract and intersection of IPv6 prefixes are exact" $
        \(p :: Ipv6Prefix) q -> subtractIsExact p q,
    QC.testProperty "subtract and intersection of AS resources are exact" $
        \(p :: AsResource) q -> subtractIsExact p q,

    -- Arbitrary prefixes rarely touch, so also subtract a prefix's own edges
    QC.testProperty "subtract of the edges of an IPv4 prefix is exact" $
        \(p :: Ipv4Prefix) -> let Range (V4.IpAddress f) (V4.IpAddress l) = toRange p
                              in all (subtractIsExact p . (`mkIpv4Block` 32)) [f, l],

    QC.testProperty "nested and overclaimed IPv4 are exact" $
        \(child :: [Ipv4Prefix]) parent -> nestedAndOverclaimedAreExact child parent,
    QC.testProperty "nested and overclaimed IPv6 are exact" $
        \(child :: [Ipv6Prefix]) parent -> nestedAndOverclaimedAreExact child parent,
    QC.testProperty "nested and overclaimed ASNs are exact" $
        \(child :: [AsResource]) parent -> nestedAndOverclaimedAreExact child parent,

    -- Independent random sets rarely overlap much; a child built from a parent
    -- with some of it removed spans the gaps in the parent, which is the case
    -- that used to go wrong.
    QC.testProperty "nested and overclaimed IPv4 are exact for a child spanning the parent" $
        \(xs :: [Ipv4Prefix]) -> QC.forAll (sublistOf xs) $ \sub -> nestedAndOverclaimedAreExact xs sub,
    QC.testProperty "nested and overclaimed IPv6 are exact for a child spanning the parent" $
        \(xs :: [Ipv6Prefix]) -> QC.forAll (sublistOf xs) $ \sub -> nestedAndOverclaimedAreExact xs sub,
    QC.testProperty "nested and overclaimed ASNs are exact for a child spanning the parent" $
        \(xs :: [AsResource]) -> QC.forAll (sublistOf xs) $ \sub -> nestedAndOverclaimedAreExact xs sub
    ]
  where
    -- What's left of p after subtracting q does not overlap q, and together
    -- with the intersection it is p again
    subtractIsExact p q =
        let rest = subtract p q
        in all (null . intersection q) rest
            && normalise (rest <> intersection p q) == normalise [p]

    nestedAndOverclaimedAreExact childXs parentXs =
        let child  = IS.fromList childXs
            parent = IS.fromList parentXs
            (Nested n, Overclaiming o) = IS.intersectionAndOverclaimedIntervals child parent
        in n <> o == child
            && all (\x -> null (IS.findIntersections x parent)) (IS.toList o)
            && all (\x -> any (`contains` x) (IS.toList parent)) (IS.toList n)


{- | A prefix in a BIT STRING may have non-zero bits past its length (the
   "unused" bits of the last octet, DER requires them to be zero). They must
   not end up in the prefix: hw-ip takes the address of a block for its first
   address, so 10.255/9 used to become the range 10.255.0.0 - 11.126.255.255.
   OpenSSL clears these bits when decoding, so clearing them here gives the
   same prefixes other RPs get.
-}
canonicalPrefixTests :: TestTree
canonicalPrefixTests = testGroup "Prefixes are canonical" [
    HU.testCase "makePrefix clears the bits past the prefix length" $ do
        makePrefix (BS.pack [10, 0xFF]) 9 @?= readIp4 "10.128.0.0/9"
        makePrefix (BS.pack [10, 0xFF, 0xFF, 0xFF]) 32 @?= readIp4 "10.255.255.255/32"
        makePrefix (BS.pack [0xFF]) 0 @?= readIp4 "0.0.0.0/0"
        makePrefix (BS.pack [0x20, 0x01, 0x0d, 0xb9]) 31 @?= readIp6 "2001:db8::/31"
        makePrefix (BS.pack [0x20, 0x01, 0x0d, 0xb8, 0xFF]) 33 @?= readIp6 "2001:db8:8000::/33"
        makePrefix (BS.pack [0xFF]) 0 @?= readIp6 "::/0",

    QC.testProperty "makePrefix makes canonical IPv4 prefixes" $
        \(ws :: [Word8]) -> QC.forAll (QC.choose (0, 32)) $ \len ->
            let p = makePrefix (BS.pack (take 4 ws)) len :: Ipv4Prefix
            in toPrefixes (toRange p) == [p],

    QC.testProperty "makePrefix makes canonical IPv6 prefixes" $
        \(ws :: [Word8]) -> QC.forAll (QC.choose (0, 128)) $ \len ->
            let p = makePrefix (BS.pack (take 16 ws)) len :: Ipv6Prefix
            in toPrefixes (toRange p) == [p],

    HU.testCase "Parsed prefixes and address ranges ignore the unused bits" $ do
        let bits n ws = BitString (BitArray n (BS.pack ws))
        runParseASN1 ipv4Address [bits 9 [10, 0xFF]]
            @?= Right [readIp4 "10.128.0.0/9"]
        -- 10.0.0.0/23 (min, unused bits set) - 10.0.4.0/23 (max)
        runParseASN1 ipv4Address [Start Sequence, bits 23 [10, 0, 1], bits 23 [10, 0, 4], End Sequence]
            @?= Right [readIp4 "10.0.0.0/22", readIp4 "10.0.4.0/23"]
        runParseASN1 ipv6Address [bits 31 [0x20, 0x01, 0x0d, 0xb9]]
            @?= Right [readIp6 "2001:db8::/31"]
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

    HU.testCase "parseAsn rejects out-of-range AS numbers instead of wrapping them" $ do
        parseAsn "AS65000" @?= Just (ASN 65000)
        parseAsn "as4294967295" @?= Just (ASN maxBound)
        parseAsn "0" @?= Just (ASN 0)
        parseAsn "AS4294967303" @?= Nothing
        parseAsn "4294967296" @?= Nothing
        parseAsn "-1" @?= Nothing
        parseAsn "ASfoo" @?= Nothing,

    HU.testCase "subtractAsn of an ASN outside of the range keeps the range" $ do
        subtractAsn (ASRange (ASN 1) (ASN 2)) (AS (ASN 0)) @?= [ASRange (ASN 1) (ASN 2)]
        subtractAsn (ASRange (ASN 1) (ASN 2)) (AS (ASN 3)) @?= [ASRange (ASN 1) (ASN 2)]
        subtractAsn (ASRange (ASN 1) (ASN 2)) (AS (ASN 2)) @?= [AS (ASN 1)],

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

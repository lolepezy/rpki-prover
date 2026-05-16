module RPKI.Resources.ValiditySpec where

import           Data.Bits

import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6

import           Test.Tasty
import qualified Test.Tasty.HUnit           as HU
import qualified Test.Tasty.QuickCheck      as QC

import           RPKI.Orphans

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Resources.Resources   (mkIpv4Block, mkIpv6Block,
                                             readIp4, readIp6,
                                             ipv4PrefixLen)
import           RPKI.Resources.Validity


validityGroup :: TestTree
validityGroup = testGroup "Prefix validity" [

        QC.testProperty "Insert and lookup IPv4" $
            \(prefixes :: [Ipv4Prefix]) -> let
                vrps = makeV4Vrps prefixes
            in checkVrps (createPrefixIndex vrps) vrps,

        QC.testProperty "Insert and lookup IPv6" $
            \(prefixes :: [Ipv6Prefix]) -> let
                vrps = makeV6Vrps prefixes
            in checkVrps (createPrefixIndex vrps) vrps,

        testGroup "Non-overlapping prefixes" [

            QC.testProperty "Disjoint IPv4 prefix not found in index" $
                \(p :: Ipv4Prefix) ->
                    let (addr, _)        = prefixEdgesV4 p
                        PrefixLength len = ipv4PrefixLen p
                        -- Force the VRP into the lower /1 half (top bit = 0) and
                        -- query from the upper /1 half (top bit = 1); they never overlap.
                        lowP  = mkIpv4Block (addr .&. 0x7FFFFFFF) len
                        highP = mkIpv4Block (addr .|. 0x80000000) len
                        idx   = createPrefixIndex [Vrp (ASN 0) (Ipv4P lowP) (PrefixLength 32)]
                    in null (lookupVrps (Ipv4P highP) idx),

            QC.testProperty "Disjoint IPv6 prefix not found in index" $
                \(p :: Ipv6Prefix) ->
                    -- HW Word128 is (Word32,Word32,Word32,Word32); top bit is bit 31 of w0.
                    let Ipv6Prefix (V6.IpBlock (V6.IpAddress (w0, w1, w2, w3)) (V6.IpNetMask len)) = p
                        lowP  = mkIpv6Block (w0 .&. 0x7FFFFFFF, w1, w2, w3) len
                        highP = mkIpv6Block (w0 .|. 0x80000000, w1, w2, w3) len
                        idx   = createPrefixIndex [Vrp (ASN 0) (Ipv6P lowP) (PrefixLength 128)]
                    in null (lookupVrps (Ipv6P highP) idx)
        ],

        testGroup "Prefix containment" [

            QC.testProperty "IPv4 parent VRP covers more-specific child" $
                \(p :: Ipv4Prefix) ->
                    let (addr, _)        = prefixEdgesV4 p
                        PrefixLength len = ipv4PrefixLen p
                    in len < 32 QC.==>
                        let parentVrp = Vrp (ASN 0) (Ipv4P p) (PrefixLength 32)
                            childP    = mkIpv4Block addr (len + 1)
                            idx       = createPrefixIndex [parentVrp]
                        in parentVrp `elem` lookupVrps (Ipv4P childP) idx,

            QC.testProperty "IPv6 parent VRP covers more-specific child" $
                \(p :: Ipv6Prefix) ->
                    let Ipv6Prefix (V6.IpBlock (V6.IpAddress addr) (V6.IpNetMask len)) = p
                    in len < 128 QC.==>
                        let parentVrp = Vrp (ASN 0) (Ipv6P p) (PrefixLength 128)
                            childP    = mkIpv6Block addr (len + 1)
                            idx       = createPrefixIndex [parentVrp]
                        in parentVrp `elem` lookupVrps (Ipv6P childP) idx
        ],

        testGroup "prefixValidity V4" [

            HU.testCase "Valid: matching ASN, prefix length within maxLength" $ do
                let vrp = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/8") (PrefixLength 24)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/24") idx
                    HU.@?= ValidOverall [vrp] [],

            HU.testCase "InvalidAsn: wrong ASN" $ do
                let vrp = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/8") (PrefixLength 24)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 2) (Ipv4P $ readIp4 "10.0.0.0/24") idx
                    HU.@?= InvalidOverall [InvalidAsn vrp],

            HU.testCase "InvalidLength: prefix more specific than maxLength" $ do
                let vrp = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/8") (PrefixLength 24)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/25") idx
                    HU.@?= InvalidOverall [InvalidLength vrp],

            HU.testCase "Unknown: no covering VRP in index" $ do
                let vrp = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/8") (PrefixLength 24)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 1) (Ipv4P $ readIp4 "192.168.0.0/24") idx
                    HU.@?= Unknown,

            HU.testCase "Mixed: valid, wrong-ASN, and too-specific VRPs for the same parent" $ do
                -- vrp1 authorises exactly; vrp2 has wrong ASN; vrp3 allows only up to /23.
                let vrp1 = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/8") (PrefixLength 24)
                    vrp2 = Vrp (ASN 2) (Ipv4P $ readIp4 "10.0.0.0/8") (PrefixLength 24)
                    vrp3 = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/8") (PrefixLength 23)
                    idx  = createPrefixIndex [vrp1, vrp2, vrp3]
                case prefixValidity (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/24") idx of
                    ValidOverall valids invalids -> do
                        valids HU.@?= [vrp1]
                        HU.assertBool "InvalidAsn vrp2 in invalids"   (InvalidAsn    vrp2 `elem` invalids)
                        HU.assertBool "InvalidLength vrp3 in invalids" (InvalidLength vrp3 `elem` invalids)
                    other -> HU.assertFailure $ "Expected ValidOverall, got: " <> show other,

            HU.testCase "More than one valid" $ do
                let vrp1 = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/8")  (PrefixLength 24)
                    vrp2 = Vrp (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/16") (PrefixLength 28)
                    idx  = createPrefixIndex [vrp1, vrp2]
                case prefixValidity (ASN 1) (Ipv4P $ readIp4 "10.0.0.0/24") idx of
                    ValidOverall valids invalids -> do
                        HU.assertBool "vrp1 in valids" (vrp1 `elem` valids)
                        HU.assertBool "vrp2 in valids" (vrp2 `elem` valids)
                        invalids HU.@?= []
                    other -> HU.assertFailure $ "Expected ValidOverall, got: " <> show other
        ],

        testGroup "prefixValidity V6" [

            HU.testCase "Valid: matching ASN, prefix length within maxLength" $ do
                let vrp = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 48)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 1) (Ipv6P $ readIp6 "2001:db8:1::/48") idx
                    HU.@?= ValidOverall [vrp] [],

            HU.testCase "InvalidAsn: wrong ASN" $ do
                let vrp = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 48)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 2) (Ipv6P $ readIp6 "2001:db8:1::/48") idx
                    HU.@?= InvalidOverall [InvalidAsn vrp],

            HU.testCase "InvalidLength: prefix more specific than maxLength" $ do
                let vrp = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 48)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 1) (Ipv6P $ readIp6 "2001:db8:1::/49") idx
                    HU.@?= InvalidOverall [InvalidLength vrp],

            HU.testCase "Unknown: no covering VRP in index" $ do
                let vrp = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 48)
                    idx = createPrefixIndex [vrp]
                prefixValidity (ASN 1) (Ipv6P $ readIp6 "2002::/16") idx
                    HU.@?= Unknown,

            HU.testCase "Mixed: valid, wrong-ASN, and too-specific VRPs for the same parent" $ do
                let vrp1 = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 48)
                    vrp2 = Vrp (ASN 2) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 48)
                    vrp3 = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 47)
                    idx  = createPrefixIndex [vrp1, vrp2, vrp3]
                case prefixValidity (ASN 1) (Ipv6P $ readIp6 "2001:db8:1::/48") idx of
                    ValidOverall valids invalids -> do
                        valids HU.@?= [vrp1]
                        HU.assertBool "InvalidAsn vrp2 in invalids"    (InvalidAsn    vrp2 `elem` invalids)
                        HU.assertBool "InvalidLength vrp3 in invalids" (InvalidLength vrp3 `elem` invalids)
                    other -> HU.assertFailure $ "Expected ValidOverall, got: " <> show other,

            HU.testCase "More than one valid" $ do
                let vrp1 = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/32") (PrefixLength 64)
                    vrp2 = Vrp (ASN 1) (Ipv6P $ readIp6 "2001:db8::/48") (PrefixLength 64)
                    idx  = createPrefixIndex [vrp1, vrp2]
                case prefixValidity (ASN 1) (Ipv6P $ readIp6 "2001:db8::/64") idx of
                    ValidOverall valids invalids -> do
                        HU.assertBool "vrp1 in valids" (vrp1 `elem` valids)
                        HU.assertBool "vrp2 in valids" (vrp2 `elem` valids)
                        invalids HU.@?= []
                    other -> HU.assertFailure $ "Expected ValidOverall, got: " <> show other
        ]
    ]
  where
    checkVrps index = all (\vrp@(Vrp _ prefix _) -> vrp `elem` lookupVrps prefix index)

    makeV4Vrps = map (\prefix -> Vrp (ASN 0) (Ipv4P prefix) (PrefixLength 32))
    makeV6Vrps = map (\prefix -> Vrp (ASN 0) (Ipv6P prefix) (PrefixLength 128))

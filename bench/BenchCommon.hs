-- | Shared VRP and probe generators used by both benchmarks.
module BenchCommon
    ( genIPv4Vrps
    , genIPv6Vrps
    , mkIPv4Probes
    , mkIPv6Probes
    ) where

import Data.Word (Word32, Word8)

import RPKI.Domain              (Vrp (..))
import RPKI.Resources.Resources (mkIpv4Block, mkIpv6Block)
import RPKI.Resources.Types     (ASN (..), IpPrefix (..), PrefixLength (..))

-- ---------------------------------------------------------------------------
-- VRP generators  (same seed in both benchmarks for comparability)
-- ---------------------------------------------------------------------------

-- | ~700k IPv4 VRPs spread across the full address space.
-- Prefix lengths range from /8 to /24, mirroring a realistic BGP table.
genIPv4Vrps :: [Vrp]
genIPv4Vrps =
    [ Vrp (ASN asn) (Ipv4P (mkIpv4Block addr len)) (PrefixLength len)
    | i   <- [0 .. 699999 :: Int]
    , let addr = fromIntegral (i * 57321)       :: Word32
    , let len  = fromIntegral (8 + i `mod` 17)  :: Word8
    , let asn  = fromIntegral (i `mod` 200000)  :: Word32
    ]

-- | ~300k IPv6 VRPs spread across global-unicast space.
-- Prefix lengths range from /32 to /48, mirroring typical ISP allocations.
genIPv6Vrps :: [Vrp]
genIPv6Vrps =
    [ Vrp (ASN asn) (Ipv6P (mkIpv6Block (w0, w1, w2, w3) len)) (PrefixLength len)
    | i   <- [0 .. 299999 :: Int]
    , let w0  = 0x20010db8 + fromIntegral (i `div` 65536) :: Word32
    , let w1  = fromIntegral (i * 13 `mod` 65536)          :: Word32
    , let w2  = fromIntegral (i * 7)                        :: Word32
    , let w3  = 0                                           :: Word32
    , let len = fromIntegral (32 + i `mod` 17)             :: Word8
    , let asn = fromIntegral (i `mod` 200000)              :: Word32
    ]

-- ---------------------------------------------------------------------------
-- Probe generators
-- ---------------------------------------------------------------------------

-- | IPv4 probes for the given index range.
-- Offsets from the VRP seed so some will match, some won't.
mkIPv4Probes :: [Int] -> [(ASN, IpPrefix)]
mkIPv4Probes is =
    [ (ASN asn, Ipv4P (mkIpv4Block addr len))
    | i   <- is
    , let addr = fromIntegral (i * 57321 + 12345) :: Word32
    , let len  = fromIntegral (16 + i `mod` 9)    :: Word8
    , let asn  = fromIntegral (i `mod` 200000)     :: Word32
    ]

-- | IPv6 probes for the given index range.
-- Same global-unicast range as the VRPs, slightly offset.
mkIPv6Probes :: [Int] -> [(ASN, IpPrefix)]
mkIPv6Probes is =
    [ (ASN asn, Ipv6P (mkIpv6Block (w0, w1, w2, w3) len))
    | i   <- is
    , let w0  = 0x20010db8 + fromIntegral (i `div` 65536) :: Word32
    , let w1  = fromIntegral (i * 13 `mod` 65536)          :: Word32
    , let w2  = fromIntegral (i * 7 + 100)                  :: Word32
    , let w3  = 0                                            :: Word32
    , let len = fromIntegral (40 + i `mod` 9)              :: Word8
    , let asn = fromIntegral (i `mod` 200000)               :: Word32
    ]

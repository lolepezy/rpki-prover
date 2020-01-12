{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-} 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}

module RPKI.Resource.Resource where

import Codec.Serialise

import Control.DeepSeq

import qualified Data.ByteString as B  

import Data.Data (Typeable)

import Data.Word
import Data.Bits
import qualified Data.Set as S

import GHC.Generics
import Data.List

import HaskellWorks.Data.Network.Ip.Validity
import HaskellWorks.Data.Network.Ip.Range 
import HaskellWorks.Data.Network.Ip.SafeEnum
import HaskellWorks.Data.Network.Ip.Word128
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6 as V6

data AddrFamily = Ipv4F | Ipv6F
    deriving (Show, Eq, Ord, Typeable, Generic)

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable, Generic)
newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable, Generic)

newtype Ipv4Range = Ipv4Range (Range V4.IpAddress) 
    deriving (Show, Eq, Ord, Typeable, Generic) 
newtype Ipv6Range = Ipv6Range (Range V6.IpAddress) 
    deriving (Show, Eq, Ord, Typeable, Generic)

data IpPrefix = Ipv4P !Ipv4Prefix | Ipv6P !Ipv6Prefix
  deriving (Show, Eq, Ord, Typeable, Generic)

newtype ASN = ASN Int
  deriving (Show, Eq, Ord, Typeable, Generic, NFData)

data AsResource =  AS !ASN
                 | ASRange !ASN !ASN
  deriving (Show, Eq, Ord, Typeable, Generic, NFData)


class Prefix p where
  make :: B.ByteString -> Word8 -> p
  contains :: p -> p -> Bool
  intersects :: p -> p -> Bool
  subtract :: p -> p -> [p]

instance Prefix Ipv4Prefix where
  make bs nonZeroBits = mkIpv4Block (fourW8sToW32 (B.unpack bs)) (fromIntegral nonZeroBits)
  contains (Ipv4Prefix ip1) (Ipv4Prefix ip2) =   
    V4.firstIpAddress ip1 <= V4.firstIpAddress ip2 && V4.lastIpAddress ip1 >= V4.lastIpAddress ip2
  intersects p1 p2 = rangesIntersect p1 p2 endsV4
  subtract p1 p2 = subtractRange f1 l1 f2 l2 p1 toPrefixes  
    where
      ((!f1, !l1), (!f2, !l2)) = endsV4 p1 p2    
      toPrefixes range = map Ipv4Prefix $ V4.rangeToBlocks range
  
instance Prefix Ipv6Prefix where
  make bs nonZeroBits = mkIpv6Block (someW8ToW128 (B.unpack bs)) (fromIntegral nonZeroBits)
  contains (Ipv6Prefix ip1) (Ipv6Prefix ip2) = 
    V6.firstIpAddress ip1 <= V6.firstIpAddress ip2 && V6.lastIpAddress ip1 >= V6.lastIpAddress ip2
  intersects p1 p2 = rangesIntersect p1 p2 endsV6    
  subtract p1 p2 = subtractRange f1 l1 f2 l2 p1 toPrefixes  
    where
      ((!f1, !l1), (!f2, !l2)) = endsV6 p1 p2
      toPrefixes range = map Ipv6Prefix $ V6.rangeToBlocks range
  

mkIpv4Block :: Word32 -> Word8 -> Ipv4Prefix
mkIpv4Block w32 nonZeroBits = Ipv4Prefix (V4.IpBlock (V4.IpAddress w32) (V4.IpNetMask nonZeroBits))

ipv4RangeToPrefixes :: Word32 -> Word32 -> [Ipv4Prefix]
ipv4RangeToPrefixes w1 w2 = map Ipv4Prefix $ V4.rangeToBlocks $ Range (V4.IpAddress w1) (V4.IpAddress w2) 

mkIpv6Block :: Word128 -> Word8 -> Ipv6Prefix
mkIpv6Block w128 nonZeroBits = Ipv6Prefix (V6.IpBlock (V6.IpAddress w128) (V6.IpNetMask nonZeroBits))

ipv6RangeToPrefixes :: Word128 -> Word128 -> [Ipv6Prefix]
ipv6RangeToPrefixes w1 w2 = map Ipv6Prefix $ V6.rangeToBlocks $ Range (V6.IpAddress w1) (V6.IpAddress w2)

    
subtractRange :: (Enum a, Ord a) => a -> a -> a -> a -> r -> (Range a -> [r]) -> [r]
subtractRange f1 l1 f2 l2 r toPrefixes = 
  case () of  
    _ | f2 > l1 || l2 <= f1  -> [r]
      | f1 < f2 && l1 < l2   -> toPrefixes $ Range f1 (pred f2)
      | f1 < f2 && l1 >= l2  -> toPrefixes (Range f1 (pred f2)) <> toPrefixes (Range l2 l1)
      | f1 >= f2 && l1 >= l2 -> toPrefixes (Range l2 l1)
      | f1 >= f2 && l1 < l2  -> []

{-
[10, 20) - [16,22) = [10,16)
[10, 20) - [12, 17) = [10, 12) + [17,20)
[10, 20) - [7,15) = [15,20)
[10, 20) - [1,22) = ()

-}

rangesIntersect :: Ord a => r1 -> r2 -> (r1 -> r2 -> ((a, a), (a, a))) -> Bool
rangesIntersect p1 p2 getEnds = f1 `between` (f2, l2) || l1 `between` (f2, l2) || f2 `between` (f1, l1)
    where
      ((!f1, !l1), (!f2, !l2)) = getEnds p1 p2

endsV4 :: Ipv4Prefix -> Ipv4Prefix -> ((V4.IpAddress, V4.IpAddress), (V4.IpAddress, V4.IpAddress))
endsV4 (Ipv4Prefix ip1) (Ipv4Prefix ip2) = ((f1, l1), (f2, l2))
  where
    (f1, l1) = (V4.firstIpAddress ip1, V4.lastIpAddress ip1)
    (f2, l2) = (V4.firstIpAddress ip2, V4.lastIpAddress ip2)

endsV6 :: Ipv6Prefix -> Ipv6Prefix -> ((V6.IpAddress, V6.IpAddress), (V6.IpAddress, V6.IpAddress))
endsV6 (Ipv6Prefix ip1) (Ipv6Prefix ip2) = ((f1, l1), (f2, l2))
  where
    (f1, l1) = (V6.firstIpAddress ip1, V6.lastIpAddress ip1)
    (f2, l2) = (V6.firstIpAddress ip2, V6.lastIpAddress ip2)

between :: Ord a => a -> (a, a) -> Bool
between a (b, c) = a >= b && a < c

-- | Prepare resource list for becoming a resourse set, sort, 
-- | merge adjucent ranges and convert to prefixes
sanitise :: (Eq a, Ord a, Prefix a, Ord r, SafeEnum r) => 
            [a] -> (a -> Range r) -> (Range r -> [a]) -> [a]
sanitise p toRange fromRange = concatMap fromRange $ mergeRanges $ map toRange $ S.toAscList $ S.fromList p

sanitiseIpvV4 ps = sanitise ps (\(Ipv4Prefix p) -> V4.blockToRange p) (\r -> map Ipv4Prefix $ V4.rangeToBlocks r)
sanitiseIpvV6 ps = sanitise ps (\(Ipv6Prefix p) -> V6.blockToRange p) (\r -> map Ipv6Prefix $ V6.rangeToBlocks r)


fourW8sToW32 :: [Word8] -> Word32
fourW8sToW32 = \case 
  []                    -> 0 
  [w1]                  -> toW32 w1 24
  [w1, w2]              -> toW32 w1 24 .|. toW32 w2 16
  [w1, w2, w3]          -> toW32 w1 24 .|. toW32 w2 16 .|. toW32 w3 8
  w1 : w2 : w3 : w4 : _ -> toW32 w1 24 .|. toW32 w2 16 .|. toW32 w3 8 .|. fromIntegral w4
  where
    {-# INLINE toW32 #-}
    toW32 !w !s = (fromIntegral w :: Word32) `shiftL` s

someW8ToW128 :: [Word8] -> (Word32, Word32, Word32, Word32)
someW8ToW128 ws = (
    fourW8sToW32 (take 4 unpacked),
    fourW8sToW32 (take 4 drop4),
    fourW8sToW32 (take 4 drop8),
    fourW8sToW32 (take 4 drop12)
  ) 
  where 
    unpacked = rightPad 16 0 ws  
    drop4 = drop 4 unpacked
    drop8 = drop 4 drop4
    drop12 = drop 4 drop8

rightPad :: Int -> a -> [a] -> [a]
rightPad n a = go 0
  where
    go !acc [] | acc < n  = a : go (acc + 1) []
               | otherwise = []  
    go !acc (x : xs) = x : go (acc + 1) xs    

-- Serialise instances
instance Serialise IpPrefix
instance Serialise Ipv4Prefix
instance Serialise Ipv6Prefix
instance Serialise Ipv4Range
instance Serialise Ipv6Range
instance Serialise AsResource
instance Serialise ASN

instance Serialise (V4.IpBlock Canonical)
instance Serialise (V6.IpBlock Canonical)
instance Serialise (Range V4.IpAddress)
instance Serialise (Range V6.IpAddress)
instance Serialise V4.IpAddress
instance Serialise V6.IpAddress
instance Serialise V4.IpNetMask
instance Serialise V6.IpNetMask


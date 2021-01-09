{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Resources.Resources where

import           Prelude                              hiding (subtract)

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Short                as BSS

import           Data.Bits
import qualified Data.List                            as List
import           Data.Maybe
import qualified Data.Set                             as Set
import           Data.Word

import qualified HaskellWorks.Data.Network.Ip.Ipv4    as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6    as V6
import           HaskellWorks.Data.Network.Ip.Range
import           HaskellWorks.Data.Network.Ip.Word128

import           RPKI.Resources.IntervalSet           as IS
import           RPKI.Resources.Types


instance WithSetOps Ipv4Prefix where
    contains (Ipv4Prefix ip1) (Ipv4Prefix ip2) =   
        V4.firstIpAddress ip1 <= V4.firstIpAddress ip2 && V4.lastIpAddress ip1 >= V4.lastIpAddress ip2
    intersection p1 p2 = ipRangesIntersection p1 p2 endsV4 (map Ipv4Prefix . V4.rangeToBlocks)
    normalise = normalisePrefixes
    subtract p1 p2 = subtractRange f1 l1 f2 l2 p1 toPrefixes  
        where
        (!f1, !l1, !f2, !l2) = endsV4 p1 p2          

instance Prefix Ipv4Prefix where
    type Address Ipv4Prefix = V4.IpAddress
    make bs nonZeroBits = mkIpv4Block (fourW8sToW32 (BS.unpack bs)) (fromIntegral nonZeroBits)
    toRange (Ipv4Prefix p) = V4.blockToRange p
    toPrefixes = map Ipv4Prefix . V4.rangeToBlocks

instance Interval Ipv4Prefix where
    type Point Ipv4Prefix = Address Ipv4Prefix
    start = startV4

instance WithSetOps Ipv6Prefix where
    contains (Ipv6Prefix ip1) (Ipv6Prefix ip2) = 
        V6.firstIpAddress ip1 <= V6.firstIpAddress ip2 && V6.lastIpAddress ip1 >= V6.lastIpAddress ip2
    intersection p1 p2 = ipRangesIntersection p1 p2 endsV6 (map Ipv6Prefix . V6.rangeToBlocks)  
    normalise = normalisePrefixes
    subtract p1 p2 = subtractRange f1 l1 f2 l2 p1 toPrefixes  
        where
        (!f1, !l1, !f2, !l2) = endsV6 p1 p2

instance Interval Ipv6Prefix where
    type Point Ipv6Prefix = Address Ipv6Prefix
    start = startV6


instance Prefix Ipv6Prefix where
    type Address Ipv6Prefix = V6.IpAddress
    make bs nonZeroBits = mkIpv6Block (someW8ToW128 (BS.unpack bs)) (fromIntegral nonZeroBits)
    toRange (Ipv6Prefix p) = V6.blockToRange p
    toPrefixes = map Ipv6Prefix . V6.rangeToBlocks

instance WithSetOps AsResource where
    contains = containsAsn    
    intersection = intersectionAsn
    subtract = subtractAsn
    normalise = normaliseAsns

instance Interval AsResource where
    type Point AsResource = ASN
    start (AS a) = a
    start (ASRange a _) = a


mkIpv4Block :: Word32 -> Word8 -> Ipv4Prefix
mkIpv4Block w32 nonZeroBits = Ipv4Prefix (V4.IpBlock (V4.IpAddress w32) (V4.IpNetMask nonZeroBits))

ipv4RangeToPrefixes :: Word32 -> Word32 -> [Ipv4Prefix]
ipv4RangeToPrefixes w1 w2 = map Ipv4Prefix $ V4.rangeToBlocks $ Range (V4.IpAddress w1) (V4.IpAddress w2) 

mkIpv6Block :: Word128 -> Word8 -> Ipv6Prefix
mkIpv6Block w128 nonZeroBits = Ipv6Prefix (V6.IpBlock (V6.IpAddress w128) (V6.IpNetMask nonZeroBits))

ipv6RangeToPrefixes :: Word128 -> Word128 -> [Ipv6Prefix]
ipv6RangeToPrefixes w1 w2 = map Ipv6Prefix $ V6.rangeToBlocks $ Range (V6.IpAddress w1) (V6.IpAddress w2)

    
subtractRange :: (Enum a, Ord a) => a -> a -> a -> a -> r -> (Range a -> [r]) -> [r]
subtractRange f1 l1 f2 l2 r fromRange = 
    case () of  
              _ | f2 > l1  || l2 <= f1 -> [r]
                | f1 <= f2 && l1 < l2  -> fromRange $ Range f1 (pred f2)
                | f1 <= f2 && l1 >= l2 -> fromRange (Range f1 (pred f2)) <> fromRange (Range (succ l2) l1)
                | f1 > f2  && l1 >= l2 -> fromRange (Range l2 l1)
                | f1 > f2  && l1 < l2  -> []

ipRangesIntersection :: Ord a => r -> r -> (r -> r -> (a, a, a, a)) -> (Range a -> [r]) -> [r]
ipRangesIntersection p1 p2 getEnds fromRange = 
    case () of  
          _ | l1 < f2  -> []
            | f1 > l2  -> []
            | otherwise -> fromRange (Range (max f1 f2) (min l1 l2))
    where
        (!f1, !l1, !f2, !l2) = getEnds p1 p2
{-# INLINE ipRangesIntersection #-}    

endsV4 :: Ipv4Prefix -> Ipv4Prefix -> (V4.IpAddress, V4.IpAddress, V4.IpAddress, V4.IpAddress)
endsV4 (Ipv4Prefix ip1) (Ipv4Prefix ip2) = (f1, l1, f2, l2)
    where
        f1 = V4.firstIpAddress ip1
        l1 = V4.lastIpAddress ip1
        f2 = V4.firstIpAddress ip2
        l2 = V4.lastIpAddress ip2
{-# INLINE endsV4 #-}    

endsV6 :: Ipv6Prefix -> Ipv6Prefix -> (V6.IpAddress, V6.IpAddress, V6.IpAddress, V6.IpAddress)
endsV6 (Ipv6Prefix ip1) (Ipv6Prefix ip2) = (f1, l1, f2, l2)
    where
        f1 = V6.firstIpAddress ip1
        l1 = V6.lastIpAddress ip1
        f2 = V6.firstIpAddress ip2
        l2 = V6.lastIpAddress ip2
{-# INLINE endsV6 #-}    

between :: Ord a => a -> (a, a) -> Bool
between a (b, c) = a >= b && a < c
{-# INLINE between #-}

startV4 :: Ipv4Prefix -> V4.IpAddress
startV4 (Ipv4Prefix p) = V4.firstIpAddress p
{-# INLINE startV4 #-}

startV6 :: Ipv6Prefix -> V6.IpAddress
startV6 (Ipv6Prefix p) = V6.firstIpAddress p
{-# INLINE startV6 #-}

-- | Prepare resource list for becoming a resourse set, sort, 
-- | merge adjucent ranges and convert to prefixes  
normalisePrefixes :: Prefix p => [p] -> [p]
normalisePrefixes p = concatMap toPrefixes $ mergeRanges $ 
    map toRange $ Set.toAscList $ Set.fromList p

normaliseAsns :: [AsResource] -> [AsResource]
normaliseAsns asns = 
    mergeAsRanges 
        $ List.sortOn rangeStart 
        $ map simplify asns
    where
        mergeAsRanges []  = []
        mergeAsRanges [r] = [r]        
        mergeAsRanges (r0 : r1 : rs) =  
            case tryMerge r0 r1 of
                Nothing     -> r0 : mergeAsRanges (r1 : rs)
                Just merged -> mergeAsRanges (merged : rs)      
            where
                tryMerge (AS a0) (AS a1) 
                    | a0      == a1 = Just $ AS a0
                    | succ a0 == a1 = Just $ ASRange a0 a1
                    | otherwise = Nothing          

                tryMerge (AS a0) r@(ASRange a10 a11) 
                    | a0 >= a10 && a0 <= a11 = Just r
                    | succ a0 == a10         = Just $ ASRange a0 a11
                    | otherwise              = Nothing 

                tryMerge r@(ASRange a00 a01) (AS a1) 
                    | a1 >= a00 && a1 <= a01 = Just r        
                    | succ a01 == a1 = Just $ ASRange a00 a1
                    | otherwise = Nothing 

                tryMerge (ASRange a00 a01) (ASRange a10 a11) 
                    | succ a01 >= a10 = Just $ ASRange a00 (max a01 a11)
                    | otherwise = Nothing 
                {-# INLINE tryMerge #-}

        rangeStart = \case
            AS a        -> a 
            ASRange a _ -> a
        {-# INLINE rangeStart #-}

        simplify = \case
            AS a -> AS a 
            r@(ASRange a b) | a == b    -> AS a
                            | otherwise -> r
        {-# INLINE simplify #-}
        
    

emptyIpSet :: IpResourceSet
emptyIpSet = IpResourceSet (RS IS.empty) (RS IS.empty)

emptyRS :: RSet (IntervalSet a)
emptyRS = RS IS.empty

toRS :: WithSetOps a => [a] -> RSet (IntervalSet a)
toRS = RS . IS.fromList

allResources :: IpResources -> AsResources -> AllResources
allResources (IpResources (IpResourceSet i4 i6)) (AsResources a) = AllResources i4 i6 a

emptyAllRS :: AllResources
emptyAllRS = AllResources emptyRS emptyRS emptyRS

emptyAll :: PrefixesAndAsns
emptyAll = PrefixesAndAsns IS.empty IS.empty IS.empty

toPrefixesAndAsns :: AllResources -> PrefixesAndAsns
toPrefixesAndAsns (AllResources ipv4 ipv6 asn) = PrefixesAndAsns (get ipv4) (get ipv6) (get asn)
    where 
        get (RS r) = r
        get Inherit = IS.empty


containsAsn :: AsResource -> AsResource -> Bool
containsAsn (AS a) (AS b) = a == b
containsAsn (AS a) (ASRange b0 b1) = a == b0 && a == b1
containsAsn (ASRange a0 a1) (AS b) = a0 <= b && b <= a1
containsAsn (ASRange a0 a1) (ASRange b0 b1) = 
    b0 >= a0 && b0 <= a1 && b1 <= a1 && b1 >= a0
{-# INLINE containsAsn #-}    

intersectionAsn :: AsResource -> AsResource -> [AsResource]
intersectionAsn (AS a) (AS b)
    | a == b = [AS a]
    | otherwise = []          

intersectionAsn (AS a) (ASRange b0 b1)
    | a >= b0 && a <= b1 = [AS a]
    | otherwise = []  

intersectionAsn (ASRange a0 a1) (AS b)
    | b >= a0 && b <= a1 = [AS b]
    | otherwise = []  

intersectionAsn (ASRange a0 a1) (ASRange b0 b1)
    | a1 < b0 || a0 > b1 = []
    | otherwise          = [ASRange (max a0 b0) (min a1 b1)]

{-# INLINE intersectionAsn #-}    

subtractAsn :: AsResource -> AsResource -> [AsResource]
subtractAsn (AS a) (AS b)
    | a == b    = []
    | otherwise = [AS a]

subtractAsn (AS a) (ASRange b0 b1)
    | a >= b0 && a <= b1 = []
    | otherwise          = [AS a]          

subtractAsn (ASRange a0 a1) (AS b)
    | a0 <= b && b <= a1 = optimiseAsns [ASRange a0 (pred b), ASRange (succ b) a1]
    | otherwise          = []

subtractAsn (ASRange a0 a1) (ASRange b0 b1) = 
    optimiseAsns go 
    where 
        go
            | a1 < b0 || a0 > b1   = [ASRange a0 a1]
            | b0 <= a0 && b1 >= a1 = []
            | b0 <= a0 && b1 < a1 = [ASRange (succ b1) a1]
            | b0 > a0 && b1 < a1  = [ASRange a0 (pred b0), ASRange (succ b1) a1]
            | b0 > a0 && b1 >= a1 = [ASRange a0 (pred b0)]

{-# INLINE subtractAsn #-}    

optimiseAsns :: [AsResource] -> [AsResource]
optimiseAsns = mapMaybe f 
    where 
        f (AS a) = Just $ AS a
        f r@(ASRange a b) 
            | a == b    = Just $ AS a
            | a > b     = Nothing
            | otherwise = Just r
{-# INLINE optimiseAsns #-}    


-- Bits munching
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
{-# INLINE fourW8sToW32 #-}

someW8ToW128 :: [Word8] -> (Word32, Word32, Word32, Word32)
someW8ToW128 w8s = (
        fourW8sToW32 (take 4 unpacked),
        fourW8sToW32 (take 4 drop4),
        fourW8sToW32 (take 4 drop8),
        fourW8sToW32 (take 4 drop12)
    ) 
    where 
        unpacked = rightPad 16 0 w8s  
        drop4 = drop 4 unpacked
        drop8 = drop 4 drop4
        drop12 = drop 4 drop8

rightPad :: Int -> a -> [a] -> [a]
rightPad n a = go 0
    where
        go !acc []  | acc < n  = a : go (acc + 1) []
                    | otherwise = []  
        go !acc (x : xs) = x : go (acc + 1) xs    
{-# INLINE rightPad #-}



prefixV4ToBytes :: Ipv4Prefix -> (Word8, Word8, Word8, Word8)
prefixV4ToBytes (Ipv4Prefix (V4.IpBlock p _)) = V4.ipAddressToWords p        

prefixV6ToBytes :: Ipv6Prefix -> (Word32, Word32, Word32, Word32)
prefixV6ToBytes (Ipv6Prefix (V6.IpBlock (V6.IpAddress i) _)) = i

ipv4PrefixLen :: Ipv4Prefix -> PrefixLength       
ipv4PrefixLen (Ipv4Prefix (V4.IpBlock _ (V4.IpNetMask mask))) = PrefixLength mask

ipv6PrefixLen :: Ipv6Prefix -> PrefixLength      
ipv6PrefixLen (Ipv6Prefix (V6.IpBlock _ (V6.IpNetMask mask))) = PrefixLength mask


readIp4 :: String -> Ipv4Prefix
readIp4 s = Ipv4Prefix (read s :: V4.IpBlock V4.Canonical)
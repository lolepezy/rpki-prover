{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Resources.Resources where

import           Prelude                              hiding (subtract, last)

import qualified Data.ByteString                      as BS
import           Data.Text                            (Text)
import qualified Data.Text                            as Text
import           Data.Bits
import qualified Data.List                            as List
import           Data.Maybe
import qualified Data.Set                             as Set
import           Data.Word
import           Text.Read

import qualified HaskellWorks.Data.Network.Ip.Ipv4    as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6    as V6
import           HaskellWorks.Data.Network.Ip.Range
import           HaskellWorks.Data.Network.Ip.Word128
import           HaskellWorks.Data.Network.Ip.Ip      as Ips

import           RPKI.Resources.IntervalContainers           as IS
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
    makePrefix bs nonZeroBits = mkIpv4Block (fourW8sToW32 bs) (fromIntegral nonZeroBits)
    toRange (Ipv4Prefix p) = V4.blockToRange p
    toPrefixes r@Range {..} 
        | first > last = []
        | otherwise = map Ipv4Prefix $ V4.rangeToBlocks r

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
    makePrefix bs nonZeroBits = mkIpv6Block (someW8ToW128 bs) (fromIntegral nonZeroBits)
    toRange (Ipv6Prefix p) = V6.blockToRange p    
    toPrefixes r@Range {..} 
        | first > last = []
        | otherwise = map Ipv6Prefix $ V6.rangeToBlocks r

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
    if | f2 > l1  || l2 <= f1 -> [r]
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
            | otherwise     = Nothing          

        tryMerge (AS a0) r@(ASRange a10 a11) 
            | a0 >= a10 && a0 <= a11 = Just r
            | succ a0 == a10         = Just $ ASRange a0 a11
            | otherwise              = Nothing 

        tryMerge r@(ASRange a00 a01) (AS a1) 
            | a1 >= a00 && a1 <= a01 = Just r        
            | succ a01 == a1         = Just $ ASRange a00 a1
            | otherwise              = Nothing 

        tryMerge (ASRange a00 a01) (ASRange a10 a11) 
            | succ a01 >= a10 = Just $ ASRange a00 (max a01 a11)
            | otherwise       = Nothing         

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

toPrefixesAndAsns :: AllResources -> PrefixesAndAsns
toPrefixesAndAsns (AllResources ipv4 ipv6 asn) = 
    PrefixesAndAsns (g ipv4) (g ipv6) (g asn)
  where 
    g (RS r) = r
    g Inherit = IS.empty

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
    | a0 == b           = optimiseAsns [ASRange (succ a0) a1]
    | a1 == b           = optimiseAsns [ASRange a0 (pred b) ]
    | a0 < b && b <= a1 = optimiseAsns [ASRange a0 (pred b), ASRange (succ b) a1]
    | otherwise         = []

subtractAsn (ASRange a0 a1) (ASRange b0 b1) = 
    optimiseAsns go 
  where 
    go
        | a1 < b0 || a0 > b1   = [ASRange a0 a1]
        | b0 <= a0 && b1 >= a1 = []
        | b0 <= a0 && b1 < a1  = [ASRange (succ b1) a1]
        | b0 > a0 && b1 < a1   = [ASRange a0 (pred b0), ASRange (succ b1) a1]
        | b0 > a0 && b1 >= a1  = [ASRange a0 (pred b0)]
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

unwrapAsns :: [AsResource] -> [ASN]
unwrapAsns = mconcat . map (
    \case
        AS asn          -> [asn]
        ASRange a1 a2
            | a1 >= a2  -> []
            | otherwise -> [ a1 .. a2 ])
{-# INLINE unwrapAsns #-}                


-- Bits munching
fourW8sToW32 :: BS.ByteString -> Word32
fourW8sToW32 bs = case BS.length bs of
    0 -> 0
    1 -> toW32 0 24
    2 -> toW32 0 24 .|. toW32 1 16
    3 -> toW32 0 24 .|. toW32 1 16 .|. toW32 2 8
    _ -> toW32 0 24 .|. toW32 1 16 .|. toW32 2 8 .|. fromIntegral (BS.index bs 3)
  where
    {-# INLINE toW32 #-}
    toW32 !i !s = (fromIntegral (BS.index bs i) :: Word32) `shiftL` s
{-# INLINE fourW8sToW32 #-}

someW8ToW128 :: BS.ByteString -> (Word32, Word32, Word32, Word32)
someW8ToW128 bs = (
        fourW8sAtOffset 0,
        fourW8sAtOffset 4,
        fourW8sAtOffset 8,
        fourW8sAtOffset 12
    )
  where
    len = BS.length bs
    byteAt i = if i < len then fromIntegral (BS.index bs i) else 0 :: Word32
    fourW8sAtOffset off =
        (byteAt off       `shiftL` 24) .|.
        (byteAt (off + 1) `shiftL` 16) .|.
        (byteAt (off + 2) `shiftL`  8) .|.
         byteAt (off + 3)
{-# INLINE someW8ToW128 #-}

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

prefixLen :: IpPrefix -> PrefixLength 
prefixLen = \case 
    Ipv4P p -> ipv4PrefixLen p
    Ipv6P p -> ipv6PrefixLen p
 
-- These are mainly for statically known values in tests
-- 
readIp4 :: String -> Ipv4Prefix
readIp4 (parseIpv4 -> Just p) = p    

readIp6 :: String -> Ipv6Prefix
readIp6 (parseIpv6 -> Just p) = p    
    
parseIpv6 :: String -> Maybe Ipv6Prefix
parseIpv6 s = 
    readMaybe s >>= 
    Ips.canonicalise >>= \case 
        IpBlockV4 _ -> Nothing
        IpBlockV6 b -> Just $ Ipv6Prefix b    

parseIpv4 :: String -> Maybe Ipv4Prefix
parseIpv4 s = Ipv4Prefix <$> readMaybe s

parsePrefixT :: Text -> Maybe IpPrefix 
parsePrefixT = parsePrefix . Text.unpack

parsePrefix :: String -> Maybe IpPrefix 
parsePrefix s =     
    case parseIpv4 s of 
        Nothing   -> Ipv6P <$> parseIpv6 s
        Just ipv4 -> Just $ Ipv4P ipv4

parseAsnT :: Text -> Maybe ASN
parseAsnT = parseAsn . Text.unpack

parseAsn :: String -> Maybe ASN
parseAsn = \case 
    a : s : n
        | (a == 'a' || a == 'A') && (s == 's' || s == 'S') 
            -> ASN <$> readMaybe n
    n -> ASN <$> readMaybe n
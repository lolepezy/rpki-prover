{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns         #-}


module RPKI.Resources where

import Prelude hiding (filter, subtract)

import           Codec.Serialise

import           Control.DeepSeq

import qualified Data.ByteString                       as B

import           Data.Data                             (Typeable)

import           Data.Bits
import           Data.Maybe
import           Data.Either
import           Data.Kind
import qualified Data.List                             as L
import qualified Data.Set                              as S
import qualified Data.Vector                           as V
import           Data.Word
import           GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6
import           HaskellWorks.Data.Network.Ip.Range
import           HaskellWorks.Data.Network.Ip.SafeEnum
import           HaskellWorks.Data.Network.Ip.Validity
import           HaskellWorks.Data.Network.Ip.Word128

data AddrFamily = Ipv4F | Ipv6F
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)
newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data IpPrefix = Ipv4P !Ipv4Prefix | Ipv6P !Ipv6Prefix
  deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

newtype ASN = ASN Int
  deriving (Show, Eq, Ord, Typeable, Generic, NFData, Serialise)

data AsResource =  AS !ASN
                 | ASRange !ASN !ASN
  deriving (Show, Eq, Ord, Typeable, Generic, NFData, Serialise)

class (Eq p, Ord p, SafeEnum (Address p), Ord (Address p)) => Prefix p where
  type Address p :: Type
  make :: B.ByteString -> Word8 -> p
  contains :: p -> p -> Bool
  intersection :: p -> p -> [p]
  subtract :: p -> p -> [p]
  toRange :: p -> Range (Address p)
  toPrefixes :: Range (Address p) -> [p]

data ValidationRFC = Strict_ | Reconsidered_
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data SmallSet a = SS !(V.Vector a)
  deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data RSet r = RS !r | Inherit
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data IpSet = IpSet
    !(RSet (SmallSet Ipv4Prefix))
    !(RSet (SmallSet Ipv6Prefix))
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

newtype IpResources (rfc :: ValidationRFC) = IpResources (IpSet)
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

newtype AsResources (rfc :: ValidationRFC) = AsResources (RSet (SmallSet AsResource))
    deriving (Show, Eq, Ord, Typeable, Generic, Serialise)

data ResourseCheckReconsidered = 
    NestedReconsidered 
  | OverclaimingReconsidered { 
        interesection :: !IpSet,
        overclaiming :: !IpSet   
    }

data ResourseCheckStrict = 
    NestedStrict 
  | ResourseCheckStrict { overclaiming :: !IpSet }

class ResourceCheck (rfc :: ValidationRFC) where
    type Check rfc :: Type
    check :: IpResources rfc -> IpResources rfc -> Check rfc


instance Prefix Ipv4Prefix where
  type Address Ipv4Prefix = V4.IpAddress
  make bs nonZeroBits = mkIpv4Block (fourW8sToW32 (B.unpack bs)) (fromIntegral nonZeroBits)
  contains (Ipv4Prefix ip1) (Ipv4Prefix ip2) =   
    V4.firstIpAddress ip1 <= V4.firstIpAddress ip2 && V4.lastIpAddress ip1 >= V4.lastIpAddress ip2
  intersection p1 p2 = rangesIntersection p1 p2 endsV4 (map Ipv4Prefix . V4.rangeToBlocks)

  toRange (Ipv4Prefix p) = V4.blockToRange p
  toPrefixes = map Ipv4Prefix . V4.rangeToBlocks

  subtract p1 p2 = subtractRange f1 l1 f2 l2 p1 toPrefixes  
    where
      ((!f1, !l1), (!f2, !l2)) = endsV4 p1 p2          
  
instance Prefix Ipv6Prefix where
  type Address Ipv6Prefix = V6.IpAddress
  make bs nonZeroBits = mkIpv6Block (someW8ToW128 (B.unpack bs)) (fromIntegral nonZeroBits)
  contains (Ipv6Prefix ip1) (Ipv6Prefix ip2) = 
    V6.firstIpAddress ip1 <= V6.firstIpAddress ip2 && V6.lastIpAddress ip1 >= V6.lastIpAddress ip2
  intersection p1 p2 = rangesIntersection p1 p2 endsV6 (map Ipv6Prefix . V6.rangeToBlocks)

  toRange (Ipv6Prefix p) = V6.blockToRange p
  toPrefixes = map Ipv6Prefix . V6.rangeToBlocks

  subtract p1 p2 = subtractRange f1 l1 f2 l2 p1 toPrefixes  
    where
      ((!f1, !l1), (!f2, !l2)) = endsV6 p1 p2
      
  

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
    _ | f2 > l1 || l2 <= f1  -> [r]
      | f1 < f2 && l1 < l2   -> fromRange $ Range f1 (pred f2)
      | f1 < f2 && l1 >= l2  -> fromRange (Range f1 (pred f2)) <> fromRange (Range l2 l1)
      | f1 >= f2 && l1 >= l2 -> fromRange (Range l2 l1)
      | f1 >= f2 && l1 < l2  -> []

rangesIntersection :: Ord a => r -> r -> (r -> r -> ((a, a), (a, a))) -> (Range a -> [r]) -> [r]
rangesIntersection p1 p2 getEnds fromRange = 
  case () of  
    _ | l1 <= f2  -> []
      | f1 >= l2  -> []
      | otherwise -> fromRange (Range (max f1 f2) (min l1 l2))
    where
      ((!f1, !l1), (!f2, !l2)) = getEnds p1 p2

endsV4 :: Ipv4Prefix -> Ipv4Prefix -> ((V4.IpAddress, V4.IpAddress), (V4.IpAddress, V4.IpAddress))
endsV4 (Ipv4Prefix ip1) (Ipv4Prefix ip2) = ((f1, l1), (f2, l2))
  where
    f1 = V4.firstIpAddress ip1
    l1 = V4.lastIpAddress ip1
    f2 = V4.firstIpAddress ip2
    l2 = V4.lastIpAddress ip2

endsV6 :: Ipv6Prefix -> Ipv6Prefix -> ((V6.IpAddress, V6.IpAddress), (V6.IpAddress, V6.IpAddress))
endsV6 (Ipv6Prefix ip1) (Ipv6Prefix ip2) = ((f1, l1), (f2, l2))
  where
    f1 = V6.firstIpAddress ip1
    l1 = V6.lastIpAddress ip1
    f2 = V6.firstIpAddress ip2
    l2 = V6.lastIpAddress ip2

between :: Ord a => a -> (a, a) -> Bool
between a (b, c) = a >= b && a < c

-- | Prepare resource list for becoming a resourse set, sort, 
-- | merge adjucent ranges and convert to prefixes
sanitise :: Prefix a => [a] -> [a]
sanitise p = concatMap toPrefixes $ mergeRanges $ map toRange $ S.toAscList $ S.fromList p


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

fromList :: [a] -> SmallSet a
fromList = SS . V.fromList

toList :: SmallSet a -> [a]
toList (SS v) = V.toList v

empty :: Eq a => SmallSet a
empty = fromList []

emptyIpSet :: IpSet
emptyIpSet = IpSet (RS empty) (RS empty)

find :: (a -> Bool) -> SmallSet a -> Maybe a
find p (SS v) = V.find p v    

filter :: (a -> Bool) -> SmallSet a -> SmallSet a
filter p (SS v) = SS $ V.filter p v    

elem :: Eq a => a -> SmallSet a -> Bool   
elem a = isJust . find (==a)


-- TODO Implement resource set subtraction
instance ResourceCheck 'Strict_ where
    type Check 'Strict_ = ResourseCheckStrict
    check (IpResources (IpSet s4 s6)) (IpResources (IpSet b4 b6)) = 
        case (checkSet s4 b4, checkSet s6 b6) of
            (Nothing, Nothing)               -> NestedStrict
            (overclaimingV4, overclaimingV6) -> 
                ResourseCheckStrict $ IpSet (toRS overclaimingV4) (toRS overclaimingV6)                
        where
            toRS = maybe (RS empty) RS            
            checkSet s b = case (s, b) of
                (Inherit, Inherit) -> Nothing
                (RS ss,   Inherit) -> Just ss
                (Inherit, RS _)    -> Nothing
                (RS ss,   RS bs)   -> Just $ filter notInBigOne ss
                    where 
                        notInBigOne p = not $ isJust $ find (\bp -> p `contains` bp) bs

instance ResourceCheck 'Reconsidered_ where
    type Check 'Reconsidered_ = ResourseCheckReconsidered
    check (IpResources (IpSet s4 s6)) (IpResources (IpSet b4 b6)) = 
        case (checkSet s4 b4, checkSet s6 b6) of
            (Nothing, Nothing)               -> NestedReconsidered
            (overclaimingV4, overclaimingV6) -> 
                OverclaimingReconsidered
                    (IpSet (toRS overclaimingV4) (toRS overclaimingV6))
                    (IpSet (toRS overclaimingV4) (toRS overclaimingV6))
        where
            toRS = maybe (RS empty) RS            
            checkSet s b = case (s, b) of
                (Inherit, Inherit) -> Nothing
                (RS ss,   Inherit) -> Just ss
                (Inherit, RS _)    -> Nothing
                (RS ss,   RS bs)   -> Just $ filter notInBigOne ss
                    where 
                        notInBigOne p = not $ isJust $ find (\bp -> p `contains` bp) bs


newtype Intersection a = Intersection (SmallSet a)
newtype Overclaiming a = Overclaiming (SmallSet a)


-- | For two sets, find intersecting and overclaming resource subsets
-- 
intersectionAndOverclaimed :: (Eq a, Prefix a) =>    
                            SmallSet a -> SmallSet a -> 
                            (Intersection a, Overclaiming a)
intersectionAndOverclaimed smaller bigger =     
    (Intersection $ fromList intersectionRS, 
     Overclaiming $ fromList overclaimingRS)
  where
    intersectionRS = sanitise $ good <> concatMap fst problematic 
    overclaimingRS = sanitise $ concatMap snd problematic

    (problematic, good) = partitionEithers $ concatMap overclamingPart $ toList smaller            

    intersections prefix =       
      L.filter (not . null . fst) $
      L.map (\big -> (prefix `intersection` big, big)) biggerList

    overclamingPart prefix = (flip map) (intersections prefix) $ 
        \(intersecting, big) ->        
            if big `contains` prefix
              then Right prefix
              else Left (intersecting, big `subtract` prefix)

    biggerList = toList bigger



-- Serialise instances
instance Serialise (V4.IpBlock Canonical)
instance Serialise (V6.IpBlock Canonical)
instance Serialise (Range V4.IpAddress)
instance Serialise (Range V6.IpAddress)
instance Serialise V4.IpAddress
instance Serialise V6.IpAddress
instance Serialise V4.IpNetMask
instance Serialise V6.IpNetMask


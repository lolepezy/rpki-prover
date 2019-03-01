{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module RPKI.IP where
  
import Data.Bifunctor

import qualified Data.ByteString as B  
import qualified Data.Text as T  
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Text.Short as TS

import Data.Data (Typeable)

import Data.Word
import Data.Bits

import HaskellWorks.Data.Network.Ip.Validity
import HaskellWorks.Data.Network.Ip.Range
import HaskellWorks.Data.Network.Ip.Word128
import HaskellWorks.Data.Network.Ip.Ipv4 as V4
import HaskellWorks.Data.Network.Ip.Ipv6 as V6

import Data.Char (chr)

data AddrFamily = Ipv4F | Ipv6F
    deriving (Show, Eq, Ord, Typeable)

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable)
newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical) 
    deriving (Show, Eq, Ord, Typeable)

newtype Ipv4Range = Ipv4Range (Range V4.IpAddress) 
    deriving (Show, Eq, Ord, Typeable)
newtype Ipv6Range = Ipv6Range (Range V6.IpAddress) 
    deriving (Show, Eq, Ord, Typeable)

data IpPrefix (f :: AddrFamily) where
    Ipv4P :: !Ipv4Prefix -> IpPrefix 'Ipv4F
    Ipv6P :: !Ipv6Prefix -> IpPrefix 'Ipv6F

deriving instance Show (IpPrefix f)
deriving instance Eq (IpPrefix f)
deriving instance Ord (IpPrefix f)
deriving instance Typeable (IpPrefix f)

data IpRange (f :: AddrFamily) where
    Ipv4R :: !Ipv4Range -> IpRange 'Ipv4F
    Ipv6R :: !Ipv6Range -> IpRange 'Ipv6F

deriving instance Show (IpRange f)
deriving instance Eq (IpRange f)
deriving instance Ord (IpRange f)
deriving instance Typeable (IpRange f)


mkIpv4Block :: Word32 -> Word8 -> Ipv4Prefix
mkIpv4Block w32 nonZeroBits = Ipv4Prefix (V4.IpBlock (V4.IpAddress w32) (V4.IpNetMask nonZeroBits))

mkIpv4 :: Word32 -> Word32 -> Either Ipv4Range Ipv4Prefix
mkIpv4 w1 w2 = 
    let r = Range (V4.IpAddress w1) (V4.IpAddress w2) 
    in case V4.rangeToBlocks r of
        [b]     -> Right $ Ipv4Prefix b
        b1 : bs -> Left  $ Ipv4Range r        

mkIpv6Block :: (Word32, Word32, Word32, Word32) -> Word8 -> Ipv6Prefix
mkIpv6Block w128 nonZeroBits = Ipv6Prefix (V6.IpBlock (V6.IpAddress w128) (V6.IpNetMask nonZeroBits))

mkIpv6 :: Word128 -> Word128 -> Either Ipv6Range Ipv6Prefix
mkIpv6 w1 w2 = 
    let r = Range (V6.IpAddress w1) (V6.IpAddress w2) 
    in case V6.rangeToBlocks r of
        [b]     -> Right $ Ipv6Prefix b
        b1 : bs -> Left  $ Ipv6Range r

mkV4Prefix :: B.ByteString -> Word8 -> Ipv4Prefix
mkV4Prefix bs nonZeroBits = 
  mkIpv4Block (fourW8sToW32 (B.unpack bs)) (fromIntegral nonZeroBits)

mkV6Prefix :: B.ByteString -> Word8 -> Ipv6Prefix
mkV6Prefix bs nonZeroBits = 
  mkIpv6Block (someW8ToW128 (B.unpack bs)) (fromIntegral nonZeroBits)


fourW8sToW32 :: [Word8] -> Word32
fourW8sToW32 ws = fst $ L.foldl' foldW8toW32 (0 :: Word32, 24) ws
  where
    foldW8toW32 (w32, shift') w8 = (
          w32 + (fromIntegral w8 :: Word32) `shiftL` shift', 
          shift' - 8)       

someW8ToW128 :: [Word8] -> (Word32, Word32, Word32, Word32)
someW8ToW128 ws = (
    fourW8sToW32 (take 4 unpacked),
    fourW8sToW32 (take 4 (drop 4 unpacked)),
    fourW8sToW32 (take 4 (drop 8 unpacked)),
    fourW8sToW32 (take 4 (drop 12 unpacked))
  ) 
  where unpacked = rightPad 16 0 ws  

rightPad :: Int -> a -> [a] -> [a]
rightPad n a as = go 0 as
  where
    go acc [] | acc < n  = a : go (acc + 1) []
              | otherwise = []  
    go acc (x : xs) = x : go (acc + 1) xs    

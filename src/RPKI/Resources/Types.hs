{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module RPKI.Resources.Types where

import Prelude hiding (subtract)

import           Codec.Serialise
import           Control.DeepSeq

import qualified Data.ByteString                       as BS

import           Data.Kind
import           Data.Vector                           (Vector)
import           Data.Word
import           GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6
import           HaskellWorks.Data.Network.Ip.Range
import           HaskellWorks.Data.Network.Ip.SafeEnum
import           HaskellWorks.Data.Network.Ip.Validity

data AddrFamily = Ipv4F | Ipv6F
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical)
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data IpPrefix = Ipv4P !Ipv4Prefix | Ipv6P !Ipv6Prefix
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

newtype ASN = ASN Word32
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (NFData, Serialise)  
    deriving newtype Enum

data AsResource = AS !ASN
                | ASRange !ASN !ASN
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise


class WithSetOps p where
    contains :: p -> p -> Bool
    intersection :: p -> p -> [p]
    subtract :: p -> p -> [p]
    normalise :: [p] -> [p]

class (Eq p, Ord p, SafeEnum (Address p), Ord (Address p), WithSetOps p) => Prefix p where
    type Address p :: Type
    make :: BS.ByteString -> Word8 -> p
    toRange :: p -> Range (Address p)
    toPrefixes :: Range (Address p) -> [p]  


data ValidationRFC = Strict_ | Reconsidered_
    deriving stock (Show, Eq, Ord, Generic) 

data RSet r = RS !r | Inherit
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data IpResourceSet = IpResourceSet
    !(RSet (IntervalSet Ipv4Prefix))
    !(RSet (IntervalSet Ipv6Prefix))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

newtype IpResources = IpResources IpResourceSet
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

newtype AsResources = AsResources (RSet (IntervalSet AsResource))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data AllResources = AllResources 
    !(RSet (IntervalSet Ipv4Prefix))
    !(RSet (IntervalSet Ipv6Prefix))
    !(RSet (IntervalSet AsResource))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

data PrefixesAndAsns = PrefixesAndAsns 
    !(IntervalSet Ipv4Prefix)
    !(IntervalSet Ipv6Prefix)
    !(IntervalSet AsResource)
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

newtype Nested a = Nested a
newtype Overclaiming a = Overclaiming a


newtype VerifiedRS a = VerifiedRS a
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise

class (WithSetOps p, Eq (Point p), Ord (Point p)) => Interval p where
    type Point p :: Type
    start :: p -> Point p

-- | Representation of the resource set
newtype IntervalSet a = IntervalSet (Vector a) 
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass Serialise


-- Serialise instances
instance Serialise (V4.IpBlock Canonical)
instance Serialise (V6.IpBlock Canonical)
instance Serialise (Range V4.IpAddress)
instance Serialise (Range V6.IpAddress)
instance Serialise V4.IpAddress
instance Serialise V6.IpAddress
instance Serialise V4.IpNetMask
instance Serialise V6.IpNetMask
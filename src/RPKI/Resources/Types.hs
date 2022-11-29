{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}


module RPKI.Resources.Types where

import Prelude hiding (subtract)

import           Codec.Serialise
import           Control.DeepSeq

import qualified Data.ByteString                       as BS

import           Data.Kind
import           Data.Vector                           (Vector)
import           Data.Word                             (Word8, Word32)
import           GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6
import           HaskellWorks.Data.Network.Ip.Range
import           HaskellWorks.Data.Network.Ip.SafeEnum
import           HaskellWorks.Data.Network.Ip.Validity

import           RPKI.Store.Base.Serialisation


data AddrFamily = Ipv4F | Ipv6F
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical)
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

data IpPrefix = Ipv4P Ipv4Prefix | Ipv6P Ipv6Prefix
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

newtype ASN = ASN Word32
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (NFData, TheBinary)  
    deriving newtype Enum

newtype PrefixLength = PrefixLength Word8
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass TheBinary

data AsResource = AS ASN
                | ASRange ASN ASN
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary


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

data RSet r = RS r | Inherit
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

data IpResourceSet = IpResourceSet
        (RSet (IntervalSet Ipv4Prefix))
        (RSet (IntervalSet Ipv6Prefix))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

newtype IpResources = IpResources IpResourceSet
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

newtype AsResources = AsResources (RSet (IntervalSet AsResource))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

data AllResources = AllResources 
        (RSet (IntervalSet Ipv4Prefix))
        (RSet (IntervalSet Ipv6Prefix))
        (RSet (IntervalSet AsResource))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

data PrefixesAndAsns = PrefixesAndAsns 
        (IntervalSet Ipv4Prefix)
        (IntervalSet Ipv6Prefix)
        (IntervalSet AsResource)
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass TheBinary

newtype Nested a = Nested a
    deriving stock (Show, Eq, Ord, Generic) 

newtype Overclaiming a = Overclaiming a
    deriving stock (Show, Eq, Ord, Generic) 

newtype VerifiedRS a = VerifiedRS a
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass TheBinary

class (WithSetOps p, Eq p, Eq (Point p), Ord (Point p)) => Interval p where
    type Point p :: Type
    start :: p -> Point p

-- | Representation of the resource set
newtype IntervalSet a = IntervalSet (Vector a) 
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass TheBinary


-- Serialise instances
instance Serialise (V4.IpBlock Canonical)
instance Serialise (V6.IpBlock Canonical)
instance Serialise (Range V4.IpAddress)
instance Serialise (Range V6.IpAddress)
instance Serialise V4.IpAddress
instance Serialise V6.IpAddress
instance Serialise V4.IpNetMask
instance Serialise V6.IpNetMask

instance Show PrefixesAndAsns where
    show (PrefixesAndAsns v4 v6 asn) = 
        show v4 <> ", " <> show v6 <> ", " <> show asn
        -- where 
        --     print x = if empty x then Nothing else Just (show x)

instance Show a => Show (IntervalSet a) where
    show (IntervalSet is) = show is        
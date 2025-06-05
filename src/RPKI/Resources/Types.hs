{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE UndecidableInstances       #-}


module RPKI.Resources.Types where

import Prelude hiding (subtract)

import           Control.DeepSeq
import qualified Data.ByteString                       as BS

import           Data.Kind
import           Data.Store
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
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

newtype Ipv4Prefix = Ipv4Prefix (V4.IpBlock Canonical) 
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

newtype Ipv6Prefix = Ipv6Prefix (V6.IpBlock Canonical)
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

data IpPrefix = Ipv4P Ipv4Prefix | Ipv6P Ipv6Prefix
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

newtype ASN = ASN Word32
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)
    deriving newtype Enum

newtype PrefixLength = PrefixLength Word8
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data AsResource = AS ASN
                | ASRange ASN ASN
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

instance Show Ipv4Prefix where
    show (Ipv4Prefix block) = show block

instance Show Ipv6Prefix where
    show (Ipv6Prefix block) = show block

instance Show IpPrefix where
    show (Ipv4P block) = show block
    show (Ipv6P block) = show block

instance Show ASN where
    show (ASN asn) = "AS" <> show asn

instance Show PrefixLength where
    show (PrefixLength len) = show len

instance Show AsResource where
    show (AS asn) = show asn
    show (ASRange asn1 asn2) = show asn1 <> " - " <> show asn2

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


data RSet r = RS r | Inherit
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

data IpResourceSet = IpResourceSet
        (RSet (IntervalSet Ipv4Prefix))
        (RSet (IntervalSet Ipv6Prefix))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

newtype IpResources = IpResources IpResourceSet
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

newtype AsResources = AsResources (RSet (IntervalSet AsResource))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

data AllResources = AllResources 
        (RSet (IntervalSet Ipv4Prefix))
        (RSet (IntervalSet Ipv6Prefix))
        (RSet (IntervalSet AsResource))
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

data PrefixesAndAsns = PrefixesAndAsns 
        (IntervalSet Ipv4Prefix)
        (IntervalSet Ipv6Prefix)
        (IntervalSet AsResource)
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

newtype Nested a = Nested a
    deriving stock (Show, Eq, Ord, Generic) 

newtype Overclaiming a = Overclaiming a
    deriving stock (Show, Eq, Ord, Generic) 

newtype VerifiedRS a = VerifiedRS a
    deriving stock (Show, Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)

class (WithSetOps p, Eq p, Eq (Point p), Ord (Point p)) => Interval p where
    type Point p :: Type
    start :: p -> Point p

-- | Representation of the resource set
newtype IntervalSet a = IntervalSet (Vector a) 
    deriving stock (Eq, Ord, Generic) 
    deriving anyclass (TheBinary, NFData)


-- Store instances
instance Store (V4.IpBlock Canonical)
instance Store (V6.IpBlock Canonical)
instance Store (Range V4.IpAddress)
instance Store (Range V6.IpAddress)
instance Store V4.IpAddress
instance Store V6.IpAddress
instance Store V4.IpNetMask
instance Store V6.IpNetMask


deriving anyclass instance NFData (V4.IpBlock Canonical) 
deriving anyclass instance NFData V4.IpAddress 
deriving anyclass instance NFData V4.IpNetMask
deriving anyclass instance NFData (V6.IpBlock Canonical) 
deriving anyclass instance NFData V6.IpAddress 
deriving anyclass instance NFData V6.IpNetMask

instance Show PrefixesAndAsns where
    show (PrefixesAndAsns v4 v6 asn) = 
        show v4 <> ", " <> show v6 <> ", " <> show asn

instance Show a => Show (IntervalSet a) where
    show (IntervalSet is) = show is        

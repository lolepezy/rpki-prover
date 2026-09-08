{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The packed, unboxed representations of a VRP, one per address family.
module RPKI.Domain.Packed (
    PackedVrp4(..),
    PackedVrp6(..),
    PackedVrp4Repr,
    PackedVrp6Repr,
) where

import           Control.DeepSeq             (NFData)
import           Data.Word                   (Word8, Word32, Word64)

import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU

import           GHC.Generics

data PackedVrp4 = PackedVrp4 {
        packed4Asn    :: {-# UNPACK #-} !Word32,
        packed4Addr   :: {-# UNPACK #-} !Word32,
        packed4Len    :: {-# UNPACK #-} !Word8,
        packed4MaxLen :: {-# UNPACK #-} !Word8
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

data PackedVrp6 = PackedVrp6 {
        packed6Asn    :: {-# UNPACK #-} !Word32,
        packed6AddrHi :: {-# UNPACK #-} !Word64,
        packed6AddrLo :: {-# UNPACK #-} !Word64,
        packed6Len    :: {-# UNPACK #-} !Word8,
        packed6MaxLen :: {-# UNPACK #-} !Word8
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

type PackedVrp4Repr = (Word32, Word32, Word8, Word8)
type PackedVrp6Repr = (Word32, Word64, Word64, Word8, Word8)

instance VU.IsoUnbox PackedVrp4 PackedVrp4Repr where
    toURepr (PackedVrp4 a addr l m) = (a, addr, l, m)
    fromURepr (a, addr, l, m) = PackedVrp4 a addr l m
    {-# INLINE toURepr #-}
    {-# INLINE fromURepr #-}

instance VU.IsoUnbox PackedVrp6 PackedVrp6Repr where
    toURepr (PackedVrp6 a hi lo l m) = (a, hi, lo, l, m)
    fromURepr (a, hi, lo, l, m) = PackedVrp6 a hi lo l m
    {-# INLINE toURepr #-}
    {-# INLINE fromURepr #-}

newtype instance VU.MVector s PackedVrp4 = MV_PackedVrp4 (VU.MVector s PackedVrp4Repr)
newtype instance VU.Vector    PackedVrp4 = V_PackedVrp4  (VU.Vector    PackedVrp4Repr)

deriving via (PackedVrp4 `VU.As` PackedVrp4Repr)
    instance VGM.MVector VU.MVector PackedVrp4
deriving via (PackedVrp4 `VU.As` PackedVrp4Repr)
    instance VG.Vector VU.Vector PackedVrp4

instance VU.Unbox PackedVrp4

newtype instance VU.MVector s PackedVrp6 = MV_PackedVrp6 (VU.MVector s PackedVrp6Repr)
newtype instance VU.Vector    PackedVrp6 = V_PackedVrp6  (VU.Vector    PackedVrp6Repr)

deriving via (PackedVrp6 `VU.As` PackedVrp6Repr)
    instance VGM.MVector VU.MVector PackedVrp6
deriving via (PackedVrp6 `VU.As` PackedVrp6Repr)
    instance VG.Vector VU.Vector PackedVrp6

instance VU.Unbox PackedVrp6

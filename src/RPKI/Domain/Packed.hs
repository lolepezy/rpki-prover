{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The packed, unboxed representations of a VRP, one per address family.
--
-- A VRP is an ASN, a prefix and a max length -- 10 bytes of information for
-- IPv4 and 22 for IPv6. Keeping both families in one uniform record meant
-- carrying IPv6-sized address fields for the ~85% of VRPs that are IPv4, at
-- 23 bytes each; split, they cost what they are (measured: 10 and 22 bytes per
-- element, ~11.8 on a realistic mix).
--
-- This lives in its own module because declaring the 'VU.Vector' data family
-- instances re-exports the @Vector@ type constructor from whichever module
-- declares them, and 'RPKI.Domain' has no export list, so it would collide
-- with every other @Vector@ in scope downstream.
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


-- | An IPv4 VRP as plain words.
--
-- Field order is load-bearing: derived Ord compares fields in declaration
-- order, and the address compares like the Word32 it is.
data PackedVrp4 = PackedVrp4 {
        packed4Asn    :: {-# UNPACK #-} !Word32,
        packed4Addr   :: {-# UNPACK #-} !Word32,
        packed4Len    :: {-# UNPACK #-} !Word8,
        packed4MaxLen :: {-# UNPACK #-} !Word8
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | An IPv6 VRP as plain words, the address split into two 64-bit halves so
-- that comparing (hi, lo) as unsigned words reproduces comparing the four
-- 32-bit words of the address in order.
data PackedVrp6 = PackedVrp6 {
        packed6Asn    :: {-# UNPACK #-} !Word32,
        packed6AddrHi :: {-# UNPACK #-} !Word64,
        packed6AddrLo :: {-# UNPACK #-} !Word64,
        packed6Len    :: {-# UNPACK #-} !Word8,
        packed6MaxLen :: {-# UNPACK #-} !Word8
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | How they are laid out inside the vector: the same fields in the same
-- order. 'VU.Vector' keeps one dense array per field, so an element costs the
-- sum of the field widths and nothing else.
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

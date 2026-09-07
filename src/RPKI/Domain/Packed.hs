{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The packed, unboxed representation of a VRP.
--
-- This lives in its own module purely because declaring the 'VU.Vector' data
-- family instances would otherwise re-export the @Vector@ type constructor
-- from 'RPKI.Domain', which has no export list, and clash with every other
-- @Vector@ in scope downstream. The export list here keeps that contained.
module RPKI.Domain.Packed (
    PackedVrp(..),
    PackedVrpRepr,
) where

import           Control.DeepSeq             (NFData)
import           Data.Word                   (Word8, Word32, Word64)

import qualified Data.Vector.Generic         as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed         as VU

import           GHC.Generics


-- | One VRP flattened into plain words: ASN, an IPv6 flag, the address as two
-- 64-bit halves, the prefix length and the ROA max length.
--
-- Field order is deliberate, and load-bearing: derived Ord compares fields in
-- declaration order, which reproduces derived Ord on @Vrp@ exactly.
-- 'packedIsV6' stands in for the @IpPrefix@ constructor tag that derived Ord
-- compares first, and an IPv4 address in the high half compares like the
-- Word32 it is.
--
-- Stored in an unboxed vector it costs the sum of its field widths -- 23
-- bytes, measured -- because 'VU.Vector' keeps one dense array per field
-- rather than a vector of constructors. That is the same as the tuple this
-- replaced; the record is for being able to name the fields. The UNPACK
-- pragmas only matter for values that escape the vector.
data PackedVrp = PackedVrp {
        packedAsn    :: {-# UNPACK #-} !Word32,
        packedIsV6   :: {-# UNPACK #-} !Word8,
        packedAddrHi :: {-# UNPACK #-} !Word64,
        packedAddrLo :: {-# UNPACK #-} !Word64,
        packedLen    :: {-# UNPACK #-} !Word8,
        packedMaxLen :: {-# UNPACK #-} !Word8
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | How it is laid out inside the vector: the same fields in the same order,
-- so the stored bytes are exactly what the tuple gave.
type PackedVrpRepr = (Word32, Word8, Word64, Word64, Word8, Word8)

instance VU.IsoUnbox PackedVrp PackedVrpRepr where
    toURepr (PackedVrp a v hi lo l m) = (a, v, hi, lo, l, m)
    fromURepr (a, v, hi, lo, l, m) = PackedVrp a v hi lo l m
    {-# INLINE toURepr #-}
    {-# INLINE fromURepr #-}

newtype instance VU.MVector s PackedVrp = MV_PackedVrp (VU.MVector s PackedVrpRepr)
newtype instance VU.Vector    PackedVrp = V_PackedVrp  (VU.Vector    PackedVrpRepr)

deriving via (PackedVrp `VU.As` PackedVrpRepr)
    instance VGM.MVector VU.MVector PackedVrp
deriving via (PackedVrp `VU.As` PackedVrpRepr)
    instance VG.Vector VU.Vector PackedVrp

instance VU.Unbox PackedVrp

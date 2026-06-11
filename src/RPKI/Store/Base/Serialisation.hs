{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Store.Base.Serialisation (
    TheBinary,
    LexOrdKey64(..),
    serialise_, deserialise_, deserialiseOrFail_
    ) where


import Data.Bifunctor
import Data.Bits   (xor, shiftR, shiftL, (.|.))
import Data.Word   (Word64, Word8)
import Data.Int    (Int64)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Data.Hashable
import Data.Store ( Store(..), Peek, decode, decodeEx, encode, Size(ConstSize) )

import qualified Data.Text                        as Text

import qualified Data.ByteString as BS

type TheBinary = Store

-- | 64-bit integers that preserve their order when serialized as byte strings.
-- Used as keys in a tree-based DB (LMDB): two values compare the same in
-- Haskell as they would when LMDB compares their raw byte representations.
-- NOTE: Word64 sounds like a more natural choice for this, but we want to keep 
-- it compatible with stuff like Instant and other library types that are 
-- based on Int64.
newtype LexOrdKey64 = LexOrdKey64 Int64
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData
    deriving anyclass Hashable

-- Encoding: XOR the Int64 bit-pattern with 0x8000000000000000 (flip sign bit)
-- then write big-endian, mapping Int64 order onto unsigned lexicographic order.
instance Store LexOrdKey64 where
    size = ConstSize 8
    poke (LexOrdKey64 i) = do
        let w = (fromIntegral i :: Word64) `xor` 0x8000000000000000
        poke (fromIntegral (w `shiftR` 56) :: Word8)
        poke (fromIntegral (w `shiftR` 48) :: Word8)
        poke (fromIntegral (w `shiftR` 40) :: Word8)
        poke (fromIntegral (w `shiftR` 32) :: Word8)
        poke (fromIntegral (w `shiftR` 24) :: Word8)
        poke (fromIntegral (w `shiftR` 16) :: Word8)
        poke (fromIntegral (w `shiftR` 8)  :: Word8)
        poke (fromIntegral w               :: Word8)
    peek = do
        b0 <- peek :: Peek Word8; b1 <- peek :: Peek Word8
        b2 <- peek :: Peek Word8; b3 <- peek :: Peek Word8
        b4 <- peek :: Peek Word8; b5 <- peek :: Peek Word8
        b6 <- peek :: Peek Word8; b7 <- peek :: Peek Word8
        let w = (fromIntegral b0 `shiftL` 56) .|. (fromIntegral b1 `shiftL` 48) .|.
                (fromIntegral b2 `shiftL` 40) .|. (fromIntegral b3 `shiftL` 32) .|.
                (fromIntegral b4 `shiftL` 24) .|. (fromIntegral b5 `shiftL` 16) .|.
                (fromIntegral b6 `shiftL` 8)  .|. (fromIntegral b7 :: Word64)
        pure $ LexOrdKey64 $ fromIntegral (w `xor` 0x8000000000000000)

serialise_ :: TheBinary a => a -> BS.ByteString
serialise_ = encode
{-# INLINE serialise_ #-}

deserialise_ :: TheBinary a => BS.ByteString -> a
deserialise_ = decodeEx
{-# INLINE deserialise_ #-}

deserialiseOrFail_ :: TheBinary a => BS.ByteString -> Either Text.Text a
deserialiseOrFail_ bs = first (Text.pack . show) $ decode bs
{-# INLINE deserialiseOrFail_ #-}

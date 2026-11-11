{- | VRPs as CSV and JSON text, written straight into the output buffer, i.e.
   without making a String, a Builder or an aeson Value for every field.

   For a million VRPs it is ~100ms instead of 0.5-1s and hundreds of MB of
   allocations instead of 5-7GB. The text is the same as the 'Show' and the
   generic 'ToJSON' instances give, which tests check.
-}
module RPKI.Http.Encoding (
    vrpCsv,
    packed4Prim,
    packed6Prim,
    vrpJsonFields
) where

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Builder.Prim     as BP
import           Data.ByteString.Builder.Prim.Internal (boundedPrim, runB)
import           Data.ByteString.Unsafe           (unsafeUseAsCStringLen)

import           Control.Monad                    (foldM)

import           Data.Bits                        (shiftR)
import           Data.Char                        (ord)
import           Data.Word                        (Word8, Word16, Word32, Word64)

import           Foreign.Marshal.Utils            (copyBytes)
import           Foreign.Ptr                      (Ptr, castPtr, plusPtr)
import           Foreign.Storable                 (poke)

import           RPKI.Domain
import           RPKI.Domain.Packed               (PackedVrp4 (..), PackedVrp6 (..))
import           RPKI.Domain.Vrps                 (pack4, pack6)
import           RPKI.Resources.Types


-- | "ASN,prefix,max length" of a VRP, the same text as their 'Show' instances
-- give, which a test checks.
vrpCsv :: Vrp -> BB.Builder
vrpCsv (Vrp asn (Ipv4P p) maxLength) = BP.primBounded packed4Prim $ pack4 asn p maxLength
vrpCsv (Vrp asn (Ipv6P p) maxLength) = BP.primBounded packed6Prim $ pack6 asn p maxLength

-- | Write a VRP straight into the output buffer, i.e. without making a String
-- or a Builder for every field of it: for a million VRPs it is ~70ms instead
-- of ~500ms and 250MB of allocations instead of 4.7GB.
--
-- The bounds are the longest text of every field: 'AS' and 10 digits of
-- an ASN, 15 characters of an IPv4 and 39 of an IPv6 address, 3 digits of a
-- prefix length and of a max length and the separators between them.
packed4Prim :: BP.BoundedPrim PackedVrp4
packed4Prim = boundedPrim (12 + 1 + 15 + 1 + 3 + 1 + 3) $ \(PackedVrp4 asn address len maxLength) p ->
    asnAt asn p >>= charAt ',' >>= ipv4At address >>= charAt '/' >>= runB BP.word8Dec len
        >>= charAt ',' >>= runB BP.word8Dec maxLength

packed6Prim :: BP.BoundedPrim PackedVrp6
packed6Prim = boundedPrim (12 + 1 + 39 + 1 + 3 + 1 + 3) $ \(PackedVrp6 asn hi lo len maxLength) p ->
    asnAt asn p >>= charAt ',' >>= ipv6At hi lo >>= charAt '/' >>= runB BP.word8Dec len
        >>= charAt ',' >>= runB BP.word8Dec maxLength

asnAt :: Word32 -> Ptr Word8 -> IO (Ptr Word8)
asnAt asn p = charAt 'A' p >>= charAt 'S' >>= runB BP.word32Dec asn

ipv4At :: Word32 -> Ptr Word8 -> IO (Ptr Word8)
ipv4At address p =
    octetAt 24 p >>= charAt '.' >>= octetAt 16 >>= charAt '.'
        >>= octetAt 8 >>= charAt '.' >>= octetAt 0
  where
    octetAt shift = runB BP.word8Dec (fromIntegral $ address `shiftR` shift)

-- | The same as 'show' of 'Data.IP.IPv6' (RFC 5952 and what glibc's inet_ntop
-- does): IPv4-mapped and IPv4-compatible addresses with a dotted IPv4 part,
-- otherwise the first of the longest runs of two or more zero fields as "::".
ipv6At :: Word64 -> Word64 -> Ptr Word8 -> IO (Ptr Word8)
ipv6At hi lo p
    | w1 == 0 && w2 == 0 && w3 == 0xffff =
        foldM (flip charAt) p ("::ffff:" :: String) >>= ipv4At w4
    | w1 == 0 && w2 == 0 && w3 == 0 && w4 >= 0x10000 =
        charAt ':' p >>= charAt ':' >>= ipv4At w4
    | zerosLength > 1 =
        fieldsAt 0 zerosStart p >>= charAt ':' >>= charAt ':' >>= fieldsAt (zerosStart + zerosLength) 8
    | otherwise =
        fieldsAt 0 8 p
  where
    w1 = fromIntegral (hi `shiftR` 32) :: Word32
    w2 = fromIntegral hi :: Word32
    w3 = fromIntegral (lo `shiftR` 32) :: Word32
    w4 = fromIntegral lo :: Word32

    field :: Int -> Word16
    field i = fromIntegral $ (if i < 4 then hi else lo) `shiftR` (48 - 16 * (i `mod` 4))

    (zerosStart, zerosLength) = longestZeros 0 0 0 0 0
      where
        longestZeros :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
        longestZeros i bestStart bestLength runStart len
            | i == 8           = if len > bestLength then (runStart, len) else (bestStart, bestLength)
            | field i == 0     = longestZeros (i + 1) bestStart bestLength (if len == 0 then i else runStart) (len + 1)
            | len > bestLength = longestZeros (i + 1) runStart len 0 0
            | otherwise        = longestZeros (i + 1) bestStart bestLength 0 0

    -- Fields [from, to) separated by ':'
    fieldsAt :: Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
    fieldsAt from to ptr
        | from >= to = pure ptr
        | otherwise  = do
            next <- runB BP.word16Hex (field from) ptr
            if from + 1 < to
                then charAt ':' next >>= fieldsAt (from + 1) to
                else pure next

charAt :: Char -> Ptr Word8 -> IO (Ptr Word8)
charAt c p = do
    poke p (fromIntegral (ord c) :: Word8)
    pure $ p `plusPtr` 1
{-# INLINE charAt #-}


-- | The fields of a VRP as a JSON object without the closing brace, so that
-- more can follow, i.e. @{"asn":"AS1","maxLength":24,"prefix":"192.0.2.0/24"@.
-- The keys are in the order aeson writes a 'Value' with them.
vrpJsonFields :: Vrp -> BB.Builder
vrpJsonFields (Vrp asn (Ipv4P p) maxLength) = BP.primBounded json4Prim $ pack4 asn p maxLength
vrpJsonFields (Vrp asn (Ipv6P p) maxLength) = BP.primBounded json6Prim $ pack6 asn p maxLength

json4Prim :: BP.BoundedPrim PackedVrp4
json4Prim = boundedPrim (jsonKeysSize + 15) $ \(PackedVrp4 asn address len maxLength) p ->
    jsonUpToPrefix asn maxLength p >>= ipv4At address >>= charAt '/' >>= runB BP.word8Dec len >>= charAt '"'

json6Prim :: BP.BoundedPrim PackedVrp6
json6Prim = boundedPrim (jsonKeysSize + 39) $ \(PackedVrp6 asn hi lo len maxLength) p ->
    jsonUpToPrefix asn maxLength p >>= ipv6At hi lo >>= charAt '/' >>= runB BP.word8Dec len >>= charAt '"'

jsonUpToPrefix :: Word32 -> Word8 -> Ptr Word8 -> IO (Ptr Word8)
jsonUpToPrefix asn maxLength p =
    bytesAt asnKey p >>= asnAt asn >>= bytesAt maxLengthKey >>= runB BP.word8Dec maxLength
        >>= bytesAt prefixKey

-- | Everything but the address: the keys, an ASN, a max length, a prefix
-- length and the separators.
jsonKeysSize :: Int
jsonKeysSize = BS.length asnKey + 12 + BS.length maxLengthKey + 3 + BS.length prefixKey + 1 + 3 + 1

asnKey, maxLengthKey, prefixKey :: BS.ByteString
asnKey       = "{\"asn\":\""
maxLengthKey = "\",\"maxLength\":"
prefixKey    = ",\"prefix\":\""

bytesAt :: BS.ByteString -> Ptr Word8 -> IO (Ptr Word8)
bytesAt bytes p =
    unsafeUseAsCStringLen bytes $ \(source, size) -> do
        copyBytes p (castPtr source) size
        pure $ p `plusPtr` size

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

{- | RPKI Canonical Cache Representation (CCR), draft-ietf-sidrops-rpki-ccr-11.

A CCR is an unsigned CMS ContentInfo in DER describing a validated cache: the
manifests that were valid and current, the VRPs, the ASPA payloads, the trust
anchors and the BGPsec router keys. It is canonical: the same cache produces
the same bytes in any implementation, so everything is sorted and
de-duplicated here, whatever order the input comes in.

DER puts the length of an element before its content, so the encoding is
built bottom-up from elements that know their own length (`Der`). Every state
aspect carries a SHA-256 of the DER of its list, including the list's own tag
and length.
-}
module RPKI.CCR (
    ManifestInstance(..),
    CcrTaState(..),
    RouterKey(..),
    Ccr(..),
    CcrFile(..),
    CcrFileVariant(..),
    encodeCcr,
    signedObjectLocations,
    signedObjectAccessDescriptions,
    ccrContentType
) where

import           Control.Applicative         ((<|>))
import           Control.Monad               (guard)
import           Control.DeepSeq
import           Data.Bits                   (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as BSS
import           Data.Hourglass
import           Data.Int                    (Int64)
import qualified Data.List                   as List
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as Text
import qualified Data.Vector.Unboxed         as VU
import           Data.Word                   (Word8, Word32, Word64)
import           GHC.Generics
import           Text.Printf                 (printf)


import           RPKI.AppTypes               (Size(..), WorldVersion)
import           RPKI.Domain
import           RPKI.Domain.Packed          (PackedVrp4(..), PackedVrp6(..))
import           RPKI.Resources.Types
import           RPKI.RTR.Types              (mergeAspasByCustomer)
import           RPKI.Store.Base.Serialisation
import           RPKI.Time
import           RPKI.Util                   (sha256)


-- | A manifest that was valid and current. Everything except `subordinates`
-- is a property of the manifest object itself.
data ManifestInstance = ManifestInstance {
        hash           :: Hash,
        size           :: Size,
        aki            :: AKI,
        manifestNumber :: Serial,
        thisUpdate     :: Instant,
        -- | The value of the EE certificate's SIA extension, i.e. the DER of
        -- SEQUENCE OF AccessDescription, which is exactly what goes into the CCR.
        locations      :: BSS.ShortByteString,
        -- | Valid CA certificates issued by the manifest's issuer
        subordinates   :: Set SKI
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

-- | The part of a CCR that comes from one TA's tree: its TA certificate, if 
-- it's valid, and the manifests under it.
data CcrTaState = CcrTaState {
        trustAnchor :: Maybe SKI,
        manifests   :: [ManifestInstance]
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

instance Semigroup CcrTaState where
    CcrTaState ta1 ms1 <> CcrTaState ta2 ms2 = CcrTaState (ta1 <|> ta2) (ms1 <> ms2)

instance Monoid CcrTaState where
    mempty = CcrTaState Nothing []

data RouterKey = RouterKey {
        asn  :: ASN,
        ski  :: SKI,
        -- | DER of SubjectPublicKeyInfo
        spki :: BS.ByteString
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

-- | Everything that goes into a CCR, in any order and possibly with duplicates.
data Ccr = Ccr {
        producedAt   :: Instant,
        manifests    :: [ManifestInstance],
        vrps         :: Vrps,
        aspas        :: [Aspa],
        trustAnchors :: [SKI],
        routerKeys   :: [RouterKey]
    }
    deriving stock (Show, Eq, Generic)


-- | A CCR written to the disk, as a file and gzipped.
data CcrFile = CcrFile {
        -- | The version of the validation it describes
        version    :: WorldVersion,
        producedAt :: Instant,
        plain      :: CcrFileVariant,
        gzipped    :: CcrFileVariant
    }
    deriving stock (Show, Eq, Generic)

data CcrFileVariant = CcrFileVariant {
        path :: FilePath,
        -- | Hex of the SHA-256 of the file
        etag :: Text,
        size :: Int64
    }
    deriving stock (Show, Eq, Generic)


encodeCcr :: Ccr -> LBS.ByteString
encodeCcr ccr =
    toLazyByteString $ derSequence [
            rawDer ccrContentType,
            explicit 0 $ derSequence $
                -- version is DEFAULT 0 and DER leaves out default values
                [ rawDer sha256AlgorithmIdentifier
                , generalizedTime ccr.producedAt
                , explicit 1 $ manifestState ccr.manifests
                , explicit 2 $ roaPayloadState ccr.vrps
                , explicit 3 $ aspaPayloadState ccr.aspas ]
                -- At least one SKI is required, so it's all or nothing
                <> [ explicit 4 $ trustAnchorState ccr.trustAnchors | not (null ccr.trustAnchors) ]
                <> [ explicit 5 $ routerKeyState ccr.routerKeys ]
        ]


-- ManifestState ::= SEQUENCE { mis, mostRecentUpdate, hash }
manifestState :: [ManifestInstance] -> Der
manifestState instances =
    stateWithHash mis [generalizedTime mostRecentUpdate]
  where
    -- Unique by hash, sorted by hash. The same manifest could be reached
    -- from two TAs, then it's the union of what was found.
    byHash = Map.fromListWith
                (\a b -> a { subordinates = a.subordinates <> b.subordinates })
                [ (mi.hash, mi) | mi <- instances ]

    mis = derSequence $ map (strictDer . manifestInstance) $ Map.elems byHash

    mostRecentUpdate =
        case map (.thisUpdate) $ Map.elems byHash of
            [] -> Instant 0
            ts -> maximum ts

    manifestInstance mi =
        derSequence $
            [ octetString $ BSS.fromShort $ unHash mi.hash
            , integer $ fromIntegral $ unSize mi.size
            , octetString $ kiBytes $ unAKI mi.aki
            , let Serial n = mi.manifestNumber in integer n
            , generalizedTime mi.thisUpdate
            , if BSS.null mi.locations
                -- It's not supposed to be empty, but keep the output valid DER
                then derSequence []
                else rawDer $ BSS.fromShort mi.locations ]
            <> [ derSequence [ octetString $ kiBytes $ unSKI s | s <- Set.toAscList mi.subordinates ]
               | not (Set.null mi.subordinates) ]


-- ROAPayloadState ::= SEQUENCE { rps, hash }
--
-- One ROAPayloadSet per AS. The addresses are in the canonical form of
-- RFC 9582, section 4.3.3: IPv4 before IPv6, sorted by address, prefix
-- length and maximum length, unique, and no maxLength that equals the
-- prefix length.
--
-- The packed VRPs sort in exactly that order (AS first), so sorting them as
-- unboxed vectors puts every AS in one slice of each family, and every AS
-- is encoded into its own bytes straight away. A million VRPs never become
-- boxed values.
roaPayloadState :: Vrps -> Der
roaPayloadState (Vrps v4 v6) =
    stateWithHash (derSequence $ map (strictDer . roaPayloadSet) $ 
                    byAsn (sortDedup compare v4) (sortDedup compare v6)) []
  where
    byAsn s4 s6 = 
        case [ a | Just a <- [packed4Asn <$> s4 VU.!? 0, packed6Asn <$> s6 VU.!? 0] ] of
            []   -> []
            asns -> 
                let asn      = minimum asns
                    (g4, r4) = VU.span ((== asn) . packed4Asn) s4
                    (g6, r6) = VU.span ((== asn) . packed6Asn) s6
                in (asn, g4, g6) : byAsn r4 r6

    roaPayloadSet (asn, g4, g6) =
        derSequence [
            integer $ fromIntegral asn,
            derSequence $
                [ addressFamily ipv4Afi $ map v4Address $ VU.toList g4 | not (VU.null g4) ] <>
                [ addressFamily ipv6Afi $ map v6Address $ VU.toList g6 | not (VU.null g6) ]
        ]

    v4Address (PackedVrp4 _ address len maxLen) = 
        roaIpAddress (word32Bytes address) len maxLen
    v6Address (PackedVrp6 _ hi lo len maxLen) = 
        roaIpAddress (word64Bytes hi <> word64Bytes lo) len maxLen

    addressFamily afi addresses = derSequence [ octetString afi, derSequence addresses ]

    roaIpAddress address len maxLen =
        derSequence $
            [ prefixBitString (fromIntegral len) address ] <>
            [ integer $ fromIntegral maxLen | maxLen /= len ]

    ipv4Afi = BS.pack [0, 1]
    ipv6Afi = BS.pack [0, 2]


-- ASPAPayloadState ::= SEQUENCE { aps, hash }
--
-- One ASPAPayloadSet per customer, the union of the providers of all its
-- ASPAs, and AS0 only if it's the only provider.
aspaPayloadState :: [Aspa] -> Der
aspaPayloadState aspas =
    stateWithHash (derSequence $ map aspaPayloadSet merged) []
  where
    merged = Set.toAscList $ mergeAspasByCustomer $ Set.fromList aspas
    aspaPayloadSet a =
        derSequence [
            asnInteger a.customer,
            derSequence $ map asnInteger $ Set.toAscList a.providers
        ]


-- TrustAnchorState ::= SEQUENCE { skis, hash }
trustAnchorState :: [SKI] -> Der
trustAnchorState skis =
    stateWithHash (derSequence [ octetString $ kiBytes $ unSKI s | s <- List.nub $ List.sort skis ]) []


-- RouterKeyState ::= SEQUENCE { rksets, hash }
routerKeyState :: [RouterKey] -> Der
routerKeyState keys =
    stateWithHash (derSequence $ map routerKeySet $ Map.toAscList perAsn) []
  where
    perAsn = Map.fromListWith (<>) [ (k.asn, Set.singleton (k.ski, k.spki)) | k <- keys ]
    routerKeySet (keyAsn, skiSpkis) =
        derSequence [
            asnInteger keyAsn,
            derSequence [ derSequence [ octetString $ kiBytes $ unSKI s, rawDer keySpki ]
                        | (s, keySpki) <- Set.toAscList skiSpkis ]
        ]


-- | A state aspect: the list, whatever comes after it, and the SHA-256 of
-- the list's DER as the last element.
stateWithHash :: Der -> [Der] -> Der
stateWithHash list others =
    derSequence $ [rawLazyDer listBytes] <> others <> [octetString $ BSS.fromShort $ unHash $ sha256 listBytes]
  where
    listBytes = toLazyByteString list


-- | SEQUENCE OF AccessDescription with one id-ad-signedObject URI per location,
-- which is what the SIA of a manifest's EE certificate normally is.
signedObjectLocations :: [Text] -> BS.ByteString
signedObjectLocations uris =
    LBS.toStrict $ toLazyByteString $ derSequence [
        derSequence [ rawDer idAdSignedObject, primitive 0x86 $ Text.encodeUtf8 uri ]
        | uri <- uris ]


-- | Only the id-ad-signedObject AccessDescriptions of the value of an EE 
-- certificate's SIA extension, byte for byte, or `Nothing` if there are none
-- or it's not DER. RFC 6487 allows only those in an EE certificate, but some 
-- CAs add others (e.g. AFRINIC adds id-ad-rpkiNotify), and implementations 
-- produce the same CCR only if everything else is left out.
signedObjectAccessDescriptions :: BS.ByteString -> Maybe BS.ByteString
signedObjectAccessDescriptions sia = do
    (0x30, content, rest) <- derElement sia
    guard $ BS.null rest
    accessDescriptions <- derElements content
    let signedObjects = [ ad | ad <- accessDescriptions, isSignedObject ad ]
    guard $ not $ null signedObjects
    pure $ LBS.toStrict $ toLazyByteString $ derSequence $ map rawDer signedObjects
  where
    isSignedObject ad = 
        case derElement ad of
            Just (0x30, adContent, _) -> idAdSignedObject `BS.isPrefixOf` adContent
            _                         -> False

    -- Split DER into its elements
    derElements bs 
        | BS.null bs = Just []
        | otherwise  = do 
            (_, _, rest) <- derElement bs
            let first = BS.take (BS.length bs - BS.length rest) bs
            (first :) <$> derElements rest

    -- The tag, the content and whatever comes after the first element
    derElement bs = do 
        (tag, afterTag) <- BS.uncons bs
        (lengthByte, afterLengthByte) <- BS.uncons afterTag
        (contentLength, afterLength) <- 
            if lengthByte < 0x80 
                then Just (fromIntegral lengthByte, afterLengthByte)
                else do 
                    let n = fromIntegral (lengthByte .&. 0x7f)
                    guard $ n > 0 && n <= 4 && BS.length afterLengthByte >= n
                    let len = BS.foldl' (\a w -> a * 256 + fromIntegral w) 0 $ BS.take n afterLengthByte
                    Just (len, BS.drop n afterLengthByte)
        guard $ BS.length afterLength >= contentLength
        pure (tag, BS.take contentLength afterLength, BS.drop contentLength afterLength)


-- id-ct-rpkiCanonicalCacheRepresentation, 1.2.840.113549.1.9.16.1.54
ccrContentType :: BS.ByteString
ccrContentType = BS.pack [0x06, 0x0b, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x09, 0x10, 0x01, 0x36]

-- SEQUENCE { id-sha256 } with no parameters, 2.16.840.1.101.3.4.2.1
sha256AlgorithmIdentifier :: BS.ByteString
sha256AlgorithmIdentifier = BS.pack [0x30, 0x0b, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x01]

-- id-ad-signedObject, 1.3.6.1.5.5.7.48.11
idAdSignedObject :: BS.ByteString
idAdSignedObject = BS.pack [0x06, 0x08, 0x2b, 0x06, 0x01, 0x05, 0x05, 0x07, 0x30, 0x0b]


-- DER

-- | A DER element that knows its encoded length before it's written.
data Der = Der {-# UNPACK #-} Int64 BB.Builder

toLazyByteString :: Der -> LBS.ByteString
toLazyByteString (Der _ b) = BB.toLazyByteString b

element :: Word8 -> Int64 -> BB.Builder -> Der
element tag contentLength content =
    Der (1 + fromIntegral (BS.length lengthBytes) + contentLength)
        (BB.word8 tag <> BB.byteString lengthBytes <> content)
  where
    lengthBytes
        | contentLength < 0x80 = BS.singleton (fromIntegral contentLength)
        | otherwise = let bytes = unsignedBytes (fromIntegral contentLength)
                      in BS.cons (0x80 .|. fromIntegral (BS.length bytes)) bytes

primitive :: Word8 -> BS.ByteString -> Der
primitive tag content = element tag (fromIntegral $ BS.length content) (BB.byteString content)

constructed :: Word8 -> [Der] -> Der
constructed tag children =
    element tag (sum [ l | Der l _ <- children ]) (mconcat [ b | Der _ b <- children ])

derSequence :: [Der] -> Der
derSequence = constructed 0x30

explicit :: Word8 -> Der -> Der
explicit n d = constructed (0xa0 .|. n) [d]

-- | Something that is DER already
rawDer :: BS.ByteString -> Der
rawDer bs = Der (fromIntegral $ BS.length bs) (BB.byteString bs)

rawLazyDer :: LBS.ByteString -> Der
rawLazyDer bs = Der (LBS.length bs) (BB.lazyByteString bs)

octetString :: BS.ByteString -> Der
octetString = primitive 0x04

integer :: Integer -> Der
integer = primitive 0x02 . integerBytes

asnInteger :: ASN -> Der
asnInteger (ASN a) = integer $ fromIntegral a

-- | Minimal two's complement, big-endian.
integerBytes :: Integer -> BS.ByteString
integerBytes n
    | n >= 0 =
        let bytes = unsignedBytes n
        in if BS.null bytes || BS.head bytes >= 0x80 then BS.cons 0 bytes else bytes
    | otherwise =
        let width = fitsIn (1 :: Int)
            fitsIn w = if n >= negate (2 ^ (8 * w - 1)) then w else fitsIn (w + 1)
            bytes = unsignedBytes (2 ^ (8 * width) + n)
        in BS.replicate (width - BS.length bytes) 0xff <> bytes

-- | Big-endian with no leading zeroes, empty for zero.
unsignedBytes :: Integer -> BS.ByteString
unsignedBytes = BS.reverse . BS.unfoldr (\k -> if k == 0 then Nothing else Just (fromIntegral (k .&. 0xff), k `shiftR` 8))

-- | GeneralizedTime in whole seconds and UTC, which is what DER requires.
generalizedTime :: Instant -> Der
generalizedTime instant =
    primitive 0x18 $ C8.pack $
        printf "%04d%02d%02d%02d%02d%02dZ"
            (dateYear dtDate) (fromEnum (dateMonth dtDate) + 1) (dateDay dtDate)
            h m s
  where
    DateTime {..} = timeGetDateTimeOfDay $ Elapsed $ Seconds $ toNanoseconds instant `div` nanosPerSecond
    TimeOfDay (Hours h) (Minutes m) (Seconds s) _ = dtTime

-- | A prefix as in RFC 3779: only the bits of the prefix, the rest of the
-- last byte is unused and zero.
prefixBitString :: Int -> BS.ByteString -> Der
prefixBitString len address =
    primitive 0x03 $ BS.cons (fromIntegral unused) masked
  where
    byteCount = (len + 7) `div` 8
    unused    = byteCount * 8 - len
    bytes     = BS.take byteCount address
    masked
        | unused == 0 = bytes
        | otherwise   = BS.snoc (BS.init bytes) (BS.last bytes .&. (0xff `shiftL` unused))

word64Bytes :: Word64 -> BS.ByteString
word64Bytes w = BS.pack [ fromIntegral (w `shiftR` s) | s <- [56, 48 .. 0] ]

-- | Encode it now and keep only the bytes rather than a Builder that holds on 
-- to everything the element was made of.
strictDer :: Der -> Der
strictDer = rawDer . LBS.toStrict . toLazyByteString

word32Bytes :: Word32 -> BS.ByteString
word32Bytes w = BS.pack [ fromIntegral (w `shiftR` s) | s <- [24, 16, 8, 0] ]

kiBytes :: KI -> BS.ByteString
kiBytes (KI bs) = BSS.fromShort bs

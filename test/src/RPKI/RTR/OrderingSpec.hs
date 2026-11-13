{- | The ordering of the PDUs the cache sends, checked the way a router can
   check it: on the bytes, with the rules of
   https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis-28#section-11.2
   applied literally and nothing shared with how the cache orders them.
-}
module RPKI.RTR.OrderingSpec where

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import           Data.Bits                         (testBit)
import qualified Data.List                         as List
import           Data.Ord                          (Down (..))
import qualified Data.Set                          as Set
import           Data.Word                         (Word32, Word64, Word8)

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC
import           Test.QuickCheck                   (Gen, arbitrary, choose, elements, frequency, listOf, listOf1, oneof, sublistOf)

import           RPKI.Domain
import           RPKI.Domain.Vrps
import           RPKI.Orphans                      ()
import           RPKI.Resources.Types
import           RPKI.RTR.Pdus
import           RPKI.RTR.Protocol
import           RPKI.RTR.RtrServer                (compatiblePduLike, diffPayloadPdus, pduBytesL)
import           RPKI.RTR.RtrState
import           RPKI.RTR.Types
import           RPKI.RTR.UniqueVrps
import           RPKI.Util                         (encodeBase64)


rtrOrderingGroup :: TestTree
rtrOrderingGroup = testGroup "RTR PDU ordering" [
        testExamples,
        testRtrOrderedVrps,
        testCacheResponseOrder,
        testDiffOrder
    ]

-- | The abstract data element of a PDU, in a form whose derived Ord is the
-- ordering: first by PDU type, then announcements (updt 1) before
-- withdrawals (updt 0), then by what the PDU type has.
data Element = Element Word8 Bool Key
    deriving stock (Show, Eq, Ord)

data Key
    -- | Higher addr, mlen, plen, asn first
    = PrefixAnnouncement (Down (Integer, Word8, Word8, Word32))
    -- | Lower addr, mlen, plen, asn first
    | PrefixWithdrawal (Integer, Word8, Word8, Word32)
    -- | ski, then spkinfolen, spkinfo, asn, all the lower first
    | RouterKey (BS.ByteString, Int, BS.ByteString, Word32)
    -- | casn, the lower first
    | AspaKey Word32
    deriving stock (Show, Eq, Ord)

-- | The PDUs of a byte stream, split by their length field.
splitPdus :: BS.ByteString -> [BS.ByteString]
splitPdus bytes
    | BS.null bytes = []
    | otherwise     = let (pdu, rest) = BS.splitAt (fromIntegral $ word32At 4 bytes) bytes
                      in pdu : splitPdus rest

element :: BS.ByteString -> Element
element pdu =
    case BS.index pdu 1 of
        4  -> prefix 4 (integerAt 12 4) (word32At 16 pdu)
        6  -> prefix 6 (integerAt 12 16) (word32At 28 pdu)
        9  -> Element 9 (withdrawal 2) $ RouterKey
                (slice 8 20, BS.length spki, spki, word32At 28 pdu)
        11 -> Element 11 (withdrawal 2) $ AspaKey (word32At 8 pdu)
        t  -> error $ "Unexpected PDU type " <> show t
  where
    prefix pduType addr asn =
        let updtIs0 = withdrawal 8
            fields  = (addr, BS.index pdu 10, BS.index pdu 9, asn)
        in Element pduType updtIs0 $
            if updtIs0 then PrefixWithdrawal fields else PrefixAnnouncement (Down fields)

    withdrawal flagsAt = not $ testBit (BS.index pdu flagsAt) 0
    spki = BS.drop 32 pdu
    slice from n = BS.take n $ BS.drop from pdu
    integerAt from n = BS.foldl' (\a b -> a * 256 + fromIntegral b) 0 $ slice from n

word32At :: Int -> BS.ByteString -> Word32
word32At from = BS.foldl' (\a b -> a * 256 + fromIntegral b) 0 . BS.take 4 . BS.drop from

-- | Every PDU strictly after the previous one: it is a total ordering, so
-- equal elements would be the same PDU twice.
strictlyOrdered :: BS.ByteString -> QC.Property
strictlyOrdered bytes =
    case [ (a, b) | (a, b) <- zip elements_ (drop 1 elements_), a >= b ] of
        []         -> QC.property True
        (a, b) : _ -> QC.counterexample ("Out of order:\n  " <> show a <> "\n  " <> show b) False
  where
    elements_ = map element $ splitPdus bytes


-- | The examples of https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis-28#section-11.1.
testExamples :: TestTree
testExamples = HU.testCase "Examples of the races are ordered so that they don't happen" $ do
    let covering     = Vrp (ASN 64495) (v4 0xC0000200 24) (PrefixLength 24)
        moreSpecific = Vrp (ASN 64494) (v4 0xC0000200 25) (PrefixLength 25)
        longerMax    = Vrp (ASN 64494) (v4 0xC0000200 25) (PrefixLength 26)
        as0          = Vrp (ASN 0)     (v4 0xC0000200 24) (PrefixLength 24)
        ipv6         = Vrp (ASN 64494) (Ipv6P $ mkIpv6Prefix 0x20010db800000000 0 32) (PrefixLength 48)
        unique       = toUniqueVrps . createVrps

    -- The more specific one first, so that the BGP announcement it makes
    -- valid is not invalid in the meantime
    HU.assertEqual "Announcements"
        [moreSpecific, covering, as0, ipv6]
        (rtrOrderedVrps $ unique [ipv6, as0, covering, moreSpecific])

    -- Replacing a VRP with one of a longer max length announces the new one
    -- before withdrawing the old one
    HU.assertEqual "Updates"
        [(Announcement, longerMax), (Withdrawal, as0), (Withdrawal, moreSpecific), (Withdrawal, ipv6)]
        (rtrOrderedUpdates (Announcement,) (Withdrawal,) (unique [longerMax]) (unique [moreSpecific, ipv6, as0]))
  where
    v4 address len = Ipv4P $ mkIpv4Prefix address len

-- | The order 'rtrOrderedVrps' gives is the one of the PDUs announcing them.
testRtrOrderedVrps :: TestTree
testRtrOrderedVrps = QC.testProperty "VRPs are in the order of the announcements of them" $
    QC.forAll (listOf genVrp) $ \vrps ->
        let announcement vrp = LBS.toStrict $ pduToBytes $ VersionedPdu (vrpToPdu Announcement vrp) V2
            expected = List.sortOn (element . announcement) $ Set.toList $ Set.fromList vrps
        in rtrOrderedVrps (toUniqueVrps $ createVrps vrps) QC.=== expected

testCacheResponseOrder :: TestTree
testCacheResponseOrder = QC.testProperty "Cache Response is in the order of 8210bis" $
    QC.forAll genPayloads $ \payloads ->
        QC.conjoin [
            QC.counterexample (show version) $ strictlyOrdered $ currentCachePayloadBS version payloads
            | version <- [V0, V1, V2] ]

-- | Diffs of payloads that have something in common, the way consecutive
-- ones do, also squashed.
testDiffOrder :: TestTree
testDiffOrder = QC.testProperty "Diffs are in the order of 8210bis" $
    QC.forAll ((,,) <$> genPayloads <*> genPayloads <*> genPayloads) $ \(a, b, c) ->
        let squashed = squashDiffs [(SerialNumber 1, evalDiffs a b), (SerialNumber 2, evalDiffs b c)]
            bytesOf version diff = BS.concat $ map pduBytesL $ filter compatiblePduLike $ diffPayloadPdus version diff
        in QC.conjoin [
            QC.counterexample (show version) $ strictlyOrdered $ bytesOf version diff
            | version <- [V0, V1, V2], diff <- [evalDiffs a b, squashed] ]


-- | Few distinct values, so that orderings have ties to decide on all fields.
-- Addresses with the highest bit set make sure they compare as unsigned.
genVrp :: Gen Vrp
genVrp = frequency [(4, oneof [genVrp4, genVrp6]), (1, arbitrary)]
  where
    genVrp4 = do
        (address, minLength) <- elements
            [(0x0A000000, 8), (0x0A010000, 16), (0xC0000200, 24), (0xC0000280, 25), (0x80000000, 1)]
        (prefixLength, maxLength) <- lengths minLength 32
        Vrp <$> genAsn <*> pure (Ipv4P $ mkIpv4Prefix address prefixLength) <*> pure (PrefixLength maxLength)

    genVrp6 = do
        (hi, lo, minLength) <- elements
            [ (0x2001000000000000, 0, 16), (0x20010db800000000, 0, 32), (0x20010db800010000, 0, 48)
            , (0x20010db800000000, 1, 128), (0xff00000000000000 :: Word64, 0 :: Word64, 8) ]
        (prefixLength, maxLength) <- lengths minLength 128
        Vrp <$> genAsn <*> pure (Ipv6P $ mkIpv6Prefix hi lo prefixLength) <*> pure (PrefixLength maxLength)

    -- Any prefix length from the minimum keeps the address of the prefix
    lengths :: Word8 -> Word8 -> Gen (Word8, Word8)
    lengths minLength maxBits = do
        prefixLength <- choose (minLength, min maxBits (minLength + 2))
        maxLength    <- choose (prefixLength, min maxBits (prefixLength + 2))
        pure (prefixLength, maxLength)

genAsn :: Gen ASN
genAsn = ASN <$> elements [0, 1, 64496, 0x80000000, 0xFFFFFFFF]

-- | The same SKI with different keys too, which doesn't happen with real ones,
-- and keys of different lengths, which are compared before their bytes.
genRouterKey :: Gen BGPSecPayload
genRouterKey = do
    ski  <- elements [BS.replicate 20 0, BS.cons 1 (BS.replicate 19 0), BS.replicate 20 0xFF]
    spki <- elements [BS.pack [0xFF], BS.pack [0, 0], BS.pack [0x30, 1, 2], BS.replicate 91 7]
    asns <- listOf1 genAsn
    pure BGPSecPayload {
            bgpSecSki  = SKI $ mkKI ski,
            bgpSecAsns = asns,
            bgpSecSpki = SPKI $ encodeBase64 $ DecodedBase64 spki
        }

genPayloads :: Gen RtrPayloads
genPayloads = do
    vrps    <- listOf genVrp
    bgpSec  <- listOf genRouterKey
    aspas   <- listOf $ Aspa <$> genAsn <*> (Set.fromList <$> sublistOf (map ASN [0, 1, 2, 64496]))
    pure RtrPayloads {
            vrps       = mempty,
            uniqueVrps = toUniqueVrps $ createVrps vrps,
            bgpSec     = Set.fromList bgpSec,
            aspas      = mergeAspasByCustomer $ Set.fromList aspas
        }

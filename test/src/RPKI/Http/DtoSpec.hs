module RPKI.Http.DtoSpec where

import           Data.Aeson                        (encode, toJSON)
import qualified Data.ByteString.Builder           as BB
import           Data.Bits                         (shiftL, (.|.))
import qualified Data.Text                         as Text
import           Data.Word

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck             as QC

import           RPKI.Domain
import           RPKI.Domain.Vrps
import           RPKI.Http.Dto
import           RPKI.Http.Types
import           RPKI.Orphans                      ()
import           RPKI.Resources.Types
import           RPKI.RTR.UniqueVrps


httpDtoGroup :: TestTree
httpDtoGroup = testGroup "HTTP API DTOs" [
        QC.testProperty "Should write unique VRPs to CSV as their Show instances do" $
            withMaxSuccess 500 $ forAll (listOf genVrp) $ \vrps ->
                let unique = toUniqueVrps $ createVrps vrps
                in unRawCSV (uniqueVrpsToCSV unique)
                    === csv "ASN,IP Prefix,Max Length\n" [ showVrp vrp | vrp <- rtrOrderedVrps unique ],

        QC.testProperty "Should write VRPs of every TA to CSV as their Show instances do" $
            withMaxSuccess 500 $ forAll (listOf ((,) <$> arbitrary <*> listOf genVrp)) $ \perTa ->
                let vrps = toPerTA [ (ta, createVrps vrps_) | (ta, vrps_) <- perTa ]
                in unRawCSV (vrpsToCSV vrps)
                    === csv "ASN,IP Prefix,Max Length,Trust Anchor\n"
                            [ showVrp vrp <> "," <> Text.unpack ta
                            | (TaName ta, vrps_) <- perTA vrps, vrp <- vrpsToList vrps_ ],

        QC.testProperty "Should write VRPs with URIs to CSV as their Show instances do" $
            forAll (listOf ((,,) <$> elements ["rsync://a/b.roa", "https://c/d.roa"] <*> arbitrary <*> genVrp)) $ \rows ->
                let dtos = [ VrpExtDto { uri = uri, vrp = toVrpDto vrp ta } | (uri, ta, vrp) <- rows ]
                in unRawCSV (vrpExtDtosToCSV dtos)
                    === csv "URI,ASN,IP Prefix,Max Length,Trust Anchor\n"
                            [ Text.unpack uri <> "," <> showVrp vrp <> "," <> Text.unpack ta
                            | (uri, TaName ta, vrp) <- rows ],

        QC.testProperty "Should write VRPs to JSON as the generic instance does" $
            withMaxSuccess 500 $ forAll (listOf ((,) <$> genVrp <*> genTa)) $ \rows ->
                let dtos = [ toVrpDto vrp (TaName ta) | (vrp, ta) <- rows ]
                in encode dtos === encode (map toJSON dtos),

        QC.testProperty "Should write unique VRPs to JSON as the generic instance does" $
            withMaxSuccess 500 $ forAll (listOf genVrp) $ \vrps ->
                let dtos = toVrpMinimalDtos $ Just $ createVrps vrps
                in encode dtos === encode (map toJSON dtos)
    ]
  where
    -- Also with what has to be escaped in JSON
    genTa = elements ["ripe", "with \"quotes\" and \\", "new\nline\ttab", "ünïcödé", "\1234 \DEL"]

    csv header rows = BB.toLazyByteString $ BB.stringUtf8 $ header <> concatMap (<> "\n") rows

    showVrp (Vrp asn prefix maxLength) = show asn <> "," <> show prefix <> "," <> show maxLength


-- | With the extremes of every field and IPv6 addresses that have runs of
-- zero fields of all lengths, ties between them, and IPv4-mapped and
-- IPv4-compatible ones.
genVrp :: Gen Vrp
genVrp = Vrp <$> (ASN <$> word) <*> oneof [Ipv4P <$> ipv4, Ipv6P <$> ipv6] <*> (PrefixLength <$> word)
  where
    ipv4 = mkIpv4Prefix <$> word <*> word

    ipv6 = do
        fields <- oneof [
                vectorOf 8 field,
                (\a b -> [0, 0, 0, 0, 0, 0xffff, a, b]) <$> field <*> field,
                (\a b -> [0, 0, 0, 0, 0, 0, a, b]) <$> field <*> field
            ]
        mkIpv6Prefix (joined $ take 4 fields) (joined $ drop 4 fields) <$> word

    field :: Gen Word16
    field = frequency [(5, pure 0), (1, pure 1), (1, pure 0xffff), (2, arbitrary)]

    joined :: [Word16] -> Word64
    joined = foldl (\w f -> (w `shiftL` 16) .|. fromIntegral f) 0

    word :: (Arbitrary a, Bounded a, Num a) => Gen a
    word = frequency [(6, arbitrary), (1, pure 0), (1, pure maxBound)]

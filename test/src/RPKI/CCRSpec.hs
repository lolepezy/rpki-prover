{-# LANGUAGE OverloadedStrings #-}

module RPKI.CCRSpec where

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base16           as Hex
import qualified Data.ByteString.Base64           as B64
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Short            as BSS
import           Data.Int                         (Int64)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Data.Word                        (Word32)

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU
import qualified Test.Tasty.QuickCheck            as QC

import           RPKI.AppTypes                    (Size(..))
import           RPKI.CCR
import           RPKI.Domain
import           RPKI.Resources.Resources         (mkIpv4Block, mkIpv6Block)
import           RPKI.Resources.Types
import           RPKI.Time


ccrGroup :: TestTree
ccrGroup = testGroup "CCR"
    [ HU.testCase "Encodes the example of draft-ietf-sidrops-rpki-ccr-11 byte for byte"
        shouldEncodeDraftExample
    , HU.testCase "Merges duplicate manifests and removes duplicate payloads"
        shouldMergeDuplicates
    , QC.testProperty "Doesn't depend on the order of the input"
        prop_orderDoesNotMatter
    , HU.testCase "Keeps only signed object locations of an EE certificate"
        shouldKeepOnlySignedObjectLocations
    ]


-- | E.g. AFRINIC's manifest EE certificates have an id-ad-rpkiNotify 
-- AccessDescription after the id-ad-signedObject one.
shouldKeepOnlySignedObjectLocations :: HU.Assertion
shouldKeepOnlySignedObjectLocations = do
    let signedObject = signedObjectLocations ["rsync://rpki.example.net/repo/manifest.mft"]
    -- SEQUENCE { SEQUENCE { id-ad-rpkiNotify, [6] "https://rrdp.example.net/notification.xml" } }
    let notifyUri = "https://rrdp.example.net/notification.xml" :: BS.ByteString
    let notify = BS.pack [0x30, fromIntegral (BS.length notifyUri + 12), 0x06, 0x08, 0x2b, 0x06, 0x01, 0x05, 0x05, 0x07, 0x30, 0x0d, 0x86, fromIntegral (BS.length notifyUri)] <> notifyUri
    let withNotify = BS.pack [0x30, fromIntegral (BS.length (BS.drop 2 signedObject) + BS.length notify)] 
                        <> BS.drop 2 signedObject <> notify
    HU.assertEqual "Only the signed object" (Just signedObject) (signedObjectAccessDescriptions withNotify)
    HU.assertEqual "Nothing to leave out" (Just signedObject) (signedObjectAccessDescriptions signedObject)
    HU.assertEqual "No signed object at all" Nothing 
        (signedObjectAccessDescriptions $ BS.pack [0x30, fromIntegral (BS.length notify)] <> notify)
    HU.assertEqual "Not DER" Nothing (signedObjectAccessDescriptions $ BS.take 10 withNotify)


-- | Appendix B of the draft, built from the values the draft lists for it.
shouldEncodeDraftExample :: HU.Assertion
shouldEncodeDraftExample = do
    expected <- BS.readFile "test/data/ccr-draft-11-example.ccr"
    let actual = LBS.toStrict $ encodeCcr draftExample
    case [ i | (i, a, e) <- zip3 [0 :: Int ..] (BS.unpack actual) (BS.unpack expected), a /= e ] of
        []    -> HU.assertEqual "Lengths differ" (BS.length expected) (BS.length actual)
        i : _ -> HU.assertFailure $
                    "First difference at byte " <> show i <> ", expected "
                    <> hexAround expected i <> ", got " <> hexAround actual i
  where
    hexAround bs i = show $ Hex.encode $ BS.take 16 $ BS.drop (max 0 (i - 4)) bs


shouldMergeDuplicates :: HU.Assertion
shouldMergeDuplicates = do
    first <- case draftExample.manifests of
                m : _ -> pure m
                []    -> HU.assertFailure "The example has manifests"
    -- The same manifest found twice, with a part of its subordinates each time
    let firstSplit =
            [ first { subordinates = Set.take 1 first.subordinates }
            , first { subordinates = Set.drop 1 first.subordinates } ]
    let duplicated = draftExample {
            manifests    = firstSplit <> draftExample.manifests,
            vrps         = draftExample.vrps <> draftExample.vrps,
            aspas        = draftExample.aspas <> draftExample.aspas,
            trustAnchors = draftExample.trustAnchors <> draftExample.trustAnchors,
            routerKeys   = draftExample.routerKeys <> draftExample.routerKeys
        }
    HU.assertEqual "Duplicates must not change anything"
        (encodeCcr draftExample) (encodeCcr duplicated)


prop_orderDoesNotMatter :: QC.Property
prop_orderDoesNotMatter =
    QC.forAll shuffled $ \ccr -> encodeCcr ccr == encodeCcr draftExample
  where
    shuffled = do
        manifests    <- QC.shuffle draftExample.manifests
        vrps         <- QC.shuffle draftExample.vrps
        aspas        <- QC.shuffle draftExample.aspas
        trustAnchors <- QC.shuffle draftExample.trustAnchors
        routerKeys   <- QC.shuffle draftExample.routerKeys
        pure $ draftExample { manifests, vrps, aspas, trustAnchors, routerKeys }


draftExample :: Ccr
draftExample = Ccr {
        producedAt = unixTime 1778803210,
        manifests = [
            manifestInstance "48JkKNPGfzSWjkALB4rFbaktXGSFaAV5qj0gj7zCCFY=" 1729
                "25F8CCFCEFC046D8DCD00FC0E444E0AA7B790F96" 0x0101 1778803206
                "rsync://example.net/ca1/OaVUOIDSaLzUbeiz6VPogXxsK5o.mft"
                [ "A2DF042FE8B0006311E894851AC11411307B6043"
                , "E7315EA515D7C20538681249D3E30D6777162585" ],
            manifestInstance "KF60zgHHRNmQSUXcsAcAPB2cB7kvToWUF60GADJuG5E=" 1001
                "A2DF042FE8B0006311E894851AC11411307B6043" 0x1321 1778803209
                "rsync://example.net/ca4/QksbQZMC7YWsNrREt4l4dWAQ1sE.mft" [],
            manifestInstance "vee5m+i2FKhzHwldksC2IX0WlVcHHVu3B8qAMnk+/Xo=" 3995
                "E7315EA515D7C20538681249D3E30D6777162585" 0x0508 1778803208
                "rsync://example.net/ca3/sbhFzz4wTqsFo2NVRM8mWfsPBKQ.mft" [],
            manifestInstance "PH84tOOYN8EterYimODMa4sDj9HkMeyTNyCsy/9Q/48=" 2040
                "FACBD02CA47E3BD9666FCBD823B37DEDD0BCEE00" 0x0203 1778803207
                "rsync://example.net/ca2/z0nzVS7SOB_9y6tapHk7-YuKkm8.mft" []
        ],
        vrps = [
            Vrp (ASN 0)     (v4 0xc0000200 24) (PrefixLength 24),
            Vrp (ASN 65536) (v4 0xc6336400 24) (PrefixLength 28),
            Vrp (ASN 65536) (v6 (0x20010db8, 0, 0, 0) 48) (PrefixLength 48),
            Vrp (ASN 65550) (v6 (0x3fff0000, 0, 0, 0) 32) (PrefixLength 32),
            Vrp (ASN 65551) (v6 (0x3fff0000, 0, 0, 0) 32) (PrefixLength 32)
        ],
        aspas = [
            Aspa (ASN 64511) (Set.fromList [ASN 64496]),
            Aspa (ASN 65536) (Set.fromList [ASN 65540, ASN 65544]),
            Aspa (ASN 65550) (Set.fromList [ASN 0])
        ],
        trustAnchors = [
            ski "25F8CCFCEFC046D8DCD00FC0E444E0AA7B790F96",
            ski "FACBD02CA47E3BD9666FCBD823B37DEDD0BCEE00"
        ],
        routerKeys = [
            RouterKey (ASN 65542) (ski "88C5DE295A3276D69E9BB7469BD46EF972DE32AC")
                (base64 "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE64mxtNmdKd1bxIjgWrGJutr11LDeA56L8cc1NLL/WW9RZ+rbi+G4rFSvfrEjxzRPt6tcNWpgEINq7tOR7J5dAg=="),
            RouterKey (ASN 65542) (ski "BE16E74E10F4BDF3F8C2618B024A9457DFBF89FA")
                (base64 "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAEKjqTNoxSLK3UnLMNj2AdN/5sk5SITnYWK5e/JebKlJPFFxmBrOXWQyijRQBFFus7GtLLIZBYgp4K/u8o2/D4ig=="),
            RouterKey (ASN 65551) (ski "4602B621B017681E61EE1F4A5EFC1D02C3B46F2C")
                (base64 "MFkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDQgAE4Xt6+dRDhjmH0QVmXlUPndJeXyzlMcsco6WkrjBf6NoX6gYahESgCm67xkBK4ZxhvCZRFWLxqH8cgT/Pgvl94w==")
        ]
    }
  where
    manifestInstance :: Text -> Int64 -> Text -> Integer -> Int64 -> Text -> [Text] -> ManifestInstance
    manifestInstance hash size aki number thisUpdate location subordinates =
        ManifestInstance {
            hash           = Hash $ BSS.toShort $ base64 hash,
            size           = Size size,
            aki            = AKI $ KI $ BSS.toShort $ unhex aki,
            manifestNumber = Serial number,
            thisUpdate     = unixTime thisUpdate,
            locations      = BSS.toShort $ signedObjectLocations [location],
            subordinates   = Set.fromList $ map ski subordinates
        }

    v4 :: Word32 -> Int -> IpPrefix
    v4 w len = Ipv4P $ mkIpv4Block w (fromIntegral len)

    v6 :: (Word32, Word32, Word32, Word32) -> Int -> IpPrefix
    v6 w len = Ipv6P $ mkIpv6Block w (fromIntegral len)

    ski = SKI . KI . BSS.toShort . unhex

    unhex = either error id . Hex.decode . Text.encodeUtf8 . Text.toLower

    base64 = either (error . Text.unpack) id . B64.decodeBase64Untyped . Text.encodeUtf8

    unixTime seconds = Instant $ seconds * 1_000_000_000

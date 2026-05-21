{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module RPKI.DomainSpec where

import           Control.Monad              (replicateM)
import           Data.Char                  (isDigit, isHexDigit, isLower)
import qualified Data.ByteString            as BS

import           Test.Tasty
import qualified Test.Tasty.QuickCheck      as QC
import           Test.QuickCheck

import           RPKI.Domain
import           RPKI.Util                  (sha256s)
import           RPKI.Orphans               ()


domainSpec :: TestTree
domainSpec = testGroup "Domain type properties"
    [ hashSpec
    , kiSpec
    ]

-- | 32-byte ByteString for Hash round-trip tests.
newtype Bytes32 = Bytes32 BS.ByteString
    deriving (Show)

instance Arbitrary Bytes32 where
    arbitrary = Bytes32 . BS.pack <$> replicateM 32 arbitrary

-- | 20-byte ByteString for KI round-trip tests.
newtype Bytes20 = Bytes20 BS.ByteString
    deriving (Show)

instance Arbitrary Bytes20 where
    arbitrary = Bytes20 . BS.pack <$> replicateM 20 arbitrary

isLowercaseHex :: Char -> Bool
isLowercaseHex c = isHexDigit c && (isDigit c || isLower c)


hashSpec :: TestTree
hashSpec = testGroup "Hash (Sha256Words)"
    [ QC.testProperty "hashToBS always yields exactly 32 bytes" $
        \(h :: Hash) ->
            BS.length (hashToBS h) == 32

    , QC.testProperty "round-trip: newHash . hashToBS == id" $
        \(h :: Hash) ->
            newHash (hashToBS h) == h

    , QC.testProperty "round-trip: hashToBS . newHash == id (from 32 raw bytes)" $
        \(Bytes32 bs) ->
            hashToBS (newHash bs) == bs

    , QC.testProperty "Eq consistent with byte representation" $
        \(h1 :: Hash) (h2 :: Hash) ->
            (h1 == h2) == (hashToBS h1 == hashToBS h2)

    , QC.testProperty "Ord consistent with lexicographic byte order" $
        \(h1 :: Hash) (h2 :: Hash) ->
            compare h1 h2 == compare (hashToBS h1) (hashToBS h2)

    , QC.testProperty "show produces exactly 64 lowercase hex characters" $
        \(h :: Hash) ->
            let s = show h
            in  length s == 64 && all isLowercaseHex s

    , QC.testProperty "show is injective (equal shows => equal hashes)" $
        \(h1 :: Hash) (h2 :: Hash) ->
            (show h1 == show h2) == (h1 == h2)

    , QC.testProperty "sha256s always produces a valid 32-byte Hash" $
        \(bs :: BS.ByteString) ->
            BS.length (hashToBS (sha256s bs)) == 32

    , QC.testProperty "sha256s output round-trips through hashToBS/newHash" $
        \(bs :: BS.ByteString) ->
            let h = sha256s bs
            in  newHash (hashToBS h) == h
    ]


kiSpec :: TestTree
kiSpec = testGroup "KI"
    [ QC.testProperty "kiToBS always yields exactly 20 bytes" $
        \(ki :: KI) ->
            BS.length (kiToBS ki) == 20

    , QC.testProperty "round-trip: mkKI . kiToBS == id" $
        \(ki :: KI) ->
            mkKI (kiToBS ki) == ki

    , QC.testProperty "round-trip: kiToBS . mkKI == id (from 20 raw bytes)" $
        \(Bytes20 bs) ->
            kiToBS (mkKI bs) == bs

    , QC.testProperty "Eq consistent with byte representation" $
        \(ki1 :: KI) (ki2 :: KI) ->
            (ki1 == ki2) == (kiToBS ki1 == kiToBS ki2)

    , QC.testProperty "Ord consistent with lexicographic byte order" $
        \(ki1 :: KI) (ki2 :: KI) ->
            compare ki1 ki2 == compare (kiToBS ki1) (kiToBS ki2)

    , QC.testProperty "show produces exactly 40 lowercase hex characters" $
        \(ki :: KI) ->
            let s = show ki
            in  length s == 40 && all isLowercaseHex s

    , QC.testProperty "show is injective (equal shows => equal KIs)" $
        \(ki1 :: KI) (ki2 :: KI) ->
            (show ki1 == show ki2) == (ki1 == ki2)

    , QC.testProperty "mkKI is idempotent on its own output" $
        \(ki :: KI) ->
            mkKI (kiToBS ki) == ki

    , QC.testProperty "mkKI short input is padded to 20-byte representation" $
        \(bs :: BS.ByteString) ->
            -- Padding rule: short BS is zero-padded on the right to 20 bytes
            let padded = BS.take 20 (bs <> BS.replicate 20 0)
            in  kiToBS (mkKI bs) == padded
    ]

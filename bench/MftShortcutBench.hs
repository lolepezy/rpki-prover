{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

{-|
  Benchmark: blob-per-manifest vs flat-per-child storage for manifest shortcut children.

  Current design (blob):
    mftChildren :: SMap "mfts-shortcut-children" s AKI (Verbatim (Compressed MftShortcutChildren))
    -- one compressed blob per manifest: serialise(Map ObjectKey MftEntry) |> compress

  Proposed alternative (flat):
    mftChildIndex   :: SMultiMap "mft-child-index"   s AKI       ObjectKey
    mftChildEntries :: SMap      "mft-child-entries"  s ObjectKey MftEntry
    -- one LMDB row per child

  From production stats (stats.json):
    mfts-shortcut-children: 52,559 manifests
      total compressed bytes: 50,474,070
      average compressed blob: ~960 B/manifest
      max compressed blob: 3,932,249 B  (the RIPE NCC root, ~20k children)

  Derived average children/manifest:
    (785,309 objects - 52,559 MFTs - 52,559 CRLs) / 52,559 ≈ 13 children

  NOTE on entry size:
    This benchmark uses TroubledChild (minimal: just ObjectKey, ~30 B serialised).
    Real entries (RoaShortcut: VRPs + resources + validity periods) are 5-10x larger.
    The blob vs flat RATIO is what matters here; both approaches scale proportionally
    with entry size.  Compression helps the blob more as entries get larger (more
    repeated structure across entries), so real blobs are even more compact relative
    to N×flat.

  Operations benchmarked (reflecting actual usage in TopDown.hs):
    write/full          -- updateMftShortcutChildren: replace entire children set
    read/full           -- getMftShorcut: load all children for a CA
    write/partial/k=K   -- K children changed in the new manifest; blob must rewrite all N,
                           flat only writes K.  This is the critical case for large manifests.
-}
module Main where

import           Control.Exception     (evaluate)
import           Data.Maybe            (fromMaybe)

import qualified Data.ByteString       as BS
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Int              (Int64)

import           Codec.Compression.LZ4 (compress, decompress)
import           Criterion.Main

import           GHC.Generics          (Generic)
import           Data.Store            (Store, encode, decodeEx)
import           RPKI.Domain           (ObjectKey (..), asKey)
import           RPKI.Store.Base.Serialisation (serialise_, deserialise_)
import           RPKI.Validation.Types (MftEntry (..), MftChild (..))

-- Local definition for the blob-approach side of the benchmark.
-- This type was removed from Database.hs when migrating to flat maps.
newtype BlobChildren = BlobChildren { nonCrlEntries :: Map.Map ObjectKey MftEntry }
    deriving stock (Generic)
    deriving anyclass (Store)


------------------------------------------------------------------------
-- Manifest sizes to benchmark (children count)
--   5     tiny   (leaf CA with a handful of ROAs)
--   15    average (derived from stats above)
--   50    medium
--   200   large
--   1000  very large
--   5000  extra large
--   20000 maximum observed (RIPE NCC root manifest)
------------------------------------------------------------------------

sizes :: [Int]
sizes = [5, 15, 50, 200, 1000, 5000, 20000]

-- K values for partial-update benchmarks
kValues :: [Int]
kValues = [1, 2, 5]


------------------------------------------------------------------------
-- Synthetic data generation
------------------------------------------------------------------------

fakeKey :: Int -> ObjectKey
fakeKey n = ObjectKey (asKey (fromIntegral (n :: Int) :: Int64))

-- Minimal MftEntry using TroubledChild.
-- Real entries are larger; scaling is proportional.
fakeEntry :: Int -> MftEntry
fakeEntry n = MftEntry
    { fileName = Text.pack ("object-" <> show n <> ".roa")
    , child    = TroubledChild (fakeKey n)
    }

makeMap :: Int -> Map.Map ObjectKey MftEntry
makeMap n = Map.fromList [(fakeKey i, fakeEntry i) | i <- [0 .. n - 1]]


------------------------------------------------------------------------
-- BLOB approach
-- serialise(MftShortcutChildren) |> compress  →  single ByteString
------------------------------------------------------------------------

blobEncode :: Map.Map ObjectKey MftEntry -> BS.ByteString
blobEncode m =
    let raw = encode (BlobChildren m)
    in fromMaybe raw (compress raw)

blobDecode :: BS.ByteString -> Map.Map ObjectKey MftEntry
blobDecode cbs =
    let raw = fromMaybe cbs (decompress cbs)
    in nonCrlEntries (decodeEx raw :: BlobChildren)

-- Returns Int (has NFData) so criterion's nf can force evaluation
blobEncodeSize :: Map.Map ObjectKey MftEntry -> Int
blobEncodeSize = BS.length . blobEncode

blobDecodeCount :: BS.ByteString -> Int
blobDecodeCount = Map.size . blobDecode
-- Note: Map.size is O(1) but deserialise_ with Store/StrictData
-- is eager, so the full blob IS parsed by this call.

-- Partial update: blob must rewrite ALL N entries regardless of K
blobPartialSize :: Int -> Map.Map ObjectKey MftEntry -> Int
blobPartialSize _k = blobEncodeSize


------------------------------------------------------------------------
-- FLAT approach
-- Each child: serialise(ObjectKey)  →  key
--             serialise(MftEntry)   →  value
-- Stored as separate LMDB rows (simulated here as [(BS, BS)])
------------------------------------------------------------------------

flatEncodeAll :: Map.Map ObjectKey MftEntry -> [(BS.ByteString, BS.ByteString)]
flatEncodeAll m = [(serialise_ k, serialise_ v) | (k, v) <- Map.toList m]

flatDecodeAll :: [(BS.ByteString, BS.ByteString)] -> Map.Map ObjectKey MftEntry
flatDecodeAll ps = Map.fromList
    [(deserialise_ k, deserialise_ v) | (k, v) <- ps]

flatEncodeAllBytes :: Map.Map ObjectKey MftEntry -> Int
flatEncodeAllBytes m = sum [BS.length k + BS.length v | (k, v) <- flatEncodeAll m]

flatDecodeCount :: [(BS.ByteString, BS.ByteString)] -> Int
flatDecodeCount ps =
    let !m = flatDecodeAll ps
    in Map.size m

-- Partial update: only serialise K changed entries
flatPartialBytes :: Int -> Map.Map ObjectKey MftEntry -> Int
flatPartialBytes k m =
    let keys = take k (Map.keys m)
    in sum
        [ BS.length (serialise_ key) + BS.length (serialise_ (m Map.! key))
        | key <- keys ]


------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    -- Build maps once and share across all benchmarks
    maps <- mapM (\n -> do
        let m = makeMap n
        _ <- evaluate (Map.size m)
        pure (n, m)
        ) sizes

    -- Pre-compute blobs (for read benchmarks)
    blobs <- mapM (\(n, m) -> do
        let bs = blobEncode m
        _ <- evaluate (BS.length bs)
        pure (n, bs)
        ) maps

    -- Pre-compute flat pairs (for read benchmarks)
    flats <- mapM (\(n, m) -> do
        let ps = flatEncodeAll m
        _ <- evaluate (length ps)
        pure (n, ps)
        ) maps

    -- Print size summary for reference
    putStrLn "\n=== Serialised size summary ==="
    putStrLn (pad "N" <> pad "blob(B)" <> pad "flat-total(B)" <> pad "flat/entry(B)" <> "blob/flat ratio")
    mapM_ (\(n, m) -> do
        let blobB = blobEncodeSize m
            flatB = flatEncodeAllBytes m
            ratio = fromIntegral flatB / fromIntegral (max 1 blobB) :: Double
        putStrLn $ pad (show n)
                <> pad (show blobB)
                <> pad (show flatB)
                <> pad (show (flatB `div` max 1 n))
                <> show (round (ratio * 10) `div` 10 :: Int) <> "x"
        ) maps
    putStrLn ""

    defaultMain
      [ bgroup "write/full"
          [ bgroup "blob"
              (map (\(n, m) -> bench (show n) $ nf blobEncodeSize m) maps)
          , bgroup "flat/all-entries"
              (map (\(n, m) -> bench (show n) $ nf flatEncodeAllBytes m) maps)
          ]

      , bgroup "read/full"
          [ bgroup "blob"
              (map (\(n, bs) -> bench (show n) $ nf blobDecodeCount bs) blobs)
          , bgroup "flat/all-entries"
              (map (\(n, ps) -> bench (show n) $ nf flatDecodeCount ps) flats)
          ]

      , bgroup "write/partial"
          (concatMap (\k ->
              [ bgroup ("blob/rewrite-all/k=" <> show k)
                  (map (\(n, m) -> bench (show n) $ nf (blobPartialSize k) m) maps)
              , bgroup ("flat/k-entries/k=" <> show k)
                  (map (\(n, m) -> bench (show n) $ nf (flatPartialBytes k) m) maps)
              ]) kValues)
      ]
  where
    pad s = s <> replicate (max 1 (16 - length s)) ' '

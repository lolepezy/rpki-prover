-- | Building, converting, counting, sorting and merging VRP sets.
module RPKI.Domain.Vrps (
    roaPayloadToVrps,
    createVrps,
    toVrps,
    unpack4,
    unpack6,
    vrpsToList,
    vrpsCount,
    filterVrps,
    estimateVrpCount,
    estimateVrpCountRoas,
    uniqueVrpCounts,
    uniqVrpsPackedBy,
    unionVrpsPackedBy,
    mergeVrpsBy,
    sortDedup,
    mergeDedup,
    countSortedDistinct,
    countDistinctUnion,
) where

import           Control.Monad.ST         (runST)

import qualified Data.Vector              as V
import qualified Data.Vector.Algorithms.Intro as VectorSortU
import qualified Data.Vector.Unboxed      as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import           Data.Foldable            (toList)
import           Data.Map.Monoidal.Strict (MonoidalMap)
import qualified Data.Map.Monoidal.Strict as MonoidalMap

import           RPKI.Domain
import           RPKI.Domain.Packed
import           RPKI.Resources.Types


roaPayloadToVrps :: VrpsPerAs -> [Vrp]
roaPayloadToVrps (VrpsPerAs asn v4s v6s) =
    map (\(Vrp4 p len) -> Vrp asn (Ipv4P p) len) v4s <>
    map (\(Vrp6 p len) -> Vrp asn (Ipv6P p) len) v6s

createVrps :: Foldable f => f Vrp -> Vrps
createVrps vrps = Vrps (VU.fromList v4s) (VU.fromList v6s)
  where
    (v4s, v6s) = foldr split ([], []) (toList vrps)
    split (Vrp asn (Ipv4P p) maxLen) (as, bs) = (pack4 asn p maxLen : as, bs)
    split (Vrp asn (Ipv6P p) maxLen) (as, bs) = (as, pack6 asn p maxLen : bs)

-- | Convert stored ROA payloads into the packed VRP set.
toVrps :: Roas -> Vrps
toVrps (Roas roas) = Vrps (VU.fromList (concatMap toPacked4 payloads))
                          (VU.fromList (concatMap toPacked6 payloads))
  where
    payloads = MonoidalMap.elems roas
    toPacked4 (VrpsPerAs asn v4s _) = [ pack4 asn p len | Vrp4 p len <- v4s ]
    toPacked6 (VrpsPerAs asn _ v6s) = [ pack6 asn p len | Vrp6 p len <- v6s ]

pack4 :: ASN -> Ipv4Prefix -> PrefixLength -> PackedVrp4
pack4 (ASN asn) p (PrefixLength maxLen) =
    let (addr, len) = ipv4PrefixWords p in PackedVrp4 asn addr len maxLen

pack6 :: ASN -> Ipv6Prefix -> PrefixLength -> PackedVrp6
pack6 (ASN asn) p (PrefixLength maxLen) =
    let (hi, lo, len) = ipv6PrefixWords p in PackedVrp6 asn hi lo len maxLen

unpack4 :: PackedVrp4 -> Vrp
unpack4 (PackedVrp4 asn addr len maxLen) =
    Vrp (ASN asn) (Ipv4P (mkIpv4Prefix addr len)) (PrefixLength maxLen)

unpack6 :: PackedVrp6 -> Vrp
unpack6 (PackedVrp6 asn hi lo len maxLen) =
    Vrp (ASN asn) (Ipv6P (mkIpv6Prefix hi lo len)) (PrefixLength maxLen)

-- | All the VRPs, IPv4 first. This is storage order, not any meaningful one --
-- for the RTR ordering see 'mergeVrpsBy'.
vrpsToList :: Vrps -> [Vrp]
vrpsToList (Vrps v4 v6) = map unpack4 (VU.toList v4) <> map unpack6 (VU.toList v6)

vrpsCount :: Vrps -> Int
vrpsCount (Vrps v4 v6) = VU.length v4 + VU.length v6

filterVrps :: (Vrp -> Bool) -> Vrps -> Vrps
filterVrps f (Vrps v4 v6) =
    Vrps (VU.filter (f . unpack4) v4) (VU.filter (f . unpack6) v6)

estimateVrpCount :: PerTA Vrps -> Int
estimateVrpCount = sum . map (vrpsCount . snd) . perTA

estimateVrpCountRoas :: Roas -> Int
estimateVrpCountRoas =
    sum
        . map payloadVrpCount
        . MonoidalMap.elems
        . unRoas
  where
    payloadVrpCount (VrpsPerAs _ v4 v6) = length v4 + length v6

-----------------------------------------------------------------
-- Here is a bunch of functions sorting and counting VRPs. This machinery
-- is so complicated mainly for performance reasons -- it's actually quite
-- CPU-demanding to sort and deduplicate a million VRPs, so this
-- seemingly minor operations take a large chunk of the validation time.
-- There is a lot of benchmarking went into this, so it's probably close
-- to "as fast as it gets" without going to C.

-- | Distinct VRP count per TA and over all TAs together, from a single sort
-- of each TA's payload.
--
-- Both numbers are reported for every validation, and computing them
-- separately meant sorting everything twice: once per TA, and once more over
-- the concatenation of all TAs, which is the more expensive of the two. The
-- per-TA sorted vectors are all the overall count needs, since sorted stays
-- sorted -- so it is a merge over them rather than a second sort.
uniqueVrpCounts :: PerTA Vrps -> (MonoidalMap TaName Int, Int)
uniqueVrpCounts (PerTA perTaVrps) = (perTaCounts, total)
  where
    sorted = MonoidalMap.map (\(Vrps v4 v6) -> (sortPacked v4, sortPacked v6)) perTaVrps

    perTaCounts = MonoidalMap.map
        (\(v4, v6) -> countSortedDistinct v4 + countSortedDistinct v6) sorted

    total = countDistinctUnion (map fst families) + countDistinctUnion (map snd families)
    families = MonoidalMap.elems sorted

-- | Sort and deduplicate each family in the packed form. Nothing is
-- materialised, so the result costs the same as the input.
--
-- This and the other functions taking orderings are INLINE, so that the
-- ordering at the call site gets inlined into the sorting or merging loop
-- instead of being called for every comparison: for a million VRPs it is
-- ~180ms instead of ~285ms here and ~8ms instead of ~26ms for the union.
uniqVrpsPackedBy :: (PackedVrp4 -> PackedVrp4 -> Ordering)
                 -> (PackedVrp6 -> PackedVrp6 -> Ordering)
                 -> Vrps -> Vrps
uniqVrpsPackedBy cmp4 cmp6 (Vrps v4 v6) =
    Vrps (sortDedup cmp4 v4) (sortDedup cmp6 v6)
{-# INLINE uniqVrpsPackedBy #-}

sortDedup :: (VU.Unbox a, Eq a) => (a -> a -> Ordering) -> VU.Vector a -> VU.Vector a
sortDedup cmp v
    | VU.null v = v
    | otherwise = let sorted = VU.modify (VectorSortU.sortBy cmp) v
                  in VU.ifilter (\i x -> i == 0 || sorted VU.! (i - 1) /= x) sorted
{-# INLINE sortDedup #-}

-- | 'uniqVrpsPackedBy' of two VRP sets together, when each of them is
-- already sorted and deduplicated by the same orderings.
--
-- A merge instead of sorting again what is sorted already: for a million
-- VRPs it is ~30ms instead of ~1.3s.
unionVrpsPackedBy :: (PackedVrp4 -> PackedVrp4 -> Ordering)
                  -> (PackedVrp6 -> PackedVrp6 -> Ordering)
                  -> Vrps -> Vrps -> Vrps
unionVrpsPackedBy cmp4 cmp6 (Vrps a4 a6) (Vrps b4 b6) =
    Vrps (mergeDedup cmp4 a4 b4) (mergeDedup cmp6 a6 b6)
{-# INLINE unionVrpsPackedBy #-}

-- | Merge two vectors that are sorted and deduplicated by cmp into one that
-- is sorted and deduplicated by it too.
mergeDedup :: VU.Unbox a => (a -> a -> Ordering) -> VU.Vector a -> VU.Vector a -> VU.Vector a
mergeDedup cmp xs ys
    | VU.null xs = ys
    | VU.null ys = xs
    | otherwise  = runST $ do
        out <- VUM.unsafeNew (lx + ly)
        let go !i !j !k
                | i >= lx   = rest ys j k
                | j >= ly   = rest xs i k
                | otherwise =
                    let x = VU.unsafeIndex xs i
                        y = VU.unsafeIndex ys j
                    in case cmp x y of
                        LT -> VUM.unsafeWrite out k x >> go (i + 1) j (k + 1)
                        GT -> VUM.unsafeWrite out k y >> go i (j + 1) (k + 1)
                        EQ -> VUM.unsafeWrite out k x >> go (i + 1) (j + 1) (k + 1)
            rest v i k = do
                let n = VU.length v - i
                VU.copy (VUM.unsafeSlice k n out) (VU.unsafeSlice i n v)
                pure (k + n)
        n <- go 0 0 0
        -- A copy, a slice would keep all of the buffer alive
        VU.freeze (VUM.unsafeTake n out)
  where
    lx = VU.length xs
    ly = VU.length ys
{-# INLINE mergeDedup #-}

-- | Merge two already-sorted families into one sequence.
--
-- The caller supplies the cross-family ordering, since which family comes
-- first for a given key is a property of the ordering being reproduced and not
-- of the storage (for the RTR order, see 'RPKI.RTR.Types.cmpPacked4Against6').
mergeVrpsBy :: (PackedVrp4 -> PackedVrp6 -> Ordering) -> Vrps -> [Vrp]
mergeVrpsBy cross (Vrps v4 v6) = go (VU.toList v4) (VU.toList v6)
  where
    go [] bs = map unpack6 bs
    go as [] = map unpack4 as
    go as@(a : as') bs@(b : bs') =
        case cross a b of
            GT -> unpack6 b : go as bs'
            _  -> unpack4 a : go as' bs

-- | Sort in place, without deduplicating.
sortPacked :: (VU.Unbox a, Ord a) => VU.Vector a -> VU.Vector a
sortPacked v
    | VU.null v = v
    | otherwise = VU.modify VectorSortU.sort v

-- | Number of distinct values in an already sorted vector.
--
-- Counting does not need the deduplicated vector that 'sortDedup' builds, and
-- for a million VRPs that vector is the larger half of the cost.
countSortedDistinct :: (VU.Unbox a, Eq a) => VU.Vector a -> Int
countSortedDistinct v
    | VU.null v = 0
    | otherwise = go 1 1 (VU.unsafeHead v)
  where
    len = VU.length v
    go !i !n !prev
        | i >= len  = n
        | otherwise =
            let x = VU.unsafeIndex v i
            in if x == prev then go (i + 1) n prev else go (i + 1) (n + 1) x

-- | Number of distinct values in the union of several already sorted vectors.
--
-- A k-way merge, so it is linear in the total length with @k@ (the number of
-- TAs, five in practice) comparisons per element, and it materialises nothing.
-- Sorting the concatenation instead would redo, at @n log n@, work the inputs
-- already carry.
countDistinctUnion :: (VU.Unbox a, Ord a) => [VU.Vector a] -> Int
countDistinctUnion vectors =
    case filter (not . VU.null) vectors of
        []  -> 0
        [v] -> countSortedDistinct v
        vs  -> runST $ do
            let sources = V.fromList vs
                k       = V.length sources
            cursors <- VUM.replicate k (0 :: Int)
            let
                -- The source whose next unconsumed value is smallest, or -1
                -- once every source is exhausted.
                smallest !i !best
                    | i >= k    = pure best
                    | otherwise = do
                        ci <- VUM.unsafeRead cursors i
                        let vi = V.unsafeIndex sources i
                        if ci >= VU.length vi
                            then smallest (i + 1) best
                            else if best < 0
                                then smallest (i + 1) i
                                else do
                                    cb <- VUM.unsafeRead cursors best
                                    let vb = V.unsafeIndex sources best
                                    smallest (i + 1) $
                                        if VU.unsafeIndex vi ci < VU.unsafeIndex vb cb
                                            then i else best

                go !n !seen !prev = do
                    i <- smallest 0 (-1)
                    if i < 0
                        then pure n
                        else do
                            ci <- VUM.unsafeRead cursors i
                            VUM.unsafeWrite cursors i (ci + 1)
                            let x = VU.unsafeIndex (V.unsafeIndex sources i) ci
                            if seen && x == prev
                                then go n seen prev
                                else go (n + 1) True x

            go 0 False (VU.unsafeHead (V.unsafeHead sources))

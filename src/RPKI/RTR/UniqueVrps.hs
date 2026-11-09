{- | The VRPs the RTR server serves, sorted and deduplicated.

   They are kept in a specific order, so that they don't have to be sorted
   before sending them to every client every time:
   https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis-02#section-11

   Merging and diffing them relies on that order and would silently give wrong
   results without it. So the constructor is not exported and 'toUniqueVrps'
   is the only way to get them.
-}
module RPKI.RTR.UniqueVrps (
    UniqueVrps,
    toUniqueVrps,
    fromUniqueVrps,
    countUniqueVrps,
    diffUniqueVrps,
    rtrOrderedVrps
) where

import           RPKI.Domain
import           RPKI.Domain.Packed
import           RPKI.Domain.Vrps


-- | No 'Generic' on purpose, it would be a way to build one around the
-- constructor.
newtype UniqueVrps = UniqueVrps Vrps
    deriving stock (Show, Eq)

-- | Their union, which is sorted and deduplicated too.
instance Semigroup UniqueVrps where
    UniqueVrps a <> UniqueVrps b = UniqueVrps $ unionVrpsPackedBy cmpPacked4 cmpPacked6 a b

instance Monoid UniqueVrps where
    mempty = UniqueVrps mempty

toUniqueVrps :: Vrps -> UniqueVrps
toUniqueVrps = UniqueVrps . uniqVrpsPackedBy cmpPacked4 cmpPacked6

fromUniqueVrps :: UniqueVrps -> Vrps
fromUniqueVrps (UniqueVrps vrps) = vrps

countUniqueVrps :: UniqueVrps -> Int
countUniqueVrps = vrpsCount . fromUniqueVrps

-- | What only the first and what only the second one has. Only these VRPs are
-- materialised, there are normally few of them next to all the VRPs.
diffUniqueVrps :: UniqueVrps -> UniqueVrps -> ([Vrp], [Vrp])
diffUniqueVrps (UniqueVrps (Vrps a4 a6)) (UniqueVrps (Vrps b4 b6)) =
    (map unpack4 only4a <> map unpack6 only6a, map unpack4 only4b <> map unpack6 only6b)
  where
    (only4a, only4b) = diffSortedBy cmpPacked4 a4 b4
    (only6a, only6b) = diffSortedBy cmpPacked6 a6 b6

-- | In the order they are sent to routers.
rtrOrderedVrps :: UniqueVrps -> [Vrp]
rtrOrderedVrps = mergeVrpsBy cmpPacked4Against6 . fromUniqueVrps


-- | 'RPKI.RTR.Types.cmpVrps' on the packed form, per address family, giving
-- byte for byte the same ordering once the two are merged with
-- 'cmpPacked4Against6'.
--
-- The prefix comparison is reversed (that is what @Down@ does in 'cmpVrps'),
-- and the IPv6 flag stands in for the 'IpPrefix' constructor tag that derived
-- Ord compares first, so (flag, address, length) compared lexicographically is
-- exactly derived Ord on 'IpPrefix'.
cmpPacked4 :: PackedVrp4 -> PackedVrp4 -> Ordering
cmpPacked4 (PackedVrp4 asn1 a1 l1 m1) (PackedVrp4 asn2 a2 l2 m2) =
    compare asn1 asn2 <> compare (a2, l2) (a1, l1) <> compare m1 m2

cmpPacked6 :: PackedVrp6 -> PackedVrp6 -> Ordering
cmpPacked6 (PackedVrp6 asn1 hi1 lo1 l1 m1) (PackedVrp6 asn2 hi2 lo2 l2 m2) =
    compare asn1 asn2 <> compare (hi2, lo2, l2) (hi1, lo1, l1) <> compare m1 m2

-- | Which of an IPv4 and an IPv6 entry comes first, for merging the two
-- sorted families back into one RTR-ordered sequence.
--
-- The ASN decides; on a tie IPv6 goes first, because derived Ord puts Ipv4P
-- before Ipv6P and 'RPKI.RTR.Types.cmpVrps' compares the prefix reversed.
cmpPacked4Against6 :: PackedVrp4 -> PackedVrp6 -> Ordering
cmpPacked4Against6 a b = compare (packed4Asn a) (packed6Asn b) <> GT

-- For sorting without boxing every element, see RPKI.Domain.Vrps
{-# OPTIONS_GHC -flate-specialise #-}

{- | The VRPs the RTR server serves, sorted and deduplicated.

   They are kept in the order they are sent to routers in, so that they don't
   have to be sorted for every client every time. That is the ordering of IP
   Prefix PDUs in
   https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis-28#section-11.2.1:

     * all IPv4 before all IPv6, since PDUs of a lower type go first;
     * announcements in each family from the higher address, max length,
       prefix length and ASN to the lower ones, compared in this order;
     * withdrawals in each family the other way around.

   So the VRPs are announced in the order of the IPv4 and then the IPv6 vector
   of 'fromUniqueVrps', and the withdrawals of a diff are the same vectors read
   backwards.

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
    rtrOrderedVrps,
    rtrOrderedUpdates
) where

import qualified Data.Vector.Unboxed as VU

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

-- | In the order they are announced to routers.
rtrOrderedVrps :: UniqueVrps -> [Vrp]
rtrOrderedVrps (UniqueVrps (Vrps v4 v6)) =
    map unpack4 (VU.toList v4) <> map unpack6 (VU.toList v6)

-- | Announcements of the first ones and withdrawals of the second ones, in
-- the order they are sent to routers: IPv4 announcements, IPv4 withdrawals,
-- IPv6 announcements, IPv6 withdrawals.
rtrOrderedUpdates :: (Vrp -> a) -> (Vrp -> a) -> UniqueVrps -> UniqueVrps -> [a]
rtrOrderedUpdates announce withdraw (UniqueVrps (Vrps a4 a6)) (UniqueVrps (Vrps w4 w6)) =
       map (announce . unpack4) (VU.toList a4)
    <> map (withdraw . unpack4) (VU.toList $ VU.reverse w4)
    <> map (announce . unpack6) (VU.toList a6)
    <> map (withdraw . unpack6) (VU.toList $ VU.reverse w6)


-- | The order of announcements in a family: the higher address, max length,
-- prefix length and ASN first, compared in this order. Comparing the
-- addresses as unsigned words is comparing them as integers, for IPv6 the
-- high word first.
cmpPacked4 :: PackedVrp4 -> PackedVrp4 -> Ordering
cmpPacked4 (PackedVrp4 asn1 a1 l1 m1) (PackedVrp4 asn2 a2 l2 m2) =
    compare (a2, m2, l2, asn2) (a1, m1, l1, asn1)

cmpPacked6 :: PackedVrp6 -> PackedVrp6 -> Ordering
cmpPacked6 (PackedVrp6 asn1 hi1 lo1 l1 m1) (PackedVrp6 asn2 hi2 lo2 l2 m2) =
    compare (hi2, lo2, m2, l2, asn2) (hi1, lo1, m1, l1, asn1)

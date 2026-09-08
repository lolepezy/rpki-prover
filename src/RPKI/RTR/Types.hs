{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.Types where

import           Data.Set       (Set)
import           Data.Monoid.Generic
import           Data.Ord
import           Deque.Strict   as Deq

import           GHC.Generics

import           RPKI.AppTypes

import           RPKI.Domain
import           RPKI.Domain.Packed
import           RPKI.RTR.Protocol

data Diff a = Diff {
        added   :: Set a,
        deleted :: Set a
    }
    deriving stock (Show, Eq, Ord, Generic)

-- This generic type is only usefull for testing, 
-- when a and b can be some primitive types instead of real VRPs
-- or BGPSec certificates.
data GenDiffs a b = GenDiffs {
        vrpDiff    :: Diff a,
        bgpSecDiff :: Diff b
    }
    deriving stock (Show, Eq, Ord)
    deriving stock Generic

type RtrDiffs = GenDiffs Vrp BGPSecPayload        


data RtrState = RtrState {
        lastKnownWorldVersion :: WorldVersion,
        currentSessionId      :: RtrSessionId,
        currentSerial         :: SerialNumber,
        maxSerialsPerSession  :: Int,
        diffs                 :: Deq.Deque (SerialNumber, RtrDiffs),
        totalDiffSize         :: Int,
        maxTotalDiffSize      :: Int
    }
    deriving stock (Show, Eq, Generic)


data RtrPayloads = RtrPayloads {
        vrps       :: PerTA Vrps,
        -- Lazy on purpose (StrictData is on for this module): nothing needs
        -- the deduplicated set unless RTR or the validity API is running, and
        -- the thunk only closes over `vrps`, which is retained anyway.
        uniqueVrps :: ~Vrps,
        bgpSec     :: Set BGPSecPayload
    }
    deriving stock (Show, Eq, Generic)
    deriving Semigroup via GenericSemigroup RtrPayloads   
    deriving Monoid    via GenericMonoid RtrPayloads           

-- We store VRPs sorteed in a specific way, so that we don't have to sort them before 
-- sending to every client every time.
-- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis-02#section-11
-- 
-- | 'cmpVrps' on the packed form, per address family, giving byte for byte
-- the same ordering once the two are merged with 'cmpPacked4Against6'.
--
-- The prefix comparison is reversed (that is what @Down@ does above), and the
-- IPv6 flag stands in for the 'IpPrefix' constructor tag that derived Ord
-- compares first, so (flag, address, length) compared lexicographically is
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
-- before Ipv6P and 'cmpVrps' compares the prefix reversed.
cmpPacked4Against6 :: PackedVrp4 -> PackedVrp6 -> Ordering
cmpPacked4Against6 a b = compare (packed4Asn a) (packed6Asn b) <> GT

cmpVrps :: Vrp -> Vrp -> Ordering
cmpVrps (Vrp asn1 p1 ml1) (Vrp asn2 p2 ml2) = 
    compare asn1 asn2 <> 
    -- Sort prefixes backwards -- it automatically means that 
    -- smaller prefixes will be in front of larger ones.
    compare (Down p1) (Down p2) <> 
    -- shorter max length should precede?
    compare ml1 ml2       
     
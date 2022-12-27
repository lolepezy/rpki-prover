{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE DerivingVia #-}

module RPKI.RTR.Types where

import           Data.Set       (Set)
import           Data.Monoid.Generic
import           Data.Ord
import           Deque.Strict   as Deq

import           GHC.Generics

import           RPKI.AppTypes

import           RPKI.Domain
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

type RtrDiffs = GenDiffs AscOrderedVrp BGPSecPayload        


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
        vrps       :: Vrps,
        uniqueVrps :: Set AscOrderedVrp,
        bgpSec     :: Set BGPSecPayload
    }
    deriving stock (Show, Eq, Generic)
    deriving Semigroup via GenericSemigroup RtrPayloads   
    deriving Monoid    via GenericMonoid RtrPayloads           

newtype AscOrderedVrp = AscOrderedVrp Vrp
    deriving stock (Show, Eq, Generic)


-- We store VRPs sorteed in a specific way, so that we don't have to sort them before 
-- sending to every client every time.
-- https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis-02#section-11
-- 
instance Ord AscOrderedVrp where    
    compare (AscOrderedVrp (Vrp asn1 p1 ml1)) (AscOrderedVrp (Vrp asn2 p2 ml2)) = 
        compare asn1 asn2 <> 
        -- Sort prefixes backwards -- it automatically means that 
        -- smaller prefixes will be in front of larger ones.
        compare (Down p1) (Down p2) <> 
        -- shorter max length should precede?
        compare ml1 ml2       
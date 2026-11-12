{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StrictData #-}

module RPKI.RTR.Types where

import qualified Data.ByteString as BS
import           Data.Set       (Set)
import qualified Data.Set       as Set
import qualified Data.Map.Strict as Map
import           Deque.Strict   as Deq

import           GHC.Generics

import           RPKI.AppTypes

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.RTR.Protocol
import           RPKI.RTR.UniqueVrps

data Diff a = Diff {
        added   :: Set a,
        deleted :: Set a
    }
    deriving stock (Show, Eq, Ord, Generic)

-- This generic type is only usefull for testing, 
-- when a, b and c can be some primitive types instead of real VRPs,
-- BGPSec certificates or ASPAs.
data GenDiffs a b c = GenDiffs {
        vrpDiff    :: Diff a,
        bgpSecDiff :: Diff b,
        aspaDiff   :: Diff c
    }
    deriving stock (Show, Eq, Ord)
    deriving stock Generic

type RtrDiffs = GenDiffs Vrp BGPSecPayload Aspa


data RtrState = RtrState {
        lastKnownWorldVersion :: WorldVersion,
        currentSessionId      :: RtrSessionId,
        currentSerial         :: SerialNumber,
        maxSerialsPerSession  :: Int,
        diffs                 :: Deq.Deque (SerialNumber, RtrDiffs),
        totalDiffSize         :: Int,
        maxTotalDiffSize      :: Int,
        -- | Sent to routers with every End of Data
        intervals             :: Intervals,
        -- | What the cache serves at 'currentSerial'. It is replaced together 
        -- with the serial, so that a response never mixes the two up.
        served                :: ServedPayloads,
        -- | Served together with the new payloads after a restart,
        -- see 'RPKI.RTR.RtrState.withCarriedOver'
        carriedOver           :: Maybe CarriedOver
    }
    deriving stock (Show, Eq, Generic)

-- | What routers got before a restart, from the persisted state.
data CarriedOver = CarriedOver {
        served  :: ServedPayloads,
        -- | Of the payloads, they are carried over until it is
        -- older than the Expire Interval
        version :: WorldVersion
    }
    deriving stock (Show, Eq, Generic)

-- | The payloads the cache serves, together with their serialised forms. 
-- 
-- A form is rendered the first time it is needed, e.g. the PDUs of a 
-- protocol version on the first Reset Query of that version, and is then 
-- shared by everything that needs it.
data ServedPayloads = ServedPayloads {
        payloads   :: RtrPayloads,
        renderedV0 :: ~BS.ByteString,
        renderedV1 :: ~BS.ByteString,
        renderedV2 :: ~BS.ByteString,
        -- | The payloads as they are persisted, see RPKI.RTR.Snapshot
        persisted  :: ~BS.ByteString
    }
    deriving stock (Generic)

-- The rendered forms are derived from the payloads, comparing or showing 
-- them would only force them.
instance Eq ServedPayloads where
    a == b = payloads a == payloads b

instance Show ServedPayloads where
    show ServedPayloads { payloads = RtrPayloads {..} } = 
        "ServedPayloads { VRPs: " <> show (countUniqueVrps uniqueVrps)
            <> ", BGPSec: " <> show (Set.size bgpSec) 
            <> ", ASPAs: " <> show (Set.size aspas) <> " }"

renderedFor :: ProtocolVersion -> ServedPayloads -> BS.ByteString
renderedFor protocolVersion ServedPayloads {..} = 
    case protocolVersion of 
        V0 -> renderedV0
        V1 -> renderedV1
        V2 -> renderedV2


data RtrPayloads = RtrPayloads {
        vrps       :: PerTA Vrps,
        -- Lazy on purpose (StrictData is on for this module): nothing needs
        -- the deduplicated set unless RTR or the validity API is running, and
        -- the thunk only closes over `vrps`, which is retained anyway.
        uniqueVrps :: ~UniqueVrps,
        bgpSec     :: Set BGPSecPayload,
        -- At most one ASPA per customer AS, see 'mergeAspasByCustomer'
        aspas      :: Set Aspa
    }
    deriving stock (Show, Eq, Generic)

emptyRtrPayloads :: RtrPayloads
emptyRtrPayloads = RtrPayloads mempty mempty mempty mempty

-- | RTR cache can have at most one ASPA record per customer AS
-- (https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-5.12),
-- while there may be several validated ASPA objects for the same customer,
-- e.g. from different TAs. Their provider sets are united.
--
-- The result also satisfies the rest of the PDU constraints: 
-- there is at least one provider and AS0 is only allowed as the sole provider.
mergeAspasByCustomer :: Set Aspa -> Set Aspa
mergeAspasByCustomer = 
    Set.fromList
        . Prelude.filter (not . Set.null . providers) 
        . map (\(Aspa customer ps) -> Aspa customer (dropAs0IfNotAlone ps)) 
        . Map.elems 
        . Map.fromListWith (\(Aspa c ps1) (Aspa _ ps2) -> Aspa c (ps1 <> ps2)) 
        . map (\a -> (customer a, a)) 
        . Set.toList
  where
    dropAs0IfNotAlone ps = if Set.size ps > 1 then Set.delete (ASN 0) ps else ps

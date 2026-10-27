{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData      #-}

module RPKI.RTR.RtrState where

import           Control.Lens

import           Data.Foldable  (toList)
import           Data.Set       (Set, (\\))
import qualified Data.Set       as Set
import qualified Data.List      as List
import qualified Data.Vector.Unboxed as VU
import           Data.Generics.Labels

import           Deque.Strict   as Deq

import           RPKI.AppTypes
import           RPKI.Domain     (BGPSecPayload (..), Vrp, Vrps, packed4, packed6)
import           RPKI.Domain.Vrps (unionVrpsPackedBy, unpack4, unpack6, vrpsCount)
import           RPKI.Time      (Instant (..), momentAfter, nanosPerSecond)
import           RPKI.RTR.Types
import           RPKI.RTR.Protocol

import           Time.Types     (Seconds)


newDiff :: Ord a => Diff a
newDiff = Diff mempty mempty

newRtrDiff :: RtrDiffs
newRtrDiff = GenDiffs newDiff newDiff newDiff

isEmptyDiff :: Diff a -> Bool
isEmptyDiff Diff {..} = Set.null added && Set.null deleted

emptyDiffs :: RtrDiffs -> Bool
emptyDiffs GenDiffs {..} = isEmptyDiff vrpDiff && isEmptyDiff bgpSecDiff && isEmptyDiff aspaDiff


-- | A state without any diffs: of a new session or of one restored from 
-- the persisted state.
newRtrState :: WorldVersion -> RtrSessionId -> SerialNumber -> Intervals -> ServedPayloads -> RtrState
newRtrState worldVersion sessionId serial intervals served =
    RtrState {
        lastKnownWorldVersion = worldVersion,
        currentSessionId      = sessionId,
        currentSerial         = serial,
        maxSerialsPerSession  = 20,
        diffs                 = mempty,
        totalDiffSize         = 0,
        -- Do not store more VRPs in the diffs than there are in the data, as much
        -- as it has been (see 'updatedRtrState'). It's totally heuristical way
        -- of avoiding memory bloat
        maxTotalDiffSize      = vrpsCount $ served ^. #payloads . #uniqueVrps,
        carriedOver           = Nothing,
        ..
    }

-- | Create a new RTR state based on the previous one, a new diff and 
-- the payloads after the diff.
-- 
updatedRtrState :: RtrState -> WorldVersion -> RtrDiffs -> ServedPayloads -> RtrState
updatedRtrState RtrState {..} worldVersion diff newServed =
    RtrState {
        lastKnownWorldVersion = worldVersion,
        diffs = diffs',
        currentSerial = newSerial,
        totalDiffSize = newTotalSize,
        served = newServed,
        maxTotalDiffSize = newMaxTotalDiffSize,
        ..
    }
    where
        newSerial = nextSerial currentSerial

        -- Not the size of the data at the start, which is nothing after a cold
        -- start: then only the latest diff would be kept and every router that
        -- missed a serial would need to get everything again.
        newMaxTotalDiffSize = max maxTotalDiffSize $ vrpsCount $ newServed ^. #payloads . #uniqueVrps

        -- strip off the oldest serial if either
        --   * the list of diffs is getting too long
        --   * or the total amount of VRPs in all diffs is getting too big
        --   * never remove the latest diff, regardless of its size
        (diffs', newTotalSize) =
            shrinkUntilSizeFits
                (let !z = (newSerial, diff) in Deq.cons z diffs)
                (totalDiffSize + diffSize diff)
            where
                shrinkUntilSizeFits newDiffs@[_] newSize = (newDiffs, newSize)
                shrinkUntilSizeFits newDiffs newSize =
                    if length newDiffs > maxSerialsPerSession || newSize > newMaxTotalDiffSize
                        then
                            case Deq.unsnoc newDiffs of
                                Nothing -> (newDiffs, newSize)
                                Just ((_, removedDiff), restDiffs) -> 
                                    shrinkUntilSizeFits restDiffs $ newSize - diffSize removedDiff
                        else (newDiffs, newSize)


-- | Total number of payload items added or deleted by the diff.
diffSize :: GenDiffs a b c -> Int
diffSize GenDiffs {..} = size vrpDiff + size bgpSecDiff + size aspaDiff
  where
    size Diff {..} = Set.size added + Set.size deleted


-- | Return all the diffs starting from some serial if we have this data.
-- 
-- The oldest diffs are dropped when there are too many of them, a client
-- that needs one of those can only get everything from scratch.
diffsFromSerial :: RtrState -> SerialNumber -> Maybe [(SerialNumber, RtrDiffs)]
diffsFromSerial RtrState {..} clientSerial =
    case List.takeWhile ((> clientSerial) . fst) (toList diffs) of
        [] -> Nothing
        z | fst (List.last z) == nextSerial clientSerial -> Just z
          | otherwise                                    -> Nothing


-- | Transform a list of diffs into one diff that doesn't contain duplicates
-- or alternating 'add' and 'remove' operations for the same RTR payload (VRP, BGPSec or ASPA).
-- 
squashDiffs :: (Ord a, Ord b, Ord c) => [(SerialNumber, GenDiffs a b c)] -> GenDiffs a b c
squashDiffs diffs =
    GenDiffs {
        vrpDiff    = foldr (squash . (^. #vrpDiff)) newDiff sortedDiffs,
        bgpSecDiff = foldr (squash . (^. #bgpSecDiff)) newDiff sortedDiffs,
        aspaDiff   = foldr (squash . (^. #aspaDiff)) newDiff sortedDiffs
    }
  where
    sortedDiffs = map snd $ List.sortOn fst diffs
    -- What a diff and the later ones, squashed already, add up to. Something that
    -- one of them adds and the other one deletes is the same before and after
    -- them, so it's not a change: a router would reject the announcement of what
    -- it has or the withdrawal of what it doesn't have.
    squash earlier later =
        Diff {
            added   = (added earlier \\ deleted later) <> (added later \\ deleted earlier),
            deleted = (deleted earlier \\ added later) <> (deleted later \\ added earlier)
        }


-- | Create a diff, optimising typical corner-cases.
-- 
setDiff :: Ord a => Set a -> Set a -> Diff a
setDiff previous current
    | Set.null previous       && Set.null current       = newDiff
    | Set.null previous       && not (Set.null current) = newDiff { added = current }
    | not (Set.null previous) && Set.null current       = newDiff { deleted = previous }
    | otherwise = Diff {
                added   = current \\ previous,
                deleted = previous \\ current
            }


-- | Diff two packed VRP sets. Only the delta -- which is normally tiny next to
-- the sets themselves -- is materialised as 'Vrp'.
setDiffVrps :: Vrps -> Vrps -> Diff Vrp
setDiffVrps previous current = Diff {
        added   = Set.map unpack4 (added   diff4) <> Set.map unpack6 (added   diff6),
        deleted = Set.map unpack4 (deleted diff4) <> Set.map unpack6 (deleted diff6)
    }
  where
    diff4 = setDiff (packedSet (packed4 previous)) (packedSet (packed4 current))
    diff6 = setDiff (packedSet (packed6 previous)) (packedSet (packed6 current))

packedSet :: (VU.Unbox a, Ord a) => VU.Vector a -> Set a
packedSet = Set.fromList . VU.toList

evalDiffs :: RtrPayloads -> RtrPayloads -> RtrDiffs
evalDiffs previous current =
    GenDiffs {
        vrpDiff    = setDiffVrps (uniqueVrps previous) (uniqueVrps current),
        bgpSecDiff = setDiff (routerKeysPerAsn $ bgpSec previous) (routerKeysPerAsn $ bgpSec current),
        aspaDiff   = setDiff (aspas previous) (aspas current)
    }

-- | Router keys one per ASN, which is what the Router Key PDUs are. 
-- 
-- How the ASNs of a key are grouped into payloads is not visible to routers, 
-- e.g. two certificates of the same key for different ASNs give the same PDUs 
-- as one certificate for both ASNs, and the payloads read back from the 
-- persisted PDUs have one ASN each. So diffs compare the PDUs.
routerKeysPerAsn :: Set BGPSecPayload -> Set BGPSecPayload
routerKeysPerAsn = 
    Set.fromList 
        . concatMap (\p -> [ p { bgpSecAsns = [asn] } | asn <- bgpSecAsns p ]) 
        . Set.toList

-- | Everything that is in either of them: a route that one of them
-- accepts is accepted by the union too.
unionPayloads :: RtrPayloads -> RtrPayloads -> RtrPayloads
unionPayloads a b =
    RtrPayloads {
        -- Only the unique ones matter for RTR, and they are always sorted
        -- and deduplicated by these orderings
        vrps       = mempty,
        uniqueVrps = unionVrpsPackedBy cmpPacked4 cmpPacked6 (uniqueVrps a) (uniqueVrps b),
        -- The same key can be there for its ASNs together and for each of them
        bgpSec     = routerKeysPerAsn $ bgpSec a <> bgpSec b,
        aspas      = mergeAspasByCustomer $ aspas a <> aspas b
    }

-- | Payloads routers got before a restart are not served once they are older
-- than the Expire Interval, routers don't keep them for longer either.
carriedUntil :: Seconds -> CarriedOver -> Instant
carriedUntil expireInterval CarriedOver {..} =
    momentAfter (Instant $ versionToInt version) expireInterval

-- | What to serve after a validation, and what to keep carrying over.
--
-- After a restart, especially with a rebuilt cache, the validation may not
-- have everything yet that routers got before it, and serving only what it has
-- would make them drop the rest. So until the validation is settled, i.e. no more
-- data can come from the fetches (see 'RPKI.AppState.settled'), what routers got
-- before the restart is served together with what is validated. Routers get
-- what the validation adds right away, and what it doesn't have anymore is
-- withdrawn once it is settled, or once the carried over payloads are too old.
withCarriedOver :: Seconds -> Instant -> Bool -> Maybe CarriedOver -> RtrPayloads
                -> (RtrPayloads, Maybe CarriedOver)
withCarriedOver expireInterval now settled carriedOver validated =
    case carriedOver of
        Just carried
            | not settled && now < carriedUntil expireInterval carried ->
                (unionPayloads (carried ^. #served . #payloads) validated, Just carried)
        _ ->
            (validated, Nothing)


-- Wrap around at 2^31 - 1
-- https://tools.ietf.org/html/rfc8210#page-5
-- 
nextSerial :: SerialNumber -> SerialNumber
nextSerial (SerialNumber n) =
    SerialNumber $
        if (fromIntegral n :: Integer) == wrapAroundSerial
            then 0
            else n + 1

initialSerial :: SerialNumber
initialSerial = SerialNumber 1

wrapAroundSerial :: Integer
wrapAroundSerial = (2 :: Integer)^(31 :: Integer) - 1

-- | Session ID for a new session, i.e. one that is not a continuation of the
-- previous one. It must differ from the ID of the previous session, so that 
-- routers that still have its data notice the change 
-- (https://datatracker.ietf.org/doc/html/draft-ietf-sidrops-8210bis#section-5.1).
newSessionId :: WorldVersion -> Maybe RtrSessionId -> RtrSessionId
newSessionId worldVersion previous 
    | Just sessionId == previous = let RtrSessionId s = sessionId in RtrSessionId (s + 1)
    | otherwise                  = sessionId
  where
    sessionId = worldVersionToRtrSessionId worldVersion

-- 1) strip everything except from seconds from the the version
-- 2) Make it 2 bytes using `mod`    
worldVersionToRtrSessionId :: WorldVersion -> RtrSessionId
worldVersionToRtrSessionId (versionToInt -> nanoseconds) =
    RtrSessionId $ fromIntegral $
        (nanoseconds `div` nanosPerSecond) `mod` (256 * 256)

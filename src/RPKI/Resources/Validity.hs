
{-# LANGUAGE Strict #-}

module RPKI.Resources.Validity where

import           Control.DeepSeq
import           Control.Lens
import           Data.Generics.Labels ()

import           Data.List                as List
import           Data.Word                (Word8, Word32, Word64)
import           Data.Bits
import           Data.Foldable
import           Data.Coerce
import           Data.Kind

import           GHC.Generics

import qualified Data.Vector.Unboxed                   as VU

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6

import           RPKI.Domain
import           RPKI.Domain.Packed
import           RPKI.Resources.Types  hiding (start)
import           RPKI.Resources.Resources

data ValidityPerVrp = InvalidAsn Vrp
                    | InvalidLength Vrp
                    | Valid Vrp
    deriving stock (Show, Eq, Ord, Generic)     

data ValidityResult = ValidOverall [Vrp] [ValidityPerVrp]
                    | InvalidOverall [ValidityPerVrp]
                    | Unknown
    deriving stock (Show, Eq, Ord, Generic)                 

-- | The tree is parameterised over the container its leaves use, so that it
-- can be built with lists -- where insertion is a cons -- and then frozen into
-- unboxed vectors, which is how it is kept for the rest of its life.
--
-- Leaves hold at most ~20 entries, so per-element overhead is what dominates:
-- a cons cell plus a boxed constructor came to 57 bytes to carry 10 bytes of
-- VRP, against 24 in an unboxed vector (measured, 1m entries).
data Bucket f a c = Bucket {
        address :: a,
        bitSize :: {-# UNPACK #-} Word8,
        subtree :: AddressTree f a c
    }
    deriving stock (Generic)

data AddressTree f a c = AllTogether (f c)
                       | Divided {
                            lower       :: Bucket f a c,
                            higher      :: Bucket f a c,
                            overlapping :: f c
                        }
    deriving stock (Generic)

instance (NFData a, NFData (f c)) => NFData (Bucket f a c)
instance (NFData a, NFData (f c)) => NFData (AddressTree f a c)

-- | What the tree needs of a leaf container: build one, and pull out the
-- entries matching a predicate (always few, so a list is the right result).
class Leaves f c where
    leavesFromList :: [c] -> f c
    leavesFilter   :: (c -> Bool) -> f c -> [c]

instance Leaves [] c where
    leavesFromList = id
    leavesFilter   = filter
    {-# INLINE leavesFromList #-}
    {-# INLINE leavesFilter #-}

instance VU.Unbox c => Leaves VU.Vector c where
    -- Share one empty vector rather than allocating one per container. Most
    -- of the tree's `overlapping` lists are empty, and there are ~150k of
    -- them; an unboxed vector of a 4-field record is a wrapper plus four
    -- primitive vectors plus four byte arrays, ~230 bytes, empty or not.
    leavesFromList [] = VU.empty
    leavesFromList xs = VU.fromList xs
    leavesFilter f = VU.foldr (\x acc -> if f x then x : acc else acc) []
    {-# INLINE leavesFilter #-}

-- | Freeze a tree built with lists into one whose leaves are vectors.
freezeBucket :: Leaves f c => Bucket [] a c -> Bucket f a c
freezeBucket (Bucket a b sub) = Bucket a b (freezeTree sub)

freezeTree :: Leaves f c => AddressTree [] a c -> AddressTree f a c
freezeTree = \case
    AllTogether vrps -> AllTogether (leavesFromList vrps)
    Divided {..}     -> Divided { lower = freezeBucket lower,
                                  higher = freezeBucket higher,
                                  overlapping = leavesFromList overlapping }
    

-- IPv4 entries are stored as plain 'PackedVrp4' -- the same four words the
-- payloads use -- so they go straight into an unboxed vector. The interval
-- edges the tree compares on are not stored: for a canonical prefix the first
-- address is the address itself and the last is it with the host bits set,
-- which is two instructions and no allocation (checked against hw-ip's
-- firstIpAddress/lastIpAddress for every prefix length).

-- | The IPv6 counterpart. The edges stay 'Integer' because that is what the
-- IPv6 tree compares on, and recomputing them per comparison would allocate.
data StoredVrp6 = StoredVrp6
        !Integer                 -- first address of the prefix
        !Integer                 -- last address of the prefix
        {-# UNPACK #-} !Word32   -- ASN
        {-# UNPACK #-} !Word64   -- prefix address, high half
        {-# UNPACK #-} !Word64   -- prefix address, low half
        {-# UNPACK #-} !Word8    -- prefix length
        {-# UNPACK #-} !Word8    -- ROA max length
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

storedToVrp6 :: StoredVrp6 -> Vrp
storedToVrp6 (StoredVrp6 _ _ asn hi lo len maxLen) =
    Vrp (ASN asn) (Ipv6P (mkIpv6Prefix hi lo len)) (PrefixLength maxLen)

class StoredVrp a where
    type ActuallyStored a :: Type
    prefixEgdes :: ActuallyStored a -> (a, a)

instance StoredVrp Word32 where 
    type ActuallyStored Word32 = PackedVrp4
    prefixEgdes (PackedVrp4 _ addr len _) =
        (addr, addr .|. (complement 0 `shiftR` fromIntegral len))
    {-# INLINE prefixEgdes #-}

instance StoredVrp Integer where 
    type ActuallyStored Integer = StoredVrp6
    prefixEgdes (StoredVrp6 s e _ _ _ _ _) = (s, e)

-- | IPv4 leaves are unboxed vectors; IPv6 leaves stay lists, because
-- 'StoredVrp6' carries its interval edges as 'Integer' (a 128-bit address does
-- not fit a machine word) and boxed values cannot go in an unboxed vector.
data PrefixIndex = PrefixIndex {
        ipv4 :: Bucket VU.Vector Word32 PackedVrp4,
        ipv6 :: Bucket [] Integer StoredVrp6
    }
    deriving stock (Generic)
    deriving anyclass (NFData)

-- | The same tree while it is being built, with list leaves so that inserting
-- is a cons rather than an O(n) vector copy.
data IndexBuilder = IndexBuilder {
        bIpv4 :: Bucket [] Word32 PackedVrp4,
        bIpv6 :: Bucket [] Integer StoredVrp6
    }
    deriving stock (Generic)

emptyBuilder :: IndexBuilder
emptyBuilder = IndexBuilder {
        bIpv4 = Bucket 0 32  (AllTogether []),
        bIpv6 = Bucket 0 128 (AllTogether [])
    }

freezeIndex :: IndexBuilder -> PrefixIndex
freezeIndex IndexBuilder {..} = PrefixIndex {
        ipv4 = freezeBucket bIpv4,
        ipv6 = bIpv6
    }

createPrefixIndex :: (Foldable f, Coercible v Vrp) => f v -> PrefixIndex
createPrefixIndex = freezeIndex . foldr (insertVrp . coerce) emptyBuilder . toList

insertVrp :: Vrp -> IndexBuilder -> IndexBuilder
insertVrp (Vrp (ASN asn) pp (PrefixLength maxLen)) t = 
    case pp of 
        Ipv4P p@(Ipv4Prefix _) -> let 
                (startToInsert, endToInsert) = prefixEdgesV4 p
                (addr, len) = ipv4PrefixWords p
                stored = PackedVrp4 asn addr len maxLen
            in t & #bIpv4 %~ insertIntoTree stored startToInsert endToInsert

        Ipv6P p@(Ipv6Prefix _) -> let 
                (startToInsert, endToInsert) = prefixEdgesV6 p
                (hi, lo, len) = ipv6PrefixWords p
                stored = StoredVrp6 startToInsert endToInsert asn hi lo len maxLen
            in t & #bIpv6 %~ insertIntoTree stored startToInsert endToInsert
  where    
    
    insertIntoTree :: (Bits a, Num a, Ord a, StoredVrp a) => ActuallyStored a -> a -> a -> Bucket [] a (ActuallyStored a) -> Bucket [] a (ActuallyStored a)
    insertIntoTree toInsert startToInsert endToInsert bucket = 
        bucket & #subtree %~ \case        
            AllTogether vrps -> let 
                    vrps' = toInsert : vrps
                    updated = AllTogether vrps'
                in if length vrps' > leafSplitThreshold
                        then divide vrps'
                        else updated                
             
            Divided {..} ->                 
                case checkInterval startToInsert endToInsert middle of  
                    Lower    -> Divided { lower = insertIntoTree toInsert startToInsert endToInsert lower, .. }
                    Higher   -> Divided { higher = insertIntoTree toInsert startToInsert endToInsert higher, .. }
                    Overlaps -> Divided { overlapping = toInsert : overlapping, ..}
      where
        newBitSize = bucket ^. #bitSize - 1
        middle = intervalMiddle bucket

        divide vrps = let
            (lowerVrps, higherVrps, overlapping) = 
                foldr (\vrp (lowers, highers, overlaps) -> let 
                        (vStart, vEnd) = prefixEgdes vrp
                    in case checkInterval vStart vEnd middle of 
                        Lower    -> (vrp : lowers, highers,       overlaps)
                        Higher   -> (lowers,       vrp : highers, overlaps)
                        Overlaps -> (lowers,       highers,       vrp : overlaps)     
                ) ([], [], []) vrps

            lower  = Bucket (bucket ^. #address) newBitSize $ AllTogether lowerVrps 
            
            higher = Bucket middle newBitSize $ AllTogether higherVrps 

            in Divided {..}


lookupVrps :: IpPrefix -> PrefixIndex -> [Vrp]
lookupVrps prefix PrefixIndex {..} =         
    case prefix of
        Ipv4P p@(Ipv4Prefix _) -> let 
                (start, end) = prefixEdgesV4 p
            in map unpack4 $ lookupTree ipv4 start end

        Ipv6P p@(Ipv6Prefix _) -> let 
                (start, end) = prefixEdgesV6 p
            in map storedToVrp6 $ lookupTree ipv6 start end
  where    
    lookupTree :: (Bits a, Num a, Ord a, StoredVrp a, Leaves f (ActuallyStored a))
               => Bucket f a (ActuallyStored a) -> a -> a -> [ActuallyStored a]
    lookupTree bucket start end =         
        case bucket ^. #subtree of 
            AllTogether vrps -> leavesFilter suitable vrps
            Divided {..}     -> let 
                    overlaps = leavesFilter suitable overlapping
                in case checkInterval start end (intervalMiddle bucket) of 
                    Lower    -> overlaps <> lookupTree lower start end
                    Higher   -> overlaps <> lookupTree higher start end
                    Overlaps -> overlaps
      where
        {-# INLINE suitable #-}
        suitable (prefixEgdes -> (vStart, vEnd)) = 
            vStart <= start && vEnd >= end

prefixValidity :: ASN -> IpPrefix -> PrefixIndex -> ValidityResult
prefixValidity asn prefix prefixIndex = 
    case coveringVrps of 
        [] -> Unknown
        _  -> case validBy of            
                [] -> InvalidOverall invalidBy
                _  -> ValidOverall [ v | Valid v <- validBy ] invalidBy  
  where
    coveringVrps = lookupVrps prefix prefixIndex

    validityPerVrp = 
        map (\vrp@(Vrp vAsn _ maxLength) -> 
                if | vAsn /= asn                  -> InvalidAsn vrp
                   | prefixLen prefix > maxLength -> InvalidLength vrp
                   | otherwise                    -> Valid vrp
            ) coveringVrps        

    (validBy, invalidBy) = List.partition (\case 
            Valid _ -> True
            _       -> False) validityPerVrp        

{-# INLINE prefixEdgesV4 #-}
prefixEdgesV4 :: Ipv4Prefix -> (Word32, Word32)
prefixEdgesV4 (Ipv4Prefix p) = (asWord32 (V4.firstIpAddress p), asWord32 (V4.lastIpAddress p))

{-# INLINE prefixEdgesV6 #-}
prefixEdgesV6 :: Ipv6Prefix -> (Integer, Integer)
prefixEdgesV6 (Ipv6Prefix p) = (v6toInteger (V6.firstIpAddress p), v6toInteger (V6.lastIpAddress p))

-- | How many VRPs a leaf holds before it splits.
--
-- Bigger leaves mean fewer containers, and with unboxed leaves the container
-- is what costs: a vector of a 4-field record is a wrapper plus four
-- primitive vectors plus four byte arrays, whatever it holds. Measured over
-- the real ~1m VRP set, against the previous list-based leaves (176mb,
-- 3.43us/lookup):
--
--     20   169mb   3.25us      50   151mb   3.34us     100   144mb   3.70us
--
-- 50 is the point where both the memory and the lookup are better than the
-- lists were; past it the linear scan inside a leaf starts to show.
leafSplitThreshold :: Int
leafSplitThreshold = 50

{-# INLINE intervalMiddle #-}
intervalMiddle :: (Bits a, Num a) => Bucket f a c -> a
intervalMiddle bucket = bucket ^. #address + 1 `shiftL` fromIntegral (bucket ^. #bitSize - 1)

data What = Lower | Higher | Overlaps
    deriving stock (Eq, Ord, Generic)     

{-# INLINE checkInterval #-}
checkInterval :: Ord a => a -> a -> a -> What
checkInterval start end middle = 
    if | end < middle   -> Lower
       | start > middle -> Higher
       | otherwise      -> Overlaps

{-# INLINE asWord32 #-}
asWord32 :: V4.IpAddress -> Word32
asWord32 (V4.IpAddress w) = w

{-# INLINE v6toInteger #-}
v6toInteger :: V6.IpAddress -> Integer
v6toInteger (V6.IpAddress (w0, w1, w2, w3)) = let 
        i3 = fromIntegral w3 
        i2 = fromIntegral w2 `shiftL` 32
        i1 = fromIntegral w1 `shiftL` 64
        i0 = fromIntegral w0 `shiftL` 96
    in i0 + i1 + i2 + i3

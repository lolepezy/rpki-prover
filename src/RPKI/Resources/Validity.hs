
{-# LANGUAGE Strict #-}

module RPKI.Resources.Validity where

import           Control.DeepSeq
import           Control.Lens
import           Data.Generics.Labels                  

import           Data.List                as List
import           Data.Word                (Word8, Word32, Word64)
import           Data.Bits
import           Data.Foldable
import           Data.Coerce
import           Data.Kind

import           GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Resources.Resources

data ValidityPerVrp = InvalidAsn Vrp
                    | InvalidLength Vrp
                    | Valid Vrp
    deriving stock (Show, Eq, Ord, Generic)     

data ValidityResult = ValidOverall [Vrp] [ValidityPerVrp]
                    | InvalidOverall [ValidityPerVrp]
                    | Unknown
    deriving stock (Show, Eq, Ord, Generic)                 

data Bucket a c = Bucket {
        address :: a,
        bitSize :: {-# UNPACK #-} Word8,
        subtree :: AddressTree a c
    }
    deriving stock (Show, Eq, Ord, Generic)     
    deriving anyclass (NFData)
        

data AddressTree a c = AllTogether [c]
                     | Divided {
                            lower       :: Bucket a c,
                            higher      :: Bucket a c,
                            overlapping :: [c]
                        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)    
    

-- | A VRP as the index stores it: flattened into words, with the interval
-- edges the tree compares on precomputed.
--
-- The tree holds these in lists, so a boxed 'Vrp' cost a cons cell plus the
-- Vrp and its four sub-boxes -- on the order of 160 bytes to carry the ~10
-- bytes a VRP actually is. Unpacking happens once per lookup hit, on the
-- handful of VRPs that cover the queried prefix, rather than being paid for
-- every VRP in the index for as long as the index exists.
data StoredVrp4 = StoredVrp4
        {-# UNPACK #-} !Word32   -- first address of the prefix
        {-# UNPACK #-} !Word32   -- last address of the prefix
        {-# UNPACK #-} !Word32   -- ASN
        {-# UNPACK #-} !Word32   -- prefix address
        {-# UNPACK #-} !Word8    -- prefix length
        {-# UNPACK #-} !Word8    -- ROA max length
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

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

storedToVrp4 :: StoredVrp4 -> Vrp
storedToVrp4 (StoredVrp4 _ _ asn addr len maxLen) =
    Vrp (ASN asn) (Ipv4P (mkIpv4Prefix addr len)) (PrefixLength maxLen)

storedToVrp6 :: StoredVrp6 -> Vrp
storedToVrp6 (StoredVrp6 _ _ asn hi lo len maxLen) =
    Vrp (ASN asn) (Ipv6P (mkIpv6Prefix hi lo len)) (PrefixLength maxLen)

class StoredVrp a where
    type ActuallyStored a :: Type
    prefixEgdes :: ActuallyStored a -> (a, a)

instance StoredVrp Word32 where 
    type ActuallyStored Word32 = StoredVrp4
    prefixEgdes (StoredVrp4 s e _ _ _ _) = (s, e)

instance StoredVrp Integer where 
    type ActuallyStored Integer = StoredVrp6
    prefixEgdes (StoredVrp6 s e _ _ _ _ _) = (s, e)

data PrefixIndex = PrefixIndex {
        ipv4 :: Bucket Word32 (ActuallyStored Word32),
        ipv6 :: Bucket Integer (ActuallyStored Integer)
    }
    deriving stock (Show, Eq, Ord, Generic)     
    deriving anyclass (NFData)


makePrefixIndex :: PrefixIndex
makePrefixIndex = let 
        ipv4 = Bucket 0 32  (AllTogether [])
        ipv6 = Bucket 0 128 (AllTogether [])
    in PrefixIndex {..}

createPrefixIndex :: (Foldable f, Coercible v Vrp) => f v -> PrefixIndex
createPrefixIndex = foldr (insertVrp . coerce) makePrefixIndex . toList

insertVrp :: Vrp -> PrefixIndex -> PrefixIndex
insertVrp (Vrp (ASN asn) pp (PrefixLength maxLen)) t = 
    case pp of 
        Ipv4P p@(Ipv4Prefix _) -> let 
                (startToInsert, endToInsert) = prefixEdgesV4 p
                (addr, len) = ipv4PrefixWords p
                stored = StoredVrp4 startToInsert endToInsert asn addr len maxLen
            in t & #ipv4 %~ insertIntoTree stored startToInsert endToInsert

        Ipv6P p@(Ipv6Prefix _) -> let 
                (startToInsert, endToInsert) = prefixEdgesV6 p
                (hi, lo, len) = ipv6PrefixWords p
                stored = StoredVrp6 startToInsert endToInsert asn hi lo len maxLen
            in t & #ipv6 %~ insertIntoTree stored startToInsert endToInsert
  where    
    
    insertIntoTree :: (Bits a, Num a, Ord a, StoredVrp a) => ActuallyStored a -> a -> a -> Bucket a (ActuallyStored a) -> Bucket a (ActuallyStored a)
    insertIntoTree toInsert startToInsert endToInsert bucket = 
        bucket & #subtree %~ \case        
            AllTogether vrps -> let 
                    vrps' = toInsert : vrps
                    updated = AllTogether vrps'
                in if length vrps' > 20 
                        then divide updated 
                        else updated                
             
            Divided {..} ->                 
                case checkInterval startToInsert endToInsert middle of  
                    Lower    -> Divided { lower = insertIntoTree toInsert startToInsert endToInsert lower, .. }
                    Higher   -> Divided { higher = insertIntoTree toInsert startToInsert endToInsert higher, .. }
                    Overlaps -> Divided { overlapping = toInsert : overlapping, ..}
      where
        newBitSize = bucket ^. #bitSize - 1
        middle = intervalMiddle bucket

        divide (AllTogether vrps) = let
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
            in map storedToVrp4 $ lookupTree ipv4 start end

        Ipv6P p@(Ipv6Prefix _) -> let 
                (start, end) = prefixEdgesV6 p
            in map storedToVrp6 $ lookupTree ipv6 start end
  where    
    lookupTree bucket start end =         
        case bucket ^. #subtree of 
            AllTogether vrps -> filter suitable vrps
            Divided {..}     -> let 
                    overlaps = filter suitable overlapping
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

{-# INLINE intervalMiddle #-}
intervalMiddle :: (Bits a, Num a) => Bucket a c -> a
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

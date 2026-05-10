
{-# LANGUAGE Strict #-}

module RPKI.Resources.Validity where

import           Control.DeepSeq
import           Control.Lens
import           Data.Generics.Labels                  

import           Data.List                as List
import           Data.Word                (Word8, Word32)
import           Data.Bits
import           Data.Foldable qualified as Foldable
import           Data.Coerce
import           Data.Kind

import           GHC.Generics

import           Data.Primitive.SmallArray
import           Data.WideWord.Word128     (Word128 (..))

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
        address :: !a,
        bitSize :: {-# UNPACK #-} Word8,
        subtree :: AddressTree a c
    }
    deriving stock (Show, Eq, Ord, Generic)     
    deriving anyclass (NFData)
        

-- | Uses SmallArray instead of [] for contiguous, cache-friendly storage
-- with a single heap object instead of N cons cells.
data AddressTree a c = AllTogether !(SmallArray c)
                     | Divided {
                            lower       :: Bucket a c,
                            higher      :: Bucket a c,
                            overlapping :: !(SmallArray c)
                        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (NFData)

-- SmallArray doesn't have Ord; provide structural Eq/Ord via list conversion
instance (Eq a, Ord c) => Ord (AddressTree a c) where
    compare x y = compare (toList x) (toList y)
      where
        toList (AllTogether arr)    = foldr (:) [] arr
        toList (Divided {..})       = foldr (:) [] overlapping

snocSmallArray :: SmallArray a -> a -> SmallArray a
snocSmallArray arr x = runSmallArray $ do
    let n = sizeofSmallArray arr
    ma <- newSmallArray (n + 1) x
    copySmallArray ma 0 arr 0 n
    pure ma


data QuickCompVrp = QuickCompVrp
    {-# UNPACK #-} !Word128
    {-# UNPACK #-} !Word128
    !Vrp
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)    
    
class StoredVrp a where
    type ActuallyStored a :: Type
    makeStoredVrp :: a -> a -> Vrp -> ActuallyStored a
    prefixEgdes :: ActuallyStored a -> (a, a)

instance StoredVrp Word32 where 
    type ActuallyStored Word32 = Vrp
    makeStoredVrp _ _ vrp = vrp
    prefixEgdes (Vrp _ (Ipv4P p) _) = prefixEdgesV4 p

instance StoredVrp Word128 where 
    type ActuallyStored Word128 = QuickCompVrp
    makeStoredVrp s e vrp = QuickCompVrp s e vrp
    prefixEgdes (QuickCompVrp s e _) = (s, e)

data PrefixIndex = PrefixIndex {
        ipv4 :: Bucket Word32 (ActuallyStored Word32),
        ipv6 :: Bucket Word128 (ActuallyStored Word128)
    }
    deriving stock (Show, Eq, Ord, Generic)     
    deriving anyclass (NFData)


makePrefixIndex :: PrefixIndex
makePrefixIndex = let 
        ipv4 = Bucket 0 32  (AllTogether mempty)
        ipv6 = Bucket (Word128 0 0) 128 (AllTogether mempty)
    in PrefixIndex {..}

createPrefixIndex :: (Foldable f, Coercible v Vrp) => f v -> PrefixIndex
createPrefixIndex = foldr (insertVrp . coerce) makePrefixIndex . Foldable.toList

insertVrp :: Vrp -> PrefixIndex -> PrefixIndex
insertVrp vrpToInsert@(Vrp _ pp _) t = 
    case pp of 
        Ipv4P p@(Ipv4Prefix _) -> let 
                (startToInsert, endToInsert) = prefixEdgesV4 p
            in t & #ipv4 %~ insertIntoTree startToInsert endToInsert

        Ipv6P p@(Ipv6Prefix _) -> let 
                (startToInsert, endToInsert) = prefixEdgesV6W128 p
            in t & #ipv6 %~ insertIntoTree startToInsert endToInsert
  where    
    
    insertIntoTree :: (Bits a, Num a, Ord a, StoredVrp a) => a -> a -> Bucket a (ActuallyStored a) -> Bucket a (ActuallyStored a)
    insertIntoTree startToInsert endToInsert bucket = 
        bucket & #subtree %~ \case        
            AllTogether arr -> let 
                    arr' = snocSmallArray arr toInsert
                    updated = AllTogether arr'
                in if sizeofSmallArray arr' > 20 
                        then divide updated 
                        else updated                
             
            Divided {..} ->                 
                case checkInterval startToInsert endToInsert middle of  
                    Lower    -> Divided { lower = insertIntoTree startToInsert endToInsert lower, .. }
                    Higher   -> Divided { higher = insertIntoTree startToInsert endToInsert higher, .. }
                    Overlaps -> Divided { overlapping = snocSmallArray overlapping toInsert, ..}
      where
        toInsert = makeStoredVrp startToInsert endToInsert vrpToInsert
        newBitSize = bucket ^. #bitSize - 1
        middle = intervalMiddle bucket

        divide (AllTogether arr) = let
            (lowerVrps, higherVrps, overlapVrps) = 
                foldr (\vrp (lowers, highers, overlaps) -> let 
                        (vStart, vEnd) = prefixEgdes vrp
                    in case checkInterval vStart vEnd middle of 
                        Lower    -> (vrp : lowers, highers,       overlaps)
                        Higher   -> (lowers,       vrp : highers, overlaps)
                        Overlaps -> (lowers,       highers,       vrp : overlaps)     
                ) ([], [], []) (foldr (:) [] arr)

            lower  = Bucket (bucket ^. #address) newBitSize $ AllTogether (smallArrayFromList lowerVrps)
            higher = Bucket middle newBitSize $ AllTogether (smallArrayFromList higherVrps)
            overlapping = smallArrayFromList overlapVrps

            in Divided {..}


lookupVrps :: IpPrefix -> PrefixIndex -> [Vrp]
lookupVrps prefix PrefixIndex {..} =         
    case prefix of
        Ipv4P p@(Ipv4Prefix _) -> let 
                (start, end) = prefixEdgesV4 p
            in lookupTree ipv4 start end

        Ipv6P p@(Ipv6Prefix _) -> let 
                (start, end) = prefixEdgesV6W128 p
            in map (\(QuickCompVrp _ _ vrp) -> vrp) $ lookupTree ipv6 start end
  where    
    lookupTree bucket start end =         
        case bucket ^. #subtree of 
            AllTogether arr  -> filter suitable (foldr (:) [] arr)
            Divided {..}     -> let 
                    overlaps = filter suitable (foldr (:) [] overlapping)
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

{-# INLINE prefixEdgesV6W128 #-}
prefixEdgesV6W128 :: Ipv6Prefix -> (Word128, Word128)
prefixEdgesV6W128 (Ipv6Prefix p) = (asWord128 (V6.firstIpAddress p), asWord128 (V6.lastIpAddress p))

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

{-# INLINE asWord128 #-}
asWord128 :: V6.IpAddress -> Word128
asWord128 (V6.IpAddress (w0, w1, w2, w3)) = 
    Word128 (fromIntegral w0 `shiftL` 32 .|. fromIntegral w1)
            (fromIntegral w2 `shiftL` 32 .|. fromIntegral w3)

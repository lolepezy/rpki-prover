
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}
-- it is a little faster
{-# LANGUAGE Strict             #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE MultiWayIf         #-}

module RPKI.Resources.Validity where

import           Control.Lens
import           Data.Generics.Labels                  

import           Data.List                as List
import           Data.Word                (Word8)
import           Data.Bits

import           GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Resources.Types
import           RPKI.Resources.Resources
                  
import           RPKI.Resources.Types                
import           RPKI.Resources.Resources

import           RPKI.Store.Base.Serialisation
                  

newtype ValidityByRoa = ValidityByRoa {
        vrp :: Vrp
    }
    deriving stock (Show, Eq, Ord, Generic)         

data ValidityPerVrp = InvalidAsn Vrp
                    | InvalidLength Vrp
                    | Valid Vrp
    deriving stock (Show, Eq, Ord, Generic)     


data ValidityResult = ValidOverall [Vrp] [ValidityPerVrp]
                    | InvalidOverall [ValidityPerVrp]
                    | Unknown
    deriving stock (Show, Eq, Ord, Generic)                         

data Node c = Node {
        address :: {-# UNPACK #-} Integer,
        bitSize :: {-# UNPACK #-} Word8,
        subtree :: AddressTree c
    }
    deriving stock (Show, Eq, Ord, Generic)     
    deriving anyclass (TheBinary)

data AddressTree c = AllTogether [c]
                     | Divided {
                            lower       :: Node c,
                            higher      :: Node c,
                            overlapping :: [c]
                        }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data FasterVrp = FasterVrp Integer Integer Vrp
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data PrefixIndex = PrefixIndex {
        ipv4 :: Node FasterVrp,
        ipv6 :: Node FasterVrp
    }
    deriving stock (Show, Eq, Ord, Generic)     
    deriving anyclass (TheBinary)

makePrefixIndex :: PrefixIndex
makePrefixIndex = let 
        ipv4 = Node 0 32  (AllTogether [])
        ipv6 = Node 0 128 (AllTogether [])
    in PrefixIndex {..}

createPrefixIndex :: Vrps -> PrefixIndex
createPrefixIndex = foldr insertVrp makePrefixIndex . mconcat . allVrps

insertVrp :: Vrp -> PrefixIndex -> PrefixIndex
insertVrp vrpToInsert@(Vrp _ pp _) t = 
    case pp of 
        Ipv4P (Ipv4Prefix _) -> t & #ipv4 %~ insertIntoTree
        Ipv6P (Ipv6Prefix _) -> t & #ipv6 %~ insertIntoTree
  where
    (startToInsert, endToInsert) = prefixEdges pp

    insertIntoTree node = 
        node & #subtree %~ \case        
            AllTogether vrps -> let 
                    vrps' = toInsert : vrps
                    updated = AllTogether vrps'
                in if length vrps' > 3 
                        then divide updated 
                        else updated                
             
            Divided {..} ->                 
                case checkInterval startToInsert endToInsert middle of  
                    Lower    -> Divided { lower = insertIntoTree lower, .. }
                    Higher   -> Divided { higher = insertIntoTree higher, .. }
                    Overlaps -> Divided { overlapping = toInsert : overlapping, ..}
      where
        toInsert = FasterVrp startToInsert endToInsert vrpToInsert
        newBitSize = node ^. #bitSize - 1
        middle = intervalMiddle node

        divide (AllTogether vrps) = let
            (lowerVrps, higherVrps, overlapping) = 
                foldr (\vrp@(FasterVrp vStart vEnd _) (lowers, highers, overlaps) -> 
                    case checkInterval vStart vEnd middle of 
                        Lower    -> (vrp : lowers, highers,       overlaps)
                        Higher   -> (lowers,       vrp : highers, overlaps)
                        Overlaps -> (lowers,       highers,       vrp : overlaps)     
                ) ([], [], []) vrps

            lower  = Node (node ^. #address) newBitSize $ AllTogether lowerVrps 
            
            higher = Node middle newBitSize $ AllTogether higherVrps 

            in Divided {..}


lookupVrps :: IpPrefix -> PrefixIndex -> [Vrp]
lookupVrps prefix PrefixIndex {..} = 
    map (\(FasterVrp _ _ vrp) -> vrp) $
        case prefix of
            Ipv4P (Ipv4Prefix _) -> lookupTree ipv4 
            Ipv6P (Ipv6Prefix _) -> lookupTree ipv6 
  where
    (start, end) = prefixEdges prefix

    lookupTree node =         
        case node ^. #subtree of 
            AllTogether vrps -> filter suitable vrps
            Divided {..}     -> 
                case checkInterval start end (intervalMiddle node) of 
                    Lower    -> lookupTree lower 
                    Higher   -> lookupTree higher
                    Overlaps -> filter suitable overlapping

    {-# INLINE suitable #-}
    suitable (FasterVrp vStart vEnd _) =
        vStart <= start && vEnd >= end

validity :: ASN -> IpPrefix -> PrefixIndex -> ValidityResult
validity asn prefix prefixIndex = 
    case coveringVrps of 
        [] -> Unknown
        _  -> case validBy of            
                [] -> InvalidOverall invalidBy
                _  -> ValidOverall [ v | Valid v <- validBy ] invalidBy  
  where
    coveringVrps = lookupVrps prefix prefixIndex

    validityPerVrp = 
        map (\vrp@(Vrp vAsn vPrefix maxLength) -> 
                if | vAsn /= asn                  -> InvalidAsn vrp
                    | prefixLen prefix > maxLength -> InvalidLength vrp
                    | otherwise                    -> Valid vrp
            ) coveringVrps        

    (validBy, invalidBy) = List.partition (\case 
            Valid _ -> True
            _       -> False) validityPerVrp        


{-# INLINE prefixEdges #-}         
prefixEdges :: IpPrefix -> (Integer, Integer)
prefixEdges = \case
    Ipv4P (Ipv4Prefix p) -> (v4toInteger (V4.firstIpAddress p), v4toInteger (V4.lastIpAddress p))
    Ipv6P (Ipv6Prefix p) -> (v6toInteger (V6.firstIpAddress p), v6toInteger (V6.lastIpAddress p))  

{-# INLINE intervalMiddle #-}
intervalMiddle :: Node a -> Integer
intervalMiddle node = node ^. #address + 1 `shiftL` (fromIntegral (node ^. #bitSize - 1))

data What = Lower | Higher | Overlaps
    deriving stock (Eq, Ord, Generic)     

{-# INLINE checkInterval #-}
checkInterval :: Integer -> Integer -> Integer -> What
checkInterval start end middle = 
    if | end < middle   -> Lower
       | start > middle -> Higher
       | otherwise      -> Overlaps

{-# INLINE v4toInteger #-}
v4toInteger :: V4.IpAddress -> Integer
v4toInteger (V4.IpAddress w) = fromIntegral w

{-# INLINE v6toInteger #-}
v6toInteger :: V6.IpAddress -> Integer
v6toInteger (V6.IpAddress (w0, w1, w2, w3)) = let 
        i3 = fromIntegral w3 
        i2 = fromIntegral w2 `shiftL` 32
        i1 = fromIntegral w1 `shiftL` 64
        i0 = fromIntegral w0 `shiftL` 96
    in i0 + i1 + i2 + i3

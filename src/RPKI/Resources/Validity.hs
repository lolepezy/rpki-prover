
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE MultiWayIf         #-}

module RPKI.Resources.Validity where

import           Control.Lens
import           Data.Generics.Labels                  

import qualified Data.List                as List
import qualified Data.Set                 as Set
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Word                (Word8, Word32)
import           Data.Bits

import           GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Resources.Resources
                  

newtype ValidityByRoa = ValidityByRoa {
        vrp :: Vrp
    }
    deriving stock (Show, Eq, Ord, Generic)     

data ValidityResult = InvalidAsn 
                    | InvalidLength 
                    | Valid [ValidityByRoa]    
                    | Unknown
    deriving stock (Show, Eq, Ord, Generic)     

data Node c = Node {
        address :: Integer,
        bitSize :: Word8,
        subtree :: AddressTree c
    }
    deriving stock (Show, Eq, Ord, Generic)     

data AddressTree c = AllTogether [c] Int
                     | Divided {
                            lower       :: Node c,
                            higher      :: Node c,
                            overlapping :: [c]
                        }
    deriving stock (Show, Eq, Ord, Generic)

data PrefixIndex = PrefixIndex {
        ipv4 :: Node Vrp,
        ipv6 :: Node Vrp
    }
    deriving stock (Show, Eq, Ord, Generic)     

makePrefixIndex = let 
        ipv4 = Node 0 32  (AllTogether [] 0)
        ipv6 = Node 0 128 (AllTogether [] 0)
    in PrefixIndex {..}

insertVrp :: Vrp -> PrefixIndex -> PrefixIndex
insertVrp vrpToInsert@(Vrp _ pp _) t = 
    case pp of 
        Ipv4P (Ipv4Prefix p) -> t & #ipv4 %~ insertIntoTree
        Ipv6P (Ipv6Prefix p) -> t & #ipv6 %~ insertIntoTree
  where
    (startToInsert, endToInsert) = prefixEdges pp

    insertIntoTree node = 
        case node ^. #subtree of
            AllTogether vrps count -> let 
                    updated = AllTogether (vrpToInsert : vrps) (count + 1)
                in 
                    node & #subtree .~ if count > 3 then divide updated else updated                
 
            Divided {..} -> 
                node & #subtree .~
                    case checkInterval startToInsert endToInsert middle of  
                        Lower    -> Divided { lower = insertIntoTree lower, .. }
                        Higher   -> Divided { higher = insertIntoTree higher, .. }
                        Overlaps -> Divided { overlapping = vrpToInsert : overlapping, ..}
      where
        newBitSize = node ^. #bitSize - 1
        middle = intervalMiddle node

        divide (AllTogether vrps _) = let
            (lowerVrps, higherVrps, overlapping) = 
                foldr (\vrp@(Vrp _ (prefixEdges -> (vStart, vEnd)) _) (lowers, highers, overlaps) -> 
                    case checkInterval vStart vEnd middle of 
                        Lower    -> (vrp : lowers, highers,       overlaps)
                        Higher   -> (lowers,       vrp : highers, overlaps)
                        Overlaps -> (lowers,       highers,       vrp : overlaps)     
                ) ([], [], []) vrps

            lower  = Node (node ^. #address) newBitSize $ 
                        AllTogether lowerVrps (length lowerVrps)
            
            higher = Node middle newBitSize $ 
                        AllTogether higherVrps (length higherVrps)

            in Divided {..}


lookupVrps :: IpPrefix -> PrefixIndex -> [Vrp]
lookupVrps prefix PrefixIndex {..} = 
    case prefix of
        Ipv4P (Ipv4Prefix _) -> lookupTree ipv4 
        Ipv6P (Ipv6Prefix _) -> lookupTree ipv6 
  where
    (start, end) = prefixEdges prefix

    lookupTree :: Node Vrp -> [Vrp]
    lookupTree node@Node {..} = 
        case subtree of 
            AllTogether vrps _ -> filter suitable vrps
            Divided {..}       -> 
                case checkInterval start end (intervalMiddle node) of 
                    Lower    -> lookupTree lower 
                    Higher   -> lookupTree higher
                    Overlaps -> filter suitable overlapping

    suitable (Vrp _ (prefixEdges -> (vStart, vEnd)) _) =
        vStart <= start && vEnd >= end
         

prefixEdges :: IpPrefix -> (Integer, Integer)
prefixEdges = \case
    Ipv4P (Ipv4Prefix p) -> (v4toInteger (V4.firstIpAddress p), v4toInteger (V4.lastIpAddress p))
    Ipv6P (Ipv6Prefix p) -> (v6toInteger (V6.firstIpAddress p), v6toInteger (V6.lastIpAddress p))  


intervalMiddle :: Node a -> Integer
intervalMiddle node = node ^. #address + 1 `shiftL` (fromIntegral (node ^. #bitSize - 1))

data What = Lower | Higher | Overlaps
    deriving stock (Eq, Ord, Generic)     


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

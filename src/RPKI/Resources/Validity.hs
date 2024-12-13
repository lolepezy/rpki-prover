
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
    deriving stock (Eq, Ord, Generic)     

data ValidityResult = InvalidAsn 
                    | InvalidLength 
                    | Valid [ValidityByRoa]    
                    | Unknown
    deriving stock (Eq, Ord, Generic)     

data Node c = Node {
        address :: Integer,
        space   :: Word8,
        subtree :: AddressTree c
    }
    deriving stock (Eq, Ord, Generic)     

data AddressTree c = AllTogether [c] Int
                     | Divided {
                            lower       :: Node c,
                            higher      :: Node c,
                            overlapping :: [c]
                        }
    deriving stock (Eq, Ord, Generic)

data PrefixIndex = PrefixIndex {
        ipv4 :: Node Vrp,
        ipv6 :: Node Vrp
    }
    deriving stock (Eq, Ord, Generic)     

makePrefixIndex = let 
        ipv4 = Node 0 32  (AllTogether [] 0)
        ipv6 = Node 0 128 (AllTogether [] 0)
    in PrefixIndex {..}

insert :: Vrp -> PrefixIndex -> PrefixIndex
insert vrpToInsert@(Vrp _ pp _) t = 
    case pp of 
        Ipv4P (Ipv4Prefix p) -> t & #ipv4 %~ insertIntoTree
        Ipv6P (Ipv6Prefix p) -> t & #ipv6 %~ insertIntoTree
  where
    (startToInsert, endToInsert) = prefixEdges pp

    insertIntoTree :: Node Vrp -> Node Vrp
    insertIntoTree node = 
        case node ^. #subtree of
            AllTogether vrps count -> let 
                    updated = AllTogether (vrpToInsert : vrps) (count + 1)
                in 
                    node & #subtree .~ if count > 5 then divide updated else updated                
 
            Divided {..} -> 
                case checkInterval startToInsert endToInsert boundary of  
                    Lower    -> insertIntoTree lower
                    Higher   -> insertIntoTree higher
                    Overlaps -> node & #subtree .~ Divided { overlapping = vrpToInsert : overlapping, ..}
      where
        space' = node ^. #space - 1
        boundary = node ^. #address + 1 `shiftL` (fromIntegral space')

        divide (AllTogether vrps _) = let
            (lowerVrps, higherVrps, overlapping) = 
                foldr (\vrp@(Vrp _ (prefixEdges -> (vStart, vEnd)) _) (lowers, highers, overlaps) -> 
                    case checkInterval vStart vEnd boundary of 
                        Lower    -> (vrp : lowers, highers,       overlaps)
                        Higher   -> (lowers,       vrp : highers, overlaps)
                        Overlaps -> (lowers,       highers,       vrp : overlaps)    
                
                ) ([], [], []) vrps

            lower  = Node (node ^. #address) space' $ 
                        AllTogether lowerVrps (length lowerVrps)
            
            higher = Node boundary space' $ 
                        AllTogether higherVrps (length higherVrps)

            in Divided {..}

    
prefixEdges :: IpPrefix -> (Integer, Integer)
prefixEdges = \case
    Ipv4P (Ipv4Prefix p) -> (v4toInteger (V4.firstIpAddress p), v4toInteger (V4.lastIpAddress p))
    Ipv6P (Ipv6Prefix p) -> (v6toInteger (V6.firstIpAddress p), v6toInteger (V6.lastIpAddress p))  


data What = Lower | Higher | Overlaps
    deriving stock (Eq, Ord, Generic)     


checkInterval :: Integer -> Integer -> Integer -> What
checkInterval start end boundary = 
    if | end <= boundary  -> Lower
       | start > boundary -> Higher
       | otherwise        -> Overlaps


{-# INLINE v4toInteger #-}
v4toInteger :: V4.IpAddress -> Integer
v4toInteger (V4.IpAddress w) = fromIntegral w

{-# INLINE v6toInteger #-}
v6toInteger :: V6.IpAddress -> Integer
v6toInteger (V6.IpAddress (w0, w1, w2, w3)) = let 
        i3 = fromIntegral w3 
        i2 = fromIntegral w2 `shiftL` 32
        i1 = fromIntegral w2 `shiftL` 64
        i0 = fromIntegral w1 `shiftL` 96
    in i0 + i1 + i2 + i3


lookup :: IpPrefix -> PrefixIndex -> [Vrp]
lookup _ _ = []

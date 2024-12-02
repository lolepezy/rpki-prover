
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Resources.Validity where

import qualified Data.List                as List
import qualified Data.Set                 as Set
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

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
    deriving stock (Eq, Ord, Generic)     


data IndexEntry c = IndexEntry PrefixLength c 
    deriving stock (Eq, Ord, Generic)     

data PrefixIndex = PrefixIndex {
        ipv4 :: Map V4.IpAddress [IndexEntry Vrp],
        ipv6 :: Map V6.IpAddress [IndexEntry Vrp]
    }
    deriving stock (Eq, Ord, Generic)     


makePrefixIndex :: Vrps -> PrefixIndex 
makePrefixIndex vrps = PrefixIndex {..}
  where
    ipv4 = mergeSortedByLength [ 
                (V4.firstIpAddress ip, [IndexEntry (ipv4PrefixLen i) vrp]) | 
                vrp@(Vrp _ (Ipv4P i@(Ipv4Prefix ip)) _) <- vrpList 
            ]

    ipv6 = mergeSortedByLength [ 
                (V6.firstIpAddress ip, [IndexEntry (ipv6PrefixLen i) vrp]) | 
                vrp@(Vrp _ (Ipv6P i@(Ipv6Prefix ip)) _) <- vrpList 
            ]            

    mergeSortedByLength :: Ord a => [(a, [IndexEntry c])] -> Map a [IndexEntry c]
    mergeSortedByLength = Map.map (List.sortOn (\(IndexEntry len _) -> len)) . Map.fromListWith (<>) 

    vrpList = mconcat $ map Set.toList $ allVrps vrps


validity :: IpPrefix -> ASN -> PrefixIndex -> ValidityResult
validity = \case
    Ipv4P p -> validityV4 p
    Ipv6P p -> validityV6 p


validityV4 i@(Ipv4Prefix v4prefix) asn PrefixIndex {..} = let 
    -- case closestLower of 

        
        
    in InvalidAsn
--   where
--     beginning = V4.firstIpAddress v4prefix    
--     end = V4.lastIpAddress v4prefix    
--     closestLower = Map.lookupLE beginning ipv4
--     rest = findTheRest closestLower

--     findTheRest prefixStart = []
--         -- walk to lower adresses until there's no overlap
--       where
--         findAllLower start = 
--             case Map.lookupLT start ipv4 of 
--                 Nothing            -> []
--                 -- Get the ones that still bigger than v4prefix and cover it
--                 Just (addr, vrps)  -> 
--                     case filter (\(IndexEntry length _) -> addr + length >= end) vrps of 
--                         [] -> []
--                         overlaps -> overlaps <> findAllLower addr

--         findHigher start = 
--             if start > v4prefix 
--                 then []
--                 else case Map.lookupGT prefixStart ipv4 of 
--                     Nothing -> []
--                     Just _  -> []




validityV6 _ _ _ = InvalidAsn
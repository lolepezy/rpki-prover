
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Resources.Validity where

import qualified Data.List                as List
import qualified Data.Vector              as V

import           GHC.Generics

import qualified HaskellWorks.Data.Network.Ip.Ipv4     as V4
import qualified HaskellWorks.Data.Network.Ip.Ipv6     as V6

import           RPKI.Domain
import           RPKI.Resources.Types


newtype ValidityByRoa = ValidityByRoa {
        vrp :: Vrp
    }
    deriving stock (Eq, Ord, Generic)     

data ValidityResult = InvalidAsn 
                    | InvalidLength 
                    | Valid [ValidityByRoa]    
    deriving stock (Eq, Ord, Generic)     

data AddressIndex a c = AddressIndex {
        ips     :: V.Vector a,
        content :: V.Vector c
    }    
    deriving stock (Eq, Ord, Generic)     

data PrefixIndex = PrefixIndex {
        ipv4 :: AddressIndex V4.IpAddress Vrp,
        ipv6 :: AddressIndex V6.IpAddress Vrp
    }
    deriving stock (Eq, Ord, Generic)     


makePrefixIndex :: Vrps -> PrefixIndex 
makePrefixIndex _ = PrefixIndex (AddressIndex mempty mempty) (AddressIndex mempty mempty)

validity :: IpPrefix -> PrefixIndex -> ValidityResult
validity _ _ = InvalidAsn
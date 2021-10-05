{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.SLURM.Filters where


import Control.Lens ( (^.) )
import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields

import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Data.These

import           RPKI.Domain
import           RPKI.Resources.Types
import           RPKI.Resources.Resources (prefixLen)
import           RPKI.SLURM.Types



slurmVrpName :: TaName
slurmVrpName = TaName "slurm"

-- TODO BgpSec stuff is not supported at the moment.

applySlurm :: Slurm -> Vrps -> Vrps
applySlurm slurm (Vrps vrps) = 
    Vrps $ filteredVrps <> Map.singleton slurmVrpName assertedVrps
  where     
    filteredVrps = Map.map (Set.filter filterFunc) vrps

    assertedVrps = Set.fromList 
        $ map toVrp 
        $ slurm ^. #locallyAddedAssertions . #prefixAssertions
      where
        toVrp PrefixAssertion {..} = 
            Vrp asn prefix (fromMaybe (prefixLen prefix) maxPrefixLength)

    filterFunc (Vrp vAsn vPrefix _) = 
            not 
            $ any matchesFilter 
            $ slurm ^. #validationOutputFilters . #prefixFilters
      where
        matchesFilter z = case z ^. #asnAndPrefix of
            This asn         -> asn == vAsn
            That prefix      -> vPrefix `isInsideOf` prefix
            These asn prefix -> asn == vAsn && vPrefix `isInsideOf` prefix
        
        isInsideOf (Ipv4P pS) (Ipv4P pB) = pB `contains` pS
        isInsideOf (Ipv6P pS) (Ipv6P pB) = pB `contains` pS
        isInsideOf _ _                   = False
    




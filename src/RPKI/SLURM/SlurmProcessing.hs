{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.SLURM.SlurmProcessing where


import Control.Lens ( (^.) )
import Control.Monad

import qualified Data.ByteString.Lazy as LBS

import           Data.Bifunctor
import qualified Data.Text as Text

import Data.Maybe (fromMaybe)

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import           Data.Aeson as Json

import           Data.These

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Resources.Types
import           RPKI.Resources.Resources (prefixLen)

import           RPKI.SLURM.Types

import RPKI.Util (fmtEx)


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
    

readSlurmF :: [String] -> ValidatorT IO Slurm
readSlurmF slurmFiles = do 
    slurms :: [Slurm] <- 
        forM slurmFiles $ \f -> do
            s <- fromTry (SlurmE . SlurmFileError . fmtEx) $ LBS.readFile f
            vHoist $ fromEither 
                   $ first (SlurmE . SlurmParseError . Text.pack) 
                   $ Json.eitherDecode s            
        
    vHoist $ validateNoOverlaps slurms
    pure $! mconcat slurms        


{- Validate overlappings as described in
    https://datatracker.ietf.org/doc/html/rfc8416#section-4.2
-}
validateNoOverlaps :: [Slurm] -> PureValidatorT ()
validateNoOverlaps slurms = do 
    -- TODO 
    pure ()


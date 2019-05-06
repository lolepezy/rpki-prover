{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module RPKI.Core where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
  
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString as B

import Data.Data (Typeable)

import qualified StmContainers.Map as SM

import Control.Concurrent.STM.TQueue

import RPKI.Domain

{-

Handling incoming changes:
    * An MFT added
        - replace current MFT
        - move objects that are not in the new MFT to the list of orphans
        - move objects from orphan list to the new MFT

    * A CRL added (or replaced, doesn't matter) 
        - remove revoked stuff from the tree
        - "revoked" means it will be removed from the tree
    
    * A CER added
        - check if it's explictly revoked
        - update the MFT entry if it exists
        - if it doesn't, move CER to the orphan list
        - check the resources set and calculate the effective resources set 
          (or reject the CER as invalid/overclaming for strict validation)
        -  
            
    - Store children not only mentioned on the MFT, but all, using SKI-AKI relation.
    - Store the latest resource certificate node for every resource set.
    - Either allow intersections or choose which resource set to pick up based 
      on serial number and validity period.
    - There are two types of orphans: those without AKI-SKI parent and 
      those not mentioned on an MFT.
    - Keep track of the "earliest to expire" object.
    - 

-}

data Change = Add | Update | Delete 

newtype ChildCAs = ChildCAs (SM.Map Hash ValidTree)

data CA = CA {
       cer        :: CerObject
    ,  mft        :: MftObject
    , ipResources :: IpResources
    , crl         :: CrlObject
    , children    :: [ValidTree]
  } deriving (Show, Eq, Typeable)

data ValidTree = CaNode  RpkiMeta CA  Blob
               | ROANode RpkiMeta Ref Blob
               deriving (Show, Eq, Typeable)               

data TATrees = TATrees {
    vts :: SM.Map TaName ValidTree
}

consumeChanges :: TQueue Change -> ValidTree -> IO ()
consumeChanges q vt = forever $ atomically $ readTQueue q >>= apply vt
        
apply :: ValidTree -> Change -> STM ()
apply _ _ = pure ()

        

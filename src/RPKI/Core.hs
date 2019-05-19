{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module RPKI.Core where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
  
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.DList as DL
import Data.Maybe
import Data.Tuple

import Data.Kind (Type)
import Data.Data (Typeable)

import Control.Monad.Trans.Writer.Strict

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

data Change = Add RefResolved | Update RefHash RefResolved | Delete RefHash

data ResolveResult = NoParent | NoMFT | Attached

newtype ChildCAs = ChildCAs (SM.Map Hash Valids)

data CA = CA {
      cer         :: CerObject
    , mft         :: MftObject ARef
    , crl         :: CrlObject    
    , ipResources :: IpResources
    , children    :: [ValidTree]
  } deriving (Show, Eq, Typeable)

data ValidTree = CaNode CA Blob
               | ROANode RoaObject Blob
               deriving (Show, Eq, Typeable)               

data Valids = Valids {
    treeRoot :: ValidTree
  , bySkiMap :: M.Map KI ValidTree
  , byAkiMap :: M.Map KI ValidTree  
  , byHash   :: M.Map Hash ValidTree 
} deriving (Show, Eq, Typeable)

data TATrees = TATrees {
    vts :: SM.Map TaName Valids
}

bySKI :: Valids -> KI -> Maybe ValidTree
bySKI valids ki = M.lookup ki (bySkiMap valids)

data Validated a = Validated {
  result :: a,
  problems :: [VError]
}

consumeChanges :: TQueue Change -> TVar Valids -> IO ()
consumeChanges q tValids = forever $ atomically $ do 
  change <- readTQueue q
  valids <- readTVar tValids
  let (validated, problems) = apply valids change
  writeTVar tValids validated
  -- TODO do something with 'problems'

        
apply :: Valids -> Change -> (Valids, DL.DList VError)
apply valids = runWriter . go
  where 
    go :: Change -> Writer (DL.DList VError) Valids
    go = \case 
      Add (RefMft mft)  -> addMft valids mft    
      Add (RefCrl crl)  -> addCrl valids crl
      Add (RefCer cer)  -> addCer valids cer      
      Add (RefRoa roa)  -> addRoa valids roa
      Add (RefGbr gbr)  -> addGbr valids gbr
      Update (RefHash hash) (RefMft mft) -> pure valids
      Update (RefHash hash) (RefCer cer) -> pure valids
      Update (RefHash hash) (RefCrl crl) -> pure valids  
      Update (RefHash hash) (RefRoa roa) -> pure valids
      Delete (RefHash hash) -> pure valids      


addMft :: Valids -> (MftObject ARef) -> Writer (DL.DList VError) Valids
addMft valids (MftObject meta newMft) =
  case aki meta of
    Nothing   -> do
      tell (DL.singleton NoAKIinManifest)
      pure valids -- TODO complain!
    Just (AKI ki) -> 
      case bySKI valids ki of
        Nothing     -> pure valids -- TODO complain!
        Just (CaNode (ca@ CA { cer = c, mft = MftObject _ oldMft }) _) -> do
          let newRefs = map snd $ mftEntries newMft
              oldRefs = map snd $ mftEntries oldMft
              -- TODO Optimize if needed for long MFTs
              -- it would require creating a Map or Set and adding Ord or Hashable 
              -- instances to certificates
              deletedRefs = L.filter (`L.notElem` newRefs) oldRefs
              addedRefs = L.filter (`L.notElem` oldRefs) newRefs

          newValids <- addChildren addedRefs =<< deleteChildren deletedRefs valids 
          validate newValids ca addedRefs

        -- TODO oops, that shouldn't happen, it's weird repository    
        Just (ROANode _ _ ) -> pure valids 
  where
    deleteChildren refs (Valids{..}) = x
      where 
        x = pure valids

    addChildren refs valids = pure valids

addCrl :: Valids -> CrlObject -> Writer (DL.DList VError) Valids
addCrl valids (CrlObject meta crl) =
  case aki meta of
    Nothing   -> pure valids -- complain!
    Just (AKI ki) -> 
      case bySKI valids ki of
        Nothing     -> pure valids -- complain!
        Just parent -> pure valids

addRoa valids roa = pure valids
addCer valids roa = pure valids
addGbr valids roa = pure valids


class Monad (StoreM s) => Store s where 
  type StoreM s :: Type -> Type
        
resolveRefs :: Store s => s -> [Change] -> (StoreM s) [Change]
resolveRefs store changes = pure $ changes

-- Validate subtree starting from the nodes, identified by 'refs'
validate :: Valids -> CA -> [ARef] -> Writer (DL.DList VError) Valids
validate valids parentCA refs = pure valids



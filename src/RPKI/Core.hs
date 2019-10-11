{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module RPKI.Core where

import qualified Data.DList                        as DL
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe                        (maybe)
import qualified Data.Text                         as T

import           Data.Data                         (Typeable)
import           Data.Kind                         (Type)

import           Control.Monad.Trans.Writer.Strict

import qualified StmContainers.Map                 as SM

import           Control.Concurrent.STM.TQueue

import           RPKI.Domain

{-

THIS IS FROZEN AND WILL BE REMOVED

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

   * Validation phase is still needed
     - start from a certain CA child
     - validate crypto
     - check resource set is under parent's
     - check resource set is not overlapping with other children
     - check the manifest entries, resolve the references
     - filter certifiicates through CRLs
     - put valid ROAs into the resouces map

    * Keep two versions: data version and "last validated version"
      to avoid validating the same thing twice

    *

-}



-- data Change = Add RefResolved | Update RefHash RefResolved | Delete RefHash

-- data ResolveResult = NoParent | NoMFT | Attached

-- newtype New o = New o deriving (Show, Eq, Typeable, Functor)
-- newtype Validated o = Validated o deriving (Show, Eq, Typeable, Functor)

-- data InState o  = SN (New o) | SV (Validated o)
--   deriving (Show, Eq, Typeable, Functor)

-- data SRef r = HashRef Hash | DirectRef r
--   deriving (Show, Eq, Ord, Typeable)

-- -- convenience pattern for objects
-- pattern InSomeState s <- ((\case
--                     SN (New s) -> Just s
--                     SV (Validated s) -> Just s
--                     _ -> Nothing
--                   ) -> Just s)

-- data MftEntry = MftEntry {
--     name  :: !T.Text
--   , child :: !(SRef StateTree)
-- } deriving (Show, Eq, Typeable)

-- data CA = CA {
--     cer         :: InState CerObject
--   , mft         :: InState MftObject
--   , crl         :: InState CrlObject
--   , ipResources :: IpResources
--   , children    :: M.Map Hash MftEntry
-- } deriving (Show, Eq, Typeable)

-- data StateTree = CaNode CA
--                | ROANode (InState RoaObject)
--                deriving (Show, Eq, Typeable)

-- data State = State {
--     treeRoot :: StateTree
--   , bySkiMap :: M.Map KI StateTree
--   , byAkiMap :: M.Map KI StateTree
--   , byHash   :: M.Map Hash StateTree
--   , orphans  :: [StateTree]
-- } deriving (Show, Eq, Typeable)

-- data TATrees = TATrees {
--     vts :: SM.Map TaName State
-- }

-- bySKI :: State -> KI -> Maybe StateTree
-- bySKI state ki = M.lookup ki (bySkiMap state)


-- consumeChanges :: TQueue Change -> TVar (State) -> IO ()
-- consumeChanges q tValids = forever $ atomically $ do
--   change <- readTQueue q
--   state <- readTVar tValids
--   let (validated, problems) = apply state change
--   writeTVar tValids validated
--   -- TODO do something with 'problems'


-- apply :: State -> Change -> (State, DL.DList VError)
-- apply state = runWriter . go
--   where
--     go :: Change -> Writer (DL.DList VError) State
--     go = \case
--       Add (RefMft mft)  -> addMft state mft
--       Add (RefCrl crl)  -> addCrl state crl
--       Add (RefCer cer)  -> addCer state cer
--       Add (RefRoa roa)  -> addRoa state roa
--       Add (RefGbr gbr)  -> addGbr state gbr
--       Update (RefHash hash) (RefMft mft) -> pure state
--       Update (RefHash hash) (RefCer cer) -> pure state
--       Update (RefHash hash) (RefCrl crl) -> pure state
--       Update (RefHash hash) (RefRoa roa) -> pure state
--       Delete (RefHash hash) -> pure state


-- -- Add a CA certificate to the hierarchy
-- addCer state newCert@ (CerObject meta cert) = pure state
--   -- case aki meta of
--   --   Nothing -> complain state NoAKI
--   --   Just (AKI ki) ->
--   --     case bySKI state ki of
--   --       Nothing -> pure $ state
--   --       Just (CaNode (ca@CA {..})) ->
--   --         pure $ withUpdatedCA state $ ca {
--   --           M.insert (hash meta) (DirectRef (CaNode )) children
--   --         }


-- -- Add new manifest to the hierarchy
-- addMft :: State -> MftObject -> Writer (DL.DList VError) State
-- addMft state newMftObject@ (MftObject meta newMft) =
--   case aki meta of
--     Nothing -> complain state NoAKIinManifest
--     Just (AKI ki) ->
--       case bySKI state ki of
--         Nothing     -> pure state -- TODO move it to orphanage
--         Just (CaNode (ca@CA {mft = InSomeState (MftObject _ oldMft), ..})) ->
--           pure $ withUpdatedCA state $ ca {
--             children = M.fromList $ map makeNewChild $ mftEntries newMft,
--             mft = SN (New newMftObject)
--           }
--           where
--             makeNewChild (name, hash) = (hash, child)
--               where
--                 child = case M.lookup hash children of
--                   Nothing -> MftEntry name (HashRef hash)
--                   Just c  -> c
--         Just (ROANode _) -> complain state ROACannotBeAParent


-- addCrl :: State -> CrlObject -> Writer (DL.DList VError) State
-- addCrl state (CrlObject meta crl) =
--   case aki meta of
--     Nothing   -> pure state -- complain!
--     Just (AKI ki) ->
--       case bySKI state ki of
--         Nothing     -> pure state -- complain!
--         Just parent -> pure state

-- addRoa state roa = pure $ state
-- addGbr state gbr = pure $ state


-- class Monad (StoreM s) => Store s where
--   type StoreM s :: Type -> Type

-- resolveRefs :: Store s => s -> [Change] -> (StoreM s) [Change]
-- resolveRefs store changes = pure $ changes

-- -- Validate subtree starting from the nodes, identified by 'refs'
-- validate :: State -> CA -> [ARef] -> Writer (DL.DList VError) State
-- validate state parentCA refs = pure state


-- -- | Replace the CA in the hierarchy, given that the hash of the certficate is not updated
-- withUpdatedCA :: State -> CA -> State
-- withUpdatedCA state ca@(CA { cer = InSomeState (CerObject meta _) }) =
--   case (aki meta) of
--     Nothing -> state { treeRoot = CaNode ca }
--     Just (AKI ki) ->
--       case bySKI state ki of
--         Just (CaNode parentCA) -> withUpdatedCA state $ parentCA {
--             children = M.update replaceMftEntry (hash meta) (children parentCA)
--           }
--           where
--             replaceMftEntry (MftEntry name ref) = Just $ MftEntry name $ DirectRef $ CaNode ca
--         Nothing -> state


-- ---------------
-- -- Utilities
-- ---------------

-- getHash :: ARef -> Hash
-- getHash (RH (RefHash h)) = h
-- getHash (RR r)           = hash (getMeta r)


-- complain :: a -> VError -> Writer (DL.DList VError) a
-- complain a verror = tell (DL.singleton verror) >> pure a

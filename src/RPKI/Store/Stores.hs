{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DeriveAnyClass       #-}

module RPKI.Store.Stores where

import Codec.Serialise

import Data.Data (Typeable)
import GHC.Generics

import RPKI.Domain
import RPKI.TAL
import RPKI.Store.Base.Map (SMap(..))
import RPKI.Store.Base.MultiMap (SMultiMap(..))
import qualified RPKI.Store.Base.Map as SM
import qualified RPKI.Store.Base.MultiMap as MM
import RPKI.Store.Base.Storage
import RPKI.Store.Base.Storable

data RpkiObjectStore s = RpkiObjectStore {
  objects :: SMap s Hash SValue,
  byAKI :: SMultiMap s AKI Hash
}

instance Storage s => WithStorage s (RpkiObjectStore s) where
  storage = storage . objects


getByHash :: Storage s => Tx s m -> RpkiObjectStore s -> Hash -> IO (Maybe RpkiObject)
getByHash tx store h = (fromSValue <$>) <$> SM.get tx (objects store) h

putObject :: Storage s => Tx s 'RW -> RpkiObjectStore s -> Hash -> StorableObject RpkiObject -> IO ()
putObject tx store h (StorableObject ro sv) = do
  SM.put tx (objects store) h sv  
  -- case getAKI ro of 
  --   Nothing   -> pure ()
  --   Just aki' -> MM.put tx (byAKI store) aki' h


deleteObject :: Storage s => Tx s 'RW -> RpkiObjectStore s -> Hash -> IO ()
deleteObject tx store h = SM.delete tx (objects store) h


findByAKI :: Storage s => Tx s m -> RpkiObjectStore s -> AKI -> IO [RpkiObject]
findByAKI tx (RpkiObjectStore s _) aki = pure []

findMftsByAKI :: Storage s => Tx s m -> RpkiObjectStore s -> AKI -> IO [WithMeta MftObject]
findMftsByAKI tx (RpkiObjectStore s _) aki = pure []


newtype TAStore s = TAStore (SMap s TaName StoredTA)

instance Storage s => WithStorage s (TAStore s) where
  storage (TAStore s) = storage s

data StoredTA = StoredTA {
  tal        :: TAL,
  taCert     :: CerObject,
  taCertMeta :: RpkiMeta
} deriving (Show, Eq, Generic, Serialise)

putTA :: Storage s => Tx s 'RW -> TAStore s -> StoredTA -> IO ()
putTA tx (TAStore s) ta = SM.put tx s (getTaName $ tal ta) ta

getTA :: Storage s => Tx s m -> TAStore s -> TaName -> IO (Maybe StoredTA)
getTA tx (TAStore s) name = SM.get tx s name


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.InMemory where

import qualified Data.Map as Map
import Data.Map (Map(..))

import Control.Concurrent.STM

import RPKI.Domain
import RPKI.Store.Storage


newtype InMemoryStore = InMemoryStore (TVar (Map Hash RpkiObject))

instance Storage InMemoryStore where
    type TxM mode InMemoryStore = STM

    storeObj (InMemoryStore entries) r = 
        modifyTVar' entries (Map.insert (getHash r) r)

    getByAKI (InMemoryStore entries) a = do
        e <- readTVar entries
        pure [ r | (_, r) <- Map.toList e, getAKI r == Just a]        

    getByHash (InMemoryStore entries) h = do
        e <- readTVar entries
        pure $ Map.lookup h e

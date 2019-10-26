{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.InMemory where

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Concurrent.STM

import RPKI.Domain
import RPKI.Store.Storage


newtype InMemoryStore = InMemoryStore (TVar (Map Hash StorableRO))

instance Storage InMemoryStore where

    readOnlyTx _ = id
    readWriteTx _ = id

    storeObj (InMemoryStore entries) (h, storable) = 
        atomically $ modifyTVar' entries (Map.insert h storable)

    getByAKI (InMemoryStore entries) a = atomically $ do
        e <- readTVar entries
        pure $ filter ((== Just a) . getAKI) [ fromStorable s | (_, s) <- Map.toList e ]

    getByHash (InMemoryStore entries) h = atomically $ do
        e <- readTVar entries
        pure $ fromStorable <$> Map.lookup h e

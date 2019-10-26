{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Store.LMDB where

import qualified Data.ByteString.Lazy as BL

import Database.LMDB.Simple
import qualified Codec.Serialise as Serialise

import RPKI.Domain
import RPKI.Store.Storage

data LMDBStore = LMDBStore { 
    env :: Environment ReadWrite,
    db :: Database Hash BL.ByteString 
}

-- instance Storage LMDBStore where
--     type TxM mode LMDBStore = Transaction mode      

--     readOnlyTx LMDBStore {..} = readOnlyTransaction env
--     readWriteTx LMDBStore {..} = readWriteTransaction env

--     storeObj LMDBStore {..} r = 
--         put db (getHash r) (Just $ Serialise.serialise r)

--     -- TODO Implement
--     getByAKI LMDBStore {..} a = pure []

--     getByHash LMDBStore {..} h = do
--         r <- get db h
--         pure $ Serialise.deserialise <$> r


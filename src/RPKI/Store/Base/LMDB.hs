{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Store.Base.LMDB where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified Codec.Serialise as Serialise

import Data.Coerce (coerce)

import RPKI.Domain
import RPKI.Store.Base.Storage

import Lmdb.Connection
import qualified Lmdb.Map as LmdbMap
import qualified Lmdb.Types as Lmdb
import Lmdb.Codec

data LmdbStore = LmdbStore { 
    env :: Lmdb.Environment 'Lmdb.ReadWrite,
    db :: Lmdb.Database Hash B.ByteString 
}

type family LmdbTxMode (m :: TxMode) :: Lmdb.Mode where
    LmdbTxMode 'RO = 'Lmdb.ReadOnly
    LmdbTxMode 'RW = 'Lmdb.ReadWrite

roTx :: Lmdb.Transaction (m :: Lmdb.Mode) -> Lmdb.Transaction 'Lmdb.ReadOnly
roTx = coerce

instance Storage LmdbStore where
    data Tx LmdbStore (m :: TxMode) = LmdbTx (Lmdb.Transaction (LmdbTxMode m))

    readOnlyTx LmdbStore {..} f = withTransaction env (f . LmdbTx . readonly)
    readWriteTx LmdbStore {..} f = withTransaction env (f . LmdbTx)

    storeObj (LmdbTx tx) LmdbStore {..} (h, StorableRO bs) = 
        LmdbMap.insert' tx db h bs

    -- TODO Take uri into account as well
    delete (LmdbTx tx) LmdbStore {..} (h, _) = 
        LmdbMap.delete' tx db h

    -- TODO Implement
    getByAKI (LmdbTx tx) LmdbStore {..} a = pure []

    getByHash (LmdbTx tx) LmdbStore {..} h = do
        x <- LmdbMap.lookup' (roTx tx) db h        
        pure $ Serialise.deserialise . BL.fromStrict <$> x


{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module RPKI.Store.Storage where

import qualified Data.ByteString.Lazy as BL

import Control.DeepSeq

import qualified Codec.Serialise as Serialise

import GHC.Generics

import RPKI.Domain
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Reader

-- newtype TxReadOnly = TxReadOnly ReadOnly
-- newtype TxReadWrite = TxReadWrite ReadWrite

-- class TxMode mode

-- instance TxMode TxReadOnly
-- instance TxMode TxReadWrite

-- newtype StorageTx mode a = StorageTx { unApp :: Transaction mode a }
--     deriving (Functor, Applicative, Monad, MonadIO)

-- deriving via ReaderT MDB_txn IO instance MonadUnliftIO (Transaction mode)

newtype StorableRO = StorableRO BL.ByteString
    deriving (Show, Eq, Ord, Generic, NFData)

class Storage s where        
    getByHash :: s -> Hash -> IO (Maybe RpkiObject)
    getByAKI :: s -> AKI -> IO [RpkiObject]
    storeObj :: s -> (Hash, StorableRO) -> IO ()
    delete :: s -> (Hash, URI) -> IO ()
    readOnlyTx :: s -> IO a -> IO a
    readWriteTx :: s -> IO a -> IO a

toStorable :: RpkiObject -> StorableRO
toStorable = StorableRO . Serialise.serialise

fromStorable :: StorableRO -> RpkiObject
fromStorable (StorableRO b) = Serialise.deserialise b
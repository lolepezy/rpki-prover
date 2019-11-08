{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingVia #-}

module RPKI.Store.Storage where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.DeepSeq

import qualified Codec.Serialise as Serialise

import GHC.Generics
import RPKI.Domain

newtype StorableRO = StorableRO B.ByteString
    deriving (Show, Eq, Ord, Generic, NFData)

data TxMode = RO | RW

class Storage s where        
    data Tx s (m :: TxMode)
    getByHash :: Tx s m -> s -> Hash -> IO (Maybe RpkiObject)
    getByAKI :: Tx s m -> s -> AKI -> IO [RpkiObject]
    storeObj :: Tx s 'RW -> s -> (Hash, StorableRO) -> IO ()
    delete :: Tx s 'RW -> s -> (Hash, URI) -> IO ()
    readOnlyTx :: s -> (Tx s 'RO -> IO a) -> IO a
    readWriteTx :: s -> (Tx s 'RW -> IO a) -> IO a

toStorable :: RpkiObject -> StorableRO
toStorable = StorableRO . BL.toStrict . Serialise.serialise

fromStorable :: StorableRO -> RpkiObject
fromStorable (StorableRO b) = Serialise.deserialise $ BL.fromStrict b
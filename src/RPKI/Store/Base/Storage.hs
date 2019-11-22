{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingVia #-}

module RPKI.Store.Base.Storage where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.DeepSeq
import Codec.Serialise
import GHC.Generics


newtype Storable = Storable B.ByteString
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

newtype SValue = SValue Storable
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

newtype SKey = SKey Storable
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

data TxMode = RO | RW

class Storage s where    
    data Tx s (m :: TxMode)
    roTx :: s -> (Tx s 'RO -> IO a) -> IO a
    rwTx :: s -> (Tx s 'RW -> IO a) -> IO a
    get :: Tx s m -> s -> SKey -> IO (Maybe SValue)    
    put :: Tx s 'RW -> s -> SKey -> SValue -> IO ()
    delete :: Tx s 'RW -> s -> SKey -> IO ()

class Storage s => WithStorage s ws where
    storage :: ws -> s

toStorable :: Serialise v => v -> Storable
toStorable = Storable . BL.toStrict . serialise

storableValue :: Serialise v => v -> SValue
storableValue = SValue . toStorable

storableKey :: Serialise v => v -> SKey
storableKey = SKey . toStorable

fromStorable :: Serialise t => Storable -> t
fromStorable (Storable b) = deserialise $ BL.fromStrict b

fromSValue :: Serialise t => SValue -> t
fromSValue (SValue b) = fromStorable b

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Store.Base.Storable where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Control.DeepSeq
import Codec.Serialise
import Data.Data (Typeable)
import GHC.Generics

newtype Storable = Storable B.ByteString
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

newtype SValue = SValue Storable
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

newtype SKey = SKey Storable
    deriving (Show, Eq, Ord, Generic, NFData, Serialise)

data StorableUnit a e = SObject !(StorableObject a) | SError !e

data StorableObject a = StorableObject !a !SValue
    deriving (Show, Eq, Typeable, Generic)

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
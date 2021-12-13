{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}


module RPKI.Store.Base.Storable where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Control.DeepSeq
import Codec.Serialise
import GHC.Generics

import Data.Monoid.Generic

import RPKI.Config

newtype Storable = Storable { unStorable :: BS.ByteString }    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, Serialise)

newtype SValue = SValue { unSValue :: Storable }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, Serialise)

newtype SKey = SKey { unSKey :: Storable }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (NFData, Serialise)

-- Strictness here is important
data StorableUnit a e = SObject {-# UNPACK #-} (StorableObject a) | SError e

data StorableObject a = StorableObject { object :: a, storable :: SValue }
    deriving (Show, Eq, Generic)

toStorableObject :: Serialise a => a -> StorableObject a
toStorableObject a = StorableObject a (force (storableValue a))

toStorable :: Serialise v => v -> Storable
toStorable = Storable . LBS.toStrict . serialise

storableValue :: Serialise v => v -> SValue
storableValue = SValue . toStorable

storableKey :: Serialise v => v -> SKey
storableKey = SKey . toStorable

-- TODO Consider having type-safe storage errors
-- 
-- fromStorable :: Serialise t => Storable -> Either StorageError t
-- fromStorable (Storable b) = first (DeserialisationError . fmtEx) $ 
--     deserialiseOrFail $ LBS.fromStrict b

-- fromSValue :: Serialise t => SValue -> Either StorageError t
-- fromSValue (SValue b) = fromStorable b

fromStorable :: Serialise t => Storable -> t
fromStorable (Storable b) = deserialise $ LBS.fromStrict b

fromSValue :: Serialise t => SValue -> t
fromSValue (SValue b) = fromStorable b


data SStats = SStats {
        statSize       :: Size,
        statKeyBytes   :: Size,        
        statValueBytes :: Size,
        statMaxKVBytes :: Size
    } 
    deriving stock (Show, Eq, Generic)    
    deriving Monoid    via GenericMonoid SStats

instance Semigroup SStats where
    ss1 <> ss2 = 
        SStats { 
            statSize = statSize ss1 + statSize ss2,
            statKeyBytes = statKeyBytes ss1 + statKeyBytes ss2,
            statValueBytes = statValueBytes ss1 + statValueBytes ss2,
            statMaxKVBytes = statMaxKVBytes ss1 `max` statMaxKVBytes ss2
        }


incrementStats :: SStats -> SKey -> SValue -> SStats
incrementStats stat (SKey (Storable k)) (SValue (Storable v)) = 
    SStats { 
        statSize = statSize stat + 1, 
        statKeyBytes = statKeyBytes stat + fromIntegral (BS.length k),
        statValueBytes = statValueBytes stat + fromIntegral (BS.length v),
        statMaxKVBytes = statMaxKVBytes stat `max` fromIntegral (BS.length v)
    }  
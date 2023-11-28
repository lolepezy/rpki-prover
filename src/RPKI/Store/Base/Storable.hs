{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Store.Base.Storable where

import qualified Data.ByteString as BS

import Control.DeepSeq
import Codec.Compression.LZ4

import GHC.Generics

import Data.Maybe (fromMaybe)
import Data.Monoid.Generic

import RPKI.Domain
import RPKI.Store.Base.Serialisation

newtype Storable = Storable { unStorable :: BS.ByteString }    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass NFData

newtype SValue = SValue { unSValue :: Storable }
    deriving stock (Eq, Ord, Show)

newtype SKey = SKey { unSKey :: Storable }
    deriving stock (Eq, Ord, Show)    

data StorableUnit a e = 
    SObject {-# UNPACK #-} (StorableObject a) 
  | SError e

data StorableObject a = StorableObject {
        object   :: a, 
        storable :: Storable 
    }
    deriving stock (Show, Eq, Generic)


newtype Raw a = Raw { unRaw :: Storable }
    deriving stock (Show, Eq, Generic)

-- data WannaStore a = OriginalWS a 
--                   | StorableWS Storable
--     deriving stock (Show, Eq, Generic)                  

toStorableObject :: AsStorable a => a -> StorableObject a
toStorableObject a = StorableObject a (force (toStorable a))

storableValue :: AsStorable v => v -> SValue
storableValue = SValue . toStorable

storableKey :: AsStorable v => v -> SKey
storableKey = SKey . toStorable

newtype Compressed a = Compressed { unCompressed :: a }
    deriving stock (Show, Eq, Generic)

class AsStorable a where
    toStorable :: a -> Storable
    fromStorable :: Storable -> a

instance {-# OVERLAPPING #-} AsStorable Storable where
    toStorable = id
    fromStorable = id

instance {-# OVERLAPPING #-} AsStorable (Raw a) where
    toStorable   = unRaw
    fromStorable = Raw

instance {-# OVERLAPPING #-} TheBinary a => AsStorable a where
    toStorable = Storable . serialise_
    fromStorable (Storable a) = deserialise_ a

instance {-# OVERLAPPING #-} AsStorable a => AsStorable (StorableObject a) where
    toStorable StorableObject {..} = storable
    fromStorable b = let o = fromStorable b in StorableObject o b

instance {-# OVERLAPPING #-} AsStorable a => AsStorable (Compressed a) where
    toStorable (Compressed a) = 
        Storable $ fromMaybe mempty $ compress $ unStorable $ toStorable a
    fromStorable (Storable b) = 
        Compressed $ fromStorable $ Storable $ fromMaybe "broken binary" $ decompress b


restoreFromRaw :: AsStorable a => Raw a -> a
restoreFromRaw = fromStorable . unRaw

data SStats = SStats {
        statSize          :: Size,
        statKeyBytes      :: Size,        
        statValueBytes    :: Size,
        statMaxKeyBytes   :: Size,
        statMaxValueBytes :: Size
    } 
    deriving stock (Show, Eq, Generic)    
    deriving Monoid via GenericMonoid SStats

instance Semigroup SStats where
    ss1 <> ss2 = 
        SStats { 
            statSize = statSize ss1 + statSize ss2,
            statKeyBytes = statKeyBytes ss1 + statKeyBytes ss2,
            statValueBytes = statValueBytes ss1 + statValueBytes ss2,
            statMaxKeyBytes = statMaxKeyBytes ss1 `max` statMaxKeyBytes ss2,
            statMaxValueBytes = statMaxValueBytes ss1 `max` statMaxValueBytes ss2
        }

incrementStats :: SStats -> SKey -> SValue -> SStats
incrementStats stat (SKey (Storable k)) (SValue (Storable v)) = 
    SStats { 
        statSize = statSize stat + 1, 
        statKeyBytes = statKeyBytes stat + fromIntegral (BS.length k),
        statValueBytes = statValueBytes stat + fromIntegral (BS.length v),
        statMaxKeyBytes = statMaxKeyBytes stat `max` fromIntegral (BS.length k),
        statMaxValueBytes = statMaxValueBytes stat `max` fromIntegral (BS.length v)
    }  
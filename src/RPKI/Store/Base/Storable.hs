{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Store.Base.Storable where

import qualified Data.ByteString as BS
import           Data.Map.Strict  (Map)

import Control.DeepSeq
import Codec.Compression.LZ4

import GHC.Generics

import Data.Maybe (fromMaybe)
import Data.Monoid.Generic

import RPKI.Domain
import RPKI.AppTypes
import RPKI.Store.Base.Serialisation

newtype Storable = Storable { unStorable :: BS.ByteString }    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass NFData

data StorableObject a = StorableObject {
        object   :: a, 
        storable :: Storable 
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

newtype Verbatim a = Verbatim { unVerbatim :: Storable }
    deriving stock (Show, Eq, Generic)


toStorableObject :: AsStorable a => a -> StorableObject a
toStorableObject a = StorableObject a (toStorable a)


newtype Compressed a = Compressed { unCompressed :: a }
    deriving stock (Show, Eq, Generic)

class AsStorable a where
    toStorable :: a -> Storable
    fromStorable :: Storable -> a

instance {-# OVERLAPPING #-} AsStorable Storable where
    toStorable = id
    fromStorable = id

instance {-# OVERLAPPING #-} AsStorable (Verbatim a) where
    toStorable   = unVerbatim
    fromStorable = Verbatim

instance {-# OVERLAPPING #-} TheBinary a => AsStorable a where
    toStorable = Storable . serialise_
    fromStorable (Storable a) = deserialise_ a

instance {-# OVERLAPPING #-} AsStorable a => AsStorable (StorableObject a) where
    toStorable StorableObject {..} = storable
    fromStorable b = StorableObject (fromStorable b) b

instance {-# OVERLAPPING #-} AsStorable a => AsStorable (Compressed a) where
    toStorable (Compressed a) = 
        Storable $ fromMaybe mempty $ compress $ unStorable $ toStorable a
    fromStorable (Storable b) = 
        Compressed $ fromStorable $ Storable $ fromMaybe "broken binary" $ decompress b


serialiseField :: AsStorable a => a -> BS.ByteString
serialiseField = unStorable . toStorable

deserialiseField :: AsStorable a => BS.ByteString -> a
deserialiseField = fromStorable . Storable

serialiseCompressed :: AsStorable a => a -> BS.ByteString
serialiseCompressed = fromMaybe BS.empty . compress . unStorable . toStorable

deserialiseCompressed :: AsStorable a => BS.ByteString -> a
deserialiseCompressed = fromStorable . Storable . fromMaybe "broken binary" . decompress

 
data ObjectStats = ObjectStats {
        totalObjects :: Size,
        totalSize    :: Size,
        countPerType    :: Map RpkiObjectType Size,
        minSizePerType   :: Map RpkiObjectType Size,
        maxSizePerType   :: Map RpkiObjectType Size,
        totalSizePerType :: Map RpkiObjectType Size,
        avgSizePerType   :: Map RpkiObjectType Size
    }
    deriving stock (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup ObjectStats
    deriving Monoid    via GenericMonoid ObjectStats

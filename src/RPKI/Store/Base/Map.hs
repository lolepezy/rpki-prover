{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module RPKI.Store.Base.Map where

import           Data.Store

import           GHC.TypeLits
import           Data.Typeable

import           Data.Maybe               (isJust)
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage  as S


data SMap (name :: Symbol) s k v where
    SMap :: Storage s => s -> SMapImpl s name -> SMap name s k v

instance Storage s => WithStorage s (SMap name s k v) where
    storage (SMap s _) = s

deriving instance (Typeable k, Typeable v) => Typeable (SMap name s k v)

put :: (Store k, Store v) =>
        Tx s 'RW -> SMap name s k v -> k -> v -> IO ()
put tx (SMap _ s) k v = S.put tx s (storableKey k) (storableValue v)    

get :: (Store k, Store v) =>
        Tx s m -> SMap name s k v -> k -> IO (Maybe v)
get tx (SMap _ s) k = do
    msv <- S.get tx s (storableKey k)
    pure $! fromStorable . unSValue <$> msv

exists :: (Store k) =>
        Tx s m -> SMap name s k v -> k -> IO Bool
exists tx (SMap _ s) k = isJust <$> S.get tx s (storableKey k)    

delete :: (Store k, Store v) =>
            Tx s 'RW -> SMap name s k v -> k -> IO ()
delete tx (SMap _ s) k = S.delete tx s (storableKey k)

fold :: (Store k, Store v) =>
        Tx s m -> SMap name s k v -> (a -> k -> v -> IO a) -> a -> IO a
fold tx (SMap _ s) f = S.foldS tx s f'
  where
    f' z (SKey sk) (SValue sv) = f z (fromStorable sk) (fromStorable sv)

traverse :: (Store k, Store v) =>
            Tx s m -> SMap name s k v -> (k -> v -> IO ()) -> IO ()
traverse tx m f = fold tx m (\_ k v -> f k v) ()    

all :: (Store k, Store v) =>
        Tx s m -> SMap name s k v -> IO [(k, v)]
all tx (SMap _ s) = S.foldS tx s f []
  where
    f z (SKey sk) (SValue sv) = pure $! (fromStorable sk, fromStorable sv) : z

keys :: (Store k, Store v) =>
        Tx s m -> SMap name s k v -> IO [k]
keys tx (SMap _ s) = S.foldS tx s f []
  where
    f z (SKey sk) _ = pure $! fromStorable sk : z

stats :: (Store k, Store v) =>
        Tx s m -> SMap name s k v -> IO SStats
stats tx (SMap _ s) = S.foldS tx s f (SStats 0 0 0 0)
  where
    f stat skey svalue = pure $! incrementStats stat skey svalue

erase :: (Store k, Store v) =>
        Tx s 'RW -> SMap name s k v -> IO ()
erase tx (SMap _ s) = S.clear tx s

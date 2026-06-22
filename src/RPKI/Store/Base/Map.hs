{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}

module RPKI.Store.Base.Map where

import           GHC.TypeLits

import           Data.Maybe               (isJust)
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage  as S

data SMap (name :: Symbol) s k v where
    SMap :: Storage s => s -> SMapImpl s name -> SMap name s k v

instance Storage s => WithStorage s (SMap name s k v) where
    storage (SMap s _) = s

put :: (AsStorable k, AsStorable v) =>
        Tx s 'RW -> SMap name s k v -> k -> v -> IO ()
put tx (SMap _ s) k v = S.put tx s (storableKey k) (storableValue v)    

get :: (AsStorable k, AsStorable v) =>
        Tx s m -> SMap name s k v -> k -> IO (Maybe v)
get tx (SMap _ s) k = do
    msv <- S.get tx s (storableKey k)
    pure $! fromStorable . unSValue <$> msv

binarySize :: (AsStorable k, AsStorable v) =>
            Tx s m -> SMap name s k v -> k -> IO (Maybe Int)
binarySize tx (SMap _ s) k = do
    msv <- S.get tx s (storableKey k)
    pure $! storableSize . unSValue <$> msv

exists :: AsStorable k => Tx s m -> SMap name s k v -> k -> IO Bool
exists tx (SMap _ s) k = isJust <$> S.get tx s (storableKey k)    

delete :: AsStorable k => Tx s 'RW -> SMap name s k v -> k -> IO ()
delete tx (SMap _ s) k = S.delete tx s (storableKey k)

fold :: (AsStorable k, AsStorable v) =>
        Tx s m -> SMap name s k v -> (a -> k -> v -> IO a) -> a -> IO a
fold tx (SMap _ s) f = S.foldS tx s f'
  where
    f' z (SKey sk) (SValue sv) = f z (fromStorable sk) (fromStorable sv)

traverse :: (AsStorable k, AsStorable v) =>
            Tx s m -> SMap name s k v -> (k -> v -> IO ()) -> IO ()
traverse tx m f = fold tx m (\_ k v -> f k v) ()    

all :: (AsStorable k, AsStorable v) =>
        Tx s m -> SMap name s k v -> IO [(k, v)]
all tx (SMap _ s) = S.foldS tx s f []
  where
    f z (SKey sk) (SValue sv) = pure $! (fromStorable sk, fromStorable sv) : z

keys :: AsStorable k => Tx s m -> SMap name s k v -> IO [k]
keys tx (SMap _ s) = S.foldS tx s f []
  where
    f z (SKey sk) _ = pure $! fromStorable sk : z

values :: AsStorable v => Tx s m -> SMap name s k v -> IO [v]
values tx (SMap _ s) = S.foldS tx s f []
  where
    f z _ (SValue sv) = pure $! fromStorable sv : z

last :: (AsStorable k, AsStorable v) =>
        Tx s m -> SMap name s k v -> IO (Maybe (k, v))
last tx (SMap _ s) = (f <$>) <$> S.last tx s
  where
    f (SKey sk, SValue sv) = (fromStorable sk, fromStorable sv)

allFrom :: (AsStorable k, AsStorable v) =>
        Tx s m -> SMap name s k v -> k -> IO [(k, v)]
allFrom tx (SMap _ s) k = S.foldSFrom tx s (storableKey k) f []
  where
    f z (SKey sk) (SValue sv) = pure $! (fromStorable sk, fromStorable sv) : z

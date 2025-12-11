{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE FlexibleInstances  #-}

module RPKI.Store.Base.SafeMap where

import qualified Data.ByteString as BS
import           Data.Tuple.Strict
import qualified Data.Hashable   as H
import           Data.Maybe

import           GHC.TypeLits
import           GHC.Generics

import RPKI.Store.Base.Map as M
import RPKI.Store.Base.Storage
import RPKI.Store.Base.Storable
import RPKI.Store.Base.Serialisation
import RPKI.Util (ifJustM)
import Data.Bifunctor


{- | DB keys can potentially be much larger than 511 bytes allowed as LMDB keys. So we 
     - calculate hash 
     - truncate the serialised values so that that truncated version + hash fit into 511 bytes.
     - use "truncated URL + hash" as a key
     - store a list of pairs of the full serialised key along the value
     - Very long keys are rare in practice and it's astronomically unlikely that the list 
       in KeysAndValues will ever be more than one element, but we want to be sure.

    The whole thing is optimised for the happy path, i.e. when the keys are short enough.
-} 
data SafeKey k = AsIs BS.ByteString 
               | Overflow (OverflowKey k)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data OverflowKey k = OverflowKey BS.ByteString Int
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype KeysAndValues v = KeysAndValues [T2 BS.ByteString BS.ByteString]
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data SafeMap (name :: Symbol) s k v = SafeMap {
        normals   :: SMap name s (Verbatim k) v,
        overflows :: SMap (AppendSymbol name "-overflow") s (OverflowKey k) (KeysAndValues v)
    }            

instance Storage s => WithStorage s (SafeMap name s k v) where
    storage (SafeMap s _) = storage s


put :: (AsStorable k, AsStorable v) =>
        Tx s 'RW -> SafeMap name s k v -> k -> v -> IO ()
put tx SafeMap {..} k v = do
    let (sk, serialisedKey) = safeKey k
    case sk of
        AsIs _       -> M.put tx normals (Verbatim $ Storable serialisedKey) v
        Overflow key -> do             
            let toStore = unStorable $ toStorable v
            M.get tx overflows key >>= \case
                Just (KeysAndValues pairs) -> do                    
                    let pairs' = T2 serialisedKey toStore : filter (\(T2 k' _) -> k' /= serialisedKey) pairs
                    M.put tx overflows key (KeysAndValues pairs')
                _ ->
                    M.put tx overflows key (KeysAndValues [T2 serialisedKey toStore])


get :: (AsStorable k, AsStorable v) =>
      Tx s mode -> SafeMap name s k v -> k -> IO (Maybe v)
get tx SafeMap {..} k = do
    let (sk, serialisedKey) = safeKey k
    case sk of
        AsIs _       -> M.get tx normals (Verbatim $ Storable serialisedKey)
        Overflow key -> do             
            M.get tx overflows key >>= \case
                Nothing -> pure Nothing
                Just (KeysAndValues pairs) -> 
                    pure $ listToMaybe [ unwrap v' 
                            | T2 k' v' <- pairs, k' == serialisedKey ]

delete :: (AsStorable k, AsStorable v) =>
          Tx s 'RW -> SafeMap name s k v -> k -> IO ()
delete tx SafeMap {..} k = do
    let (sk, serialisedKey) = safeKey k
    case sk of
        AsIs _       -> M.delete tx normals (Verbatim $ Storable serialisedKey)
        Overflow key -> do             
            ifJustM(M.get tx overflows key) $ \(KeysAndValues pairs) -> do
                case filter (\(T2 k' _) -> k' /= serialisedKey) pairs of 
                    []     -> M.delete tx overflows key
                    pairs' -> M.put tx overflows key (KeysAndValues pairs')


values :: AsStorable v =>
          Tx s mode -> SafeMap name s k v -> IO [v]
values tx SafeMap {..} =
    M.values tx normals <> 
    (concatMap extract <$> M.values tx overflows)
  where
    extract (KeysAndValues pairs) = map (\(T2 _ v) -> unwrap v) pairs


all :: (AsStorable k, AsStorable v) =>
        Tx s mode -> SafeMap name s k v -> IO [(k, v)]
all tx SafeMap {..}  =
    (map (first restoreFromRaw) <$> M.all tx normals) <> 
    (concatMap extract <$> M.values tx overflows)
  where
    extract (KeysAndValues pairs) = map (\(T2 bs v) -> (unwrap bs, unwrap v)) pairs

unwrap :: AsStorable c => BS.ByteString -> c
unwrap = fromStorable . Storable


safeKey :: AsStorable k => k -> (SafeKey a, BS.ByteString)
safeKey k = let         
    maxLmdbKeyBytes = 511

    -- Header for the OverflowKey type tags.    
    -- It is 9 for the Amd64, GHC 9.6 but it depends on compiler version 
    -- and what store library does, so we make it 20 just in case. 
    headerBytes = 20

    -- size if Int
    hashBytes = 8

    Storable bs = toStorable k
    hash_ = H.hash bs

    bytesLeft = maxLmdbKeyBytes - hashBytes - headerBytes
    
    shortened = BS.take bytesLeft bs
    safe_ = if BS.length bs < bytesLeft
        then AsIs bs
        else Overflow (OverflowKey shortened hash_)
    in (safe_, bs)

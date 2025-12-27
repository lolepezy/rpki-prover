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


{- | DB keys can potentially be much larger than the underlying key-value storage allows.
     According to LMDB documentation, for instance, the maximum key size is 511 bytes. 
     So we:
     - calculate an integer (8 byte) hash and use it as a key     
     - store a list of pairs of the full serialised key along with the value.
     Very long keys are rare in practice and it's astronomically unlikely that the list 
     in KeysAndValues will ever be more than one element, but we want to be sure.

    The whole thing is optimised for the happy path, i.e. when the keys are short enough.
-} 
data SafeKey k = AsIs BS.ByteString 
               | Overflow (OverflowKey k)
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype OverflowKey k = OverflowKey Int
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

newtype KeysAndValues v = KeysAndValues [T2 BS.ByteString BS.ByteString]
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data SafeMap (name :: Symbol) s k v = SafeMap {
        normals     :: SMap name s (Verbatim k) v,
        overflows   :: SMap (AppendSymbol name "-overflow") s (OverflowKey k) (KeysAndValues v),
        maxKeyBytes :: Int
    }            

instance Storage s => WithStorage s (SafeMap name s k v) where
    storage (SafeMap s _ _) = storage s


put :: (AsStorable k, AsStorable v) =>
        Tx s 'RW -> SafeMap name s k v -> k -> v -> IO ()
put tx SafeMap {..} k v = do
    let (sk, serialisedKey) = safeKey k maxKeyBytes
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
    let (sk, serialisedKey) = safeKey k maxKeyBytes
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
    let (sk, serialisedKey) = safeKey k maxKeyBytes
    case sk of
        AsIs _       -> M.delete tx normals (Verbatim $ Storable serialisedKey)
        Overflow key -> do             
            ifJustM (M.get tx overflows key) $ \(KeysAndValues pairs) -> do
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


safeKey :: AsStorable k => k -> Int -> (SafeKey a, BS.ByteString)
safeKey k limit = (safe_, bs)
  where
    bs = unStorable $ toStorable k
    safe_ = 
        if BS.length bs < limit
        then AsIs bs
        else Overflow (OverflowKey $ H.hash bs)    
    

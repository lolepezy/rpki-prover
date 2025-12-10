{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}

module RPKI.Store.Base.SafeMap where

import qualified Data.ByteString as BS
import           Data.Tuple.Strict
import qualified Data.Hashable   as H
import           Data.Maybe

import           GHC.Generics

import RPKI.Store.Types
import RPKI.Store.Base.Map as M
import RPKI.Store.Base.Storage
import RPKI.Store.Base.Storable
import RPKI.Store.Base.Serialisation
import RPKI.Util (ifJustM)


{- | DB keys can potentially be much larger than 512 bytes allowed as LMDB keys. So we 
     - calculate hash 
     - truncate the serialised values so that that truncated version + hash fit into 512 bytes.
     - use "truncated URL + hash" as a key
     - store a list of pairs of the full serialised key along the value
     - Very long keys are rare in practive and it's astronomically unlikely that the list 
       ValueWithKey will every be more than one element, but we want to be sure.
-} 
data SafeValue v = ValueAsIs v
                 | ValueWithKey [T2 BS.ByteString v]
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

type SafeMap name s k v = SMap name s (SafeKey k) (SafeValue v)

data SafeBin a = SafeBinDirect BS.ByteString
               | SafeBinExtended [T2 BS.ByteString BS.ByteString]
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

-- TODO This creates and intermediate structure and double-serialisation 
-- which is a serious hit to performance. Do something about it.
instance {-# OVERLAPPING #-} AsStorable v => AsStorable (SafeValue v) where
    toStorable   = toStorable . toSafeBin

    fromStorable (Storable a) = fromSafeBin $ deserialise_ a

toSafeBin :: AsStorable v => SafeValue v -> SafeBin v
toSafeBin (ValueAsIs v) = SafeBinDirect (unStorable $ toStorable v)
toSafeBin (ValueWithKey pairs) = SafeBinExtended $ 
    map (\(T2 k v) -> T2 k (unStorable $ toStorable v)) pairs   

fromSafeBin :: AsStorable v => SafeBin v -> SafeValue v
fromSafeBin (SafeBinDirect bs) = ValueAsIs (fromStorable $ Storable bs)
fromSafeBin (SafeBinExtended pairs) = ValueWithKey $
    map (\(T2 k v) -> T2 k (fromStorable $ Storable v)) pairs


put :: (AsStorable k, AsStorable v) =>
      Tx s 'RW -> SafeMap name s k v -> k -> v -> IO ()
put tx m k v = do
    let (sk, serialisedKey) = safeKey k
    case sk of
        AsIs _               -> M.put tx m sk (ValueAsIs v)
        ExtendedWithHash _ _ -> do 
            M.get tx m sk >>= \case
                Just (ValueWithKey pairs) -> do
                    let pairs' = T2 serialisedKey v : filter (\(T2 k' _) -> k' /= serialisedKey) pairs
                    M.put tx m sk (ValueWithKey pairs')
                _ ->
                    M.put tx m sk (ValueWithKey [T2 serialisedKey v])

get :: (AsStorable k, AsStorable v) =>
      Tx s mode -> SafeMap name s k v -> k -> IO (Maybe v)
get tx m k = do
    let (sk, serialisedKey) = safeKey k
    M.get tx m sk >>= \case
        Nothing -> pure Nothing
        Just sv -> pure $ case sv of
            ValueAsIs v        -> Just v
            ValueWithKey pairs -> listToMaybe [ v' | T2 k' v' <- pairs, k' == serialisedKey ]


delete :: (AsStorable k, AsStorable v) =>
          Tx s 'RW -> SafeMap name s k v -> k -> IO ()
delete tx m k = do
    let (sk, serialisedKey) = safeKey k
    case sk of
        AsIs _               -> M.delete tx m sk
        ExtendedWithHash _ _ -> do 
            ifJustM(M.get tx m sk) $ \case
                -- That should never happen
                ValueAsIs _        -> M.delete tx m sk
                ValueWithKey pairs -> do
                  case filter (\(T2 k' _) -> k' /= serialisedKey) pairs of 
                      []     -> M.delete tx m sk
                      pairs' -> M.put tx m sk (ValueWithKey pairs')

values :: AsStorable v =>
          Tx s mode -> SafeMap name s k v -> IO [v]
values tx m =
    concatMap extract <$> M.values tx m
  where
    extract = \case 
        ValueAsIs v        -> [v]
        ValueWithKey pairs -> map (\(T2 _ v) -> v) pairs

all :: (AsStorable k, AsStorable v) =>
        Tx s mode -> SafeMap name s k v -> IO [(k, v)]
all tx m =
    concatMap extract <$> M.all tx m
  where
    extract = \case 
        (AsIs k, ValueAsIs v)   -> [(unwrap k, v)]
        (_, ValueWithKey pairs) -> map (\(T2 bs v) -> (unwrap bs, v)) pairs
        -- this is just to keep the compiler happy
        _ -> []

    unwrap = fromStorable . Storable


safeKey :: AsStorable k => k -> (SafeKey a, BS.ByteString)
safeKey k = let         
    maxLmdbKeyBytes = 512

    -- Header for the SafeKey type tags.    
    -- It is 9 for the Amd64, GHC 9.6 but it depends on compiler version 
    -- and what store library does, so we make it 20 just in case. 
    headerBytes = 20

    Storable bs = toStorable k
    hash_ = H.hash bs

    bytesLeft = maxLmdbKeyBytes - 8 - headerBytes
    
    shortened = BS.take bytesLeft bs
    safe_ = if BS.length bs < bytesLeft
        then AsIs bs
        else ExtendedWithHash shortened hash_
    in (safe_, bs)

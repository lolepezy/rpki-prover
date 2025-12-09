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
import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage
import RPKI.Store.Base.Serialisation
import RPKI.Util (ifJustM)


-- | DB keys can potentially be much larger than 512 bytes allowed as LMDB keys. So we 
--     - calculate hash 
--     - truncate the serialised values so that that truncated version + hash fit into 512 bytes.
--     - use "truncated URL + hash" as a key
--     - store a list of pairs of the full serialised key along the value
-- 
data SafeValue v = ValueAsIs v
                 | ValueWithKey [T2 BS.ByteString v]
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

type SafeMap name s k v = SMap name s (SafeKey k) (SafeValue v)

safePut :: (TheBinary k, TheBinary v) =>
            Tx s 'RW -> SafeMap name s k v -> k -> v -> IO ()
safePut tx m k v = do
    let (sk, serialisedKey) = safeKey k
    case sk of
        AsIs _               -> M.put tx m sk $ ValueAsIs v
        ExtendedWithHash _ _ -> do 
            M.get tx m sk >>= \case
                Just (ValueWithKey pairs) -> do
                    let pairs' = map (\(T2 k' v') -> if k' == serialisedKey then T2 k' v else T2 k' v') pairs            
                    M.put tx m sk (ValueWithKey pairs')
                _ ->
                    M.put tx m sk (ValueWithKey [T2 serialisedKey v])

safeGet :: (TheBinary k, TheBinary v) =>
           Tx s mode -> SafeMap name s k v -> k -> IO (Maybe v)
safeGet tx m k = do
    let (sk, serialisedKey) = safeKey k
    M.get tx m sk >>= \case
        Nothing -> pure Nothing
        Just sv -> pure $ case sv of
            ValueAsIs v        -> Just v
            ValueWithKey pairs -> listToMaybe [ v' | T2 k' v' <- pairs, k' == serialisedKey ]


safeDelete :: (TheBinary k, TheBinary v) =>
             Tx s 'RW -> SafeMap name s k v -> k -> IO ()
safeDelete tx m k = do
    let (sk, serialisedKey) = safeKey k
    case sk of
        AsIs _               -> M.delete tx m sk
        ExtendedWithHash _ _ -> do 
            ifJustM(M.get tx m sk) $ \(ValueWithKey pairs) -> do
                case filter (\(T2 k' v') -> k' /= serialisedKey) pairs of 
                    []     -> M.delete tx m sk
                    pairs' -> M.put tx m sk (ValueWithKey pairs')

safeValues :: (TheBinary k, TheBinary v) =>
             Tx s mode -> SafeMap name s k v -> IO [v]
safeValues tx m =
    concatMap extract <$> M.values tx m
  where
    extract = \case 
        ValueAsIs v        -> [v]
        ValueWithKey pairs -> map (\(T2 _ v) -> v) pairs


safeKey :: TheBinary k => k -> (SafeKey a, BS.ByteString)
safeKey k = let         
    maxLmdbKeyBytes = 512

    -- Header for the SafeKey type tags.    
    -- It is 9 for the Amd64, GHC 9.6 but it depends on compiler version 
    -- and what store library does, so we make it 20 just in case. 
    headerBytes = 20

    bs = serialise_ k
    hash_ = H.hash bs

    bytesLeft = maxLmdbKeyBytes - 8 - headerBytes
    
    shortened = BS.take bytesLeft bs
    safe_ = if BS.length bs < bytesLeft
        then AsIs bs
        else ExtendedWithHash shortened hash_
    in (safe_, bs)

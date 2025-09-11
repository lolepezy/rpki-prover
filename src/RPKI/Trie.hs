{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Trie where

import           Control.DeepSeq    

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           GHC.Generics

import Prelude hiding (lookup, null)

import           RPKI.Store.Base.Serialisation

data Trie k v = Trie { 
        value    :: Maybe v, 
        children :: Map k (Trie k v)  
    } 
    deriving stock (Show, Eq, Functor, Generic)
    deriving anyclass (TheBinary, NFData)

instance Ord k => Semigroup (Trie k v) where
    Trie v1 c1 <> Trie v2 c2 = Trie combinedValues combinedChildren
      where
        combinedValues = case (v1, v2) of
            (Nothing, Nothing) -> Nothing
            (Just x, Nothing)  -> Just x
            (_, Just y)  -> Just y            
        
        combinedChildren = Map.unionWith (<>) c1 c2

instance Ord k => Monoid (Trie k v) where
    mempty = Trie Nothing Map.empty    

null :: Trie k v -> Bool
null (Trie Nothing children_) = Map.null children_
null _ = False

size :: Trie k v -> Int
size (Trie Nothing children) = sum (size <$> children)
size (Trie (Just _) children) = 1 + sum (size <$> children)

lookup :: Ord k => [k] -> Trie k v -> Maybe v
lookup [] (Trie v _) = v
lookup (k:ks) Trie {..} = 
    lookup ks =<< Map.lookup k children         

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert [] v (Trie _ children) = Trie (Just v) children
insert (k:ks) v (Trie value children) = 
    Trie value (Map.alter updateChild k children)
  where
    updateChild Nothing = Just (insert ks v mempty)
    updateChild (Just trie) = Just (insert ks v trie)

delete :: Ord k => [k] -> Trie k v -> Trie k v
delete = alter (const Nothing)

alter :: Ord k => (Maybe v -> Maybe v) -> [k] -> Trie k v -> Trie k v
alter f [] (Trie v children) = Trie (f v) children
alter f (k:ks) (Trie v children) = 
    Trie v (Map.alter updateChild k children)
  where
    updateChild Nothing = 
        let altered = alter f ks mempty
        in if null altered then Nothing else Just altered
    updateChild (Just trie) = 
        let altered = alter f ks trie
        in if null altered then Nothing else Just altered    

fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldr (uncurry insert) mempty

toList :: Trie k v -> [([k], v)]
toList = toList' []
  where
    toList' p (Trie v children) = 
        maybeToList p v ++ concatMap (\(k, t) -> toList' (p ++ [k]) t) (Map.toList children)
    
    maybeToList p (Just v) = [(p, v)]
    maybeToList _ Nothing = []        
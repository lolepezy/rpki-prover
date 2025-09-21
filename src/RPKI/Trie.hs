{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Trie where

import           Control.DeepSeq    

import           Data.Maybe (isJust)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)

import           GHC.Generics

import Prelude hiding (lookup, null)

import           RPKI.Store.Base.Serialisation

{- 
    A simple Trie (prefix tree) implementation.

    The only reason it exists here is to be able to derive TheBinary instance for it.
    Doing it for a library type is a PITA.
-}

data Trie k v = Trie { 
        value    :: Maybe v, 
        children :: Map k (Trie k v)  
    } 
    deriving stock (Show, Eq, Ord, Functor, Generic)
    deriving anyclass (TheBinary, NFData)

    
empty :: Trie k v
empty = Trie Nothing Map.empty

singleton :: Ord k => [k] -> v -> Trie k v
singleton k v = insert k v empty

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

member :: Ord k => [k] -> Trie k v -> Bool
member ks t = isJust (lookup ks t)

elems :: Trie k v -> [v]
elems (Trie v children) = maybeToList v ++ concatMap elems (Map.elems children)  

insert :: Ord k => [k] -> v -> Trie k v -> Trie k v
insert [] v (Trie _ children) = Trie (Just v) children
insert (k:ks) v (Trie value children) = 
    Trie value (Map.alter updateChild k children)
  where
    updateChild Nothing = Just (insert ks v empty)
    updateChild (Just trie) = Just (insert ks v trie)

delete :: Ord k => [k] -> Trie k v -> Trie k v
delete = alter (const Nothing)

alter :: Ord k => (Maybe v -> Maybe v) -> [k] -> Trie k v -> Trie k v
alter f [] (Trie v children) = Trie (f v) children
alter f (k:ks) (Trie v children) = 
    Trie v (Map.alter updateChild k children)
  where
    updateChild Nothing = 
        let altered = alter f ks empty
        in if null altered then Nothing else Just altered
    updateChild (Just trie) = 
        let altered = alter f ks trie
        in if null altered then Nothing else Just altered    

adjust :: Ord k => (v -> v) -> [k] -> Trie k v -> Trie k v
adjust f [] (Trie v children) = Trie (fmap f v) children
adjust f (k:ks) t@Trie {..} = 
    case Map.lookup k children of 
        Nothing    -> t
        Just child -> 
            let adjustedChild = adjust f ks child
            in Trie value (Map.insert k adjustedChild children)

fromList :: Ord k => [([k], v)] -> Trie k v
fromList = foldr (uncurry insert) empty

toList :: Trie k v -> [([k], v)]
toList = toList' []
  where
    toList' path (Trie v children) = 
        valueToList path v ++ concatMap (\(k, t) -> toList' (path ++ [k]) t) (Map.toList children)

    valueToList p (Just v) = [(p, v)]
    valueToList _ Nothing = []


unionWith :: Ord k => (v -> v -> v) -> Trie k v -> Trie k v -> Trie k v
unionWith f (Trie v1 c1) (Trie v2 c2) = Trie combinedValues combinedChildren
      where
        combinedValues = case (v1, v2) of
            (Nothing, Nothing) -> Nothing
            (Just x, Nothing)  -> Just x
            (Nothing, Just y)  -> Just y            
            (Just x, Just y)   -> Just $ f x y            
        
        combinedChildren = Map.unionWith (unionWith f) c1 c2    


filterWithKey :: Ord k => ([k] -> v -> Bool) -> Trie k v -> Trie k v
filterWithKey p = go []
  where
    go path (Trie mv children) =
        Trie filteredValue prunedChildren
      where        
        filteredValue = case mv of
            Just v | p path v -> Just v
            _                 -> Nothing
            
        filteredChildren = Map.mapWithKey 
            (\k child -> go (path ++ [k]) child) children
            
        prunedChildren = Map.filter (not . null) filteredChildren
        
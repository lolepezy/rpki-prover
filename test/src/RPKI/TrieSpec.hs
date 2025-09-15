{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.TrieSpec where

import Control.Monad (replicateM)
import Data.List (nub)
import Data.Maybe (isNothing)
import qualified Data.Set as Set 
import GHC.Generics (Generic)

import Test.Tasty
import Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Gen

import RPKI.Trie as Trie

-- | Test group for Trie tests
trieSpec :: TestTree
trieSpec = testGroup "Trie" [
    QC.testProperty 
        "lookup after insert returns the value" 
        prop_lookup_after_insert,
        
    QC.testProperty 
        "lookup of non-existent key returns Nothing" 
        prop_lookup_nonexistent,
        
    QC.testProperty 
        "insert overrides existing values" 
        prop_insert_override,
        
    QC.testProperty 
        "delete removes keys" 
        prop_delete_removes,
        
    QC.testProperty 
        "null after deleting the only key is True" 
        prop_null_after_delete,
        
    QC.testProperty 
        "size counts key-value pairs correctly" 
        prop_size_counts_correctly,            
        
    QC.testProperty 
        "Trie is a Monoid (left identity)" 
        prop_monoid_left_identity,
        
    QC.testProperty 
        "Trie is a Monoid (right identity)" 
        prop_monoid_right_identity,
        
    QC.testProperty 
        "Trie is a Semigroup (associativity)" 
        prop_semigroup_associativity,

    QC.testProperty 
        "filterWithKey with always-true predicate is identity" 
        prop_filterWithKey_true_is_identity,
        
    QC.testProperty 
        "filterWithKey with always-false predicate gives empty trie" 
        prop_filterWithKey_false_gives_empty,

    QC.testProperty 
        "filterWithKey behaves as expected" 
        prop_filterWithKey_returns_subset
    ]

newtype TestKey = TestKey Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (QC.Arbitrary)

newtype KeyPath = KeyPath [TestKey]
    deriving stock (Eq, Show)

instance QC.Arbitrary KeyPath where
    arbitrary = do
        len <- choose (0, 5)
        KeyPath <$> replicateM len QC.arbitrary
    shrink (KeyPath ks) = KeyPath <$> QC.shrink ks

newtype KeyValueList = KeyValueList [([TestKey], String)]
    deriving stock (Eq, Show)

instance QC.Arbitrary KeyValueList where
    arbitrary = do
        n <- choose (0, 20)  -- Reasonable number of entries
        keys <- replicateM n QC.arbitrary
        values <- replicateM n genNonEmptyString
        return $ KeyValueList $ zip (map (\(KeyPath ks) -> ks) keys) values
      where
        genNonEmptyString = listOf1 $ elements ['a'..'z']    
    shrink (KeyValueList kvs) = KeyValueList <$> QC.shrink kvs

-- | Helper to build a trie from a list of key-value pairs
kvListToTrie :: KeyValueList -> Trie TestKey String
kvListToTrie (KeyValueList kvs) = fromList kvs

prop_lookup_after_insert :: KeyPath -> String -> KeyValueList -> Bool
prop_lookup_after_insert (KeyPath ks) v kvl =
    Trie.lookup ks (insert ks v (kvListToTrie kvl)) == Just v

prop_lookup_nonexistent :: KeyPath -> KeyPath -> String -> QC.Property
prop_lookup_nonexistent (KeyPath ks1) (KeyPath ks2) v =
    ks1 /= ks2 QC.==> isNothing (Trie.lookup ks2 (insert ks1 v mempty))

prop_insert_override :: KeyPath -> String -> String -> KeyValueList -> Bool
prop_insert_override (KeyPath ks) v1 v2 kvl =
    Trie.lookup ks (insert ks v2 (insert ks v1 (kvListToTrie kvl))) == Just v2

prop_delete_removes :: KeyPath -> String -> KeyValueList -> Bool
prop_delete_removes (KeyPath ks) v kvl =
    isNothing (Trie.lookup ks (delete ks (insert ks v (kvListToTrie kvl))))

prop_null_after_delete :: KeyPath -> String -> Bool
prop_null_after_delete (KeyPath ks) v =
    Trie.null (delete ks (insert ks v mempty))

prop_size_counts_correctly :: KeyValueList -> Bool
prop_size_counts_correctly kvl@(KeyValueList kvs) =
    size (kvListToTrie kvl) == length (nub (map fst kvs))

prop_monoid_left_identity :: KeyValueList -> Bool
prop_monoid_left_identity kvl =
    let t = kvListToTrie kvl
    in mempty <> t == t

prop_monoid_right_identity :: KeyValueList -> Bool
prop_monoid_right_identity kvl =
    let t = kvListToTrie kvl
    in t <> mempty == t

prop_semigroup_associativity :: KeyValueList -> KeyValueList -> KeyValueList -> Bool
prop_semigroup_associativity kvl1 kvl2 kvl3 =
    let t1 = kvListToTrie kvl1
        t2 = kvListToTrie kvl2
        t3 = kvListToTrie kvl3
    in (t1 <> t2) <> t3 == t1 <> (t2 <> t3)

prop_filterWithKey_true_is_identity :: KeyValueList -> Bool
prop_filterWithKey_true_is_identity kvl = let 
    t = kvListToTrie kvl
    in t == Trie.filterWithKey (\_ _ -> True) t    

prop_filterWithKey_false_gives_empty :: KeyValueList -> Bool
prop_filterWithKey_false_gives_empty kvl =
    let t = kvListToTrie kvl
    in Trie.null $ Trie.filterWithKey (\_ _ -> False) t


prop_filterWithKey_returns_subset :: KeyValueList -> Int -> Bool
prop_filterWithKey_returns_subset (KeyValueList list) count =
    let 
        list' = nub list
        t1 = kvListToTrie $ KeyValueList list'
        cutOff = count `mod` length list'
        (toKeep, _) = splitAt cutOff list'
        keyToKeep = Set.fromList $ map fst toKeep
        
        kept' = Trie.filterWithKey (\ks _ -> ks `Set.member` keyToKeep) t1                    
        kept = kvListToTrie (KeyValueList toKeep)

    in Prelude.null list' || (kept == kept')


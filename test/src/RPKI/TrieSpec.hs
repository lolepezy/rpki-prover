{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.TrieSpec where

import Control.Monad (replicateM)
import Data.List (nub)
import Data.Maybe (isNothing)
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
        prop_semigroup_associativity        
    ]

-- | Simple key type for testing
newtype TestKey = TestKey Int
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (QC.Arbitrary)

-- | Path of keys
newtype KeyPath = KeyPath [TestKey]
    deriving stock (Eq, Show)

instance QC.Arbitrary KeyPath where
    arbitrary = do
        len <- choose (0, 5)  -- Keep paths reasonably short
        KeyPath <$> replicateM len QC.arbitrary
    
    shrink (KeyPath ks) = KeyPath <$> QC.shrink ks

-- | List of key-value pairs
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

prop_lookup_after_insert :: KeyPath -> String -> Bool
prop_lookup_after_insert (KeyPath ks) v = 
    Trie.lookup ks (insert ks v mempty) == Just v

-- | Property: lookup of non-existent key returns Nothing
prop_lookup_nonexistent :: KeyPath -> KeyPath -> String -> QC.Property
prop_lookup_nonexistent (KeyPath ks1) (KeyPath ks2) v =
    ks1 /= ks2 QC.==> isNothing (Trie.lookup ks2 (insert ks1 v mempty))

-- | Property: inserting with the same key overrides previous values
prop_insert_override :: KeyPath -> String -> String -> Bool
prop_insert_override (KeyPath ks) v1 v2 =
    Trie.lookup ks (insert ks v2 (insert ks v1 mempty)) == Just v2

-- | Property: delete removes keys
prop_delete_removes :: KeyPath -> String -> Bool
prop_delete_removes (KeyPath ks) v =
    isNothing (Trie.lookup ks (delete ks (insert ks v mempty)))

-- | Property: trie is null after deleting the only key
prop_null_after_delete :: KeyPath -> String -> Bool
prop_null_after_delete (KeyPath ks) v =
    Trie.null (delete ks (insert ks v mempty))

-- | Property: size counts the number of key-value pairs correctly
prop_size_counts_correctly :: KeyValueList -> Bool
prop_size_counts_correctly kvl@(KeyValueList kvs) =
    size (kvListToTrie kvl) == length (nub (map fst kvs))


-- | Property: mempty is the left identity for (<>)
prop_monoid_left_identity :: KeyValueList -> Bool
prop_monoid_left_identity kvl =
    let t = kvListToTrie kvl
    in mempty <> t == t

-- | Property: mempty is the right identity for (<>)
prop_monoid_right_identity :: KeyValueList -> Bool
prop_monoid_right_identity kvl =
    let t = kvListToTrie kvl
    in t <> mempty == t

-- | Property: (<>) is associative
prop_semigroup_associativity :: KeyValueList -> KeyValueList -> KeyValueList -> Bool
prop_semigroup_associativity kvl1 kvl2 kvl3 =
    let t1 = kvListToTrie kvl1
        t2 = kvListToTrie kvl2
        t3 = kvListToTrie kvl3
    in (t1 <> t2) <> t3 == t1 <> (t2 <> t3)

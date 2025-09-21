{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import qualified RPKI.Trie as Trie
import Data.Text (Text)
import qualified Data.Text as T

import RPKI.Reporting (Focus(..), Scope(..), VScope, focuses)

genUrl :: Int -> Int -> Gen Text
genUrl minLen maxLen = do
  len <- choose (minLen, maxLen)
  
  -- Generate random hostname components
  subdomainLen <- choose (3, 10)
  subdomain <- replicateM subdomainLen (elements $ ['a'..'z'] ++ ['0'..'9'] ++ "-")
  
  -- Choose a TLD
  tld <- elements ["com", "org", "net", "io", "co", "info", "rpki", "arin", "ripe", "apnic", "lacnic", "afrinic"]
  
  -- Choose a second-level domain or use the subdomain directly
  useSecondLevel <- choose (0, 2 :: Int)
  let hostname = case useSecondLevel of
                   0 -> T.pack $ subdomain ++ "." ++ tld
                   1 -> T.pack $ subdomain ++ ".example." ++ tld
                   _ -> T.pack $ subdomain ++ ".rpki." ++ tld
  
  -- Generate the path portion
  pathLen <- choose (max 0 (len - T.length hostname - 10), len - T.length hostname - 10)
  let genPathChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".-_/"
  path <- replicateM pathLen genPathChar
  
  -- Combine into full URL
  return $ "https://" <> hostname <> "/" <> T.pack path

-- Generator for VScope with 10-20 TextFocus items
genVScope :: Gen VScope
genVScope = do
  -- Generate 10-20 focuses
  count <- choose (10, 20)
  urls <- replicateM count (genUrl 50 150)
  let focuses = map TextFocus urls
  
  -- Create a non-empty list of focuses
  case focuses of
    [] -> error "This should never happen due to count range"
    (x:xs) -> pure $ Scope (x :| xs)


main :: IO ()
main = do
  scopes <- generate $ replicateM 1000 genVScope
  let scopeFocuses = map focuses scopes
  
  -- Build the data structures for tests§
  let mapDS = Map.fromList $ zip scopes [1..1000 :: Int]
      hashmapDS = HashMap.fromList $ zip scopes [1..1000 :: Int]
      trieDS = Trie.fromList $ zip scopeFocuses [1..1000 :: Int]
  
  defaultMain [
      bgroup "Map vs HashMap vs Trie with VScope keys" [
        bgroup "Construction (1000 elements)" [
          bench "Map.fromList" $ nf (Map.fromList . flip zip [1..1000 :: Int]) scopes,
          bench "HashMap.fromList" $ nf (HashMap.fromList . flip zip [1..1000 :: Int]) scopes,
          bench "Trie.fromList" $ nf (Trie.fromList . flip zip [1..1000 :: Int]) scopeFocuses
        ],
        
        bgroup "Lookup (all 1000 keys)" [
          bench "Map.lookup" $ nf (\keys -> map (`Map.lookup` mapDS) keys) scopes,
          bench "HashMap.lookup" $ nf (\keys -> map (`HashMap.lookup` hashmapDS) keys) scopes,
          bench "Trie.lookup" $ nf (\keys -> map (`Trie.lookup` trieDS) scopeFocuses) scopeFocuses
        ],
        
        bgroup "Membership Testing (all 1000 keys)" [
          bench "Map.member" $ nf (\keys -> map (`Map.member` mapDS) keys) scopes,
          bench "HashMap.member" $ nf (\keys -> map (`HashMap.member` hashmapDS) keys) scopes,
          bench "Trie.member" $ nf (\keys -> map (`Trie.member` trieDS) keys) scopeFocuses
        ]                        
      ]
    ]
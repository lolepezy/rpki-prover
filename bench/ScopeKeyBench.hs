{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T

import RPKI.Reporting (Focus(..), Scope(..), VScope)

-- Generator for random URLs of specified length
genUrl :: Int -> Int -> Gen Text
genUrl minLen maxLen = do
  len <- choose (minLen, maxLen)
  let genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".-_/:"
  chars <- replicateM len genChar
  return $ T.pack $ "https://rpki.example.com/" ++ chars

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
  
  -- Build the data structures for tests
  let mapDS = Map.fromList $ zip scopes [1..1000 :: Int]
      hashmapDS = HashMap.fromList $ zip scopes [1..1000 :: Int]
  
  defaultMain [
      bgroup "Map vs HashMap with VScope keys" [
          bgroup "Construction (1000 elements)" [
            bench "Map.fromList" $ nf (Map.fromList . flip zip [1..1000 :: Int]) scopes,
            bench "HashMap.fromList" $ nf (HashMap.fromList . flip zip [1..1000 :: Int]) scopes
          ],
          
          bgroup "Lookup (all 1000 keys)" [
            bench "Map.lookup" $ nf (\keys -> map (`Map.lookup` mapDS) keys) scopes,
            bench "HashMap.lookup" $ nf (\keys -> map (`HashMap.lookup` hashmapDS) keys) scopes
          ],
          
          bgroup "Membership Testing (all 1000 keys)" [
            bench "Map.member" $ nf (\keys -> map (`Map.member` mapDS) keys) scopes,
            bench "HashMap.member" $ nf (\keys -> map (`HashMap.member` hashmapDS) keys) scopes
          ],
          
          bgroup "Iteration (sum of all values)" [
            bench "Map.foldr" $ nf (Map.foldr (+) 0) mapDS,
            bench "HashMap.foldr" $ nf (HashMap.foldr (+) 0) hashmapDS
          ]
        ]
    ]
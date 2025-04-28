{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.WorkflowSpec where

import           Data.Text (Text)

import           Test.Tasty
import           Test.Tasty.HUnit                as HU

import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set

import           RPKI.Domain
import           RPKI.Workflow


-- Helper to create a Map of URL to TA names
urlToTas :: [(Text, [Text])] -> Map.Map RpkiURL (Set.Set TaName)
urlToTas pairs = Map.fromList [
    (RrdpU (RrdpURL (URI url)), Set.fromList (map TaName tas)) 
    | (url, tas) <- pairs
    ]

-- Helper to create a Map of TA names to URLs
taToUrls :: [(Text, [Text])] -> Map.Map TaName (Set.Set RpkiURL)
taToUrls pairs = Map.fromList [
    (TaName ta, Set.fromList [RrdpU (RrdpURL (URI url)) | url <- urls])
    | (ta, urls) <- pairs
    ]


workflowSpec :: TestTree
workflowSpec = testGroup "Workflow"  [ 
  HU.testCase "Keeps entries that are in both maps" $
      let fetcheableToTAs = urlToTas [(repo1, ["TA1"])]
          discoveredUrls = taToUrls [("TA1", [repo1])]
          expected = urlToTas [(repo1, ["TA1"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected

  , HU.testCase "Adds newly discovered URLs" $
      let fetcheableToTAs = urlToTas [(repo1, ["TA1", "TA2"])]
          discoveredUrls = taToUrls [("TA1", [repo1, repo2])]
          expected = urlToTas [(repo1, ["TA1", "TA2"]),(repo2, ["TA1"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected

  , HU.testCase "Removes URLs that aren't discovered by their TA" $
      let fetcheableToTAs = urlToTas [(repo1, ["TA1"]),(repo2, ["TA1"])]
          discoveredUrls = taToUrls [("TA1", [repo1])]
          expected = urlToTas [(repo1, ["TA1"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected

  , HU.testCase "Keeps URLs for TAs not mentioned in discoveredUrls" $
      let fetcheableToTAs = urlToTas [(repo1, ["TA1", "TA2"]),(repo2, ["TA2"])]
          discoveredUrls = taToUrls [("TA1", [repo1])]
          expected = urlToTas [(repo1, ["TA1", "TA2"]),(repo2, ["TA2"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected

  , HU.testCase "Merges TAs for the same URL" $
      let fetcheableToTAs = urlToTas [(repo1, ["TA1"]),(repo2, ["TA1"])]
          discoveredUrls = taToUrls [("TA1", [repo1]),("TA2", [repo1])]
          expected = urlToTas [(repo1, ["TA1", "TA2"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected

  , HU.testCase "Handles empty fetcheableToTAs map" $
      let fetcheableToTAs = Map.empty
          discoveredUrls = taToUrls [("TA1", [repo1])]
          expected = urlToTas [(repo1, ["TA1"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected
            
  , HU.testCase "Handles empty discoveredUrls map with non-empty fetcheableToTAs" $
      let fetcheableToTAs = urlToTas [(repo1, ["TA1"])]
          discoveredUrls = Map.empty
          expected = urlToTas [(repo1, ["TA1"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected

  , HU.testCase "Handles empty set of TAs for a URL" $
      let fetcheableToTAs = urlToTas [(repo1, [])]
          discoveredUrls = taToUrls [("TA1", [repo1])]
          expected = urlToTas [(repo1, ["TA1"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected

  , HU.testCase "Handles multiple TAs per URL" $
      let fetcheableToTAs = urlToTas [(repo1, ["TA1", "TA2"]), (repo2, ["TA1", "TA3"])]
          discoveredUrls = taToUrls [("TA1", [repo1])]
          expected = urlToTas [(repo1, ["TA1", "TA2"]), (repo2, ["TA3"])]
      in updateRepositoryToTAs fetcheableToTAs discoveredUrls @?= expected
  ]
  where
    repo1, repo2 :: Text
    repo1 = "https://repo1.example.com"
    repo2 = "https://repo2.example.com"
 
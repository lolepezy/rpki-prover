{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.FetchSpec where

import           Data.Text (Text)

import           Test.Tasty
import           Test.Tasty.HUnit                as HU

import qualified Data.Map.Strict                 as Map
import qualified Data.Map.Monoidal.Strict        as MonoidalMap
import qualified Data.IxSet.Typed                as IxSet

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Fetch


uriTaSet :: [(Text, Text)] -> UriTaIxSet
uriTaSet pairs = IxSet.fromList [
    UrlTA (RrdpU (RrdpURL (URI url))) (TaName ta)
    | (url, ta) <- pairs
    ]

taFetchables :: [(Text, [Text])] -> Map.Map TaName Fetcheables
taFetchables = Map.fromList . map convertEntry
  where
    convertEntry (ta, urls) = (TaName ta, Fetcheables $ MonoidalMap.fromList primaryFallbacks)
      where
        primaryFallbacks = [(RrdpU (RrdpURL (URI primary)), mempty) | primary <- urls ]

repo1, repo2, repo3, oldRepo :: Text
repo1 = "https://repo1.example.com"
repo2 = "https://repo2.example.com"
repo3 = "https://repo3.example.com"
oldRepo = "https://old-repo.example.com"

ta1, ta2, ta3 :: Text
ta1 = "TA1"
ta2 = "TA2"
ta3 = "TA3"

workflowSpec :: TestTree
workflowSpec = testGroup "Fetching" [ 

    HU.testCase "Adds entries to empty IxSet" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1])]) 
            (uriTaSet []) 
            @?= uriTaSet [(repo1, ta1)]

  , HU.testCase "Replaces all entries for a TA" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo2])]) 
            (uriTaSet [(repo1, ta1)]) 
            @?= uriTaSet [(repo2, ta1)]

  , HU.testCase "Preserves entries for TAs not in fs" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1])]) 
            (uriTaSet [(repo2, ta2)]) 
            @?= uriTaSet [(repo1, ta1), (repo2, ta2)]

  , HU.testCase "Handles multiple URLs per TA" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1, repo2])]) 
            (uriTaSet []) 
            @?= uriTaSet [(repo1, ta1), (repo2, ta1)]

  , HU.testCase "Handles multiple TAs and URLs" $
        updateUriPerTa 
            (taFetchables [(ta1, [repo1]),(ta2, [repo2])])
            (uriTaSet [(oldRepo, ta1),(repo3, ta3)])
            @?= uriTaSet [(repo1, ta1),(repo2, ta2),(repo3, ta3)]

  , HU.testCase "Handles TAs with no URLs in fs" $
        updateUriPerTa 
            (taFetchables [(ta1, []),(ta2, [repo2])])
            (uriTaSet [(repo1, ta1),(oldRepo, ta2),(repo3, ta3)])
            @?= uriTaSet [(repo2, ta2),(repo3, ta3)]

  , HU.testCase "Handles empty fs map" $
        updateUriPerTa 
            (taFetchables [])
            (uriTaSet [(repo1, ta1), (repo2, ta2)])
            @?= uriTaSet [(repo1, ta1), (repo2, ta2)]
  ]
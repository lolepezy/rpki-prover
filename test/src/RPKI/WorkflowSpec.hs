{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.WorkflowSpec where

import           Data.Text (Text)

import           Test.Tasty
import           Test.Tasty.HUnit                as HU

import qualified Data.Map.Strict                 as Map
import qualified Data.Map.Monoidal.Strict        as MonoidalMap
import qualified Data.Set                        as Set
import qualified Data.IxSet.Typed                as IxSet

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Workflow


uriTaSet :: [(Text, Text)] -> UriTaIxSet
uriTaSet pairs = IxSet.fromList [
    UriTA (RrdpU (RrdpURL (URI url))) (TaName ta)
    | (url, ta) <- pairs
    ]

taFetchables :: [(Text, [Text])] -> Map.Map TaName Fetcheables
taFetchables = Map.fromList . map convertEntry
  where
    convertEntry (ta, urls) = (TaName ta, Fetcheables $ MonoidalMap.fromList primaryFallbacks)
      where
        primaryFallbacks = [(RrdpU (RrdpURL (URI primary)), mempty) | primary <- urls ]

repo1, repo2, repo3 :: Text
repo1 = "https://repo1.example.com"
repo2 = "https://repo2.example.com"
repo3 = "https://repo3.example.com"
oldRepo = "https://old-repo.example.com"

ta1, ta2 :: Text
ta1 = "TA1"
ta2 = "TA2"
ta3 = "TA3"

workflowSpec :: TestTree
workflowSpec = testGroup "Workflow"  [ 

    HU.testCase "Adds entries to empty IxSet" $
        let fetcheables = taFetchables [(ta1, [repo1])]
            initialIxSet = uriTaSet []
            expected = uriTaSet [(repo1, ta1)]
        in updateUriPerTa fetcheables initialIxSet @?= expected

  , HU.testCase "Replaces all entries for a TA" $
        let fetcheables = taFetchables [(ta1, [repo2])]
            initialIxSet = uriTaSet [(repo1, ta1)]
            expected = uriTaSet [(repo2, ta1)]
        in updateUriPerTa fetcheables initialIxSet @?= expected

  , HU.testCase "Preserves entries for TAs not in fetcheables" $
        let fetcheables = taFetchables [(ta1, [repo1])]
            initialIxSet = uriTaSet [(repo2, ta2)]
            expected = uriTaSet [(repo1, ta1), (repo2, ta2)]
        in updateUriPerTa fetcheables initialIxSet @?= expected

  , HU.testCase "Handles multiple URLs per TA" $
        let fetcheables = taFetchables [(ta1, [repo1, repo2])]
            initialIxSet = uriTaSet []
            expected = uriTaSet [(repo1, ta1),(repo2, ta1)]
        in updateUriPerTa fetcheables initialIxSet @?= expected

  , HU.testCase "Handles multiple TAs and URLs" $
        let fetcheables = taFetchables [
                (ta1, [repo1]),
                (ta2, [repo2])
              ]
            initialIxSet = uriTaSet [
                (oldRepo, ta1),
                (repo3, ta3)
              ]
            expected = uriTaSet [
                (repo1, ta1),
                (repo2, ta2),
                (repo3, ta3)
              ]
        in updateUriPerTa fetcheables initialIxSet @?= expected

  , HU.testCase "Handles TAs with no URLs in fetcheables" $
        let fetcheables = taFetchables [
                (ta1, []),
                (ta2, [repo2])
              ]
            initialIxSet = uriTaSet [
                (repo1, ta1),
                (oldRepo, ta2),
                (repo3, ta3)
              ]
            expected = uriTaSet [
                (repo2, ta2),
                (repo3, ta3)
              ]
        in updateUriPerTa fetcheables initialIxSet @?= expected

  , HU.testCase "Handles empty fetcheables map" $
        let fetcheables = taFetchables []
            initialIxSet = uriTaSet [
                (repo1, ta1),
                (repo2, ta2)
              ]
            expected = initialIxSet
        in updateUriPerTa fetcheables initialIxSet @?= expected
  ]
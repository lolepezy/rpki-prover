{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RPKI.WorkflowSpec where

import RPKI.Workflow
import Test.Tasty
import qualified Test.Tasty.HUnit                as HU
import qualified Test.Tasty.QuickCheck           as QC

import qualified Data.Map.Strict                 as Map
import qualified Data.Set                        as Set

import           RPKI.Domain
import           RPKI.Workflow
import           RPKI.Orphans


workflowSpec :: TestTree
workflowSpec = testGroup "Workflow" [      
      propertyTests
    ]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ QC.testProperty "Includes all URLs discovered by any TA" $
      \fetcheableToTAs discoveredUrls -> 
          let allDiscoveredUrls = Set.unions $ Map.elems discoveredUrls
              result = updateRepositoryToTAs fetcheableToTAs discoveredUrls
          in all (`Set.member` allDiscoveredUrls) (Map.keys result)

  , QC.testProperty "Preserves TAs for URLs not mentioned in discoveredUrls" $
      \fetcheableToTAs discoveredUrls ->
          let result = updateRepositoryToTAs fetcheableToTAs discoveredUrls
              tasInDiscovered = Map.keysSet discoveredUrls
              urlsInDiscovered = Set.unions $ Map.elems discoveredUrls
              unmentionedUrls = Set.filter (`Set.notMember` urlsInDiscovered) $ Map.keysSet fetcheableToTAs
          in all (\url -> 
                case (Map.lookup url fetcheableToTAs, Map.lookup url result) of
                  (Just oldTas, Just newTas) -> 
                      Set.filter (`Set.notMember` tasInDiscovered) oldTas `Set.isSubsetOf` newTas
                  _ -> True
              ) (Set.toList unmentionedUrls)

  , QC.testProperty "All TAs in result actually discovered their URLs" $
      \fetcheableToTAs discoveredUrls -> 
          let result = updateRepositoryToTAs fetcheableToTAs discoveredUrls
          in all (\(url, tas) -> 
                all (\ta -> 
                    maybe True (Set.member url) (Map.lookup ta discoveredUrls)
                ) (Set.toList tas)
              ) (Map.toList result)
  ]

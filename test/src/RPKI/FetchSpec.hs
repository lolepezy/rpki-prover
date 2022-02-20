{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.FetchSpec where    

import Control.Monad (replicateM)

import Data.ByteString.Short (toShort)
import Data.Maybe (maybeToList, catMaybes)
import Data.List (sort, isPrefixOf, sortOn)

import           GHC.Generics

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           Test.QuickCheck.Gen

import           RPKI.Domain
import           RPKI.Repository
import           RPKI.Util
import           RPKI.Orphans


fetchGroup :: TestTree
fetchGroup = testGroup "Fetching" [

    ]
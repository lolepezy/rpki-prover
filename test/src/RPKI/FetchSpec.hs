{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module RPKI.FetchSpec where    

import Control.Monad (replicateM)
import Control.Concurrent.STM

import Data.ByteString.Short (toShort)
import Data.Maybe (maybeToList, catMaybes)
import Data.List (sort, isPrefixOf, sortOn)

import           GHC.Generics

import           Data.String.Interpolate.IsString

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           Test.QuickCheck.Gen

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Store.Database
import           RPKI.Repository
import           RPKI.Util
import           RPKI.Orphans
import           RPKI.Fetch
import           RPKI.TestSetup
import           RPKI.Logging
import           RPKI.Reporting
import System.IO.Temp
import Control.Monad.IO.Class



fetchGroup :: TestTree
fetchGroup = testGroup "Fetching" [
        HU.testCase "No URLs" shouldReturnEmptyList,
        HU.testCase "Doanload one URL" shouldDownloadOneURL        
    ]

shouldReturnEmptyList :: HU.Assertion
shouldReturnEmptyList = appContextTest $ \appContext -> do 
    rp1 <- atomically newRepositoryProcessing1
    -- let appContenxt = defaultConfig
    pure ()

shouldDownloadOneURL :: HU.Assertion
shouldDownloadOneURL = do 
    pure ()

                    

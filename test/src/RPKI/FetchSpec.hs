{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes #-}

module RPKI.FetchSpec where    

import           Control.Concurrent
import Control.Monad (replicateM)
import Control.Concurrent.STM

import Control.Monad.IO.Class
import Data.Text

import Data.ByteString.Short (toShort)
import Data.Maybe (maybeToList, catMaybes)
import Data.List (sort, isPrefixOf, sortOn)
import qualified Data.Map.Strict as Map

import           Data.List.NonEmpty

import           GHC.Generics

import           Data.String.Interpolate.IsString

import           Test.Tasty
import           Test.QuickCheck.Arbitrary.Generic
import qualified Test.Tasty.HUnit                  as HU
import qualified Test.Tasty.QuickCheck             as QC

import           Test.QuickCheck.Gen

import System.IO.Temp
import System.Random

import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Domain
import           RPKI.Store.Database
import           RPKI.Repository
import           RPKI.Time
import           RPKI.Util
import           RPKI.Fetch
import           RPKI.Orphans
import           RPKI.Fetch
import           RPKI.TestSetup
import           RPKI.Logging
import           RPKI.Reporting



fetchGroup :: TestTree
fetchGroup = testGroup "Fetching" [    
        HU.testCase "Download one URL" shouldDownloadOneURL,
        HU.testCase "setNode" shouldSetAndCheckNode
    ]

shouldDownloadOneURL :: HU.Assertion
shouldDownloadOneURL = appContextTest $ \appContext -> do 
    now <- thisInstant
    rp1 <- atomically newRepositoryProcessing1
    worldVersion <- newWorldVersion    
    results <- newTVarIO Map.empty
    let rsyncPP1 = rsyncPP_ "rsync://rpki.ripe.net/repository"
    let ppAccess = PublicationPointAccess $ rsyncPP1 :| []
    z <- runValidatorT (newScopes "fetch") $ fetchWithFallback1 
            appContext rp1 worldVersion now ppAccess (kindaFetch results)
    
    r <- readTVarIO results
    HU.assertEqual 
        "Not the same Validations" 
        (Map.lookup (getRpkiURL rsyncPP1) r)
        (Just 1)
  where 
    kindaFetch result repo = do         
        r <- randomRIO (1, 10)        
        threadDelay $ 10_000 * r
        atomically $ modifyTVar' result $ Map.unionWith (+) (Map.singleton (getRpkiURL repo) 1)         
        pure (Right repo, mempty)        
    

shouldSetAndCheckNode :: HU.Assertion
shouldSetAndCheckNode = do 
    let RsyncFetches tree = RsyncFetches mempty
    node <- newTVarIO Stub
    leaf <- newTVarIO NeverTried
    let url = rsyncURL_ "rsync://rpki.ripe.net/repository"
    let (tree', _) = setNode url tree node leaf
    case findInRsync' url tree' of 
        Nothing -> HU.assertBool "Wrong" False
        Just (z, q) -> do 
            HU.assertEqual "Wrong 2" z url        
            qq <- readTVarIO q
            HU.assertBool "Wrong 2" (Stub == qq)        


rsyncPP_ :: Text -> PublicationPoint
rsyncPP_ u = RsyncPP $ RsyncPublicationPoint $ let Right r = parseRsyncURL u in r

rsyncURL_ :: Text -> RsyncURL
rsyncURL_ u = let Right r = parseRsyncURL u in r
                    

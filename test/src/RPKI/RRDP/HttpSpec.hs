{-# LANGUAGE OverloadedStrings   #-}

module RPKI.RRDP.HttpSpec where

import Control.Lens

import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Text               as Text

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Config
import           RPKI.Repository
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.RRDP.Http
import           RPKI.TestTypes
import           RPKI.Util

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

import Network.Wreq
import Data.Either (isRight)

httpSpec :: TestTree
httpSpec = testGroup "Unit tests for Http updates" [
    HU.testCase "Should download and calculate hash and size" $ do        

        let uri = "https://rrdp.ripe.net/notification.xml"
        r <- get uri
        let body = r ^. responseBody
        let hash = sha256 body
        let size = LBS.length body

        (bs, s, status, _) :: (LBS.ByteString, Size, HttpStatus, Maybe ETag) <- 
            downloadToBS testConfig (URI uri) Nothing

        HU.assertEqual "Status" status (HttpStatus 200)
        HU.assertEqual "Size" s (Size size)
        HU.assertEqual "Body" body bs

        z <- downloadHashedBS testConfig (URI uri) Nothing hash 
                (\actual -> Left $ "Hash was " <> show actual <> " instead of " <> show hash)

        HU.assertBool ("No errors: " <> show z) (isRight z)
        let Right (bs1, s1, status1, _) = z

        HU.assertEqual "Status" status1 (HttpStatus 200)
        HU.assertEqual "Size" s1 (Size size)
        HU.assertEqual "Body" body bs1
                
  ]


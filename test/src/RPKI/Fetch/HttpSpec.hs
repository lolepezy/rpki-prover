{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}

module RPKI.Fetch.HttpSpec where

import Control.Lens

import qualified Data.ByteString.Lazy    as LBS
import           Data.Generics.Product.Typed

import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.Fetch.Http
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
        let hash_ = sha256 body
        let size = LBS.length body

        let tmpDir = configValue $ testConfig ^. #tmpDirectory
        let maxSize = testConfig ^. typed @RrdpConf . #maxSize

        (bs, s, status, _) :: (LBS.ByteString, Size, HttpStatus, Maybe ETag) <- 
                downloadToBS tmpDir (URI uri) Nothing maxSize

        HU.assertEqual "Status" status (HttpStatus 200)
        HU.assertEqual "Size" s (Size size)
        HU.assertEqual "Body" body bs
        
        z <- downloadHashedBS tmpDir (URI uri) Nothing hash_ maxSize
                (\actual -> Left $ "Hash was " <> show actual <> " instead of " <> show hash_)

        HU.assertBool ("No errors: " <> show z) (isRight z)
        let Right (bs1, s1, status1, _) = z

        HU.assertEqual "Status" status1 (HttpStatus 200)
        HU.assertEqual "Size" s1 (Size size)
        HU.assertEqual "Body" body bs1
                
  ]


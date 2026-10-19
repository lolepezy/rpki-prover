module RPKI.Fetch.HttpSpec where

import Control.Concurrent (threadDelay)
import Control.Exception (try)
import Control.Lens
import Control.Monad (forever, replicateM_)
import Data.IORef
import System.IO.Temp (withSystemTempDirectory)

import qualified Data.ByteString.Lazy    as LBS
import           Data.Generics.Product.Typed

import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.Fetch.Http
import           RPKI.TestCommons
import           RPKI.Util

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

import Network.Wreq

httpSpec :: TestTree
httpSpec = testGroup "Unit tests for Http updates" [
    HU.testCase "Aborts a download that receives too little for too long" $ do
        received <- newIORef 0
        -- 2 bytes a period at most
        r <- try $ abortIfStalled testStallLimit (URI "http://stalled") received $ 
                forever $ modifyIORef' received (+ 1) >> threadDelay 50_000
        case r of 
            Left (StalledDownload _ n _) -> HU.assertBool "Less than the minimum" (n < 10)
            Right ()                     -> HU.assertFailure "Should have been aborted",

    HU.testCase "Doesn't abort a download that keeps going" $ do
        received <- newIORef 0
        -- 50 bytes a period, for three periods
        r <- abortIfStalled testStallLimit (URI "http://slow-but-fine") received $ do 
                replicateM_ 30 $ modifyIORef' received (+ 5) >> threadDelay 10_000
                pure ("done" :: String)
        HU.assertEqual "Finished" "done" r,

    HU.testCase "Should download and calculate hash and size" $ do        
        withSystemTempDirectory "rpki-http-spec" $ \tmpDir -> do
            let maxSize = testConfig ^. typed @RrdpConf . #maxSize

            let uri = "https://rrdp.ripe.net/notification.xml"
            r <- get uri
            let body = r ^. responseBody
            let hash_ = sha256 body
            let size = LBS.length body

            (bs, s, status, _) :: (LBS.ByteString, Size, HttpStatus, Maybe ETag) <- 
                    downloadToBS tmpDir (URI uri) Nothing maxSize

            HU.assertEqual "Status" status (HttpStatus 200)
            HU.assertEqual "Size" s (Size size)
            HU.assertEqual "Body" body bs

            z <- downloadHashedBS tmpDir (URI uri) Nothing hash_ maxSize
                    (\actual -> Left $ "Hash was " <> show actual <> " instead of " <> show hash_)

            case z of
                Left e -> HU.assertFailure ("No errors: " <> e)
                Right (bs1, s1, status1, _) -> do
                    HU.assertEqual "Status" status1 (HttpStatus 200)
                    HU.assertEqual "Size" s1 (Size size)
                    HU.assertEqual "Body" body bs1
  ]



testStallLimit :: StallLimit
testStallLimit = StallLimit { periodMicros = 100_000, minBytes = 10 }

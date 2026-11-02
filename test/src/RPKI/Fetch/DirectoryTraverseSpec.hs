module RPKI.Fetch.DirectoryTraverseSpec where

import           Control.Concurrent               (threadDelay)
import           Control.Monad

import qualified Data.ByteString                  as BS
import           Data.IORef
import           Data.List                        (sort)
import           Data.Time.Clock                  (UTCTime(..))
import           Data.Time.Calendar               (fromGregorian)

import qualified Streaming.Prelude                as S

import           System.Directory
import           System.FilePath
import           System.IO.Temp                   (withSystemTempDirectory)

import           Test.Tasty
import           Test.Tasty.HUnit                 as HU

import           RPKI.Fetch.DirectoryTraverse
import           RPKI.Time


directoryTraverseSpec :: TestTree
directoryTraverseSpec = testGroup "Object files in a directory" [

    HU.testCase "Without a mark, every object file in the tree" $ withTree $ \root -> do
        writeFiles root ["a.roa", "b.cer", "sub/c.mft", "sub/deeper/d.crl", "e.txt", "sub/f.tal"]
        found <- objectFilesIn Nothing root
        found @?= (["a.roa", "b.cer", "sub/c.mft", "sub/deeper/d.crl"], 0)

  , HU.testCase "With a mark, only the object files written since then" $ withTree $ \root -> do
        writeFiles root ["old.roa", "sub/old.cer", "sub/changed.mft"]
        mark <- markInBetween
        writeFiles root ["new.roa", "sub/new.crl", "sub/changed.mft", "new.txt"]
        found <- objectFilesIn (Just mark) root
        found @?= (["new.roa", "sub/changed.mft", "sub/new.crl"], 2)

  , HU.testCase "A file replaced the way rsync does it is changed, whatever its mtime" $ withTree $ \root -> do
        writeFiles root ["object.roa", "other.roa"]
        mark <- markInBetween
        -- rsync writes a temporary file, gives it the server's mtime and renames it
        let temporary = root </> ".object.roa.XyZ123"
        BS.writeFile temporary "new content"
        setModificationTime temporary $ UTCTime (fromGregorian 2020 1 1) 0
        renameFile temporary (root </> "object.roa")
        found <- objectFilesIn (Just mark) root
        found @?= (["object.roa"], 1)
  ]

-- | Paths of the object files relative to the root, sorted, and the number
-- of skipped ones
objectFilesIn :: Maybe Instant -> FilePath -> IO ([FilePath], Int)
objectFilesIn since root = do
    skipped <- newIORef 0
    files   <- S.toList_ $ S.map fst $ objectFiles since skipped (const Nothing) root
    (sort $ map (makeRelative root) files, ) <$> readIORef skipped

-- | A moment surely later than the timestamps of the files written before
-- and earlier than those of the files written after, even with timestamps
-- of a second
markInBetween :: IO Instant
markInBetween = do
    threadDelay 1_100_000
    Now mark <- thisInstant
    threadDelay 1_100_000
    pure mark

withTree :: (FilePath -> IO a) -> IO a
withTree = withSystemTempDirectory "directory-traverse"

writeFiles :: FilePath -> [FilePath] -> IO ()
writeFiles root files =
    forM_ files $ \file -> do
        let path = root </> file
        createDirectoryIfMissing True $ takeDirectory path
        BS.writeFile path "content"

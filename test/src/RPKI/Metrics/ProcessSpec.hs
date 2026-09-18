{-# LANGUAGE OverloadedStrings #-}

module RPKI.Metrics.ProcessSpec where

import           Data.Int                (Int64)
import qualified Data.ByteString.Char8   as C8

import           System.FilePath         ((</>))
import           System.IO.Temp          (withSystemTempDirectory)

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

import           RPKI.Metrics.Process

processMetricsSpec :: TestTree
processMetricsSpec = testGroup "Process metrics" [
        procNumbersSpec
    ]

{- | The two procfs files these metrics are read from don't agree on their own
     format, and one of the keys in /proc/<pid>/io is a suffix of another, so
     both are worth pinning down.
-}
procNumbersSpec :: TestTree
procNumbersSpec = testGroup "Parsing procfs key/value files" [

        HU.testCase "Reads a value separated by a space, the way /proc/<pid>/io writes it" $
            pick "rchar" "rchar: 12345\nwchar: 7\n" >>= HU.assertEqual "rchar" (Just 12345),

        HU.testCase "Reads a value separated by a tab, the way /proc/<pid>/status writes it" $
            -- Getting this one wrong makes the peak RSS silently read as zero
            pick "VmHWM" "Name:\tsomething\nVmHWM:\t  204800 kB\n"
                >>= HU.assertEqual "VmHWM" (Just 204800),

        HU.testCase "Matches whole keys, not suffixes of them" $ do
            -- /proc/<pid>/io has `write_bytes` and `cancelled_write_bytes` both
            let procIo = "read_bytes: 100\nwrite_bytes: 200\ncancelled_write_bytes: 999\n"
            pick "write_bytes" procIo >>= HU.assertEqual "write_bytes" (Just 200)
            pick "cancelled_write_bytes" procIo >>= HU.assertEqual "cancelled" (Just 999),

        HU.testCase "Takes nothing for a key that isn't there" $
            pick "nosuchkey" "rchar: 1\nwchar: 2\n" >>= HU.assertEqual "missing key" Nothing,

        HU.testCase "Survives junk lines and a file that isn't there" $ do
            pick "rchar" "garbage\nrchar:\nrchar: notanumber\n"
                >>= HU.assertEqual "junk" Nothing
            missing <- foldProcNumbers "/definitely/not/a/file" Nothing (const $ Just $ \v _ -> Just v)
            HU.assertEqual "missing file" Nothing missing,

        HU.testCase "Keeps the last value when a key repeats" $
            pick "rchar" "rchar: 1\nrchar: 2\n" >>= HU.assertEqual "repeated" (Just 2)
    ]
  where
    -- Runs the real parser over a real file, which is what it is for
    pick :: String -> String -> IO (Maybe Int64)
    pick key content =
        withSystemTempDirectory "rpki-proc-test" $ \dir -> do
            let file = dir </> "procfile"
            writeFile file content
            foldProcNumbers file Nothing $ \k ->
                if k == C8.pack key then Just (\v _ -> Just v) else Nothing

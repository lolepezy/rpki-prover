{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.TestCommons where

import           Control.Lens
import           Control.Concurrent.STM
import           System.IO.Temp
import           System.FilePath ((</>))
import           System.Directory

import           Data.String.Interpolate.IsString

import RPKI.Config
import RPKI.AppState
import RPKI.AppContext
import RPKI.Logging
import RPKI.Store.AppSqliteStorage
import RPKI.Meta.UniqueId
import RPKI.AppMonad (runValidatorT)
import RPKI.Reporting (newScopes)
import RPKI.Resources.Resources (parseIpv4, parseIpv6)
import RPKI.Resources.Types (Ipv4Prefix, Ipv6Prefix)


-- | Parsers for statically known prefixes in tests. Partial on purpose:
-- a malformed literal in a test is a bug in the test.
readIp4 :: String -> Ipv4Prefix
readIp4 (parseIpv4 -> Just p) = p
readIp4 s = error $ "Not an IPv4 prefix: " <> s

readIp6 :: String -> Ipv6Prefix
readIp6 (parseIpv6 -> Just p) = p
readIp6 s = error $ "Not an IPv6 prefix: " <> s


testConfig :: Config
testConfig = defaultConfig


withTestContext :: (AppContext SqliteBackend -> IO b) -> IO b
withTestContext f = do
    withLogger (newLogConfig DebugL MainLog) $ \logger -> do
        dir <- createTempDirectory "/tmp" "rpki-prover-test"

        logDebug logger [i|Creating temporary directory #{dir}.|]

        let cacheDir = dir </> "cache"
        let tmpDir = dir </> "tmp"
        let talDir = dir </> "tals"


        createDirectoryIfMissing False cacheDir
        createDirectoryIfMissing False tmpDir
        createDirectoryIfMissing False talDir

        let config = testConfig 
                & #rootDirectory .~ Public dir
                & #tmpDirectory .~ Public tmpDir
                & #talDirectory .~ Public talDir
                & #cacheDirectory .~ Public cacheDir        

        (Right db, _) <- runValidatorT (newScopes "create-db") $
                    setupSqliteCache Reset logger cacheDir config

        appState <- newAppState
        database <- newTVarIO db
        let executableVersion = thisExecutableVersion
        f AppContext {             
                ..
            }
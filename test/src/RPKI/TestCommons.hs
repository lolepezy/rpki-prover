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
import RPKI.Store.Base.LMDB
import RPKI.Store.AppLmdbStorage
import RPKI.AppState
import RPKI.AppContext
import RPKI.Logging
import qualified RPKI.Store.MakeLmdb as Lmdb
import RPKI.UniqueId
import RPKI.AppMonad (runValidatorT)
import RPKI.Reporting (newScopes)


testConfig :: Config
testConfig = defaultConfig


withTestContext :: (AppContext LmdbStorage -> IO b) -> IO b
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

        (Right lmdbEnv, _) <- runValidatorT (newScopes "create-db") $ 
                    setupLmdbCache Reset logger cacheDir config

        appState <- newAppState
        database <- newTVarIO =<< makeLmdb logger lmdbEnv config 
        let executableVersion = thisExecutableVersion
        f AppContext {             
                ..
            }        
  where
    makeLmdb logger lmdbEnv config = do
        fst <$> Lmdb.createDatabase lmdbEnv logger config Lmdb.DontCheckVersion 
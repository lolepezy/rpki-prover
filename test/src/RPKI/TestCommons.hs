{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE OverloadedLabels #-}

module RPKI.TestCommons where

import           Control.Lens
import           Control.Concurrent.STM
import           System.IO.Temp
import           System.FilePath ((</>))
import           System.Directory

import           Data.String.Interpolate.IsString

import RPKI.Config
import RPKI.Store.Base.Storage (Storage)
import RPKI.AppState
import RPKI.AppContext
import RPKI.Logging
import qualified RPKI.Store.MakeLmdb as Lmdb
import RPKI.UniqueId


testConfig :: Config
testConfig = defaultConfig


withTestContext :: (forall s . Storage s => AppContext s -> IO b) -> IO b
withTestContext f = do
    withLogger (makeLogConfig DebugL MainLog) $ \logger -> do
        dir <- createTempDirectory "/tmp" "rpki-prover-test"

        logDebug logger [i|Creating temporary directory #{dir}.|]

        let cacheDir = dir </> "cache"
        let tmpDir = dir </> "tmp"
        let talDir = dir </> "tals"

        let config = testConfig 
                & #rootDirectory .~ Public dir
                & #tmpDirectory .~ Public tmpDir
                & #talDirectory .~ Public talDir
                & #cacheDirectory .~ Public cacheDir

        createDirectoryIfMissing False cacheDir
        createDirectoryIfMissing False tmpDir
        createDirectoryIfMissing False talDir

        appState <- newAppState
        database <- newTVarIO =<< makeLmdb logger cacheDir
        let executableVersion = thisExecutableVersion
        let appContext = AppContext {             
                ..
            }
        f appContext        
  where
    makeLmdb logger cachedDir = do                 
        e <- Lmdb.mkLmdb cachedDir testConfig
        fst <$> Lmdb.createDatabase e logger Lmdb.DontCheckVersion        

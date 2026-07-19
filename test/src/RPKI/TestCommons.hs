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
import RPKI.AppMonad
import RPKI.Logging
import RPKI.Reporting
import RPKI.Store.Base.Storage (Storage)
import RPKI.Store.Base.LMDB
import qualified RPKI.Store.MakeLmdb as Lmdb
import RPKI.Store.AppLmdbStorage
import RPKI.UniqueId


testConfig :: Config
testConfig = defaultConfig


withTestContext :: (forall s . Storage s => AppContext s -> IO b) -> IO b
withTestContext f = withLmdbTestContext f

withLmdbTestContext :: (AppContext LmdbStorage -> IO b) -> IO b
withLmdbTestContext f = do
    withLogger (newLogConfig InfoL MainLog) $ \logger -> do
        -- withTempDirectory "/tmp" "rpki-prover-test" $ \dir -> do

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

            appState <- newAppState
            database <- newTVarIO =<< makeLmdb logger cacheDir config

            let executableVersion = thisExecutableVersion
            f AppContext {..}
  where
    makeLmdb logger cachedDir config = do
        (Right e, _) <- runValidatorT (newScopes "setup-lmdb-cache")  $ 
                setupLmdbCache UseExisting logger cachedDir config
        fst <$> Lmdb.createDatabase e logger testConfig Lmdb.DontCheckVersion



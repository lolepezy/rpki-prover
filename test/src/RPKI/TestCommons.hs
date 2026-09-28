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

        (db, _) <- createSqliteDatabase cacheDir config True False

        appState <- newAppState
        database <- newTVarIO db
        let executableVersion = thisExecutableVersion
        f AppContext {             
                ..
            }
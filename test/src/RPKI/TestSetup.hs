{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes #-}

module RPKI.TestSetup where

import Control.Lens

import           Data.String.Interpolate.IsString

import RPKI.AppMonad
import RPKI.AppState
import RPKI.Config
import RPKI.AppContext
import RPKI.Parallel
import qualified RPKI.Store.MakeLmdb as Lmdb
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Control.Concurrent.STM
import System.Environment ( getExecutablePath )
import RPKI.Store.AppLmdbStorage
import RPKI.Store.AppStorage
import RPKI.Logging
import RPKI.Reporting
import RPKI.Util (fmtEx)
import System.Directory
import System.FilePath
import GHC.Natural (Natural)
import RPKI.Store.Base.LMDB
import RPKI.Store.Database
import Test.Tasty
import System.IO.Temp


testConfig :: Config
testConfig = defaultConfig


testAppContext :: AppLogger -> FilePath -> ValidatorT IO AppLmdbEnv
testAppContext logger root = do    

    tald   <- createDir $ root </> "tals"
    rsyncd <- createDir $ root </> "rsync"
    tmpd   <- createDir $ root </> "tmp"
    cached <- createDir $ root </> "cache"

    let defaults = defaultConfig

    let lmdbRealSize = defaults ^. #lmdbSizeMb
    lmdbEnv <- setupLmdbCache Reset logger cached lmdbRealSize

    database <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv
    
    -- Set capabilities to the values from the CLI or to all available CPUs,
    -- (disregard the HT issue for now it needs more testing).
    liftIO $ setCpuCount 2
    let parallelism = makeParallelism 2

    appBottlenecks <- liftIO $ AppBottleneck <$>
                        newBottleneckIO (parallelism ^. #cpuParallelism) <*>
                        newBottleneckIO (parallelism ^. #ioParallelism)

    let rtrConfig = Nothing

    appState <- liftIO newAppState
    tvarDatabase <- liftIO $ newTVarIO database        

    programPath <- liftIO getExecutablePath
    pure AppContext {
        appState = appState,
        database = tvarDatabase,
        appBottlenecks = appBottlenecks,
        logger = logger,        
        config = defaults               
                & #rootDirectory .~ root                
                & #talDirectory .~ tald 
                & #tmpDirectory .~ tmpd 
                & #cacheDirectory .~ cached 
                & #parallelism .~ parallelism
                & #rsyncConf . #rsyncRoot .~ rsyncd
                & #rrdpConf . #tmpRoot .~ tmpd
                & #lmdbSizeMb .~ lmdbRealSize
                & #logLevel .~ DebugL                
    }        
  where
    createDir dir = liftIO $ do 
        createDirectory dir
        pure dir


withDB :: (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> TestTree) -> TestTree
withDB = withResource (makeLmdbStuff Lmdb.createDatabase) releaseLmdb

withRpkiDir :: (IO ((FilePath, LmdbEnv), DB LmdbStorage) -> TestTree) -> TestTree
withRpkiDir = withResource (makeLmdbStuff Lmdb.createDatabase) releaseLmdb

makeLmdbStuff :: (LmdbEnv -> IO b) -> IO ((FilePath, LmdbEnv), b)
makeLmdbStuff mkStore = do 
    dir <- createTempDirectory "/tmp" "lmdb-test"
    e <- Lmdb.mkLmdb dir 1000 1000 
    store <- mkStore e
    pure ((dir, e), store)

releaseLmdb :: ((FilePath, LmdbEnv), b) -> IO ()
releaseLmdb ((dir, e), _) = do    
    Lmdb.closeLmdb e
    removeDirectoryRecursive dir    


appContextTest f = 
    withWorkerLogger DebugL $ \logger -> 
        liftIO $ withSystemTempDirectory "tests" $ \tmpRoot -> do 
            (z, _) <- runValidatorT (newScopes "test") $ testAppContext logger tmpRoot
            case z of 
                Left e  -> logError_ logger [i|Broken test #{e}|]
                Right a -> f a     
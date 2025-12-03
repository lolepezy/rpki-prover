{-# LANGUAGE RecordWildCards #-}
module RPKI.TestTypes where

import           Control.Concurrent.STM

import           System.IO.Temp

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
    withLogger (makeLogConfig defaultsLogLevel MainLog) $ \logger -> do
        appState <- newAppState
        (_, db) <- makeLmdb logger
        database <- newTVarIO db    
        let executableVersion = thisExecutableVersion
        let appContext = AppContext {             
                config = testConfig,
                ..
            }
        f appContext        
  where
    -- makeLmdb :: IO ((FilePath, LmdbEnv), b)
    makeLmdb logger = do 
        dir <- createTempDirectory "/tmp" "lmdb-test"
        e <- Lmdb.mkLmdb dir testConfig
        (store, _) <- Lmdb.createDatabase e logger Lmdb.DontCheckVersion
        pure ((dir, e), store)

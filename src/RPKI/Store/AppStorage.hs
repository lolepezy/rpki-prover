module RPKI.Store.AppStorage where

import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Store.Base.LMDB
import           RPKI.Store.AppLmdbStorage

type AppLmdbEnv = AppContext LmdbStorage

class MaintainableStorage s where
    runMaintenance :: AppContext s -> IO ()
    closeStorage :: AppContext s -> IO ()
    cleanUpStaleTx :: AppContext s -> IO Int
    getCacheFsSize :: AppContext s  -> IO Size

instance MaintainableStorage LmdbStorage where
    runMaintenance = compactStorageWithTmpDir
    closeStorage = closeLmdbStorage
    cleanUpStaleTx = cleanupReaders
    getCacheFsSize = cacheFsSize

module RPKI.Store.AppStorage where

import           RPKI.AppContext
import           RPKI.Domain              (Size)
import           RPKI.Store.Base.Storable (StorageStats)
import           RPKI.Store.Base.LMDB
import           RPKI.Store.AppLmdbStorage

type AppLmdbEnv = AppContext LmdbStorage

class MaintainableStorage s where
    runMaintenance  :: AppContext s -> IO ()
    closeStorage    :: AppContext s -> IO ()
    cleanUpStaleTx  :: AppContext s -> IO Int
    getCacheFsSize  :: AppContext s  -> IO Size
    getStorageStats :: AppContext s  -> IO StorageStats

instance MaintainableStorage LmdbStorage where
    runMaintenance  = compactStorageWithTmpDir
    closeStorage    = closeLmdbStorage
    cleanUpStaleTx  = cleanupReaders
    getCacheFsSize  = cacheFsSize
    getStorageStats = lmdbGetStats

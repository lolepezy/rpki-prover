module RPKI.Store.AppStorage where

import           RPKI.AppContext
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.InMemory
import           RPKI.Store.AppLmdbStorage

class MaintainableStorage s where
    runMaintenance :: AppContext s -> IO ()
    closeStorage :: AppContext s -> IO ()

instance MaintainableStorage LmdbStorage where
    runMaintenance = compactStorageWithTmpDir
    closeStorage = closeLmdbStorage

instance MaintainableStorage InMemoryStorage where
    runMaintenance _ = pure ()
    closeStorage _ = pure ()

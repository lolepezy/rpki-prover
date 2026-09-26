module RPKI.Store.AppStorage where

import RPKI.AppContext
import RPKI.AppTypes

data AppStorageTag

-- | Lifecycle operations for the storage backend.
-- `s` is kept as a phantom parameter so that existing call-sites that carry
-- a type-annotated AppContext compile without changes.
class MaintainableStorage s where
    runMaintenance  :: AppContext s -> IO ()
    checkpointDatabase :: AppContext s -> IO ()
    reopenStorage   :: AppContext s -> IO ()
    closeStorage    :: AppContext s -> IO ()
    cleanUpStaleTx  :: AppContext s -> IO Int
    getCacheFsSize  :: AppContext s -> IO Size

instance {-# OVERLAPPABLE #-} MaintainableStorage s where
    runMaintenance  _ = pure ()
    checkpointDatabase _ = pure ()
    reopenStorage   _ = pure ()
    closeStorage    _ = pure ()
    cleanUpStaleTx  _ = pure 0
    getCacheFsSize  _ = pure (Size 0)



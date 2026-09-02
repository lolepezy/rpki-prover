{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.AppSqliteStorage (
    SqliteBackend,
    AppSQLiteEnv,
    SqliteFlow(..),
    setupSqliteCache,
) where

import           Control.Lens
import           Control.Concurrent.MVar  (withMVar)
import           Control.Concurrent.STM   (readTVarIO)
import           Control.Monad.IO.Class   (liftIO)

import           Data.Hourglass
import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Reporting
import           RPKI.Util                (fmtEx)

import           RPKI.Store.AppStorage
import           RPKI.Store.Database      (DB(..), Tx(..))
import qualified RPKI.Store.Database      as DB
import qualified RPKI.Store.SQLite        as SQLite
import           RPKI.Store.SQLite        (SqliteDB(..))
import           RPKI.Store.Base.Storable (StorageStats(..))

import           System.Directory         (createDirectoryIfMissing, removePathForcibly)
import           System.FilePath          ((</>))


-- ---------------------------------------------------------------------------
-- Phantom type and alias
-- ---------------------------------------------------------------------------

-- | Phantom type tag for the SQLite backend; callers only see DB.
data SqliteBackend

type AppSQLiteEnv = AppContext SqliteBackend


-- ---------------------------------------------------------------------------
-- MaintainableStorage instance
-- ---------------------------------------------------------------------------

instance MaintainableStorage SqliteBackend where
    closeStorage AppContext{database} = do
        db <- readTVarIO database
        SQLite.closeDB (unDB db)
    runMaintenance  _ = pure ()   -- TODO: PRAGMA wal_checkpoint(TRUNCATE)
    reopenStorage   _ = pure ()
    cleanUpStaleTx  _ = pure 0
    getCacheFsSize  _ = pure (Size 0)
    getStorageStats _ = pure (StorageStats mempty)


-- ---------------------------------------------------------------------------
-- Setup
-- ---------------------------------------------------------------------------

data SqliteFlow = UseExisting | Reset

-- | Create or reuse the SQLite database at <cacheDir>/rpki-cache.sqlite.
-- Used for the main process and workers alike; no separate worker variant is needed.
setupSqliteCache :: SqliteFlow -> AppLogger -> FilePath -> Config -> ValidatorT IO DB
setupSqliteCache flow logger cacheDir config = do

    case flow of
        Reset -> do
            logInfo logger [i|The option `reset` is present: removing #{dbPath}.|]
            liftIO $ removePathForcibly dbPath
        UseExisting -> pure ()

    liftIO $ createDirectoryIfMissing True cacheDir

    db <- fromTry (InitE . InitError . fmtEx) $ do
        sdb <- SQLite.createDB dbPath busyTimeoutMs poolSize
        withMVar (writeConn sdb) SQLite.initSchema
        pure (DB sdb)

    version <- liftIO $ DB.roTx db $ \tx -> DB.getDatabaseVersion tx db

    case version of
        Just v | v == DB.currentDatabaseVersion ->
            logInfo logger [i|Using SQLite cache at #{dbPath} (version #{v}).|]
        _ -> do
            logInfo logger [i|Cache version mismatch; reinitialising SQLite cache at #{dbPath}.|]
            fromTry (InitE . InitError . fmtEx) $
                DB.rwTx db $ \tx -> do
                    let Tx conn = tx
                    SQLite.dropSchema conn
                    SQLite.initSchema conn
                    DB.saveCurrentDatabaseVersion tx db

    pure db
  where
    dbPath        = cacheDir </> "rpki-cache.sqlite"
    busyTimeoutMs = let Seconds s = config ^. #storageConfig . #rwTransactionTimeout
                    in fromIntegral $ s * 1000
    -- Every parsing async (there are up to `cpuParallelism` of them running
    -- concurrently, see `newParallelism`) does at least one read-only query
    -- against this pool (e.g. a hash-exists check) before/while parsing, so
    -- sizing the pool after the CPU count keeps those reads from queuing up
    -- behind each other on machines with more cores. 8 is kept as a floor so
    -- small machines don't regress from the previous hardcoded value.
    poolSize      = max 8 (fromIntegral (config ^. #parallelism . #cpuParallelism))

{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.AppSqliteStorage (
    SqliteBackend,
    AppSQLiteEnv,
    SqliteFlow(..),
    setupSqliteCache,
) where

import           Effectful
import           Control.Lens
import           Control.Concurrent.MVar  (withMVar)
import           Control.Concurrent.STM   (readTVarIO)

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


-- ---------------------------------------------------------------------------
-- Setup
-- ---------------------------------------------------------------------------

data SqliteFlow = UseExisting | Reset

-- | Create or reuse the SQLite database at <cacheDir>/rpki-cache.sqlite.
-- Used for the main process and workers alike; no separate worker variant is needed.
setupSqliteCache :: ValidatorIO es => SqliteFlow -> AppLogger -> FilePath -> Config -> Eff es DB
setupSqliteCache flow logger cacheDir config = do

    case flow of
        Reset -> do
            logInfo logger [i|The option `reset` is present: removing #{dbPath}.|]
            liftIO $ removePathForcibly dbPath
        UseExisting -> pure ()

    liftIO $ createDirectoryIfMissing True cacheDir

    db <- fromTry (InitE . InitError . fmtEx) $ do
        sdb <- SQLite.createDB dbPath busyTimeoutMs poolSize
        withMVar (writeConn sdb) (SQLite.initSchema . SQLite.rawConn)
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
                    SQLite.dropSchema (SQLite.rawConn conn)
                    SQLite.initSchema (SQLite.rawConn conn)
                    DB.saveCurrentDatabaseVersion tx db

    pure db
  where
    dbPath        = cacheDir </> "rpki-cache.sqlite"
    busyTimeoutMs = let Seconds s = config ^. #storageConfig . #rwTransactionTimeout
                    in fromIntegral $ s * 1000        
    poolSize      = max 8 (fromIntegral (config ^. #parallelism . #cpuParallelism))

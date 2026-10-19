module RPKI.Store.AppSqliteStorage (
    closeStorage,
    runMaintenance,
    checkpointDatabase,
    DbCheckResult(..),
    DbOpenMode(..),
    sqliteFileName,
    sqliteCacheFiles,
    cleanUpCacheDirectory,
    createSqliteDatabase,
    openExistingSqliteDatabase,
) where

import           Control.Lens
import           Control.Concurrent.MVar  (withMVar)
import           Control.Concurrent.STM   (readTVarIO)
import           Control.Exception        (SomeException, try)
import           Control.Monad            (forM_, when)

import           Data.Hourglass
import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Logging
import           RPKI.Time                (timedMS)
import           RPKI.Util                (fmtEx)

import qualified RPKI.Store.Database      as DB
import qualified RPKI.Store.SQLite        as SQLite
import           RPKI.Store.SQLite        (SqliteDB(..))

import           System.Directory         (createDirectoryIfMissing, doesDirectoryExist,
                                           doesFileExist, listDirectory,
                                           removeDirectoryRecursive, removeFile)
import           System.FilePath          ((</>))


closeStorage :: AppContext -> IO ()
closeStorage AppContext{database} = do
    db <- readTVarIO database
    SQLite.closeDB (DB.unDB db)

-- | Checkpoint the WAL fully and give the free pages back to the file system.
runMaintenance :: AppContext -> IO ()
runMaintenance AppContext{database} = do
    SqliteDB{..} <- DB.unDB <$> readTVarIO database
    withMVar writeConn $ \cc -> do
        SQLite.checkpointTruncate cc
        SQLite.incrementalVacuum cc
        SQLite.optimize cc

checkpointDatabase :: AppContext -> IO ()
checkpointDatabase AppContext{database, logger} = do
    sdb@SqliteDB{..} <- DB.unDB <$> readTVarIO database
    before <- SQLite.walFileSize sdb
    -- Nothing to do most of the time, so don't take the write lock for nothing
    when (before > 0) $ do
        (_, elapsed) <- timedMS $ withMVar writeConn SQLite.checkpointTruncate
        after <- SQLite.walFileSize sdb
        let inMb n = n `div` (1024 * 1024)
        logDebug logger $ if after == 0
            then [i|Checkpointed #{inMb before}mb of WAL in #{elapsed}ms.|]
            else [i|Checkpointed WAL in #{elapsed}ms, #{inMb before}mb before, |] <>
                 [i|#{inMb after}mb still left (readers are holding it).|]


data DbCheckResult = WasIncompatible | WasCompatible | DidntHaveVersion

-- | The database file, and the two files SQLite keeps beside it while it is open.
-- These are the only things that are supposed to exist in the cache directory.
sqliteFileName :: FilePath
sqliteFileName = "rpki.sqlite"

sqliteCacheFiles :: [FilePath]
sqliteCacheFiles = [ sqliteFileName, sqliteFileName <> "-wal", sqliteFileName <> "-shm" ]

-- | Delete everything in the cache directory that is not one of 'sqliteCacheFiles'.
--
-- The cache directory belongs to us alone, so anything else in it is leftovers that
-- nothing else will ever clean up:
--
--   * the LMDB cache (@data.mdb@, @lock.mdb@) of versions before 0.11, which is not
--     migrated and can easily be gigabytes,
--   * SQLite temporary files of a worker that was killed, since workers run with
--     SQLITE_TMPDIR pointing here.
--
-- Called before the database is opened, so there is nothing of SQLite's own to race
-- with, and only in the main process: workers share this directory and must not
-- delete anything in it.
cleanUpCacheDirectory :: AppLogger -> FilePath -> IO ()
cleanUpCacheDirectory logger cacheDir = do
    dirExists <- doesDirectoryExist cacheDir
    when dirExists $ do
        leftovers <- filter (`notElem` sqliteCacheFiles) <$> listDirectory cacheDir
        forM_ leftovers $ \entry -> do
            let path = cacheDir </> entry
            isDir <- doesDirectoryExist path
            removed <- try $ if isDir
                                then removeDirectoryRecursive path
                                else removeFile path
            case removed of
                Right () ->
                    logInfo logger [i|Removed #{path}, it is not a part of the cache anymore.|]
                Left (e :: SomeException) ->
                    logWarn logger [i|Could not remove #{path} from the cache directory: #{fmtEx e}.|]


newSqliteDB :: FilePath -> Config -> SQLite.WalCheckpointing -> IO SQLite.SqliteDB
newSqliteDB dbPath config walCheckpointing = 
    SQLite.createDB dbPath busyTimeoutMs walCheckpointing poolSize
  where
    poolSize      = max 2 $ fromIntegral $ config ^. #parallelism . #cpuParallelism
    -- The longest anybody may hold the write lock is how long a writer may
    -- have to wait for it. A little more than that, to give whoever is holding
    -- the lock time to be noticed and rolled back or killed.
    busyTimeoutMs = let Seconds s = config ^. #storageConfig . #txTimeout + Seconds 10
                    in fromIntegral $ s * 1000    


-- | What 'createSqliteDatabase' does to the database before handing it out.
data DbOpenMode
    -- | Delete the database files and start with an empty database of the current version.
    = Reset
    -- | Keep the data if the stored version is the current one, wipe it if it isn't,
    -- store the current version if there is none.
    | RecreateIfDifferentVersion
    -- | Don't touch the schema or the version, for those who only read what is
    -- already there. Doesn't take the write lock and doesn't run DDL against
    -- what may be an older schema.
    | UseAsIs
    deriving stock (Show, Eq)

createSqliteDatabase :: FilePath -> Config -> DbOpenMode -> IO (DB.DB, DbCheckResult)
createSqliteDatabase cacheDir config mode = do
    createDirectoryIfMissing True cacheDir

    let dbPath = cacheDir </> sqliteFileName
    when (mode == Reset) $
        mapM_ (removeIfExists . (cacheDir </>)) sqliteCacheFiles

    sdb <- newSqliteDB dbPath config SQLite.CheckpointWhenCommitting
    let db = DB.DB sdb
    dbCheck <- case mode of
        Reset                      -> initAndCheckVersion sdb db
        RecreateIfDifferentVersion -> initAndCheckVersion sdb db
        UseAsIs                    -> pure WasCompatible

    pure (db, dbCheck)
  where
    removeIfExists filePath = do
        exists <- doesFileExist filePath
        when exists $ removeFile filePath

    initAndCheckVersion sdb db = do
        SQLite.withWriteTx sdb $ \(SQLite.Tx conn) -> SQLite.initSchema (SQLite.rawConn conn)
        existingVersion <- DB.roTx db $ \tx -> DB.getDatabaseVersion tx
        case existingVersion of
            Nothing -> do
                DB.rwTx db $ \tx -> DB.saveCurrentDatabaseVersion tx
                pure DidntHaveVersion

            Just version
                | version == DB.currentDatabaseVersion -> pure WasCompatible
                | otherwise -> do
                    SQLite.withWriteTx sdb $ \tx@(SQLite.Tx conn) -> do
                        SQLite.dropSchema (SQLite.rawConn conn)
                        SQLite.initSchema (SQLite.rawConn conn)
                        DB.saveCurrentDatabaseVersion tx
                    pure WasIncompatible


-- | Open an already-initialised SQLite database without touching the schema or version.
-- Used by worker processes to avoid unnecessary write-transaction contention on startup.
-- Workers never checkpoint the WAL: they are killed when they exceed their disk IO
-- limits, and checkpointing would charge them for writing out a backlog that the 
-- other processes produced. The main process does it on a timer instead.
openExistingSqliteDatabase :: FilePath -> Config -> IO DB.DB
openExistingSqliteDatabase cacheDir config = do
    sdb <- newSqliteDB (cacheDir </> sqliteFileName) config SQLite.CheckpointedByOthers
    pure (DB.DB sdb)

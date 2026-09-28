{-# OPTIONS_GHC -Wno-orphans #-}

module RPKI.Store.SQLite (
    -- * Transaction mode
    TxMode(..),
    Tx(..),
    -- * Database handle
    SqliteDB(..),
    CachedConn(..),
    -- * Transaction runners
    withReadTx,
    withWriteTx,
    withoutTx,
    -- * Transaction timeouts
    TxKind(..),
    txKindName,
    TxTimeouts(..),
    StuckTx(..),
    TransactionTimedOut(..),
    watchTransactions,
    abortTransaction,
    -- * Lifecycle
    initConn,
    createDB,
    closeDB,
    WalCheckpointing(..),
    walFileSize,
    -- * Maintenance
    checkpointTruncate,
    incrementalVacuum,
    optimize,
    -- * Schema
    initSchema,
    dropSchema,
    -- * Cached statement API (drop-in replacements for the same-named
    -- Database.SQLite.Simple functions, operating on 'CachedConn' instead of
    -- a raw 'Connection')
    query,
    query_,
    fold_,
    queryNamed,
    execute,
    execute_,
    executeMany,
    executeNamed,
    changes,
    -- * Key helpers
    HasInt64Key(..),
    kiToBlob,
    blobToKI,
    skiToBlob,
    akiToBlob,
    blobToSKI,
    blobToAKI,
    hashToBlob,
    blobToHash,
) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay, throwTo)
import Control.Concurrent.MVar
import Control.Exception (Exception(..), asyncExceptionFromException, asyncExceptionToException,
                          finally, handleJust, mask, onException, throwIO)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class

import Data.Hourglass (Seconds(..))
import Data.IORef
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesFileExist, getFileSize)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS
import Data.Text (Text)
import qualified Data.Text             as Text

import Database.SQLite.Simple (
    Connection(..), Query, Only(..), NamedParam, ToRow, FromRow,
    Statement(..), fromQuery,
    open, close,
    openStatement, closeStatement, bindNamed, reset, withBind, nextRow)
import qualified Database.SQLite.Simple as Raw
import Database.SQLite.Simple.QQ (sql)
import Database.SQLite.Simple.ToField (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..))
import qualified Database.SQLite3.Direct as Direct

import RPKI.AppTypes (WorldVersion(..))
import RPKI.Domain   (ArtificialKey(..), ObjectKey(..), UrlKey(..), SKI(..), AKI(..), Hash(..), KI(..))


-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

data TxMode = RO | RW | NOTX

-- | Phantom wrapper over 'CachedConn' preserving the RO/RW call-site discipline.
newtype Tx (m :: TxMode) = Tx { unTx :: CachedConn }

-- | A connection plus a cache of its prepared statements, keyed by SQL text.
data CachedConn = CachedConn
    { rawConn   :: Connection
    , stmtCache :: IORef (Map Text Statement)
    , connId    :: Int              -- ^ Unique within its 'SqliteDB'
    , txSlot    :: IORef TxSlot     -- ^ The transaction it is running, for 'watchTransactions'
    }

data SqliteDB = SqliteDB
    { readPool    :: Pool CachedConn  -- ^ Shared pool for read connections
    , writeConn   :: MVar CachedConn  -- ^ Single serialised write connection
    , dbPath      :: FilePath         -- ^ To find the -wal file next to it
    , connections :: IORef (IntMap CachedConn) -- ^ Every open connection, for 'watchTransactions'
    , nextConnId  :: IORef Int
    }

{- Who folds the WAL back into the database. Workers do not checkpoint 
  the DB because it distorts their disk read/write stats, so all checkoints 
  are done by the main process.
-}
data WalCheckpointing
    = CheckpointWhenCommitting
    | CheckpointedByOthers
    deriving stock (Show, Eq, Ord)


-- ---------------------------------------------------------------------------
-- Transaction helpers
-- ---------------------------------------------------------------------------

withReadTx :: MonadIO m => SqliteDB -> (Tx 'RO -> IO a) -> m a
withReadTx SqliteDB{readPool} f = liftIO $ Pool.withResource readPool $ \cc ->
    withCachedTransaction ReadTx cc "BEGIN TRANSACTION" (f (Tx cc))

withWriteTx :: MonadIO m => SqliteDB -> (Tx 'RW -> IO a) -> m a
withWriteTx SqliteDB{writeConn} f = liftIO $ withMVar writeConn $ \cc ->
    withCachedTransaction WriteTx cc "BEGIN IMMEDIATE TRANSACTION" (f (Tx cc))

-- | sqlite-simple's own 'withTransaction' issues BEGIN, COMMIT and ROLLBACK on
-- the raw connection, which compiles and throws away a statement for each of
-- them. One validation run opens ~164k transactions, so that is ~328k
-- statements prepared and finalised to say nothing but BEGIN and COMMIT.
-- Through the statement cache each becomes a reset and a step of one that is
-- already compiled.
--
-- The semantics are sqlite-simple's @withTransactionPrivate@: masked, rolled
-- back if the action throws, committed otherwise. On top of that the
-- transaction is visible to 'watchTransactions' from BEGIN on, and if
-- 'abortTransaction' aborts it, the caller gets 'TransactionTimedOut'.
withCachedTransaction :: TxKind -> CachedConn -> Query -> IO a -> IO a
withCachedTransaction kind cc begin action =
    handleJust ownAbort (throwIO . TransactionTimedOut kind) $
        mask $ \restore -> do
            execute_ cc begin
            watched kind cc $ do
                r <- restore action `onException` rollback cc
                execute_ cc "COMMIT TRANSACTION"
                pure r
  where
    ownAbort (TxAborted aborted runningFor)
        | aborted == connId cc = Just runningFor
        | otherwise            = Nothing

rollback :: CachedConn -> IO ()
rollback cc = do
    autoCommit <- Direct.getAutoCommit $ connectionHandle $ rawConn cc
    when (not autoCommit) $
        execute_ cc "ROLLBACK TRANSACTION"


-- ---------------------------------------------------------------------------
-- Transaction timeouts
--
-- Every connection has a slot saying which transaction it is running, if
-- any. 'watchTransactions' looks at all of them once a second and hands the
-- ones running for too long to whoever started it: a worker gives up and
-- exits, the main process aborts them with 'abortTransaction'.
--
-- That costs a transaction two writes to an IORef that no other thread
-- touches unless the transaction is stuck, which is next to nothing next to
-- the BEGIN and COMMIT themselves. A timer per transaction ('timeout' or
-- direct-sqlite's 'interruptibly') is a thread or a timer manager entry per
-- transaction, and a validation run has ~164k of them.
-- ---------------------------------------------------------------------------

data TxKind = ReadTx | WriteTx
    deriving stock (Show, Eq, Ord)

txKindName :: TxKind -> Text
txKindName = \case
    ReadTx  -> "read"
    WriteTx -> "write"

-- | How long a transaction may run, from BEGIN on. Waiting for the write lock
-- before that is 'busy_timeout'\'s business.
data TxTimeouts = TxTimeouts
    { readTxTimeout  :: Seconds
    , writeTxTimeout :: Seconds
    }
    deriving stock (Show, Eq, Ord)

data TxSlot
    = NoTx
    | InTx RunningTx
    -- | 'abortTransaction' is throwing 'TxAborted' to the owner, the MVar is
    -- filled once it has. The owner doesn't leave the transaction before
    -- that, so the exception can't land anywhere else.
    | AbortingTx (MVar ())

data RunningTx = RunningTx
    { kind      :: TxKind
    , startedAt :: Word64    -- ^ 'getMonotonicTimeNSec'
    , owner     :: ThreadId
    , reported  :: Bool      -- ^ Already handed to the 'watchTransactions' callback
    }

-- | A transaction that has been running for longer than it is allowed to.
data StuckTx = StuckTx
    { kind       :: TxKind
    , runningFor :: Seconds
    , limit      :: Seconds
    , conn       :: CachedConn
    , startedAt  :: Word64
    , owner      :: ThreadId
    }

-- | What the caller of a transaction gets when 'abortTransaction' aborted it.
-- The transaction is rolled back, unless it was aborted while committing.
data TransactionTimedOut = TransactionTimedOut TxKind Seconds
    deriving stock (Show, Eq, Ord)

instance Exception TransactionTimedOut

-- | Thrown to the owner of an aborted transaction, 'withCachedTransaction'
-- turns it into 'TransactionTimedOut'. It is asynchronous so that catch-alls
-- inside the transaction let it through.
data TxAborted = TxAborted Int Seconds
    deriving stock (Show)

instance Exception TxAborted where
    toException   = asyncExceptionToException
    fromException = asyncExceptionFromException

watched :: TxKind -> CachedConn -> IO a -> IO a
watched kind CachedConn{txSlot} io = do
    startedAt <- getMonotonicTimeNSec
    owner     <- myThreadId
    writeIORef txSlot $ InTx RunningTx { reported = False, .. }
    io `finally` unwatch
  where
    unwatch =
        atomicModifyIORef' txSlot (NoTx,) >>= \case
            -- Interruptible, so if the abort hasn't landed yet, it lands here
            AbortingTx thrown -> readMVar thrown
            _                 -> pure ()

-- | Check all transactions once a second, forever, and call 'onStuck' once
-- for every transaction running for longer than 'TxTimeouts' allow.
watchTransactions :: SqliteDB -> TxTimeouts -> (StuckTx -> IO ()) -> IO ()
watchTransactions SqliteDB{connections} TxTimeouts{..} onStuck = forever $ do
    threadDelay 1_000_000
    now   <- getMonotonicTimeNSec
    conns <- readIORef connections
    forM_ conns $ \conn@CachedConn{txSlot} -> do
        -- Looking and marking it as reported is one step, so that a
        -- transaction is reported once and not after it has finished.
        stuck <- atomicModifyIORef' txSlot $ \case
            InTx tx@RunningTx{..}
                | not reported && runningFor > limit ->
                    (InTx tx { reported = True }, Just StuckTx {..})
              where
                runningFor = Seconds $ fromIntegral $ (now - startedAt) `div` 1_000_000_000
                limit      = case kind of
                                ReadTx  -> readTxTimeout
                                WriteTx -> writeTxTimeout
            slot -> (slot, Nothing)
        forM_ stuck onStuck

-- | Roll a stuck transaction back: interrupt whatever SQLite is doing on its
-- connection and throw 'TxAborted' to its owner, which gets
-- 'TransactionTimedOut' out of the transaction. Does nothing if the
-- transaction has finished by now.
--
-- Doesn't wait for any of it: 'throwTo' blocks for as long as the owner is
-- in a foreign call or masked.
abortTransaction :: StuckTx -> IO ()
abortTransaction StuckTx{ conn = CachedConn{..}, .. } = do
    thrown <- newEmptyMVar
    aborting <- atomicModifyIORef' txSlot $ \case
        InTx tx | tx.startedAt == startedAt && tx.owner == owner -> (AbortingTx thrown, True)
        slot                                                     -> (slot, False)
    when aborting $ void $ forkIO $ do
        -- The owner waits for 'thrown' before it leaves the transaction, so
        -- the connection can't be running anybody else's statements by now.
        Direct.interrupt $ connectionHandle rawConn
        throwTo owner (TxAborted connId runningFor) `finally` putMVar thrown ()

withoutTx :: MonadIO m => SqliteDB -> (Tx 'NOTX -> IO a) -> m a
withoutTx SqliteDB{readPool} f = liftIO $ Pool.withResource readPool $ \cc ->
    f (Tx cc)


-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

initConn :: Int -> WalCheckpointing -> FilePath -> IO Connection
initConn busyTimeoutMs walCheckpointing path = do
    conn <- open path
    forM_ pragmas (Raw.execute_ conn)
    pure conn
  where
    pragmas =
        [ "PRAGMA journal_mode = WAL"
        , "PRAGMA foreign_keys = ON"
        , Raw.Query $ Text.pack $ "PRAGMA busy_timeout = " <> show busyTimeoutMs
        , "PRAGMA synchronous = NORMAL"
        , "PRAGMA optimize = 0x10002"
        , "PRAGMA auto_vacuum = INCREMENTAL"
        ] <> checkpoint

    checkpoint = case walCheckpointing of
                CheckpointWhenCommitting -> []
                CheckpointedByOthers     -> [ "PRAGMA wal_autocheckpoint = 0" ]

-- | Open a connection and register it with 'watchTransactions'.
openCachedConn :: IORef Int -> IORef (IntMap CachedConn)
               -> Int -> WalCheckpointing -> FilePath -> IO CachedConn
openCachedConn nextConnId connections busyTimeoutMs walCheckpointing path = do
    rawConn   <- initConn busyTimeoutMs walCheckpointing path
    stmtCache <- newIORef Map.empty
    txSlot    <- newIORef NoTx
    connId    <- atomicModifyIORef' nextConnId $ \n -> (n + 1, n)
    let cc = CachedConn {..}
    atomicModifyIORef' connections $ \cs -> (IntMap.insert connId cc cs, ())
    pure cc

-- | Finalise every cached prepared statement, then close the connection.
-- SQLite requires all statements finalised before (or as part of) closing.
closeCachedConn :: IORef (IntMap CachedConn) -> CachedConn -> IO ()
closeCachedConn connections CachedConn{..} = do
    atomicModifyIORef' connections $ \cs -> (IntMap.delete connId cs, ())
    cached <- readIORef stmtCache
    mapM_ closeStatement (Map.elems cached)
    writeIORef stmtCache Map.empty
    close rawConn

createDB :: FilePath -> Int -> WalCheckpointing -> Int -> IO SqliteDB
createDB path busyTimeoutMs walCheckpointing poolSize = do
    connections <- newIORef IntMap.empty
    nextConnId  <- newIORef 0
    let open_ = openCachedConn nextConnId connections busyTimeoutMs walCheckpointing path
    readPool  <- Pool.newPool $
                    Pool.defaultPoolConfig
                        open_
                        (closeCachedConn connections)
                        60      -- idle TTL seconds
                        poolSize
    writeConn <- newMVar =<< open_
    pure SqliteDB{ dbPath = path, .. }

closeDB :: SqliteDB -> IO ()
closeDB SqliteDB{..} = do
    Pool.destroyAllResources readPool
    withMVar writeConn (closeCachedConn connections)


-- | Unlike the automatic passive checkpoint SQLite that runs every 1000 WAL pages, 
-- this blocks new writers only briefly and is guaranteed to shrink the -wal file 
-- when it succeeds.
checkpointTruncate :: CachedConn -> IO ()
checkpointTruncate CachedConn{rawConn} =
    Raw.execute_ rawConn "PRAGMA wal_checkpoint(TRUNCATE)"

-- | Size of the -wal file, i.e. how much is waiting to be checkpointed.
-- Zero if it isn't there, which is what a checkpoint with TRUNCATE leaves behind.
walFileSize :: SqliteDB -> IO Integer
walFileSize SqliteDB{dbPath} = do
    let wal = dbPath <> "-wal"
    exists <- doesFileExist wal
    if exists then getFileSize wal else pure 0

-- | Reclaim pages freed by deletes back into the OS, a few hundred at a
-- time so it doesn't stall other writers the way a full 'VACUUM' would.
incrementalVacuum :: CachedConn -> IO ()
incrementalVacuum CachedConn{rawConn} =
    Raw.execute_ rawConn "PRAGMA incremental_vacuum(500)"

-- | Refresh the query planner's statistics. Cheap: internally a no-op
-- unless enough rows have changed since the last run to be worth it.
optimize :: CachedConn -> IO ()
optimize CachedConn{rawConn} =
    Raw.execute_ rawConn "PRAGMA optimize"


-- ---------------------------------------------------------------------------
-- Cached statement API
--
-- Drop-in (same name, same argument order) replacements for the
-- Database.SQLite.Simple functions of the same name, operating on a
-- 'CachedConn' instead of a raw 'Connection'. 'RPKI.Store.Database' imports
-- these instead of the originals, so its ~80 call sites needed no changes
-- beyond the import list.
-- ---------------------------------------------------------------------------

-- | Look up (or prepare and cache) the statement for this exact SQL text.
checkoutStatement :: CachedConn -> Query -> IO Statement
checkoutStatement CachedConn{..} q = do
    cached <- readIORef stmtCache
    case Map.lookup (fromQuery q) cached of
        Just stmt -> pure stmt
        Nothing   -> do
            stmt <- openStatement rawConn q
            modifyIORef' stmtCache (Map.insert (fromQuery q) stmt)
            pure stmt

-- | Drain all rows of an already-bound statement, then leave it reset (ready
-- to be rebound on the next checkout).
collectRows :: FromRow r => Statement -> IO [r]
collectRows stmt = go []
  where
    go acc = nextRow stmt >>= \case
        Nothing -> pure $! reverse acc
        Just r  -> go (r : acc)

-- | Step a statement expected to return no rows (INSERT/UPDATE/DELETE).
-- 'nextRow' only ever forces its row-parser argument when a row is actually
-- returned, so this is safe to call with an unused (never-forced) type.
stepNoRows :: Statement -> IO ()
stepNoRows stmt = void (nextRow stmt :: IO (Maybe (Only Int64)))

query :: (ToRow q, FromRow r) => CachedConn -> Query -> q -> IO [r]
query cc tmpl qs = do
    stmt <- checkoutStatement cc tmpl
    withBind stmt qs (collectRows stmt)

query_ :: FromRow r => CachedConn -> Query -> IO [r]
query_ cc tmpl = do
    stmt <- checkoutStatement cc tmpl
    collectRows stmt `finally` reset stmt

-- | Fold over the result rows without materialising them.
--
-- 'query_' collects every row into a list first, which is fine for the small
-- results most callers want but ruinous for a sweep over the whole objects
-- table: a million rows of boxed columns is hundreds of megabytes that stay
-- live for as long as the traversal runs. Here each row is consumed and
-- becomes garbage immediately, so only the accumulator survives.
fold_ :: FromRow r => CachedConn -> Query -> a -> (a -> r -> IO a) -> IO a
fold_ cc tmpl z f = do
    stmt <- checkoutStatement cc tmpl
    let go !acc = nextRow stmt >>= \case
            Nothing  -> pure acc
            Just row -> go =<< f acc row
    go z `finally` reset stmt

queryNamed :: FromRow r => CachedConn -> Query -> [NamedParam] -> IO [r]
queryNamed cc tmpl params = do
    stmt <- checkoutStatement cc tmpl
    bindNamed stmt params
    collectRows stmt `finally` reset stmt

execute :: ToRow q => CachedConn -> Query -> q -> IO ()
execute cc tmpl qs = do
    stmt <- checkoutStatement cc tmpl
    withBind stmt qs (stepNoRows stmt)

execute_ :: CachedConn -> Query -> IO ()
execute_ cc tmpl = do
    stmt <- checkoutStatement cc tmpl
    stepNoRows stmt `finally` reset stmt

executeMany :: ToRow q => CachedConn -> Query -> [q] -> IO ()
executeMany cc tmpl rows = do
    stmt <- checkoutStatement cc tmpl
    forM_ rows $ \row -> withBind stmt row (stepNoRows stmt)

executeNamed :: CachedConn -> Query -> [NamedParam] -> IO ()
executeNamed cc tmpl params = do
    stmt <- checkoutStatement cc tmpl
    bindNamed stmt params
    stepNoRows stmt `finally` reset stmt

changes :: CachedConn -> IO Int
changes CachedConn{..} = Raw.changes rawConn


-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

initSchema :: Connection -> IO ()
initSchema conn = forM_ schemaDDL (Raw.execute_ conn)

-- | Drop all application tables (used for version-incompatible cache wipe).
dropSchema :: Connection -> IO ()
dropSchema conn = forM_ dropDDL (Raw.execute_ conn)

schemaDDL :: [Query]
schemaDDL =
    [ [sql|
        CREATE TABLE IF NOT EXISTS objects (
            object_key    INTEGER PRIMARY KEY,
            hash          BLOB    NOT NULL UNIQUE,
            type          TEXT    NOT NULL,
            data          BLOB,
            original      BLOB,
            world_version INTEGER NOT NULL,
            CHECK (data IS NOT NULL OR original IS NOT NULL)
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS urls (
            url_key INTEGER PRIMARY KEY,
            url     BLOB    NOT NULL UNIQUE
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS object_urls (
            object_key    INTEGER NOT NULL REFERENCES objects(object_key) ON DELETE CASCADE,
            url_key       INTEGER NOT NULL REFERENCES urls(url_key)       ON DELETE CASCADE,
            -- The last world version in which the object was seen at this URL.            
            world_version INTEGER NOT NULL,
            PRIMARY KEY (object_key, url_key)
        )
      |]
    , "CREATE INDEX IF NOT EXISTS idx_object_urls_url ON object_urls(url_key)"
    , [sql|
        CREATE TABLE IF NOT EXISTS certificates (
            object_key INTEGER NOT NULL PRIMARY KEY REFERENCES objects(object_key) ON DELETE CASCADE,
            ski        BLOB    NOT NULL,
            aki        BLOB
        )
      |]
    , "CREATE INDEX IF NOT EXISTS idx_cert_ski ON certificates(ski)"
    , [sql|
        CREATE TABLE IF NOT EXISTS manifest_meta (
            object_key      INTEGER NOT NULL PRIMARY KEY REFERENCES objects(object_key) ON DELETE CASCADE,
            aki             BLOB    NOT NULL,
            manifest_number BLOB    NOT NULL,
            meta            BLOB    NOT NULL
        )
      |]
    , "CREATE INDEX IF NOT EXISTS idx_mft_aki ON manifest_meta(aki)"
    , [sql|
        CREATE TABLE IF NOT EXISTS mft_shortcut_meta (
            aki  BLOB NOT NULL PRIMARY KEY,
            data BLOB NOT NULL
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS shortcuts (
            object_key INTEGER PRIMARY KEY REFERENCES objects(object_key) ON DELETE CASCADE,
            data       BLOB    NOT NULL
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS mft_shortcut_children (
            aki       BLOB    NOT NULL,
            file_name TEXT    NOT NULL,
            child_key INTEGER NOT NULL REFERENCES shortcuts(object_key) ON DELETE CASCADE,
            PRIMARY KEY (aki, child_key)
        )
      |]
    , "CREATE INDEX IF NOT EXISTS idx_mft_shortcut_children_child_key ON mft_shortcut_children(child_key)"
    , [sql|
        CREATE TABLE IF NOT EXISTS trust_anchors (
            ta_name     TEXT    NOT NULL PRIMARY KEY,
            ta_cert_key INTEGER REFERENCES objects(object_key),
            data        BLOB    NOT NULL,
            active      INTEGER NOT NULL DEFAULT 1,
            validations BLOB
        )
      |]
    , "CREATE INDEX IF NOT EXISTS idx_trust_anchors_active ON trust_anchors(active)"
    , [sql|
        CREATE TABLE IF NOT EXISTS repositories (
            key  BLOB NOT NULL,
            kind TEXT NOT NULL,
            data BLOB NOT NULL,
            PRIMARY KEY (key, kind)
        )
      |]
    , [sql|
            CREATE TABLE IF NOT EXISTS validation_outcomes (
                    ta_name     TEXT,
                    version     INTEGER NOT NULL,
                    validations BLOB,
                    metrics     BLOB,
                    roas        BLOB,
                    spls        BLOB,
                    aspa        BLOB,
                    bgps        BLOB,
                    gbrs        BLOB,
                    PRIMARY KEY (ta_name, version)
            )
        |]
    , "CREATE UNIQUE INDEX IF NOT EXISTS idx_validation_outcomes_common ON validation_outcomes(version) WHERE ta_name IS NULL"
    , "CREATE TABLE IF NOT EXISTS slurm       (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS jobs        (key TEXT NOT NULL PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS metadata    (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)"
    , [sql|        
        CREATE TABLE IF NOT EXISTS validated_by_version (
            key   TEXT NOT NULL PRIMARY KEY,
            value BLOB NOT NULL
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS erik_indexes (
            relay_uri BLOB NOT NULL,
            fqdn      TEXT NOT NULL,
            data      BLOB NOT NULL,
            PRIMARY KEY (relay_uri, fqdn)
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS erik_partitions (
            hash BLOB NOT NULL PRIMARY KEY,
            data BLOB NOT NULL
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS erik_index_partitions (
            relay_uri      BLOB NOT NULL,
            fqdn           TEXT NOT NULL,
            partition_hash BLOB NOT NULL,
            PRIMARY KEY (relay_uri, fqdn, partition_hash),
            FOREIGN KEY (relay_uri, fqdn)
                REFERENCES erik_indexes(relay_uri, fqdn) ON DELETE CASCADE
        )
      |]
    , "CREATE INDEX IF NOT EXISTS idx_erik_index_partitions_hash ON erik_index_partitions(partition_hash)"
    ]

dropDDL :: [Query]
dropDDL = map (\t -> "DROP TABLE IF EXISTS " <> t)
    [ "object_urls", "certificates", "manifest_meta"
    , "mft_shortcut_children", "shortcuts", "mft_shortcut_meta"
    , "trust_anchors"
    , "objects", "urls"
    , "repositories"
    , "validations", "metrics", "roas", "spls", "aspas", "gbrs", "bgps"
    , "validation_outcomes", "slurm"
    , "jobs", "metadata", "validated_by_version"
    , "erik_index_partitions", "erik_partitions", "erik_indexes"
    ]



-- ---------------------------------------------------------------------------
-- Key helpers
-- ---------------------------------------------------------------------------

class HasInt64Key a where
    toInt64   :: a -> Int64
    fromInt64 :: Int64 -> a

instance HasInt64Key ArtificialKey where
    toInt64   (ArtificialKey n) = n
    fromInt64 = ArtificialKey

instance HasInt64Key ObjectKey where
    toInt64   (ObjectKey k) = toInt64 k
    fromInt64 = ObjectKey . fromInt64

instance HasInt64Key UrlKey where
    toInt64   (UrlKey k) = toInt64 k
    fromInt64 = UrlKey . fromInt64

instance HasInt64Key WorldVersion where
    toInt64   (WorldVersion n) = n
    fromInt64 = WorldVersion

kiToBlob :: KI -> BS.ByteString
kiToBlob (KI sbs) = BSS.fromShort sbs

blobToKI :: BS.ByteString -> KI
blobToKI = KI . BSS.toShort

skiToBlob :: SKI -> BS.ByteString
skiToBlob (SKI ki) = kiToBlob ki

akiToBlob :: AKI -> BS.ByteString
akiToBlob (AKI ki) = kiToBlob ki

blobToSKI :: BS.ByteString -> SKI
blobToSKI = SKI . blobToKI

blobToAKI :: BS.ByteString -> AKI
blobToAKI = AKI . blobToKI

hashToBlob :: Hash -> BS.ByteString
hashToBlob (Hash sbs) = BSS.fromShort sbs

blobToHash :: BS.ByteString -> Hash
blobToHash = Hash . BSS.toShort


-- ---------------------------------------------------------------------------
-- ToField/FromField instances: let query/execute take these key types
-- directly instead of every call site converting to/from Int64 or BLOB by hand.
-- ---------------------------------------------------------------------------

instance ToField ObjectKey where
    toField = toField . toInt64

instance FromField ObjectKey where
    fromField f = fromInt64 <$> fromField f

instance ToField UrlKey where
    toField = toField . toInt64

instance FromField UrlKey where
    fromField f = fromInt64 <$> fromField f

instance ToField WorldVersion where
    toField = toField . toInt64

instance FromField WorldVersion where
    fromField f = fromInt64 <$> fromField f

instance ToField AKI where
    toField = toField . akiToBlob

instance FromField AKI where
    fromField f = blobToAKI <$> fromField f

instance ToField SKI where
    toField = toField . skiToBlob

instance FromField SKI where
    fromField f = blobToSKI <$> fromField f

instance ToField Hash where
    toField = toField . hashToBlob

instance FromField Hash where
    fromField f = blobToHash <$> fromField f

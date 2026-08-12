{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Store.SQLite (
    -- * Transaction mode
    TxMode(..),
    Tx(..),
    -- * Database handle
    SqliteDB(..),
    -- * Transaction runners
    withReadTx,
    withWriteTx,    
    withoutTx,    
    -- * Lifecycle
    initConn,
    createDB,
    closeDB,
    -- * Schema
    initSchema,
    dropSchema,    
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

import Control.Concurrent.MVar
import Control.Monad (forM_)
import Control.Monad.IO.Class

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Text             as Text

import Database.SQLite.Simple
import Database.SQLite.Simple.QQ (sql)
import Codec.Compression.LZ4 (compress, decompress)
import Data.Store (Store, encode, decodeEx)

import RPKI.AppTypes (WorldVersion(..))
import RPKI.Domain   (ArtificialKey(..), ObjectKey(..), UrlKey(..), SKI(..), AKI(..), Hash(..), KI(..))
import RPKI.Store.Base.Serialisation (LexOrdKey64(..))


-- ---------------------------------------------------------------------------
-- Core types
-- ---------------------------------------------------------------------------

data TxMode = RO | RW | NOTX

-- | Phantom wrapper over Connection preserving the RO/RW call-site discipline.
newtype Tx (m :: TxMode) = Tx { unTx :: Connection }

data SqliteDB = SqliteDB
    { readPool  :: Pool Connection  -- ^ Shared pool for read connections
    , writeConn :: MVar Connection  -- ^ Single serialised write connection
    }


-- ---------------------------------------------------------------------------
-- Transaction helpers
-- ---------------------------------------------------------------------------

withReadTx :: MonadIO m => SqliteDB -> (Tx 'RO -> IO a) -> m a
withReadTx SqliteDB{readPool} f = liftIO $ Pool.withResource readPool $ \conn ->
    withTransaction conn (f (Tx conn))

withWriteTx :: MonadIO m => SqliteDB -> (Tx 'RW -> IO a) -> m a
withWriteTx SqliteDB{writeConn} f = liftIO $ withMVar writeConn $ \conn ->
    withImmediateTransaction conn (f (Tx conn))

withoutTx :: MonadIO m => SqliteDB -> (Tx 'NOTX -> IO a) -> m a
withoutTx SqliteDB{readPool} f = liftIO $ Pool.withResource readPool $ \conn ->
    f (Tx conn)


-- ---------------------------------------------------------------------------
-- Lifecycle
-- ---------------------------------------------------------------------------

initConn :: Int -> FilePath -> IO Connection
initConn busyTimeoutMs path = do
    conn <- open path
    forM_ pragmas (execute_ conn)
    pure conn
  where
    pragmas =
        [ "PRAGMA journal_mode = WAL"
        , "PRAGMA foreign_keys = ON"
        , Query $ Text.pack $ "PRAGMA busy_timeout = " <> show busyTimeoutMs
        , "PRAGMA synchronous = NORMAL"
        , "PRAGMA optimize = 0x10002"
        ]

createDB :: FilePath -> Int -> Int -> IO SqliteDB
createDB path busyTimeoutMs poolSize = do
    readPool  <- Pool.createPool
                    (initConn busyTimeoutMs path)
                    close
                    1       -- stripes
                    60      -- idle TTL seconds
                    poolSize
    writeConn <- newMVar =<< initConn busyTimeoutMs path
    pure SqliteDB{..}

closeDB :: SqliteDB -> IO ()
closeDB SqliteDB{..} = do
    Pool.destroyAllResources readPool
    withMVar writeConn close


-- ---------------------------------------------------------------------------
-- Schema
-- ---------------------------------------------------------------------------

initSchema :: Connection -> IO ()
initSchema conn = forM_ schemaDDL (execute_ conn)

-- | Drop all application tables (used for version-incompatible cache wipe).
dropSchema :: Connection -> IO ()
dropSchema conn = forM_ dropDDL (execute_ conn)

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
            url     TEXT    NOT NULL UNIQUE
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS object_urls (
            object_key INTEGER NOT NULL REFERENCES objects(object_key) ON DELETE CASCADE,
            url_key    INTEGER NOT NULL REFERENCES urls(url_key)       ON DELETE CASCADE,
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
        CREATE TABLE IF NOT EXISTS mft_shortcut_children (
            aki  BLOB NOT NULL PRIMARY KEY,
            data BLOB NOT NULL
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS trust_anchors (
            ta_name TEXT NOT NULL PRIMARY KEY,
            data    BLOB NOT NULL
        )
      |]
    , [sql|
        CREATE TABLE IF NOT EXISTS repositories (
            key  BLOB NOT NULL,
            kind TEXT NOT NULL,
            data BLOB NOT NULL,
            PRIMARY KEY (key, kind)
        )
      |]
    , "CREATE TABLE IF NOT EXISTS validations (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS metrics     (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS roas        (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS spls        (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS aspas       (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS gbrs        (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS bgps        (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS slurm       (key INTEGER PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS versions    (key BLOB NOT NULL PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS jobs        (key TEXT NOT NULL PRIMARY KEY, value BLOB NOT NULL)"
    , "CREATE TABLE IF NOT EXISTS metadata    (key TEXT NOT NULL PRIMARY KEY, value TEXT NOT NULL)"
    , [sql|
        CREATE TABLE IF NOT EXISTS validated_by_version (
            key   TEXT NOT NULL PRIMARY KEY,
            value BLOB NOT NULL
        )
      |]
    ]

dropDDL :: [Query]
dropDDL = map (\t -> "DROP TABLE IF EXISTS " <> t)
    [ "object_urls", "certificates", "manifest_meta"
    , "objects", "urls"
    , "mft_shortcut_meta", "mft_shortcut_children"
    , "trust_anchors", "repositories"
    , "validations", "metrics", "roas", "spls", "aspas", "gbrs", "bgps", "slurm"
    , "versions", "jobs", "metadata", "validated_by_version"
    ]



-- ---------------------------------------------------------------------------
-- Key helpers
-- ---------------------------------------------------------------------------

class HasInt64Key a where
    toInt64   :: a -> Int64
    fromInt64 :: Int64 -> a

instance HasInt64Key ArtificialKey where
    toInt64   (ArtificialKey (LexOrdKey64 n)) = n
    fromInt64 = ArtificialKey . LexOrdKey64

instance HasInt64Key ObjectKey where
    toInt64   (ObjectKey k) = toInt64 k
    fromInt64 = ObjectKey . fromInt64

instance HasInt64Key UrlKey where
    toInt64   (UrlKey k) = toInt64 k
    fromInt64 = UrlKey . fromInt64

instance HasInt64Key WorldVersion where
    toInt64   (WorldVersion (LexOrdKey64 n)) = n
    fromInt64 = WorldVersion . LexOrdKey64

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

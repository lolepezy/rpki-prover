{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}

module RPKI.Store.Database (
    -- * Public database handle (implementation hidden)
    DB(..),
    -- * Transaction types (re-exported so callers need only this module)
    Tx(..),
    TxMode(..),
    -- * Transaction runners
    withReadTx, withWriteTx, roTx, rwTx, roTxT, rwTxT,
    -- * Validator integration
    roAppTx, rwAppTx, appTx, roAppTxEx, rwAppTxEx, appTxEx,
    TxRollbackException(..),
    -- * Constants
    currentDatabaseVersion,
    databaseVersionKey, validatedByVersionKey,
    -- * DTOs
    MftShortcutMeta(..), ShortcutChildRow(..),
    -- * Query functions
    getKeyByHash, getObjectKey, getByHash, getKeyedByHash,
    getMultiLocationShortcutChildren,
    getByUri, getKeysByUri,
    getObjectByKey, getLocatedByKey,
    getLocationsByKey, getHashByKey,
    saveObject, saveStorableObject, prepareObject, insertPreparedObject,
    getObjectMeta, linkObjectToUrl,
    hashExists, existingHashes, getObjectsByHashes,
    deleteObjectByHash, deleteObjectByKey,
    -- * Erik protocol functions
    getErikIndex, saveErikIndex, getAllErikIndexes,
    getErikPartition, existingErikPartitions, saveErikPartition, deleteOrphanedErikPartitions,
    getMftsForAKI, findAllMftsByAKI, getMftByKey,
    getMftShorcut, getMftShorcutMeta, getMftShorcutChildrenLight, getMftShorcutChildrenFull,
    getMftShortcutChildFileName,
    saveMftShorcutMeta, insertMftShortcutChildren, deleteMftShortcutChildren,
    deleteMftShortcut, getBySKI, getFirstCaCertBySKI, getTaCertByKey,
    markAsValidated,
    saveTA, getTA, getTAs, setActiveTAs,
    saveTaValidations, getTaValidations,
    versionsBackwards, previousVersion, getLatestVersion,
    getValidationsPerTA, getMetricsPerTA, getCommonMetrics,
    getValidationOutcomes,
    getVrps, getVrpsForTA, getRoas, getAspas, getGbrs, getBgps, getSpls,
    saveValidationVersion, deleteValidationVersion,
    saveSlurm, getSlurm, addCommonValidations, getLatestVersions,
    updateRrdpMeta, updateRrdpMetaM,
    getPublicationPoints, getRepository,
    getRrdpRepository, getRsyncRepository, getRsyncRepositories,
    getRsyncAnything,
    saveRepositories, saveRepositoryValidationStates,
    saveRsyncRepositories, saveRsyncValidationStates,
    saveRsyncAnything, getRepositories,
    saveErikRepositories, saveErikRepositoryValidationStates,
    getErikRepository, getErikRepositories,
    setJobCompletionTime, allJobs,
    getDatabaseVersion, saveCurrentDatabaseVersion,
    updateValidatedByVersionMap,
    getObjectsStats,
    CleanUpResult(..), DeletionCriteria(..),
    deleteOldestVersionsIfNeeded,
    deleteStaleContent, deleteDanglingUrls,
    getAll, getMftMeta, getGbrObjects, getRtrPayloads,
    storageError,
    -- * Encoding helpers (for AppSqliteStorage etc.)
    encodeSO, decodeSO,
) where

import           Effectful
import           Effectful.Error.Static           (tryError)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe

import           Data.Generics.Product.Typed

import qualified Data.List                as List
import           Data.Maybe               (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.List.NonEmpty       as NonEmpty
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Data.String              (fromString)
import qualified Data.Map.Strict          as Map
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import           Data.Int                 (Int64)
import           Data.Ord                 (Down(..))
import           Data.Tuple.Strict

import           GHC.Generics
import           GHC.Natural
import           Text.Read

import           Database.SQLite.Simple hiding (query, query_, queryNamed, execute, execute_, executeMany, executeNamed, changes)
import           Database.SQLite.Simple.QQ (sql)
import           Database.SQLite.Simple.ToField (ToField)
import           Data.Bits                (shiftR, (.&.))
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.TAL
import           RPKI.RRDP.Types
import           RPKI.SLURM.Types
import           RPKI.Repository

import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Base.Storable
-- Cached-statement 'query'/'execute'/etc. (operating on 'CachedConn') instead
-- of the raw Database.SQLite.Simple ones: see 'RPKI.Store.SQLite.CachedConn'.
import           RPKI.Store.SQLite            (Tx(..), SqliteDB(..), TxMode(..),
                                                query, query_, queryNamed,
                                                execute, execute_, executeMany, executeNamed, changes)
import qualified RPKI.Store.SQLite            as SQLite
import           RPKI.Store.Types
import           RPKI.Validation.Types

import           RPKI.Util                (ifJustM, fmtEx)
import           RPKI.Parse.Internal.Common (tbsSiaExt)
import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.AppTypes
import           RPKI.RTR.Types
import           RPKI.Time


-- ---------------------------------------------------------------------------
-- DB newtype: hides SqliteDB from callers
-- ---------------------------------------------------------------------------

-- | Opaque database handle. Import only from this module; never from SQLite.
newtype DB = DB { unDB :: SqliteDB }

-- | Transaction wrappers that accept the opaque DB.
withReadTx :: MonadIO m => DB -> (Tx 'RO -> IO a) -> m a
withReadTx (DB sdb) = SQLite.withReadTx sdb

withWriteTx :: MonadIO m => DB -> (Tx 'RW -> IO a) -> m a
withWriteTx (DB sdb) = SQLite.withWriteTx sdb

roTx :: MonadIO m => DB -> (Tx 'RO -> IO a) -> m a
roTx = withReadTx

rwTx :: MonadIO m => DB -> (Tx 'RW -> IO a) -> m a
rwTx = withWriteTx

roTxT :: MonadIO m => TVar DB -> (Tx 'RO -> IO a) -> m a
roTxT tdb f = liftIO $ do
    db <- readTVarIO tdb
    roTx db f

rwTxT :: MonadIO m => TVar DB -> (Tx 'RW -> IO a) -> m a
rwTxT tdb f = liftIO $ do
    db <- readTVarIO tdb
    rwTx db f

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- Increment whenever any serialised type changes incompatibly.
currentDatabaseVersion :: Integer
currentDatabaseVersion = 62

databaseVersionKey, validatedByVersionKey :: Text
databaseVersionKey    = "database-version"
validatedByVersionKey = "validated-by-version-map"


-- ---------------------------------------------------------------------------
-- DTOs that are not store wrappers
-- ---------------------------------------------------------------------------

data MftShortcutMeta = MftShortcutMeta
    { key            :: ObjectKey
    , notBefore      :: Instant
    , notAfter       :: Instant
    , serial         :: Serial
    , manifestNumber :: Serial
    , crlShortcut    :: CrlShortcut
    , hasIssues      :: Bool
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

-- | One child of a manifest shortcut, as it's written: its shortcut goes to
-- `shortcuts`, the row itself to `mft_shortcut_ca_children` for a CA
-- certificate and to `mft_shortcut_payload_children` otherwise.
data ShortcutChildRow = ShortcutChildRow
    { childKey :: ObjectKey
    , fileName :: Text
    , caCert   :: Maybe CaChildValidity
    , shortcut :: BS.ByteString
    }
    deriving stock (Show, Eq, Generic)

instance {-# OVERLAPPING #-} WithValidityPeriod MftShortcutMeta where
    getValidityPeriod MftShortcutMeta {..} = ValidityPeriod notBefore notAfter


-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

onlyValue :: [Only a] -> Maybe a
onlyValue []          = Nothing
onlyValue (Only v : _) = Just v

-- | Encode a pre-serialised object wrapper as compressed bytes.
encodeSO :: AsStorable a => StorableObject a -> BS.ByteString
encodeSO = unStorable . toStorable . Compressed

-- | Decode a StorableObject from compressed bytes.
decodeSO :: AsStorable a => BS.ByteString -> StorableObject a
decodeSO bs = unCompressed (fromStorable (Storable bs))

storageError :: SomeException -> AppError
storageError = StorageE . StorageError . fmtEx

-- | Encode a non-negative Integer as a length-prefixed big-endian BLOB.
-- Length-then-bytes encoding means memcmp / SQLite BLOB ORDER BY gives numeric order.
serialToBlob :: Integer -> BS.ByteString
serialToBlob n = BS.pack (fromIntegral (length bytes) : bytes)
  where
    bytes = go n []
    go 0 acc = acc
    go m acc = go (m `shiftR` 8) (fromIntegral (m .&. 0xFF) : acc)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
        let (h, t) = splitAt n xs
        in h : chunksOf n t

-- | Split `keys` into SQLite-parameter-limit-safe batches (see `chunksOf`),
-- generating a ":k1, :k2, ..." placeholder list and matching named params
-- for each batch, ready to splice into an `IN (...)` clause.
--
-- Batch sizes are rounded up to a power of two, padding with a repeat of the
-- first key -- duplicates inside `IN (...)` are harmless. Without the padding
-- every distinct batch size yields a distinct SQL string, and since prepared
-- statements are cached per SQL text (see 'RPKI.Store.SQLite.CachedConn'),
-- a full spread of arities costs ~46mb of SQLite heap per connection against
-- ~0.5mb for the 10 power-of-two buckets.
inClauseBatches :: ToField k => [k] -> [(Text, [NamedParam])]
inClauseBatches = mapMaybe toBatch . chunksOf maxBatch
  where
    maxBatch = 512

    toBatch []             = Nothing
    toBatch batch@(first : _) =
        let padded = batch <> replicate (bucketSize (length batch) - length batch) first
            keyParams = zip [1 :: Int ..] padded
            placeholders = Text.intercalate ", "
                [":k" <> Text.pack (show i) | (i, _) <- keyParams]
            params = [ (":k" <> Text.pack (show i)) := key | (i, key) <- keyParams ]
        in Just (placeholders, params)

    -- Round up to the next power of two, so only ~10 distinct arities
    -- (and so ~10 cached prepared statements) ever exist per call site.
    bucketSize n = fromMaybe maxBatch $ List.find (>= n) $ takeWhile (<= maxBatch) $ iterate (* 2) 1

-- ---------------------------------------------------------------------------
-- Object functions
-- ---------------------------------------------------------------------------

getKeyByHash :: MonadIO m => Tx mode -> Hash -> m (Maybe ObjectKey)
getKeyByHash (Tx conn) h = liftIO $ do
    rows <- query conn
        "SELECT object_key FROM objects WHERE hash = ?"
        (Only h)
    pure $ onlyValue rows

getObjectKey :: MonadIO m => Tx mode -> Hash -> m (Maybe ObjectKey)
getObjectKey = getKeyByHash

getByHash :: MonadIO m => Tx mode -> Hash -> m (Maybe (Located RpkiObjectLifecycle))
getByHash tx h = ((^. #object) <$>) <$> getKeyedByHash tx h

getKeyedByHash :: MonadIO m => Tx mode -> Hash -> m (Maybe (Keyed (Located RpkiObjectLifecycle)))
getKeyedByHash tx h = liftIO $ runMaybeT $ do
    objectKey <- MaybeT $ getKeyByHash tx h
    z         <- MaybeT $ getLocatedByKey tx objectKey
    pure $ Keyed z objectKey

getHashByKey :: MonadIO m => Tx mode -> ObjectKey -> m (Maybe Hash)
getHashByKey (Tx conn) k = liftIO $ do
    rows <- query conn "SELECT hash FROM objects WHERE object_key = ?" (Only k)
    pure $ case rows of
        [Only hash] -> Just hash
        _           -> Nothing

getByUri :: MonadIO m => Tx mode -> RpkiURL -> m [Located RpkiObjectLifecycle]
getByUri tx uri = liftIO $ do
    keys_ <- getKeysByUri tx uri
    catMaybes <$> mapM (getLocatedByKey tx) keys_

getKeysByUri :: MonadIO m => Tx mode -> RpkiURL -> m [ObjectKey]
getKeysByUri (Tx conn) uri = liftIO $ do
    rows <- query conn
        [sql|
            SELECT ou.object_key
            FROM object_urls ou JOIN urls u USING(url_key)
            WHERE u.url = ?
        |]
        (Only (serialiseField uri))
    pure $ map fromOnly rows

getObjectByKey :: MonadIO m => Tx mode -> ObjectKey -> m (Maybe RpkiObjectLifecycle)
getObjectByKey (Tx conn) k = liftIO $ do
    rows <- query conn
        "SELECT data FROM objects WHERE object_key = ? AND data IS NOT NULL"
        (Only k)
    pure $ case rows of
        [Only bs] -> let StorableObject{object = ro} = decodeSO bs :: StorableObject RpkiObjectLifecycle
                     in Just ro
        _         -> Nothing

-- | An object with no locations at all (fetched via an Erik relay) is still
-- found by key -- it is only the location, not the object, that's optional.
getLocatedByKey :: MonadIO m => Tx mode -> ObjectKey -> m (Maybe (Located RpkiObjectLifecycle))
getLocatedByKey tx k = liftIO $ runMaybeT $ do
    obj       <- MaybeT $ getObjectByKey tx k
    locations <- MaybeT $ Just <$> getLocationsByKey tx k
    pure $ Located locations obj

-- | Keys of every object published at more than one location.
-- Also we only care about objects that are either children of
-- manifest shortcuts or manifest shortcuts themselves.
getMultiLocationShortcutChildren :: MonadIO m => Tx mode -> m (Set.Set ObjectKey)
getMultiLocationShortcutChildren (Tx conn) = liftIO $ do
    rows <- query_ conn
        [sql|
            WITH multi_location AS (
                SELECT object_key FROM object_urls
                GROUP BY object_key HAVING COUNT(*) > 1
            )
            SELECT object_key FROM multi_location m
            WHERE EXISTS (
                SELECT 1 FROM mft_shortcut_payload_children
                WHERE child_key = m.object_key
            ) OR EXISTS (
                SELECT 1 FROM mft_shortcut_ca_children
                WHERE child_key = m.object_key
            ) OR EXISTS (
                SELECT 1 FROM manifest_meta
                WHERE object_key = m.object_key
            )
        |]
    pure $! Set.fromList $ map fromOnly rows

getLocationsByKey :: MonadIO m => Tx mode -> ObjectKey -> m (Maybe Locations)
getLocationsByKey (Tx conn) k = liftIO $ do
    rows <- query conn
        [sql|
            SELECT u.url FROM urls u
            JOIN object_urls ou USING(url_key)
            WHERE ou.object_key = ?
        |]
        (Only k)
    let urls = map (deserialiseField . fromOnly) rows :: [RpkiURL]
    pure $ case urls of
        [] -> Nothing
        us -> Locations <$> toNESet us

saveObject :: MonadIO m
           => Tx 'RW
           -> RpkiObjectLifecycle
           -> Maybe Size
           -> WorldVersion
           -> m ObjectKey
saveObject tx lifecycle = saveStorableObject tx (toStorableObject (Compressed lifecycle))


saveStorableObject :: MonadIO m
                => Tx 'RW
                -> StorableObject (Compressed RpkiObjectLifecycle)
                -> Maybe Size
                -> WorldVersion
                -> m ObjectKey
saveStorableObject tx so size = insertPreparedObject tx (prepareObject so size)


-- | Everything about storing an object that does not need the transaction.
-- Pure, so a caller can run it on whatever thread did the parsing.
--
-- The size of the object's DER is whatever the caller knows, the bytes of an 
-- unparsed object are here anyway.
prepareObject :: StorableObject (Compressed RpkiObjectLifecycle) -> Maybe Size -> PreparedObject
prepareObject StorableObject { object = Compressed lifecycle, storable = Storable payload } knownSize =
    PreparedObject {..}
  where
    hash       = getHash lifecycle
    objectType = getRpkiObjectType lifecycle
    original   = case lifecycle of
                    OriginalRO (ObjectOriginal blob) _ _ _ -> Just blob
                    WellStructuredRO _                     -> Nothing
    size       = case lifecycle of
                    OriginalRO (ObjectOriginal blob) _ _ _ -> Just $ Size $ fromIntegral $ BS.length blob
                    WellStructuredRO _                     -> knownSize
    validity   = case lifecycle of
                    OriginalRO {}        -> Nothing
                    WellStructuredRO ro  -> Just $ effectiveValidityPeriod ro
    indexEntry = case lifecycle of
                    WellStructuredRO (CerRO c)   -> Just $ CertificateIndex (getSKI c) (getAKI c)
                    WellStructuredRO (MftRO mft) ->
                        let Manifest {..} = mft ^. #content
                            sia = tbsSiaExt $ BSS.fromShort $ mft ^. #eeCert . #encoded
                        in (\aki_ -> ManifestIndex aki_ mftNumber thisTime nextTime sia) <$> getAKI mft
                    _                            -> Nothing


insertPreparedObject :: MonadIO m
                     => Tx 'RW
                     -> PreparedObject
                     -> WorldVersion
                     -> m ObjectKey
insertPreparedObject (Tx conn) PreparedObject {..} wv = liftIO $ do
    existing <- query conn
        "SELECT object_key, original IS NOT NULL FROM objects WHERE hash = ?" (Only hash)
    case (existing, original) of
        -- The same bytes can be stored unparsed first (e.g. a TA certificate
        -- that failed RRDP prevalidation) and then come as a parsed object.
        -- Replace the unparsed copy, otherwise the parsed one is never readable.
        ((objectKey, True) : _, Nothing) -> do
            execute conn
                [sql|UPDATE objects SET type = ?, data = ?, original = NULL, world_version = ?,
                                        not_before = ?, not_after = ?
                     WHERE object_key = ?|]
                (typ, payload, wv, notBefore, notAfter, objectKey)
            saveIndexes objectKey
            pure objectKey

        ((objectKey, _) : _, _) -> pure objectKey

        ([], _) -> do
            [Only objectKey] <- query conn
                [sql|INSERT INTO objects(hash, type, size, not_before, not_after, data, original, world_version)
                     VALUES (?, ?, ?, ?, ?, ?, ?, ?) RETURNING object_key|]
                ((hash, typ, unSize <$> size, notBefore, notAfter) :. (payload, original, wv))

            saveIndexes objectKey
            pure objectKey
  where
    typ = show objectType

    notBefore = toNanoseconds . (.notBefore) <$> validity
    notAfter  = toNanoseconds . (.notAfter)  <$> validity

    saveIndexes objectKey =
        case indexEntry of
            Just (CertificateIndex ski aki_) ->
                execute conn
                    [sql|INSERT OR IGNORE INTO certificates(object_key, ski, aki) VALUES (?, ?, ?)|]
                    (objectKey, ski, aki_)
            Just (ManifestIndex aki_ mftNumber thisTime nextTime eeSia) ->
                let meta = MftMeta { key = objectKey, .. }
                    Serial mftNum = mftNumber
                in execute conn
                    [sql|
                        INSERT OR IGNORE INTO manifest_meta(object_key, aki, manifest_number, meta, ee_sia)
                        VALUES (?, ?, ?, ?, ?)
                    |]
                    (objectKey, aki_, serialToBlob mftNum, serialiseField meta, eeSia)
            Nothing -> pure ()


getObjectMeta :: MonadIO m => Tx mode -> ObjectKey -> m (Maybe ObjectMeta)
getObjectMeta (Tx conn) k = liftIO $ do
    rows <- query conn
        "SELECT world_version, type FROM objects WHERE object_key = ?"
        (Only k)
    pure $ case rows of
        [(wv, typText)] -> case readMaybe typText of
            Just typ -> Just $ ObjectMeta wv typ
            Nothing  -> Nothing
        _ -> Nothing

-- | Record that the object is published at the given URL as of `worldVersion`.
--
-- The version is refreshed on every call, so an association only stays old if
-- the object stopped being seen at that URL (it moved to another repository,
-- say). 'deleteStaleObjectUrls' expires those, which is what makes the
-- "object has multiple locations" warning go away after a migration.
linkObjectToUrl :: MonadIO m => Tx 'RW -> RpkiURL -> ObjectKey -> WorldVersion -> m ()
linkObjectToUrl (Tx conn) rpkiURL objectKey worldVersion = liftIO $ do
    [Only urlKey] <- query conn
        [sql|INSERT INTO urls(url) VALUES (?)
             ON CONFLICT(url) DO UPDATE SET url = excluded.url
             RETURNING url_key|]
        (Only (serialiseField rpkiURL))
    execute conn
        [sql|INSERT INTO object_urls(object_key, url_key, world_version) VALUES (?, ?, ?)
             ON CONFLICT(object_key, url_key) DO UPDATE SET world_version = excluded.world_version|]
        (objectKey, urlKey :: UrlKey, worldVersion)

hashExists :: MonadIO m => Tx mode -> Hash -> m Bool
hashExists (Tx conn) h = liftIO $ do
    rows <- query conn "SELECT 1 FROM objects WHERE hash = ?" (Only h)
    pure $ not (null (rows :: [Only Int]))

{- | Which of the given hashes are already in the object store.

     One query per batch of 512 instead of one transaction per hash, which is
     what makes it affordable for an Erik fetch to ask "what am I missing?"
     about every object a repository publishes.
-}
existingHashes :: MonadIO m => Tx mode -> [Hash] -> m (Set.Set Hash)
existingHashes (Tx conn) hashes = liftIO $
    fmap (Set.fromList . concat) $
        forM (inClauseBatches hashes) $ \(placeholders, params) -> do
            rows <- queryNamed conn
                (fromString $ Text.unpack $
                    "SELECT hash FROM objects WHERE hash IN (" <> placeholders <> ")")
                params
            pure $ map fromOnly rows

-- | Bulk 'getObjectByKey' by hash. Hashes with no object, or with a row that
-- has been stripped of its payload, are simply absent from the result.
getObjectsByHashes :: MonadIO m => Tx mode -> [Hash] -> m [(Hash, RpkiObjectLifecycle)]
getObjectsByHashes (Tx conn) hashes = liftIO $
    fmap concat $
        forM (inClauseBatches hashes) $ \(placeholders, params) -> do
            rows <- queryNamed conn
                (fromString $ Text.unpack $
                    "SELECT hash, data FROM objects WHERE data IS NOT NULL AND hash IN ("
                        <> placeholders <> ")")
                params
            pure [ (h, ro)
                 | (h, bs) <- rows
                 , let StorableObject { object = ro } = decodeSO bs :: StorableObject RpkiObjectLifecycle ]

-- ---------------------------------------------------------------------------
-- Erik protocol functions
-- https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-erik-protocol/
-- ---------------------------------------------------------------------------

-- | The last index seen for this (relay, scope) pair, kept so a fetch can tell
-- whether anything changed since the previous synchronisation.
getErikIndex :: MonadIO m => Tx mode -> URI -> FQDN -> m (Maybe ErikIndex)
getErikIndex (Tx conn) relayUri (FQDN fqdn) = liftIO $ do
    rows <- query conn
        "SELECT data FROM erik_indexes WHERE relay_uri = ? AND fqdn = ?"
        (serialiseField relayUri, fqdn)
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

-- | Replaces the index and the partition hashes it refers to. The membership
-- rows are what `deleteOrphanedErikPartitions` reads, so they have to move in
-- the same transaction as the index itself.
saveErikIndex :: MonadIO m => Tx 'RW -> URI -> FQDN -> ErikIndex -> m ()
saveErikIndex (Tx conn) relayUri (FQDN fqdn) index_ = liftIO $ do
    execute conn
        "INSERT OR REPLACE INTO erik_indexes(relay_uri, fqdn, data) VALUES (?, ?, ?)"
        (relayBlob, fqdn, serialiseField index_)

    existing_ :: [Only Hash] <- query conn
        "SELECT partition_hash FROM erik_index_partitions WHERE relay_uri = ? AND fqdn = ?"
        (serialiseField relayUri, fqdn)

    let existing = Set.fromList [ h | Only h <- existing_]
        new      = Set.fromList [ ref.hash | ref <- index_.partitionList]
        toDelete = Set.toList (existing `Set.difference` new)

    forM_ (inClauseBatches toDelete) $ \(placeholders, params) ->
        executeNamed conn
            (fromString $ Text.unpack $
                "DELETE FROM erik_index_partitions "
             <> "WHERE relay_uri = ? AND fqdn = ? AND hash IN (" <> placeholders <> ")")
            params

    executeMany conn
        [sql|INSERT OR IGNORE INTO erik_index_partitions(relay_uri, fqdn, partition_hash)
             VALUES (?, ?, ?)|]
        [ (relayBlob, fqdn, ref.hash) | ref <- index_.partitionList, not $ ref.hash `Set.member` existing ]

  where
    relayBlob = serialiseField relayUri

getAllErikIndexes :: MonadIO m => Tx mode -> m [(URI, FQDN, ErikIndex)]
getAllErikIndexes (Tx conn) = liftIO $ do
    rows <- query_ conn "SELECT relay_uri, fqdn, data FROM erik_indexes"
    pure [ (deserialiseField relayUri, FQDN fqdn, deserialiseField blob)
         | (relayUri, fqdn, blob) <- rows ]

getErikPartition :: MonadIO m => Tx mode -> Hash -> m (Maybe ErikPartition)
getErikPartition (Tx conn) h = liftIO $ do
    rows <- query conn "SELECT data FROM erik_partitions WHERE hash = ?" (Only h)
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

{- | Which of the partitions an index names are already cached.

     Only the hash column, deliberately: the caller wants to know which
     partitions to download, and pulling the blobs here would mean holding every
     manifest list in the index in memory at once.
-}
existingErikPartitions :: MonadIO m => Tx mode -> [Hash] -> m (Set.Set Hash)
existingErikPartitions (Tx conn) hashes = liftIO $
    fmap (Set.fromList . concat) $
        forM (inClauseBatches hashes) $ \(placeholders, params) -> do
            rows <- queryNamed conn
                (fromString $ Text.unpack $
                    "SELECT hash FROM erik_partitions WHERE hash IN ("
                        <> placeholders <> ")")
                params
            pure $ map fromOnly rows

saveErikPartition :: MonadIO m => Tx 'RW -> Hash -> ErikPartition -> m ()
saveErikPartition (Tx conn) h partition = liftIO $
    execute conn
        "INSERT OR REPLACE INTO erik_partitions(hash, data) VALUES (?, ?)"
        (h, serialiseField partition)

-- | Partitions stop being reachable as soon as an index that referred to them is
-- replaced, so they are collected the same way dangling URLs are.
deleteOrphanedErikPartitions :: Tx 'RW -> IO Int
deleteOrphanedErikPartitions (Tx conn) = do
    execute_ conn
        [sql|DELETE FROM erik_partitions
             WHERE hash NOT IN (SELECT DISTINCT partition_hash FROM erik_index_partitions)|]
    changes conn

deleteObjectByHash :: MonadIO m => Tx 'RW -> Hash -> m ()
deleteObjectByHash tx h = liftIO $
    ifJustM (getKeyByHash tx h) (\k -> deleteObjectByKey tx [k])

-- | ON DELETE CASCADE handles certificates, manifest_meta, and object_urls.
deleteObjectByKey :: MonadIO m => Tx 'RW -> [ObjectKey] -> m ()
deleteObjectByKey (Tx conn) keys = liftIO $
    forM_ (inClauseBatches keys) $ \(placeholders, params) ->
        executeNamed conn
            (fromString $ Text.unpack $ "DELETE FROM objects WHERE object_key IN (" <> placeholders <> ")")
            params


-- ---------------------------------------------------------------------------
-- Manifest / Certificate index functions
-- ---------------------------------------------------------------------------

-- | Sorted newest-first by `Ord MftMeta`
getMftsForAKI :: MonadIO m => Tx mode -> AKI -> m [MftMeta]
getMftsForAKI (Tx conn) aki_ = liftIO $ do
    rows <- query conn
        "SELECT meta FROM manifest_meta WHERE aki = ?"
        (Only aki_)
    pure $! List.sortOn Down $ map (deserialiseField . fromOnly) rows

findAllMftsByAKI :: MonadIO m
                 => Tx mode -> AKI -> m [(MftMeta, Keyed (Located WellStructuredMft))]
findAllMftsByAKI tx aki_ = liftIO $ do
    metas <- getMftsForAKI tx aki_
    fmap catMaybes $ forM metas $ \meta ->
        fmap (meta,) <$> getMftByKey tx (meta ^. #key)

getMftByKey :: MonadIO m
            => Tx mode -> ObjectKey -> m (Maybe (Keyed (Located WellStructuredMft)))
getMftByKey tx k = do
    o <- getLocatedByKey tx k
    pure $! case o of
        Just (Located loc (WellStructuredRO (MftRO mft))) -> Just $ Keyed (Located loc mft) k
        _                              -> Nothing

getMftShorcutMeta :: MonadIO m => Tx mode -> AKI -> m (Maybe MftShortcutMeta)
getMftShorcutMeta (Tx conn) aki = liftIO $ do
    rows <- query conn "SELECT data FROM mft_shortcut_meta WHERE aki = ?" (Only aki)
    pure $! deserialiseCompressed . fromOnly <$> listToMaybe rows

-- | Children without file_name, for the hot "nothing changed" path that never needs it.
getMftShorcutChildrenLight :: MonadIO m => Tx mode -> AKI -> m (Map.Map ObjectKey (MftChild, Maybe CaChildValidity))
getMftShorcutChildrenLight (Tx conn) aki = liftIO $ do
    rows <- query conn
        [sql|
            SELECT c.child_key, s.data, NULL
            FROM mft_shortcut_payload_children c
            JOIN shortcuts s ON s.object_key = c.child_key
            WHERE c.aki = ?
            UNION ALL
            SELECT c.child_key, s.data, c.valid
            FROM mft_shortcut_ca_children c
            JOIN shortcuts s ON s.object_key = c.child_key
            WHERE c.aki = ?
        |]
        (aki, aki)
    pure $! Map.fromList
        [ (childKey, (deserialiseCompressed dataBs, caChildValidity <$> valid))
        | (childKey, dataBs, valid) <- rows ]

-- | Full children incl. file_name, for the diff path that needs to detect renames.
getMftShorcutChildrenFull :: MonadIO m => Tx mode -> AKI -> m (Map.Map ObjectKey MftEntry)
getMftShorcutChildrenFull (Tx conn) aki = liftIO $ do
    rows <- query conn
        [sql|
            SELECT c.file_name, c.child_key, s.data, NULL
            FROM mft_shortcut_payload_children c
            JOIN shortcuts s ON s.object_key = c.child_key
            WHERE c.aki = ?
            UNION ALL
            SELECT c.file_name, c.child_key, s.data, c.valid
            FROM mft_shortcut_ca_children c
            JOIN shortcuts s ON s.object_key = c.child_key
            WHERE c.aki = ?
        |]
        (aki, aki)
    pure $! Map.fromList
        [ (childKey, MftEntry { fileName = fileName_,
                                child    = deserialiseCompressed dataBs,
                                caCert   = caChildValidity <$> valid })
        | (fileName_, childKey, dataBs, valid) <- rows ]

-- | On-demand single-row lookup, used only by the rare TroubledChild fallback
-- on the light (file_name-free) read path.
getMftShortcutChildFileName :: MonadIO m => Tx mode -> AKI -> ObjectKey -> m (Maybe Text)
getMftShortcutChildFileName (Tx conn) aki childKey = liftIO $ do
    rows <- query conn
        [sql|
            SELECT file_name FROM mft_shortcut_payload_children WHERE aki = ? AND child_key = ?
            UNION ALL
            SELECT file_name FROM mft_shortcut_ca_children WHERE aki = ? AND child_key = ?
        |]
        (aki, childKey, aki, childKey)
    pure $! fromOnly <$> listToMaybe rows

caChildValidity :: Int -> CaChildValidity
caChildValidity = \case
    0 -> InvalidCaChild
    _ -> ValidCaChild

caChildValidityField :: CaChildValidity -> Int
caChildValidityField = \case
    InvalidCaChild -> 0
    ValidCaChild   -> 1

getMftShorcut :: MonadIO m => Tx mode -> AKI -> m (Maybe MftShortcut)
getMftShorcut tx aki = do
    metaM <- getMftShorcutMeta tx aki
    case metaM of
        Nothing -> pure Nothing
        Just MftShortcutMeta {..} -> do
            nonCrlEntries <- getMftShorcutChildrenFull tx aki
            pure $! Just $! MftShortcut {..}

saveMftShorcutMeta :: MonadIO m => Tx 'RW -> AKI -> Verbatim (Compressed MftShortcutMeta) -> m ()
saveMftShorcutMeta (Tx conn) aki meta = liftIO $
    execute conn
        "INSERT OR REPLACE INTO mft_shortcut_meta(aki, data) VALUES (?, ?)"
    (aki, unStorable $ unVerbatim meta)

-- | Insert only the given (new) children; never touches rows for unchanged children.
-- `OR REPLACE` on purpose: a TroubledChild re-validation, or a manifest-entry
-- rename (same child_key, new file_name), can legitimately overwrite an
-- existing row for a key that's already cached.
--
-- A CA certificate goes to `mft_shortcut_ca_children`, which references
-- `certificates`. Every parsed CA certificate has a row there, but if one
-- didn't, the foreign key would fail the whole write transaction, so such
-- a child goes to the payload table instead. An object that was stored
-- unparsed first and parsed later can move from the payload table to the
-- CA one, never the other way.
insertMftShortcutChildren :: MonadIO m => Tx 'RW -> AKI -> [ShortcutChildRow] -> m ()
insertMftShortcutChildren (Tx conn) aki newEntries = liftIO $ do
    executeMany conn
        "INSERT OR REPLACE INTO shortcuts(object_key, data) VALUES (?, ?)"
        [ (childKey, shortcut) | ShortcutChildRow {..} <- newEntries ]
    executeMany conn
        "INSERT OR REPLACE INTO mft_shortcut_payload_children(aki, file_name, child_key) VALUES (?, ?, ?)"
        [ (aki, fileName, childKey) | ShortcutChildRow { caCert = Nothing, .. } <- newEntries ]

    let caChildren = [ (childKey, fileName, validity)
                     | ShortcutChildRow { caCert = Just validity, .. } <- newEntries ]
    unless (null caChildren) $ do
        executeMany conn
            "DELETE FROM mft_shortcut_payload_children WHERE aki = ? AND child_key = ?"
            [ (aki, childKey) | (childKey, _, _) <- caChildren ]
        executeMany conn
            [sql|
                INSERT OR REPLACE INTO mft_shortcut_ca_children(aki, file_name, child_key, valid)
                SELECT ?, ?, ?, ? WHERE EXISTS (SELECT 1 FROM certificates WHERE object_key = ?)
            |]
            [ (aki, fileName, childKey, caChildValidityField validity, childKey)
            | (childKey, fileName, validity) <- caChildren ]
        executeMany conn
            [sql|
                INSERT OR REPLACE INTO mft_shortcut_payload_children(aki, file_name, child_key)
                SELECT ?, ?, ? WHERE NOT EXISTS (SELECT 1 FROM certificates WHERE object_key = ?)
            |]
            [ (aki, fileName, childKey, childKey) | (childKey, fileName, _) <- caChildren ]

-- | Delete only this AKI's (aki, child_key) membership rows. Never touches
-- `shortcuts` -- an orphaned shortcut is cleaned up by the general objects
-- cleanup/GC (deleteObjectByKey etc.), which cascades objects -> shortcuts ->
-- mft_shortcut_payload_children (and objects -> certificates ->
-- mft_shortcut_ca_children) once nothing marks the underlying object as used.
deleteMftShortcutChildren :: MonadIO m => Tx 'RW -> AKI -> [ObjectKey] -> m ()
deleteMftShortcutChildren (Tx conn) aki deletedKeys = liftIO $
    forM_ (inClauseBatches deletedKeys) $ \(placeholders, params) ->
        forM_ ["mft_shortcut_payload_children", "mft_shortcut_ca_children"] $ \table ->
            executeNamed conn
                (fromString $ Text.unpack $
                    "DELETE FROM " <> table <> " WHERE aki = :aki AND child_key IN (" <> placeholders <> ")")
                ((":aki" := aki) : params)

deleteMftShortcut :: MonadIO m => Tx 'RW -> AKI -> m ()
deleteMftShortcut (Tx conn) aki = liftIO $ do
    execute conn "DELETE FROM mft_shortcut_meta WHERE aki = ?" (Only aki)
    execute conn "DELETE FROM mft_shortcut_payload_children WHERE aki = ?" (Only aki)
    execute conn "DELETE FROM mft_shortcut_ca_children WHERE aki = ?" (Only aki)

-- | Returns all candidates for the SKI; callers must verify signatures.
getBySKI :: MonadIO m => Tx mode -> SKI -> m [Located WellStructuredCaCert]
getBySKI tx@(Tx conn) ski = liftIO $ do
    rows <- query conn
        "SELECT object_key FROM certificates WHERE ski = ?"
        (Only ski)
    let objectKeys = map fromOnly rows
    fmap catMaybes $ forM objectKeys $ \k ->
        getLocatedByKey tx k >>= \case
            Just (Located loc (WellStructuredRO (CerRO c))) ->
                pure $ Just (Located loc c)
            _ -> pure Nothing

-- | Backward-compat wrapper: returns the first CA cert matching the SKI.
getFirstCaCertBySKI :: MonadIO m => Tx mode -> SKI -> m (Maybe (Located WellStructuredCaCert))
getFirstCaCertBySKI tx ski =
    listToMaybe <$> getBySKI tx ski

getTaCertByKey :: MonadIO m => Tx mode -> ObjectKey -> m (Maybe WellStructuredCaCert)
getTaCertByKey tx k =
    getLocatedByKey tx k >>= \case
        Just (Located _ (WellStructuredRO (CerRO c))) -> pure $ Just c
        _                                             -> pure Nothing

{- This one is intentionally designed as an update of one big blob rather than a row-per-key,
   because the set of validated keys is expected to be large (every object touched during
   top-down validation). Upserting these key by key would likely cause largely amplified disk writes.
-}
markAsValidated :: MonadIO m
                => Tx 'RW -> Set.Set ObjectKey -> WorldVersion -> m ()
markAsValidated tx allKeys worldVersion =
    liftIO $ void $ updateValidatedByVersionMap tx $ \m ->
        foldr (`Map.insert` worldVersion) m allKeys

-- ---------------------------------------------------------------------------
-- TA functions
-- ---------------------------------------------------------------------------

saveTA :: MonadIO m => Tx 'RW -> StorableTA -> m ()
saveTA (Tx conn) ta = liftIO $
    -- Deliberately not `INSERT OR REPLACE`: that deletes the row and inserts a
    -- new one, which would wipe the `validations` column written by the TA
    -- certificate job.
    execute conn
        [sql|
            INSERT INTO trust_anchors(ta_name, ta_cert_key, data, active)
            VALUES (?, ?, ?, 1)
            ON CONFLICT(ta_name) DO UPDATE SET
                ta_cert_key = excluded.ta_cert_key,
                data        = excluded.data,
                active      = 1
        |]
        (unTaName (getTaName (tal ta)), taCertKey ta, serialiseField ta)

saveTaValidations :: MonadIO m => Tx 'RW -> TaName -> Validations -> m ()
saveTaValidations (Tx conn) taName validations = liftIO $
    execute conn
        "UPDATE trust_anchors SET validations = ? WHERE ta_name = ?"
        (serialiseCompressed validations, unTaName taName)

getTaValidations :: MonadIO m => Tx mode -> TaName -> m Validations
getTaValidations (Tx conn) taName = liftIO $ do
    rows <- query conn
        "SELECT validations FROM trust_anchors WHERE ta_name = ? AND validations IS NOT NULL"
        (Only (unTaName taName))
    pure $ maybe mempty (deserialiseCompressed . fromOnly) (listToMaybe rows)

getTA :: MonadIO m => Tx mode -> TaName -> m (Maybe StorableTA)
getTA (Tx conn) name = liftIO $ do
    rows <- query conn "SELECT data FROM trust_anchors WHERE ta_name = ?" (Only (unTaName name))
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

getTAs :: MonadIO m => Tx mode -> m [StorableTA]
getTAs (Tx conn) = liftIO $ do
    rows <- query_ conn "SELECT data FROM trust_anchors WHERE active = 1"
    pure $ map (deserialiseField . fromOnly) rows

setActiveTAs :: MonadIO m => Tx 'RW -> [TaName] -> m ()
setActiveTAs (Tx conn) taNames = liftIO $ do
    execute_ conn "UPDATE trust_anchors SET active = 0"
    forM_ taNames $ \(TaName taName) ->
        execute conn
            "UPDATE trust_anchors SET active = 1 WHERE ta_name = ?"
            (Only taName)

-- ---------------------------------------------------------------------------
-- Version / Validation payload functions
-- ---------------------------------------------------------------------------

-- | Every world version that has ever been validated, newest first.
-- `validation_outcomes` is the ground truth for this -- a version always
-- gets at least its common (ta_name IS NULL) row written by
-- `saveValidationVersion`, so there's no need for a separate `versions` table.
versionsBackwards :: MonadIO m => Tx mode -> m [WorldVersion]
versionsBackwards (Tx conn) = liftIO $
    map fromOnly <$> query_ conn "SELECT DISTINCT version FROM validation_outcomes ORDER BY version DESC"

previousVersion :: MonadIO m => Tx mode -> WorldVersion -> m (Maybe WorldVersion)
previousVersion tx version = liftIO $ do
    vs <- versionsBackwards tx
    pure $ case filter (< version) vs of
        [] -> Nothing
        xs -> Just $ maximum xs

getLatestVersion :: MonadIO m => Tx mode -> m (Maybe WorldVersion)
getLatestVersion tx = listToMaybe <$> versionsBackwards tx

rowsToPerTa :: AsStorable a => [(Text, BS.ByteString)] -> PerTA a
rowsToPerTa rows = toPerTA
    [ (TaName taName, deserialiseCompressed bs) | (taName, bs) <- rows ]

-- | Which rows of `validation_outcomes` a "latest values" query ranks.
data OutcomeScope
    = EveryActiveTA -- ^ per-TA rows of all active TAs, ranked per TA and prefixed with `ta_name`
    | OneActiveTA   -- ^ per-TA rows of the single active TA bound to `:ta_name`
    | Common        -- ^ the common, i.e. not TA-specific, rows


-- | The latest non-NULL values of `columns` at or before `:version`.
latestOutcomeQuery :: OutcomeScope -> [Text] -> Query
latestOutcomeQuery scope columns =
    fromString $ Text.unpack $ Text.unlines $ filter (not . Text.null)
        [ "WITH ranked AS ("
        , "    SELECT vo.ta_name AS ta_name, vo.version AS version,"
        , "           ROW_NUMBER() OVER (" <> partitionBy <> "ORDER BY vo.version DESC) AS rn"
        , "    FROM validation_outcomes vo"
        , activeTaJoin
        , "    WHERE " <> Text.intercalate "\n      AND " filters
        , ")"
        , "SELECT " <> commaSeparated (map ("vo." <>) selected)
        , "FROM ranked r"
        -- `IS` rather than `=` so the Common scope, whose ta_name is NULL,
        -- matches too. SQLite still uses the (ta_name, version) primary key.
        , "JOIN validation_outcomes vo ON vo.ta_name IS r.ta_name AND vo.version = r.version"
        , "WHERE r.rn = 1"
        ]
  where
    selected = case scope of
                    EveryActiveTA -> "ta_name" : columns
                    _             -> columns

    partitionBy = case scope of
                    EveryActiveTA -> "PARTITION BY vo.ta_name "
                    _             -> ""

    activeTaJoin = case scope of
                    Common -> ""
                    _      -> "    JOIN trust_anchors ta ON ta.ta_name = vo.ta_name AND ta.active = 1"

    filters = taFilter
           <> [ "vo.version <= :version" ]
           <> [ "vo." <> column <> " IS NOT NULL" | column <- columns ]

    taFilter = case scope of
                    EveryActiveTA -> []
                    OneActiveTA   -> [ "vo.ta_name = :ta_name" ]
                    Common        -> [ "vo.ta_name IS NULL" ]

    commaSeparated = Text.intercalate ", "

-- | Latest value of one payload column for every active TA.
getLatestPerTA :: (MonadIO m, AsStorable a) => Tx mode -> Text -> WorldVersion -> m (PerTA a)
getLatestPerTA (Tx conn) column version = liftIO $
    rowsToPerTa <$> queryNamed conn
        (latestOutcomeQuery EveryActiveTA [column])
        [":version" := version]

-- | Same as `getLatestPerTA`, with the per-TA values merged into one.
getLatestAcrossTAs :: (MonadIO m, AsStorable a, Monoid a) => Tx mode -> Text -> WorldVersion -> m a
getLatestAcrossTAs tx column version = allTAs <$> getLatestPerTA tx column version

getValidationsPerTA :: MonadIO m => Tx mode -> WorldVersion -> m (PerTA Validations)
getValidationsPerTA tx = getLatestPerTA tx "validations"

getMetricsPerTA :: MonadIO m => Tx mode -> WorldVersion -> m (PerTA Metrics)
getMetricsPerTA tx = getLatestPerTA tx "metrics"

getCommonMetrics :: MonadIO m => Tx mode -> WorldVersion -> m Metrics
getCommonMetrics (Tx conn) version = liftIO $ do
    rows <- queryNamed conn
        (latestOutcomeQuery Common ["metrics"])
        [":version" := version]
    pure $ maybe mempty (deserialiseCompressed . fromOnly) (listToMaybe rows)

getValidationOutcomes :: MonadIO m
                      => Tx mode
                      -> WorldVersion
                      -> m (Validations, Metrics, PerTA (Validations, Metrics))
getValidationOutcomes (Tx conn) version = liftIO $ do
    commonRows <- queryNamed conn
        (latestOutcomeQuery Common ["validations", "metrics"])
        [":version" := version]

    perTaRows <- queryNamed conn
        (latestOutcomeQuery EveryActiveTA ["validations", "metrics"])
        [":version" := version]

    let (commonV, commonM) =
            case listToMaybe commonRows of
                Just (v, m) -> (deserialiseCompressed v, deserialiseCompressed m)
                Nothing     -> mempty
        perTa = toPerTA
            [ (TaName taName, (deserialiseCompressed v, deserialiseCompressed m))
            | (taName, v, m) <- perTaRows
            ]
    pure (commonV, commonM, perTa)

getVrps :: MonadIO m => Tx mode -> WorldVersion -> m (PerTA Vrps)
getVrps tx version = fmap toVrps <$> getRoas tx version

getVrpsForTA :: MonadIO m => Tx mode -> WorldVersion -> TaName -> m Vrps
getVrpsForTA (Tx conn) version taName = liftIO $ do
    rows <- queryNamed conn
        (latestOutcomeQuery OneActiveTA ["roas"])
        [":ta_name" := unTaName taName, ":version" := version]
    pure $ toVrps $ maybe mempty (deserialiseCompressed . fromOnly) (listToMaybe rows)

getRoas :: MonadIO m => Tx mode -> WorldVersion -> m (PerTA Roas)
getRoas tx = getLatestPerTA tx "roas"

getAspas :: MonadIO m => Tx mode -> WorldVersion -> m (Maybe (Set.Set Aspa))
getAspas tx version = Just <$> getLatestAcrossTAs tx "aspa" version

getGbrs :: MonadIO m => Tx mode -> WorldVersion -> m (Maybe (Set.Set (T2 Hash Gbr)))
getGbrs tx version = Just <$> getLatestAcrossTAs tx "gbrs" version

getBgps :: MonadIO m => Tx mode -> WorldVersion -> m (Maybe (Set.Set BGPSecPayload))
getBgps tx version = Just <$> getLatestAcrossTAs tx "bgps" version

getSpls :: MonadIO m => Tx mode -> WorldVersion -> m (Maybe (Set.Set SplN))
getSpls tx version = Just <$> getLatestAcrossTAs tx "spls" version

saveValidationVersion :: MonadIO m
                      => Tx 'RW
                      -> WorldVersion
                      -> PerTA (Payloads, ValidationState)
                      -> ValidationState
                      -> m ()
saveValidationVersion (Tx conn) validatedBy results commonVS =
    liftIO $ do
    execute conn "DELETE FROM validation_outcomes WHERE version = ?" (Only validatedBy)

    execute conn
        [sql|
            INSERT OR REPLACE INTO validation_outcomes
                (ta_name, version, validations, metrics, roas, spls, aspa, bgps, gbrs)
            VALUES (?, ?, ?, ?, NULL, NULL, NULL, NULL, NULL)
        |]
        ( Nothing :: Maybe Text
        , validatedBy
        , Just $ serialiseCompressed (commonVS ^. typed @Validations)
        , Just $ serialiseCompressed (commonVS ^. typed @Metrics)
        )

    forM_ (perTA results) $ \(taName, (Payloads{..}, vs)) ->
        execute conn
            [sql|
                INSERT OR REPLACE INTO validation_outcomes
                    (ta_name, version, validations, metrics, roas, spls, aspa, bgps, gbrs)
                VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            |]
            ( Just $ unTaName taName
            , validatedBy
            , Just $ serialiseCompressed (vs ^. typed @Validations)
            , Just $ serialiseCompressed (vs ^. typed @Metrics)
            , Just $ serialiseCompressed roas
            , Just $ serialiseCompressed spls
            , Just $ serialiseCompressed aspas
            , Just $ serialiseCompressed bgpCerts
            , Just $ serialiseCompressed gbrs
            )

deleteValidationVersion :: MonadIO m => Tx 'RW -> WorldVersion -> m ()
deleteValidationVersion (Tx conn) worldVersion = liftIO $ do
        execute conn "DELETE FROM validation_outcomes WHERE version = ?"
            (Only worldVersion)
        execute conn "DELETE FROM slurm    WHERE key = ?" (Only worldVersion)

saveSlurm :: MonadIO m => Tx 'RW -> WorldVersion -> Slurm -> m ()
saveSlurm (Tx conn) version slurm = liftIO $
    execute conn "INSERT OR REPLACE INTO slurm(key, value) VALUES (?, ?)"
        (version, serialiseCompressed slurm)

-- | Merge validations and metrics into the common (not TA-specific) outcome
-- of an already saved version. Used for what the main process finds out after
-- the validation worker has saved the version, i.e. SLURM problems.
addCommonValidations :: MonadIO m => Tx 'RW -> WorldVersion -> ValidationState -> m ()
addCommonValidations (Tx conn) version vs = liftIO $ do
    rows <- query conn
        "SELECT validations, metrics FROM validation_outcomes WHERE version = ? AND ta_name IS NULL"
        (Only version)
    let (validations, metrics) = case rows of
            (v, m) : _ -> (maybe mempty deserialiseCompressed v, maybe mempty deserialiseCompressed m)
            []         -> mempty
    let newValidations = serialiseCompressed $ validations <> vs ^. typed @Validations
    let newMetrics     = serialiseCompressed $ metrics <> vs ^. typed @Metrics
    case rows of
        [] -> execute conn
                [sql|
                    INSERT INTO validation_outcomes
                        (ta_name, version, validations, metrics, roas, spls, aspa, bgps, gbrs)
                    VALUES (NULL, ?, ?, ?, NULL, NULL, NULL, NULL, NULL)
                |]
                (version, newValidations, newMetrics)
        _  -> execute conn
                "UPDATE validation_outcomes SET validations = ?, metrics = ? WHERE version = ? AND ta_name IS NULL"
                (newValidations, newMetrics, version)

getSlurm :: MonadIO m => Tx mode -> WorldVersion -> m (Maybe Slurm)
getSlurm (Tx conn) version = liftIO $ do
    rows <- query conn "SELECT value FROM slurm WHERE key = ?"
                (Only version)
    pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getLatestVersions :: MonadIO m => Tx mode -> m (PerTA WorldVersion)
getLatestVersions (Tx conn) = liftIO $ do
    rows <- query_ conn
        [sql|
            SELECT vo.ta_name, MAX(vo.version)
            FROM validation_outcomes vo
            JOIN trust_anchors ta ON ta.ta_name = vo.ta_name
            WHERE ta.active = 1
            AND vo.ta_name IS NOT NULL
            GROUP BY vo.ta_name
        |] :: IO [(Text, WorldVersion)]
    pure $ toPerTA
        [ (TaName taName, latestVersion)
        | (taName, latestVersion) <- rows
        ]


-- ---------------------------------------------------------------------------
-- Repository functions
-- ---------------------------------------------------------------------------

updateRrdpMeta :: MonadIO m => Tx 'RW -> RrdpMeta -> RrdpURL -> m ()
updateRrdpMeta tx meta url = liftIO $ updateRrdpMetaM tx url (const $ pure $ Just meta)

updateRrdpMetaM :: MonadIO m
                => Tx 'RW
                -> RrdpURL
                -> (Maybe RrdpMeta -> IO (Maybe RrdpMeta))
                -> m ()
updateRrdpMetaM (Tx conn) url f = liftIO $ do
    let k = serialiseField url
    rows <- query conn "SELECT data FROM repositories WHERE key = ? AND kind = 'rrdp-pp'" (Only k)
    forM_ (listToMaybe rows) $ \(Only bs) -> do
        let repo = deserialiseField bs :: RrdpRepository
        f (repo ^. #rrdpMeta) >>= \case
            Nothing      -> pure ()
            Just newMeta ->
                execute conn
                    "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rrdp-pp', ?)"
                    (k, serialiseField (repo & #rrdpMeta ?~ newMeta))

getPublicationPoints :: MonadIO m => Tx mode -> m PublicationPoints
getPublicationPoints (Tx conn) = liftIO $ do
    rrdpRows  <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rrdp-pp'"
    rsyncRows <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rsync-pp'"
    let rrdps  = [ (deserialiseField k, deserialiseField v) | (k, v) <- rrdpRows ]
        rsyncs = [ (deserialiseField k, deserialiseField v) | (k, v) <- rsyncRows ]
    pure $ PublicationPoints
        (RrdpMap $ Map.fromList rrdps)
        (RsyncForestGen $ Map.fromList rsyncs)

getRepository :: MonadIO m => Tx mode -> RpkiURL -> m (Maybe Repository)
getRepository tx = \case
    RrdpU u  -> fmap RrdpR  <$> getRrdpRepository tx u
    RsyncU u -> fmap RsyncR <$> getRsyncRepository tx u

getRrdpRepository :: MonadIO m => Tx mode -> RrdpURL -> m (Maybe RrdpRepository)
getRrdpRepository (Tx conn) url = liftIO $ do
    rows <- query conn "SELECT data FROM repositories WHERE key = ? AND kind = 'rrdp-pp'"
                (Only (serialiseField url))
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

getRsyncRepository :: MonadIO m => Tx mode -> RsyncURL -> m (Maybe RsyncRepository)
getRsyncRepository tx url = Map.lookup url <$> getRsyncRepositories tx [url]

getRsyncRepositories :: MonadIO m
                     => Tx mode -> [RsyncURL] -> m (Map.Map RsyncURL RsyncRepository)
getRsyncRepositories tx urls =
    getRsyncAnything urls
        (\host -> do
            let Tx conn = tx
            rows <- query conn
                "SELECT data FROM repositories WHERE key = ? AND kind = 'rsync-pp'"
                (Only (serialiseField host))
            pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows))
        (\url meta -> RsyncRepository { repoPP = RsyncPublicationPoint url, .. })

getRsyncAnything :: MonadIO m
                 => [RsyncURL]
                 -> (RsyncHost -> IO (Maybe (RsyncTree a)))
                 -> (RsyncURL -> a -> b)
                 -> m (Map.Map RsyncURL b)
getRsyncAnything urls extractTree create = liftIO $ do
    let grouped = Map.fromListWith (<>) [ (host, [u]) | u@(RsyncURL host _) <- urls ]
    fmap (Map.fromList . mconcat) $
        forM (Map.toList grouped) $ \(host, thisHostUrls) -> do
            z <- extractTree host
            pure $ case z of
                Nothing   -> []
                Just tree ->
                    [ (u, create url' content)
                    | u@(RsyncURL _ path) <- thisHostUrls
                    , Just (path', content) <- [lookupInRsyncTree path tree]
                    , let url' = RsyncURL host path' ]

saveRepositories :: MonadIO m => Tx 'RW -> [Repository] -> m ()
saveRepositories tx repos = liftIO $ do
    let (rrdps, rsyncs) = foldr sep ([], []) repos
    let Tx conn = tx
    executeMany conn
        "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rrdp-pp', ?)"
        [ (serialiseField (r ^. #uri), serialiseField r) | r <- rrdps ]
    saveRsyncRepositories tx rsyncs
  where
    sep (RrdpR r)  (rs, ss) = (r : rs, ss)
    sep (RsyncR r) (rs, ss) = (rs, r : ss)

saveRepositoryValidationStates :: MonadIO m
                                => Tx 'RW -> [(Repository, ValidationState)] -> m ()
saveRepositoryValidationStates tx repos = liftIO $ do
    let (rrdps, rsyncs) = foldr sep ([], []) repos
    let Tx conn = tx
    executeMany conn
        "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rrdp-vstate', ?)"
        [ (serialiseField (r ^. #uri), serialiseCompressed vs) | (r, vs) <- rrdps ]
    saveRsyncValidationStates tx rsyncs
  where
    sep (RrdpR r,  a) (rs, ss) = ((r, a) : rs, ss)
    sep (RsyncR r, a) (rs, ss) = (rs, (r, a) : ss)

saveRsyncRepositories :: MonadIO m => Tx 'RW -> [RsyncRepository] -> m ()
saveRsyncRepositories (Tx conn) repos = liftIO $
    saveRsyncAnything (map (\r -> (r, r ^. #meta)) repos)
        (\host -> do
            rows <- query conn
                "SELECT data FROM repositories WHERE key = ? AND kind = 'rsync-pp'"
                (Only (serialiseField host))
            pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows))
        (\host tree ->
            execute conn
                "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rsync-pp', ?)"
                (serialiseField host, serialiseField tree))

saveRsyncValidationStates :: MonadIO m
                          => Tx 'RW -> [(RsyncRepository, ValidationState)] -> m ()
saveRsyncValidationStates tx repos = liftIO $
    saveRsyncAnything repos
        (\host -> do
            let Tx conn = tx
            rows <- query conn
                "SELECT data FROM repositories WHERE key = ? AND kind = 'rsync-vstate'"
                (Only (serialiseField host))
            pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows))
        (\host tree ->
            let Tx conn = tx
            in execute conn
                "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rsync-vstate', ?)"
                (serialiseField host, serialiseCompressed tree))

saveRsyncAnything :: MonadIO m
                  => [(RsyncRepository, a)]
                  -> (RsyncHost -> IO (Maybe (RsyncTree a)))
                  -> (RsyncHost -> RsyncTree a -> IO ())
                  -> m ()
saveRsyncAnything repos extractTree saveTree = liftIO $ do
    let grouped = Map.fromListWith (<>)
            [ (host, [(path, a)])
            | (RsyncRepository { repoPP = RsyncPublicationPoint (RsyncURL host path) }, a) <- repos ]
    forM_ (Map.toList grouped) $ \(host, pathAndA) -> do
        startTree <- fromMaybe newRsyncTree <$> extractTree host
        saveTree host $ foldr (uncurry pathToRsyncTree) startTree pathAndA

getRepositories :: MonadIO m
                => Tx mode -> (RpkiURL -> Bool) -> m [(Repository, ValidationState)]
getRepositories (Tx conn) filterF = liftIO $ do
    rrdpRows  <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rrdp-pp'"
    rsyncRows <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rsync-pp'"
    let rrdps  = [ (deserialiseField k :: RrdpURL,  deserialiseField v) | (k, v) <- rrdpRows ]
        rsyncs = [ (deserialiseField k :: RsyncHost, deserialiseField v) | (k, v) <- rsyncRows ]

    -- Bulk-fetch validation states in two queries instead of one query per
    -- repository/host -- with hundreds of repositories that N+1 pattern was
    -- hundreds of round-trips on every call (this is on the main UI page's
    -- request path).
    rrdpVstateRows  <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rrdp-vstate'"
    rsyncVstateRows <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rsync-vstate'"
    let rrdpVstates  = Map.fromList rrdpVstateRows  :: Map.Map BS.ByteString BS.ByteString
        rsyncVstates = Map.fromList rsyncVstateRows :: Map.Map BS.ByteString BS.ByteString

    let rrdpResults =
            [ (RrdpR r, deserialiseCompressed bs)
            | (url, r) <- rrdps
            , filterF (RrdpU url)
            , Just bs <- [Map.lookup (serialiseField url) rrdpVstates]
            ]

        rsyncResults =
            [ (RsyncR repo, vs)
            | (host, metas) <- rsyncs
            , Just bs <- [Map.lookup (serialiseField host) rsyncVstates]
            , let vss = deserialiseCompressed bs :: RsyncTree ValidationState
            , (RsyncURL _ path, meta) <- flattenTree host metas
            , let uri  = RsyncURL host path
            , let repo = RsyncRepository { repoPP = RsyncPublicationPoint uri, .. }
            , filterF (RsyncU uri)
            , Just (_, vs) <- [lookupInRsyncTree path vss]
            ]
    pure $ rrdpResults <> rsyncResults

{-
    Erik "repositories" are bookkeeping for the UI and for refresh scheduling:
    one row per FQDN, since that is the unit an Erik fetch works on (a relay
    serves an index per FQDN, regardless of how many publication points live
    under it). They are stored in the same table as the RRDP/rsync ones, under
    their own `erik-pp`/`erik-vstate` kinds, and they are not part of
    'PublicationPoints' -- nothing in validation looks them up.
-}
saveErikRepositories :: MonadIO m => Tx 'RW -> [ErikRepository] -> m ()
saveErikRepositories (Tx conn) repos = liftIO $
    executeMany conn
        "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'erik-pp', ?)"
        [ (serialiseField (r ^. #fqdn), serialiseField r) | r <- repos ]

saveErikRepositoryValidationStates :: MonadIO m
                                   => Tx 'RW -> [(ErikRepository, ValidationState)] -> m ()
saveErikRepositoryValidationStates (Tx conn) repos = liftIO $
    executeMany conn
        "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'erik-vstate', ?)"
        [ (serialiseField (r ^. #fqdn), serialiseCompressed vs) | (r, vs) <- repos ]

getErikRepository :: MonadIO m => Tx mode -> FQDN -> m (Maybe ErikRepository)
getErikRepository (Tx conn) fqdn = liftIO $ do
    rows <- query conn "SELECT data FROM repositories WHERE key = ? AND kind = 'erik-pp'"
                (Only (serialiseField fqdn))
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

getErikRepositories :: MonadIO m
                    => Tx mode -> (FQDN -> Bool) -> m [(ErikRepository, ValidationState)]
getErikRepositories (Tx conn) filterF = liftIO $ do
    ppRows     <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'erik-pp'"
    vstateRows <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'erik-vstate'"
    let vstates = Map.fromList vstateRows :: Map.Map BS.ByteString BS.ByteString
    pure [ (repo, maybe mempty deserialiseCompressed (Map.lookup k vstates))
         | (k, v) <- ppRows
         , let repo = deserialiseField v :: ErikRepository
         , filterF (repo ^. #fqdn) ]


-- ---------------------------------------------------------------------------
-- Job / Metadata
-- ---------------------------------------------------------------------------

setJobCompletionTime :: MonadIO m => Tx 'RW -> Text -> Instant -> m ()
setJobCompletionTime (Tx conn) job t = liftIO $
    execute conn "INSERT OR REPLACE INTO jobs(key, value) VALUES (?, ?)"
        (job, serialiseField t)

allJobs :: MonadIO m => Tx mode -> m [(Text, Instant)]
allJobs (Tx conn) = liftIO $ do
    rows <- query_ conn "SELECT key, value FROM jobs"
    pure [ (k, deserialiseField v) | (k, v) <- rows ]

getDatabaseVersion :: MonadIO m => Tx mode -> m (Maybe Integer)
getDatabaseVersion (Tx conn) = liftIO $ do
    rows <- query conn "SELECT value FROM metadata WHERE key = ?"
                (Only databaseVersionKey)
    pure $ case rows of
        [Only t] -> readMaybe (Text.unpack t)
        _        -> Nothing

saveCurrentDatabaseVersion :: MonadIO m => Tx 'RW -> m ()
saveCurrentDatabaseVersion (Tx conn) = liftIO $
    execute conn "INSERT OR REPLACE INTO metadata(key, value) VALUES (?, ?)"
        (databaseVersionKey, Text.pack $ show currentDatabaseVersion)

getValidatedByVersionMap :: SQLite.CachedConn -> IO (Map.Map ObjectKey WorldVersion)
getValidatedByVersionMap conn = do
    rows <- query conn "SELECT value FROM validated_by_version WHERE key = ?"
                (Only validatedByVersionKey)
    pure $ case rows of
        [Only bs] -> deserialiseCompressed bs
        _         -> mempty

updateValidatedByVersionMap :: MonadIO m
                            => Tx 'RW
                            -> (Map.Map ObjectKey WorldVersion -> Map.Map ObjectKey WorldVersion)
                            -> m (Map.Map ObjectKey WorldVersion)
updateValidatedByVersionMap (Tx conn) f = liftIO $ do
    updated <- f <$> getValidatedByVersionMap conn
    execute conn "INSERT OR REPLACE INTO validated_by_version(key, value) VALUES (?, ?)"
        (validatedByVersionKey, serialiseCompressed updated)
    pure updated


-- ---------------------------------------------------------------------------
-- Stats
-- ---------------------------------------------------------------------------

getObjectsStats :: MonadIO m => Tx mode -> m ObjectStats
getObjectsStats (Tx conn) = liftIO $ do
    rows <- query_ conn
        [sql|
            SELECT type,
                   COUNT(*),
                   SUM(LENGTH(COALESCE(data, original))),
                   MIN(LENGTH(COALESCE(data, original))),
                   MAX(LENGTH(COALESCE(data, original)))
            FROM objects GROUP BY type
        |]
    pure $ foldr accumulate mempty rows
  where
    accumulate (typText, cnt, total, smallest, biggest) acc =
        case readMaybe typText of
            Nothing  -> acc
            Just typ ->
                let count     = Size (fromIntegral (cnt :: Int64))
                    totalSize = Size (fromIntegral (total :: Int64))
                    minSize   = Size (fromIntegral (smallest :: Int64))
                    maxSize   = Size (fromIntegral (biggest :: Int64))
                    avgSize   = Size $ if cnt == 0 then 0 else total `div` cnt
                in  acc & #totalObjects %~ (+ count)
                        & #totalSize    %~ (+ totalSize)
                        & #countPerType     %~ Map.insertWith (+) typ count
                        & #totalSizePerType %~ Map.insertWith (+) typ totalSize
                        & #minSizePerType   %~ Map.insertWith min typ minSize
                        & #maxSizePerType   %~ Map.insertWith max typ maxSize
                        & #avgSizePerType   %~ Map.insert typ avgSize


-- ---------------------------------------------------------------------------
-- Complex cleanup operations
-- ---------------------------------------------------------------------------

data CleanUpResult = CleanUpResult
    { deletedObjects        :: Int
    , deletedPerType        :: Map.Map RpkiObjectType Integer
    , keptObjects           :: Int
    , deletedObjectUrls     :: Int
    , deletedURLs           :: Int
    , deletedVersions       :: Int
    , deletedErikPartitions :: Int
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

-- | Accumulator for the objects sweep in 'deleteStaleContent'.
data SweepAcc = SweepAcc {
        sweepToDelete :: [ObjectKey],
        sweepPerType  :: Map.Map RpkiObjectType Integer,
        sweepKept     :: !Int
    }
    deriving (Generic)

emptySweep :: SweepAcc
emptySweep = SweepAcc [] Map.empty 0

data DeletionCriteria = DeletionCriteria
    { versionIsTooOld   :: WorldVersion -> Bool
    , objectIsTooOld    :: WorldVersion -> RpkiObjectType -> Bool
    , objectUrlIsTooOld :: WorldVersion -> Bool
    }
    deriving (Generic)


deleteOldestVersionsIfNeeded :: MonadIO m
                             => Tx 'RW -> Natural -> m [WorldVersion]
deleteOldestVersionsIfNeeded tx@(Tx conn) versionNumberToKeep =
    mapException (AppException . storageError) <$> liftIO $ do
        versions <- versionsBackwards tx
        let reallyToKeep = max 2 (fromIntegral versionNumberToKeep)
        case NonEmpty.nonEmpty versions of
            Just neVersions
                | NonEmpty.length neVersions > reallyToKeep -> do

                taVersionRows :: [(Text, WorldVersion)] <- query_ conn
                    "SELECT ta_name, version FROM validation_outcomes WHERE ta_name IS NOT NULL"

                let taRealVersions = MonoidalMap.fromListWith (<>)
                        [ (ta, Set.singleton v) | (ta, v) <- taVersionRows ]

                    -- The Nth most recent (1-indexed) distinct real version, or the
                    -- oldest known version overall if there are fewer than N.
                    cutoffFor realVersions =
                        case drop (reallyToKeep - 1) (Set.toDescList realVersions) of
                            v : _ -> v
                            []    -> NonEmpty.last neVersions

                    cutoff
                        | MonoidalMap.null taRealVersions = NonEmpty.head neVersions
                        | otherwise = minimum $ map cutoffFor $ MonoidalMap.elems taRealVersions

                    versionsToDelete = filter (< cutoff) versions

                forM_ versionsToDelete $ deleteValidationVersion tx
                pure versionsToDelete
            _ -> pure []

deleteStaleContent :: MonadIO m => DB -> DeletionCriteria -> m CleanUpResult
deleteStaleContent db DeletionCriteria{..} =
    mapException (AppException . storageError) <$> liftIO $
        rwTx db $ \tx -> do
            deletedVersions <- deleteOldVersions tx
            (deletedObjects, deletedPerType, keptObjects) <- deleteStaleObjects tx
            deletedObjectUrls <- deleteStaleObjectUrls tx objectUrlIsTooOld
            deletedURLs <- deleteDanglingUrls tx
            -- Delete Erik partitions that are not referenced by any index any more
            deletedErikPartitions <- deleteOrphanedErikPartitions tx
            pure CleanUpResult{..}
  where
    deleteOldVersions tx = do
        toDelete <- filter versionIsTooOld <$> versionsBackwards tx
        forM_ toDelete $ deleteValidationVersion tx
        pure $ length toDelete

    -- Streamed on purpose: the objects table has ~a million rows, and
    -- collecting them into a list before looking at any of them was the bulk
    -- of this worker's heap. Only the accumulator -- the keys actually being
    -- deleted, plus counters -- outlives a row.
    deleteStaleObjects tx@(Tx conn) = do
        validatedBy <- getValidatedByVersionMap conn

        SweepAcc {..} <- SQLite.fold_ conn
            "SELECT object_key, world_version, type FROM objects"
            emptySweep $ \acc (objectKey, insertedBy, typText) ->
                pure $! case readMaybe (Text.unpack typText) of
                    Nothing  -> acc
                    Just typ ->
                        let insertedOld  = objectIsTooOld insertedBy typ
                            validatedOld = case Map.lookup objectKey validatedBy of
                                Just wv -> objectIsTooOld wv typ
                                Nothing -> True
                        in if insertedOld && validatedOld
                            then acc { sweepToDelete = objectKey : acc.sweepToDelete,
                                       sweepPerType  = Map.insertWith (+) typ 1 (acc.sweepPerType) }
                            else acc { sweepKept = acc.sweepKept + 1 }

        let validatedBy' = foldr Map.delete validatedBy sweepToDelete
        execute conn "INSERT OR REPLACE INTO validated_by_version(key, value) VALUES (?, ?)"
            (validatedByVersionKey, serialiseCompressed validatedBy')

        deleteObjectByKey tx sweepToDelete

        let deletedCount = fromIntegral $ sum $ Map.elems sweepPerType
        pure (deletedCount, sweepPerType, sweepKept)

-- | Forget that an object used to be published at a URL where it hasn't been
-- seen for a while.
--
-- Without this, an object that moved between repositories keeps both URLs
-- forever and keeps warning about having multiple locations.
--
-- The newest association of every object is always kept, however old it is:
-- objects must never end up with zero locations.
deleteStaleObjectUrls :: Tx 'RW -> (WorldVersion -> Bool) -> IO Int
deleteStaleObjectUrls (Tx conn) tooOld = do
    stale <- filter tooOld . map fromOnly <$> query_ conn
        "SELECT DISTINCT world_version FROM object_urls"
    fmap sum $ forM (inClauseBatches stale) $ \(placeholders, params) -> do
        executeNamed conn
            (fromString $ Text.unpack $
                "DELETE FROM object_urls AS ou WHERE ou.world_version IN (" <> placeholders <> ")"
                <> " AND EXISTS (SELECT 1 FROM object_urls newer"
                <> "             WHERE newer.object_key = ou.object_key"
                <> "               AND newer.world_version > ou.world_version)")
            params
        changes conn

deleteDanglingUrls :: Tx 'RW -> IO Int
deleteDanglingUrls (Tx conn) = do
    execute_ conn
        "DELETE FROM urls WHERE url_key NOT IN (SELECT DISTINCT url_key FROM object_urls)"
    changes conn

getAll :: MonadIO m => Tx mode -> m [Located RpkiObjectLifecycle]
getAll tx = liftIO $ do
    let Tx conn = tx
    rows <- query_ conn "SELECT object_key FROM objects WHERE data IS NOT NULL"
    catMaybes <$> forM rows (getLocatedByKey tx . fromOnly)

getMftMeta :: MftObject -> ObjectKey -> MftMeta
getMftMeta mft key =
    let Manifest{..} = getCMSContent $ cmsPayload mft
    in MftMeta{..}

getGbrObjects :: MonadIO m => Tx mode -> WorldVersion -> m [Located RpkiObjectLifecycle]
getGbrObjects tx version = do
    gbrs <- maybe [] Set.toList <$> getGbrs tx version
    fmap catMaybes $ forM gbrs $ \(T2 hash _) -> getByHash tx hash

getRtrPayloads :: MonadIO m => Tx 'RO -> WorldVersion -> m (Maybe RtrPayloads)
getRtrPayloads tx worldVersion = liftIO $ runMaybeT $ do
    vrps <- MaybeT $ Just <$> getVrps tx worldVersion
    bgps <- MaybeT $ getBgps tx worldVersion
    aspas <- MaybeT $ getAspas tx worldVersion
    pure $ mkRtrPayloads vrps bgps aspas


-- ---------------------------------------------------------------------------
-- Transaction wiring (validator integration)
-- ---------------------------------------------------------------------------

roAppTx :: ValidatorIO es => DB -> (Tx 'RO -> Eff es a) -> Eff es a
roAppTx db f = appTx db f withReadTx

rwAppTx :: ValidatorIO es => DB -> (Tx 'RW -> Eff es a) -> Eff es a
rwAppTx db f = appTx db f withWriteTx

appTx :: ValidatorIO es => DB
      -> (Tx mode -> Eff es a)
      -> (DB -> (Tx mode -> IO (Either AppError a, ValidationState))
             -> IO (Either AppError a, ValidationState))
      -> Eff es a
appTx db f txF = do
    {- The transaction runners take an IO callback, so the validator has to be
       unlifted. `withSeqEffToIO` is the right strategy: SQLite runs the callback
       on the calling thread, and SeqUnlift allows repeated *sequential* calls,
       so nested `roAppTx`/`rwAppTx` still work.

       Note the `tryError` inside the callback: it turns a validator error into
       a `Left` *before* it can escape as an exception, which keeps `effectful`'s
       error (a real, async-classified exception) away from SQLite's
       `onException` rollback path. The rollback is instead driven explicitly by
       `TxRollbackException`, exactly as before.

       Unlike the MTL version there is no nested `runValidator` here: the body
       writes into the enclosing (shared) `ValidationState` directly, and those
       writes survive the error, so there is nothing left to merge back. -}
    r <- withSeqEffToIO $ \unlift ->
            txF db (\tx ->
                unlift (tryError @AppError (f tx)) >>= \case
                    Left (_, e) -> throwIO $ TxRollbackException e
                    Right a     -> pure (Right a, mempty))
            `catch` (\(TxRollbackException e) -> pure (Left e, mempty))
    embedValidatorT (pure r)

roAppTxEx :: (ValidatorIO es, Exception exc) => DB
          -> (exc -> AppError)
          -> (Tx 'RO -> Eff es a)
          -> Eff es a
roAppTxEx db err f = appTxEx db err f withReadTx

rwAppTxEx :: (ValidatorIO es, Exception exc) => DB
          -> (exc -> AppError)
          -> (Tx 'RW -> Eff es a)
          -> Eff es a
rwAppTxEx db err f = appTxEx db err f withWriteTx

appTxEx :: (ValidatorIO es, Exception exc) => DB
        -> (exc -> AppError)
        -> (Tx mode -> Eff es a)
        -> (DB -> (Tx mode -> IO (Either AppError a, ValidationState))
               -> IO (Either AppError a, ValidationState))
        -> Eff es a
appTxEx db err f txF = do
    r <- withSeqEffToIO $ \unlift ->
            txF db (\tx ->
                unlift (tryError @AppError (f tx)) >>= \case
                    Left (_, e) -> throwIO $ TxRollbackException e
                    Right a     -> pure (Right a, mempty))
            `catches`
                [ Handler $ \(TxRollbackException e) -> pure (Left e, mempty)
                , Handler $ \e                           -> pure (Left (err e), mempty)
                ]
    embedValidatorT (pure r)

data TxRollbackException = TxRollbackException AppError
    deriving stock (Show, Eq, Ord, Generic)

data StorageCorruptedException = StorageCorruptedException Text
    deriving stock (Show, Eq, Ord, Generic)

instance Exception TxRollbackException
instance Exception StorageCorruptedException

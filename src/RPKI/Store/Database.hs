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
    -- * ValidatorT integration
    roAppTx, rwAppTx, appTx, roAppTxEx, rwAppTxEx, appTxEx,
    TxRollbackException(..),
    -- * Constants
    currentDatabaseVersion,
    databaseVersionKey, validatedByVersionKey,
    -- * DTOs
    MftShortcutMeta(..),
    -- * Query functions
    getKeyByHash, getObjectKey, getByHash, getKeyedByHash,
    getByUri, getKeysByUri,
    getObjectByKey, getLocatedByKey,
    getLocationCountByKey, getLocationsByKey,
    saveObject, saveStorableObject,
    getObjectMeta, linkObjectToUrl,
    hashExists, deleteObjectByHash, deleteObjectByKey,
    getMftsForAKI, findAllMftsByAKI, getMftByKey,
    getMftShorcut, getMftShorcutMeta, getMftShorcutChildrenLight, getMftShorcutChildrenFull,
    getMftShortcutChildFileName,
    saveMftShorcutMeta, insertMftShortcutChildren, deleteMftShortcutChildren,
    deleteMftShortcut, getBySKI, getFirstCaCertBySKI, getTaCertByKey,
    markAsValidated,
    saveTA, deleteTA, getTA, getTAs, setActiveTAs,
    versionsBackwards, previousVersion, getLatestVersion,
    getValidationsPerTA, getMetricsPerTA, getCommonMetrics,
    getValidationOutcomes,
    getVrps, getVrpsForTA, getRoas, getAspas, getGbrs, getBgps, getSpls,
    saveValidationVersion, deleteValidationVersion,
    saveSlurm, getSlurm, getLatestVersions,
    updateRrdpMeta, updateRrdpMetaM,
    getPublicationPoints, getRepository,
    getRrdpRepository, getRsyncRepository, getRsyncRepositories,
    getRsyncAnything,
    saveRepositories, saveRepositoryValidationStates,
    saveRsyncRepositories, saveRsyncValidationStates,
    saveRsyncAnything, getRepositories,
    setJobCompletionTime, allJobs,
    getDatabaseVersion, saveCurrentDatabaseVersion,
    updateValidatedByVersionMap,
    getObjectsStats, totalStats,
    CleanUpResult(..), DeletionCriteria(..),
    deleteOldestVersionsIfNeeded,
    deleteStaleContent, deleteDanglingUrls,
    getAll, getMftMeta, getGbrObjects, getRtrPayloads,
    storageError,
    -- * Encoding helpers (for AppSqliteStorage etc.)
    encodeSO, decodeSO,
) where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader     (ask)
import           Control.Monad.Trans.Maybe

import           Data.Generics.Product.Typed

import qualified Data.List                as List
import           Data.Maybe               (catMaybes, fromMaybe, listToMaybe)
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

roTxT :: MonadIO m => TVar DB -> (Tx 'RO -> DB -> IO a) -> m a
roTxT tdb f = liftIO $ do
    db <- readTVarIO tdb
    roTx db (\tx -> f tx db)

rwTxT :: MonadIO m => TVar DB -> (Tx 'RW -> DB -> IO a) -> m a
rwTxT tdb f = liftIO $ do
    db <- readTVarIO tdb
    rwTx db (\tx -> f tx db)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- Increment whenever any serialised type changes incompatibly.
currentDatabaseVersion :: Integer
currentDatabaseVersion = 55

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
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

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
inClauseBatches :: ToField k => [k] -> [(Text, [NamedParam])]
inClauseBatches = map toBatch . chunksOf 500
  where
    toBatch batch =
        let keyParams = zip [1 :: Int ..] batch
            placeholders = Text.intercalate ", "
                [":k" <> Text.pack (show i) | (i, _) <- keyParams]
            params = [ (":k" <> Text.pack (show i)) := key | (i, key) <- keyParams ]
        in (placeholders, params)


-- ---------------------------------------------------------------------------
-- Object functions
-- ---------------------------------------------------------------------------

getKeyByHash :: MonadIO m => Tx mode -> DB -> Hash -> m (Maybe ObjectKey)
getKeyByHash (Tx conn) _ h = liftIO $ do
    rows <- query conn
        "SELECT object_key FROM objects WHERE hash = ?"
        (Only h)
    pure $ onlyValue rows

getObjectKey :: MonadIO m => Tx mode -> DB -> Hash -> m (Maybe ObjectKey)
getObjectKey = getKeyByHash

getByHash :: MonadIO m => Tx mode -> DB -> Hash -> m (Maybe (Located RpkiObjectLifecycle))
getByHash tx db h = ((^. #object) <$>) <$> getKeyedByHash tx db h

getKeyedByHash :: MonadIO m => Tx mode -> DB -> Hash -> m (Maybe (Keyed (Located RpkiObjectLifecycle)))
getKeyedByHash tx db h = liftIO $ runMaybeT $ do
    objectKey <- MaybeT $ getKeyByHash tx db h
    z         <- MaybeT $ getLocatedByKey tx db objectKey
    pure $ Keyed z objectKey

getByUri :: MonadIO m => Tx mode -> DB -> RpkiURL -> m [Located RpkiObjectLifecycle]
getByUri tx db uri = liftIO $ do
    keys_ <- getKeysByUri tx db uri
    catMaybes <$> mapM (getLocatedByKey tx db) keys_

getKeysByUri :: MonadIO m => Tx mode -> DB -> RpkiURL -> m [ObjectKey]
getKeysByUri (Tx conn) _ uri = liftIO $ do
    rows <- query conn
        [sql|
            SELECT ou.object_key
            FROM object_urls ou JOIN urls u USING(url_key)
            WHERE u.url = ?
        |]
        (Only (serialiseField uri))
    pure $ map fromOnly rows

getObjectByKey :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe RpkiObjectLifecycle)
getObjectByKey (Tx conn) _ k = liftIO $ do
    rows <- query conn
        "SELECT data FROM objects WHERE object_key = ? AND data IS NOT NULL"
        (Only k)
    pure $ case rows of
        [Only bs] -> let StorableObject{object = ro} = decodeSO bs :: StorableObject RpkiObjectLifecycle
                     in Just ro
        _         -> Nothing

getLocatedByKey :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe (Located RpkiObjectLifecycle))
getLocatedByKey tx db k = liftIO $ runMaybeT $ do
    obj       <- MaybeT $ getObjectByKey tx db k
    locations <- MaybeT $ getLocationsByKey tx db k
    pure $ Located locations obj

getLocationCountByKey :: MonadIO m => Tx mode -> DB -> ObjectKey -> m Int
getLocationCountByKey (Tx conn) _ k = liftIO $ do
    rows <- query conn
        "SELECT COUNT(*) FROM object_urls WHERE object_key = ?"
        (Only k)
    pure $ maybe 0 fromOnly (listToMaybe rows)

getLocationsByKey :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe Locations)
getLocationsByKey (Tx conn) _ k = liftIO $ do
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
           -> DB
           -> RpkiObjectLifecycle
           -> WorldVersion
           -> m ObjectKey
saveObject tx db lifecycle = saveStorableObject tx db (toStorableObject (Compressed lifecycle))

-- | Like 'saveObject', but takes an already-built @StorableObject (Compressed
-- RpkiObjectLifecycle)@ (its serialised-and-compressed bytes already forced,
-- via 'toStorableObject' dispatching to the 'Compressed' 'AsStorable'
-- instance) instead of encoding the lifecycle here. Use this from hot paths
-- that parse many objects concurrently and want that (CPU-heavy)
-- serialisation+compression done on the parsing (parallel) thread rather
-- than the single serial DB-writer thread.
saveStorableObject :: MonadIO m
                => Tx 'RW
                -> DB
                -> StorableObject (Compressed RpkiObjectLifecycle)
                -> WorldVersion
                -> m ObjectKey
saveStorableObject (Tx conn) _ StorableObject { object = Compressed lifecycle, storable = Storable dataBs } wv = liftIO $ do
    let hash_ = getHash lifecycle

    existing <- query conn "SELECT object_key FROM objects WHERE hash = ?" (Only hash_)
    case existing of
        Only objectKey : _ -> pure objectKey
        [] -> do
            let typ    = show (getRpkiObjectType lifecycle)
                originalBs = case lifecycle of
                    OriginalRO (ObjectOriginal blob) _ _ _ -> Just blob
                    _                                      -> Nothing

            [Only objectKey] <- query conn
                [sql|INSERT INTO objects(hash, type, data, original, world_version)
                     VALUES (?, ?, ?, ?, ?) RETURNING object_key|]
                (hash_, typ, dataBs, originalBs, wv)

            case lifecycle of
                WellStructuredRO (CerRO c) ->
                    execute conn
                        [sql|INSERT OR IGNORE INTO certificates(object_key, ski, aki) VALUES (?, ?, ?)|]
                        (objectKey, getSKI c, getAKI c)
                WellStructuredRO (MftRO mft) ->
                    forM_ (getAKI mft) $ \aki_ ->
                        let meta = getMftMetaFromWellStructured mft objectKey
                        in execute conn
                            [sql|
                                INSERT OR IGNORE INTO manifest_meta(object_key, aki, manifest_number, meta)
                                VALUES (?, ?, ?, ?)
                            |]
                            ( objectKey
                            , aki_
                            , let Serial mftNum = meta ^. #mftNumber in serialToBlob mftNum
                            , serialiseField meta )
                _ -> pure ()

            pure objectKey


getObjectMeta :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe ObjectMeta)
getObjectMeta (Tx conn) _ k = liftIO $ do
    rows <- query conn
        "SELECT world_version, type FROM objects WHERE object_key = ?"
        (Only k)
    pure $ case rows of
        [(wv, typText)] -> case readMaybe typText of
            Just typ -> Just $ ObjectMeta wv typ
            Nothing  -> Nothing
        _ -> Nothing

linkObjectToUrl :: MonadIO m => Tx 'RW -> DB -> RpkiURL -> ObjectKey -> m ()
linkObjectToUrl (Tx conn) _ rpkiURL objectKey = liftIO $ do
    [Only urlKey] <- query conn
        [sql|INSERT INTO urls(url) VALUES (?)
             ON CONFLICT(url) DO UPDATE SET url = excluded.url
             RETURNING url_key|]
        (Only (serialiseField rpkiURL))
    execute conn
        "INSERT OR IGNORE INTO object_urls(object_key, url_key) VALUES (?, ?)"
        (objectKey, urlKey :: UrlKey)

hashExists :: MonadIO m => Tx mode -> DB -> Hash -> m Bool
hashExists (Tx conn) _ h = liftIO $ do
    rows <- query conn "SELECT 1 FROM objects WHERE hash = ?" (Only h)
    pure $ not (null (rows :: [Only Int]))

deleteObjectByHash :: MonadIO m => Tx 'RW -> DB -> Hash -> m ()
deleteObjectByHash tx db h = liftIO $
    ifJustM (getKeyByHash tx db h) (\k -> deleteObjectByKey tx db [k])

-- | ON DELETE CASCADE handles certificates, manifest_meta, and object_urls.
deleteObjectByKey :: MonadIO m => Tx 'RW -> DB -> [ObjectKey] -> m ()
deleteObjectByKey (Tx conn) _ keys = liftIO $
    forM_ (inClauseBatches keys) $ \(placeholders, params) ->
        executeNamed conn
            (fromString $ Text.unpack $ "DELETE FROM objects WHERE object_key IN (" <> placeholders <> ")")
            params

getMftMetaFromWellStructured :: WellStructuredCms Manifest -> ObjectKey -> MftMeta
getMftMetaFromWellStructured WellStructuredCms { content = Manifest {..} } key = MftMeta {..}


-- ---------------------------------------------------------------------------
-- Manifest / Certificate index functions
-- ---------------------------------------------------------------------------

-- | Sorted newest-first by `Ord MftMeta` (thisTime, then nextTime, then
-- mftNumber as a last-resort tiebreaker) -- NOT by manifest_number alone,
-- since manifest serial numbers aren't guaranteed to grow monotonically
-- (e.g. ARIN's don't in practice), so sorting purely by manifest_number
-- can pick the wrong "latest" manifest. Sorted here instead of via SQL
-- `ORDER BY` because thisTime/nextTime live inside the serialised `meta`
-- BLOB, not as their own columns.
getMftsForAKI :: MonadIO m => Tx mode -> DB -> AKI -> m [MftMeta]
getMftsForAKI (Tx conn) _ aki_ = liftIO $ do
    rows <- query conn
        "SELECT meta FROM manifest_meta WHERE aki = ?"
        (Only aki_)
    pure $! List.sortOn Down $ map (deserialiseField . fromOnly) rows

findAllMftsByAKI :: MonadIO m
                 => Tx mode -> DB -> AKI -> m [(MftMeta, Keyed (Located WellStructuredMft))]
findAllMftsByAKI tx db aki_ = liftIO $ do
    metas <- getMftsForAKI tx db aki_
    fmap catMaybes $ forM metas $ \meta ->
        fmap (meta,) <$> getMftByKey tx db (meta ^. #key)

getMftByKey :: MonadIO m
            => Tx mode -> DB -> ObjectKey -> m (Maybe (Keyed (Located WellStructuredMft)))
getMftByKey tx db k = do
    o <- getLocatedByKey tx db k
    pure $! case o of
        Just (Located loc (WellStructuredRO (MftRO mft))) -> Just $ Keyed (Located loc mft) k
        _                              -> Nothing

getMftShorcutMeta :: MonadIO m => Tx mode -> DB -> AKI -> m (Maybe MftShortcutMeta)
getMftShorcutMeta (Tx conn) _ aki = liftIO $ do
    rows <- query conn "SELECT data FROM mft_shortcut_meta WHERE aki = ?" (Only aki)
    pure $! deserialiseCompressed . fromOnly <$> listToMaybe rows

-- | Children without file_name, for the hot "nothing changed" path that never needs it.
getMftShorcutChildrenLight :: MonadIO m => Tx mode -> DB -> AKI -> m (Map.Map ObjectKey MftChild)
getMftShorcutChildrenLight (Tx conn) _ aki = liftIO $ do
    rows <- query conn
        [sql|
            SELECT c.child_key, s.data
            FROM mft_shortcut_children c
            JOIN shortcuts s ON s.object_key = c.child_key
            WHERE c.aki = ?
        |]
        (Only aki)
    pure $! Map.fromList
        [ (childKey, deserialiseCompressed dataBs)
        | (childKey, dataBs) <- rows ]

-- | Full children incl. file_name, for the diff path that needs to detect renames.
getMftShorcutChildrenFull :: MonadIO m => Tx mode -> DB -> AKI -> m (Map.Map ObjectKey MftEntry)
getMftShorcutChildrenFull (Tx conn) _ aki = liftIO $ do
    rows <- query conn
        [sql|
            SELECT c.file_name, c.child_key, s.data
            FROM mft_shortcut_children c
            JOIN shortcuts s ON s.object_key = c.child_key
            WHERE c.aki = ?
        |]
        (Only aki)
    pure $! Map.fromList
        [ (childKey, MftEntry { fileName = fileName_, child = deserialiseCompressed dataBs })
        | (fileName_, childKey, dataBs) <- rows ]

-- | On-demand single-row lookup, used only by the rare TroubledChild fallback
-- on the light (file_name-free) read path.
getMftShortcutChildFileName :: MonadIO m => Tx mode -> DB -> AKI -> ObjectKey -> m (Maybe Text)
getMftShortcutChildFileName (Tx conn) _ aki childKey = liftIO $ do
    rows <- query conn
        "SELECT file_name FROM mft_shortcut_children WHERE aki = ? AND child_key = ?"
        (aki, childKey)
    pure $! fromOnly <$> listToMaybe rows

getMftShorcut :: MonadIO m => Tx mode -> DB -> AKI -> m (Maybe MftShortcut)
getMftShorcut tx db aki = do
    metaM <- getMftShorcutMeta tx db aki
    case metaM of
        Nothing -> pure Nothing
        Just MftShortcutMeta {..} -> do
            nonCrlEntries <- getMftShorcutChildrenFull tx db aki
            pure $! Just $! MftShortcut {..}

saveMftShorcutMeta :: MonadIO m => Tx 'RW -> DB -> AKI -> Verbatim (Compressed MftShortcutMeta) -> m ()
saveMftShorcutMeta (Tx conn) _ aki meta = liftIO $
    execute conn
        "INSERT OR REPLACE INTO mft_shortcut_meta(aki, data) VALUES (?, ?)"
    (aki, unStorable $ unVerbatim meta)

-- | Insert only the given (new) children; never touches rows for unchanged children.
-- `OR REPLACE` on purpose: a TroubledChild re-validation, or a manifest-entry
-- rename (same child_key, new file_name), can legitimately overwrite an
-- existing row for a key that's already cached.
insertMftShortcutChildren :: MonadIO m => Tx 'RW -> DB -> AKI -> [(ObjectKey, Text, BS.ByteString)] -> m ()
insertMftShortcutChildren (Tx conn) _ aki newEntries = liftIO $ do
    executeMany conn
        "INSERT OR REPLACE INTO shortcuts(object_key, data) VALUES (?, ?)"
        [ (childKey, dataBs) | (childKey, _, dataBs) <- newEntries ]
    executeMany conn
        "INSERT OR REPLACE INTO mft_shortcut_children(aki, file_name, child_key) VALUES (?, ?, ?)"
        [ (aki, fileName_, childKey) | (childKey, fileName_, _) <- newEntries ]

-- | Delete only this AKI's (aki, child_key) membership rows. Never touches
-- `shortcuts` -- an orphaned shortcut is cleaned up by the general objects
-- cleanup/GC (deleteObjectByKey etc.), which cascades objects -> shortcuts ->
-- mft_shortcut_children once nothing marks the underlying object as used.
deleteMftShortcutChildren :: MonadIO m => Tx 'RW -> DB -> AKI -> [ObjectKey] -> m ()
deleteMftShortcutChildren (Tx conn) _ aki deletedKeys = liftIO $
    forM_ (inClauseBatches deletedKeys) $ \(placeholders, params) ->
        executeNamed conn
            (fromString $ Text.unpack $
                "DELETE FROM mft_shortcut_children WHERE aki = :aki AND child_key IN (" <> placeholders <> ")")
            ((":aki" := aki) : params)

deleteMftShortcut :: MonadIO m => Tx 'RW -> DB -> AKI -> m ()
deleteMftShortcut tx@(Tx conn) db aki = liftIO $ do
    childKeys <- map fromOnly <$>
        query conn "SELECT child_key FROM mft_shortcut_children WHERE aki = ?" (Only aki)
    execute conn "DELETE FROM mft_shortcut_meta WHERE aki = ?" (Only aki)
    deleteMftShortcutChildren tx db aki childKeys

-- | Returns all candidates for the SKI; callers must verify signatures.
getBySKI :: MonadIO m => Tx mode -> DB -> SKI -> m [Located WellStructuredCaCert]
getBySKI tx@(Tx conn) db ski = liftIO $ do
    rows <- query conn
        "SELECT object_key FROM certificates WHERE ski = ?"
        (Only ski)
    let objectKeys = map fromOnly rows
    fmap catMaybes $ forM objectKeys $ \k ->
        getLocatedByKey tx db k >>= \case
            Just (Located loc (WellStructuredRO (CerRO c))) ->
                pure $ Just (Located loc c)
            _ -> pure Nothing

-- | Backward-compat wrapper: returns the first CA cert matching the SKI.
getFirstCaCertBySKI :: MonadIO m => Tx mode -> DB -> SKI -> m (Maybe (Located WellStructuredCaCert))
getFirstCaCertBySKI tx db ski =
    listToMaybe <$> getBySKI tx db ski

getTaCertByKey :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe WellStructuredCaCert)
getTaCertByKey tx db k =
    getLocatedByKey tx db k >>= \case
        Just (Located _ (WellStructuredRO (CerRO c))) -> pure $ Just c
        _                                             -> pure Nothing

markAsValidated :: MonadIO m
                => Tx 'RW -> DB -> Set.Set ObjectKey -> WorldVersion -> m ()
markAsValidated tx db allKeys worldVersion =
    liftIO $ void $ updateValidatedByVersionMap tx db $ \m ->
        foldr (`Map.insert` worldVersion) m allKeys

-- ---------------------------------------------------------------------------
-- TA functions
-- ---------------------------------------------------------------------------

saveTA :: MonadIO m => Tx 'RW -> DB -> StorableTA -> m ()
saveTA (Tx conn) _ ta = liftIO $
    execute conn
        "INSERT OR REPLACE INTO trust_anchors(ta_name, ta_cert_key, data, active) VALUES (?, ?, ?, 1)"
        (unTaName (getTaName (tal ta)), taCertKey ta, serialiseField ta)

deleteTA :: MonadIO m => Tx 'RW -> DB -> TAL -> m ()
deleteTA (Tx conn) _ t = liftIO $
    execute conn "DELETE FROM trust_anchors WHERE ta_name = ?" (Only (unTaName (getTaName t)))

getTA :: MonadIO m => Tx mode -> DB -> TaName -> m (Maybe StorableTA)
getTA (Tx conn) _ name = liftIO $ do
    rows <- query conn "SELECT data FROM trust_anchors WHERE ta_name = ?" (Only (unTaName name))
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

getTAs :: MonadIO m => Tx mode -> DB -> m [StorableTA]
getTAs (Tx conn) _ = liftIO $ do
    rows <- query_ conn "SELECT data FROM trust_anchors WHERE active = 1"
    pure $ map (deserialiseField . fromOnly) rows

setActiveTAs :: MonadIO m => Tx 'RW -> DB -> [TaName] -> m ()
setActiveTAs (Tx conn) _ taNames = liftIO $ do
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
versionsBackwards :: MonadIO m => Tx mode -> DB -> m [WorldVersion]
versionsBackwards (Tx conn) _ = liftIO $
    map fromOnly <$> query_ conn "SELECT DISTINCT version FROM validation_outcomes ORDER BY version DESC"

previousVersion :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe WorldVersion)
previousVersion tx db version = liftIO $ do
    vs <- versionsBackwards tx db
    pure $ case filter (< version) vs of
        [] -> Nothing
        xs -> Just $ maximum xs

getLatestVersion :: MonadIO m => Tx mode -> DB -> m (Maybe WorldVersion)
getLatestVersion tx db = listToMaybe <$> versionsBackwards tx db

rowsToPerTa :: AsStorable a => [(Text, BS.ByteString)] -> PerTA a
rowsToPerTa rows = toPerTA
    [ (TaName taName, deserialiseCompressed bs) | (taName, bs) <- rows ]

mkLatestPerTaQuery :: [Text] -> Query
mkLatestPerTaQuery columns =
    fromString . Text.unpack $ Text.unlines $
        [ "WITH ranked AS ("
        , "    SELECT " <> Text.intercalate ", " (["vo.ta_name"] <> fmap ("vo." <>) columns) <> ","
        , "           ROW_NUMBER() OVER (PARTITION BY vo.ta_name ORDER BY vo.version DESC) AS rn"
        , "    FROM validation_outcomes vo"
        , "    JOIN trust_anchors ta ON ta.ta_name = vo.ta_name"
        , "    WHERE ta.active = 1"
        , "      AND vo.version <= :version"
        ]
        <> fmap (\column -> "      AND vo." <> column <> " IS NOT NULL") columns
        <>
        [ ")"
        , "SELECT " <> Text.intercalate ", " ("ta_name" : columns)
        , "FROM ranked"
        , "WHERE rn = 1"
        ]

mkLatestCommonQuery :: [Text] -> Query
mkLatestCommonQuery columns =
    fromString . Text.unpack $ Text.unlines $
        [ "WITH ranked AS ("
        , "    SELECT " <> Text.intercalate ", " (fmap ("vo." <>) columns) <> ","
        , "           ROW_NUMBER() OVER (ORDER BY vo.version DESC) AS rn"
        , "    FROM validation_outcomes vo"
        , "    WHERE vo.ta_name IS NULL"
        , "      AND vo.version <= :version"
        ]
        <> fmap (\column -> "      AND vo." <> column <> " IS NOT NULL") columns
        <>
        [ ")"
        , "SELECT " <> Text.intercalate ", " columns
        , "FROM ranked"
        , "WHERE rn = 1"
        ]

mkLatestPayloadForTaQuery :: Text -> Query
mkLatestPayloadForTaQuery column =
    fromString . Text.unpack $ Text.unlines
        [ "WITH ranked AS ("
        , "    SELECT vo." <> column <> ","
        , "           ROW_NUMBER() OVER (ORDER BY vo.version DESC) AS rn"
        , "    FROM validation_outcomes vo"
        , "    JOIN trust_anchors ta ON ta.ta_name = vo.ta_name"
        , "    WHERE ta.active = 1"
        , "      AND vo.ta_name = :ta_name"
        , "      AND vo.version <= :version"
        , "      AND vo." <> column <> " IS NOT NULL"
        , ")"
        , "SELECT " <> column
        , "FROM ranked"
        , "WHERE rn = 1"
        ]

getValidationsPerTA :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Validations)
getValidationsPerTA (Tx conn) _ version = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPerTaQuery ["validations"])
        [":version" := version]
    pure $ rowsToPerTa rows

getMetricsPerTA :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Metrics)
getMetricsPerTA (Tx conn) _ version = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPerTaQuery ["metrics"])
        [":version" := version]
    pure $ rowsToPerTa rows

getCommonMetrics :: MonadIO m => Tx mode -> DB -> WorldVersion -> m Metrics
getCommonMetrics (Tx conn) _ version = liftIO $ fmap (fromMaybe mempty) $ do
    rows <- queryNamed conn
        (mkLatestCommonQuery ["metrics"])
        [":version" := version]
    pure $ deserialiseCompressed . fromOnly <$> listToMaybe rows

getValidationOutcomes :: MonadIO m
                      => Tx mode
                      -> DB
                      -> WorldVersion
                      -> m (Validations, Metrics, PerTA (Validations, Metrics))
getValidationOutcomes (Tx conn) _ version = liftIO $ do
    commonRows <- queryNamed conn
                (mkLatestCommonQuery ["validations", "metrics"])
        [":version" := version]

    perTaRows <- queryNamed conn
                (mkLatestPerTaQuery ["validations", "metrics"])
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

getVrps :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Vrps)
getVrps tx db version = fmap toVrps <$> getRoas tx db version

getVrpsForTA :: MonadIO m => Tx mode -> DB -> WorldVersion -> TaName -> m Vrps
getVrpsForTA (Tx conn) _ version taName = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPayloadForTaQuery "roas")
        [":ta_name" := unTaName taName, ":version" := version]
    pure $ toVrps $ maybe mempty (deserialiseCompressed . fromOnly) (listToMaybe rows)

getRoas :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Roas)
getRoas (Tx conn) _ version = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPerTaQuery ["roas"])
        [":version" := version]
    pure $ rowsToPerTa rows

getAspas :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set Aspa))
getAspas (Tx conn) _ version = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPerTaQuery ["aspa"])
        [":version" := version]
    pure $ Just $ allTAs (rowsToPerTa rows)

getGbrs :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set (T2 Hash Gbr)))
getGbrs (Tx conn) _ version = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPerTaQuery ["gbrs"])
        [":version" := version]
    pure $ Just $ allTAs (rowsToPerTa rows)

getBgps :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set BGPSecPayload))
getBgps (Tx conn) _ version = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPerTaQuery ["bgps"])
        [":version" := version]
    pure $ Just $ allTAs (rowsToPerTa rows)

getSpls :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set SplN))
getSpls (Tx conn) _ version = liftIO $ do
    rows <- queryNamed conn
        (mkLatestPerTaQuery ["spls"])
        [":version" := version]
    pure $ Just $ allTAs (rowsToPerTa rows)

saveValidationVersion :: MonadIO m
                      => Tx 'RW
                      -> DB
                      -> WorldVersion
                      -> PerTA (Payloads, ValidationState)
                      -> ValidationState
                      -> m ()
saveValidationVersion (Tx conn) _ validatedBy results commonVS =
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

deleteValidationVersion :: MonadIO m => Tx 'RW -> DB -> WorldVersion -> m ()
deleteValidationVersion (Tx conn) _ worldVersion = liftIO $ do
        execute conn "DELETE FROM validation_outcomes WHERE version = ?"
            (Only worldVersion)
        execute conn "DELETE FROM slurm    WHERE key = ?" (Only worldVersion)

saveSlurm :: MonadIO m => Tx 'RW -> DB -> WorldVersion -> Slurm -> m ()
saveSlurm (Tx conn) _ version slurm = liftIO $
    execute conn "INSERT OR REPLACE INTO slurm(key, value) VALUES (?, ?)"
        (version, serialiseCompressed slurm)

getSlurm :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe Slurm)
getSlurm (Tx conn) _ version = liftIO $ do
    rows <- query conn "SELECT value FROM slurm WHERE key = ?"
                (Only version)
    pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getLatestVersions :: MonadIO m => Tx mode -> DB -> m (PerTA WorldVersion)
getLatestVersions (Tx conn) _ = liftIO $ do
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

updateRrdpMeta :: MonadIO m => Tx 'RW -> DB -> RrdpMeta -> RrdpURL -> m ()
updateRrdpMeta tx db meta url = liftIO $ updateRrdpMetaM tx db url (const $ pure $ Just meta)

updateRrdpMetaM :: MonadIO m
                => Tx 'RW
                -> DB
                -> RrdpURL
                -> (Maybe RrdpMeta -> IO (Maybe RrdpMeta))
                -> m ()
updateRrdpMetaM (Tx conn) _ url f = liftIO $ do
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

getPublicationPoints :: MonadIO m => Tx mode -> DB -> m PublicationPoints
getPublicationPoints (Tx conn) _ = liftIO $ do
    rrdpRows  <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rrdp-pp'"
    rsyncRows <- query_ conn "SELECT key, data FROM repositories WHERE kind = 'rsync-pp'"
    let rrdps  = [ (deserialiseField k, deserialiseField v) | (k, v) <- rrdpRows ]
        rsyncs = [ (deserialiseField k, deserialiseField v) | (k, v) <- rsyncRows ]
    pure $ PublicationPoints
        (RrdpMap $ Map.fromList rrdps)
        (RsyncForestGen $ Map.fromList rsyncs)

getRepository :: MonadIO m => Tx mode -> DB -> RpkiURL -> m (Maybe Repository)
getRepository tx db = \case
    RrdpU u  -> fmap RrdpR  <$> getRrdpRepository tx db u
    RsyncU u -> fmap RsyncR <$> getRsyncRepository tx db u

getRrdpRepository :: MonadIO m => Tx mode -> DB -> RrdpURL -> m (Maybe RrdpRepository)
getRrdpRepository (Tx conn) _ url = liftIO $ do
    rows <- query conn "SELECT data FROM repositories WHERE key = ? AND kind = 'rrdp-pp'"
                (Only (serialiseField url))
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

getRsyncRepository :: MonadIO m => Tx mode -> DB -> RsyncURL -> m (Maybe RsyncRepository)
getRsyncRepository tx db url = Map.lookup url <$> getRsyncRepositories tx db [url]

getRsyncRepositories :: MonadIO m
                     => Tx mode -> DB -> [RsyncURL] -> m (Map.Map RsyncURL RsyncRepository)
getRsyncRepositories tx db urls =
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

saveRepositories :: MonadIO m => Tx 'RW -> DB -> [Repository] -> m ()
saveRepositories tx db repos = liftIO $ do
    let (rrdps, rsyncs) = foldr sep ([], []) repos
    let Tx conn = tx
    executeMany conn
        "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rrdp-pp', ?)"
        [ (serialiseField (r ^. #uri), serialiseField r) | r <- rrdps ]
    saveRsyncRepositories tx db rsyncs
  where
    sep (RrdpR r)  (rs, ss) = (r : rs, ss)
    sep (RsyncR r) (rs, ss) = (rs, r : ss)

saveRepositoryValidationStates :: MonadIO m
                                => Tx 'RW -> DB -> [(Repository, ValidationState)] -> m ()
saveRepositoryValidationStates tx db repos = liftIO $ do
    let (rrdps, rsyncs) = foldr sep ([], []) repos
    let Tx conn = tx
    executeMany conn
        "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rrdp-vstate', ?)"
        [ (serialiseField (r ^. #uri), serialiseCompressed vs) | (r, vs) <- rrdps ]
    saveRsyncValidationStates tx db rsyncs
  where
    sep (RrdpR r,  a) (rs, ss) = ((r, a) : rs, ss)
    sep (RsyncR r, a) (rs, ss) = (rs, (r, a) : ss)

saveRsyncRepositories :: MonadIO m => Tx 'RW -> DB -> [RsyncRepository] -> m ()
saveRsyncRepositories (Tx conn) _ repos = liftIO $
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
                          => Tx 'RW -> DB -> [(RsyncRepository, ValidationState)] -> m ()
saveRsyncValidationStates tx db repos = liftIO $
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
                => Tx mode -> DB -> (RpkiURL -> Bool) -> m [(Repository, ValidationState)]
getRepositories (Tx conn) _ filterF = liftIO $ do
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


-- ---------------------------------------------------------------------------
-- Job / Metadata
-- ---------------------------------------------------------------------------

setJobCompletionTime :: MonadIO m => Tx 'RW -> DB -> Text -> Instant -> m ()
setJobCompletionTime (Tx conn) _ job t = liftIO $
    execute conn "INSERT OR REPLACE INTO jobs(key, value) VALUES (?, ?)"
        (job, serialiseField t)

allJobs :: MonadIO m => Tx mode -> DB -> m [(Text, Instant)]
allJobs (Tx conn) _ = liftIO $ do
    rows <- query_ conn "SELECT key, value FROM jobs"
    pure [ (k, deserialiseField v) | (k, v) <- rows ]

getDatabaseVersion :: MonadIO m => Tx mode -> DB -> m (Maybe Integer)
getDatabaseVersion (Tx conn) _ = liftIO $ do
    rows <- query conn "SELECT value FROM metadata WHERE key = ?"
                (Only databaseVersionKey)
    pure $ case rows of
        [Only t] -> readMaybe (Text.unpack t)
        _        -> Nothing

saveCurrentDatabaseVersion :: MonadIO m => Tx 'RW -> DB -> m ()
saveCurrentDatabaseVersion (Tx conn) _ = liftIO $
    execute conn "INSERT OR REPLACE INTO metadata(key, value) VALUES (?, ?)"
        (databaseVersionKey, Text.pack $ show currentDatabaseVersion)

-- | Shared by updateValidatedByVersionMap and deleteStaleContent's sweep --
-- both need the same "current validated-by-version map, or empty" read.
getValidatedByVersionMap :: SQLite.CachedConn -> IO (Map.Map ObjectKey WorldVersion)
getValidatedByVersionMap conn = do
    rows <- query conn "SELECT value FROM validated_by_version WHERE key = ?"
                (Only validatedByVersionKey)
    pure $ case rows of
        [Only bs] -> deserialiseCompressed bs
        _         -> mempty

updateValidatedByVersionMap :: MonadIO m
                            => Tx 'RW
                            -> DB
                            -> (Map.Map ObjectKey WorldVersion -> Map.Map ObjectKey WorldVersion)
                            -> m (Map.Map ObjectKey WorldVersion)
updateValidatedByVersionMap (Tx conn) _ f = liftIO $ do
    updated <- f <$> getValidatedByVersionMap conn
    execute conn "INSERT OR REPLACE INTO validated_by_version(key, value) VALUES (?, ?)"
        (validatedByVersionKey, serialiseCompressed updated)
    pure updated


-- ---------------------------------------------------------------------------
-- Stats
-- ---------------------------------------------------------------------------

getObjectsStats :: MonadIO m => Tx mode -> DB -> m ObjectStats
getObjectsStats (Tx conn) _ = liftIO $ do
    rows <- query_ conn
        [sql|
            SELECT type, COUNT(*), SUM(LENGTH(COALESCE(data, original)))
            FROM objects GROUP BY type
        |]
    pure $ foldr accumulate mempty rows
  where
    accumulate (typText, cnt, sz) acc =
        case readMaybe typText of
            Nothing  -> acc
            Just typ ->
                let count      = Size (fromIntegral (cnt :: Int64))
                    objectSize = Size (fromIntegral (sz  :: Int64))
                in  acc & #totalObjects %~ (+ count)
                        & #totalSize    %~ (+ objectSize)
                        & #countPerType     %~ Map.insertWith (+) typ count
                        & #totalSizePerType %~ Map.insertWith (+) typ objectSize
                        & #minSizePerType   %~ Map.alter (Just . maybe objectSize (min objectSize)) typ
                        & #maxSizePerType   %~ Map.alter (Just . maybe objectSize (max objectSize)) typ

totalStats :: StorageStats -> SStats
totalStats (StorageStats s) = mconcat $ Map.elems s


-- ---------------------------------------------------------------------------
-- Complex cleanup operations
-- ---------------------------------------------------------------------------

data CleanUpResult = CleanUpResult
    { deletedObjects  :: Int
    , deletedPerType  :: Map.Map RpkiObjectType Integer
    , keptObjects     :: Int
    , deletedURLs     :: Int
    , deletedVersions :: Int
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)

data DeletionCriteria = DeletionCriteria
    { versionIsTooOld :: WorldVersion -> Bool
    , objectIsTooOld  :: WorldVersion -> RpkiObjectType -> Bool
    }
    deriving (Generic)

-- | Keep the newest `versionNumberToKeep` versions that actually carry data
-- for each TA, and delete everything older. A version round doesn't
-- necessarily have fresh data for every TA (a TA that failed to fetch just
-- keeps reusing older data), so we can't just keep the last N rounds -- some
-- of the last N rounds may not have moved a given TA forward at all. Instead,
-- for every TA find the round at which it accumulates N *distinct* rounds of
-- real data counting backwards from the newest, and keep everything back to
-- the earliest such round across all TAs (the most demanding one). A TA that
-- never reaches N real rounds in its whole history blocks deletion entirely,
-- same as it would if we kept everything to satisfy it.
deleteOldestVersionsIfNeeded :: MonadIO m
                             => Tx 'RW -> DB -> Natural -> m [WorldVersion]
deleteOldestVersionsIfNeeded tx@(Tx conn) db versionNumberToKeep =
    mapException (AppException . storageError) <$> liftIO $ do
        versions <- versionsBackwards tx db
        let reallyToKeep = max 2 (fromIntegral versionNumberToKeep)
        case NonEmpty.nonEmpty versions of
            Just neVersions | NonEmpty.length neVersions > reallyToKeep -> do
                taVersionRows <- query_ conn
                    "SELECT ta_name, version FROM validation_outcomes WHERE ta_name IS NOT NULL"
                    :: IO [(Text, WorldVersion)]
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

                forM_ versionsToDelete $ deleteValidationVersion tx db
                pure versionsToDelete
            _ -> pure []

deleteStaleContent :: MonadIO m => DB -> DeletionCriteria -> m CleanUpResult
deleteStaleContent db DeletionCriteria{..} =
    mapException (AppException . storageError) <$> liftIO $
        rwTx db $ \tx -> do
            deletedVersions <- deleteOldVersions tx
            (deletedObjects, deletedPerType, keptObjects) <- deleteStaleObjects tx
            deletedURLs <- deleteDanglingUrls tx
            pure CleanUpResult{..}
  where
    deleteOldVersions tx = do
        toDelete <- filter versionIsTooOld <$> versionsBackwards tx db
        forM_ toDelete $ deleteValidationVersion tx db
        pure $ length toDelete

    deleteStaleObjects tx = do
        let Tx conn = tx
        validatedBy <- getValidatedByVersionMap conn

        allObjs <- query_ conn
            "SELECT object_key, world_version, type FROM objects"

        deletedPerType <- newTVarIO mempty
        keptTotal      <- newTVarIO (0 :: Int)
        keysToDelete   <- fmap catMaybes $ forM allObjs $
            \(objectKey, insertedBy, typText) -> case readMaybe typText of
                Nothing  -> pure Nothing
                Just typ -> do
                    let insertedOld = objectIsTooOld insertedBy typ
                        validatedOld = case Map.lookup objectKey validatedBy of
                            Just wv -> objectIsTooOld wv typ
                            Nothing -> True
                    if insertedOld && validatedOld
                        then do
                            atomically $ modifyTVar' deletedPerType $
                                Map.unionWith (+) (Map.singleton typ 1)
                            pure $ Just objectKey
                        else do
                            atomically $ modifyTVar' keptTotal (+1)
                            pure Nothing

        let validatedBy' = foldr Map.delete validatedBy keysToDelete
        execute conn "INSERT OR REPLACE INTO validated_by_version(key, value) VALUES (?, ?)"
            (validatedByVersionKey, serialiseCompressed validatedBy')

        deleteObjectByKey tx db keysToDelete

        atomically $ do
            deleted <- readTVar deletedPerType
            let deletedCount = fromIntegral $ sum $ Map.elems deleted
            kept    <- readTVar keptTotal
            pure (deletedCount, deleted, kept)

deleteDanglingUrls :: Tx 'RW -> IO Int
deleteDanglingUrls (Tx conn) = do
    execute_ conn
        "DELETE FROM urls WHERE url_key NOT IN (SELECT DISTINCT url_key FROM object_urls)"
    changes conn

getAll :: MonadIO m => Tx mode -> DB -> m [Located RpkiObjectLifecycle]
getAll tx db = liftIO $ do
    let Tx conn = tx
    rows <- query_ conn "SELECT object_key FROM objects WHERE data IS NOT NULL"
    catMaybes <$> forM rows (getLocatedByKey tx db . fromOnly)

getMftMeta :: MftObject -> ObjectKey -> MftMeta
getMftMeta mft key =
    let Manifest{..} = getCMSContent $ cmsPayload mft
    in MftMeta{..}

getGbrObjects :: MonadIO m => Tx mode -> DB -> WorldVersion -> m [Located RpkiObjectLifecycle]
getGbrObjects tx db version = do
    gbrs <- maybe [] Set.toList <$> getGbrs tx db version
    fmap catMaybes $ forM gbrs $ \(T2 hash _) -> getByHash tx db hash

getRtrPayloads :: MonadIO m => Tx 'RO -> DB -> WorldVersion -> m (Maybe RtrPayloads)
getRtrPayloads tx db worldVersion = liftIO $ runMaybeT $ do
    vrps <- MaybeT $ Just <$> getVrps tx db worldVersion
    bgps <- MaybeT $ getBgps tx db worldVersion
    pure $ mkRtrPayloads vrps bgps


-- ---------------------------------------------------------------------------
-- Transaction wiring (ValidatorT integration)
-- ---------------------------------------------------------------------------

roAppTx :: DB -> (Tx 'RO -> ValidatorT IO a) -> ValidatorT IO a
roAppTx db f = appTx db f withReadTx

rwAppTx :: DB -> (Tx 'RW -> ValidatorT IO a) -> ValidatorT IO a
rwAppTx db f = appTx db f withWriteTx

appTx :: DB
      -> (Tx mode -> ValidatorT IO a)
      -> (DB -> (Tx mode -> IO (Either AppError a, ValidationState))
             -> IO (Either AppError a, ValidationState))
      -> ValidatorT IO a
appTx db f txF = do
    scopes <- ask
    embedValidatorT $
        txF db (\tx -> do
            z@(r, vs) <- runValidatorT scopes (f tx)
            case r of
                Left e  -> throwIO (TxRollbackException e vs)
                Right _ -> pure z)
        `catch` (\(TxRollbackException e vs) -> pure (Left e, vs))

roAppTxEx :: Exception exc
          => DB
          -> (exc -> AppError)
          -> (Tx 'RO -> ValidatorT IO a)
          -> ValidatorT IO a
roAppTxEx db err f = appTxEx db err f withReadTx

rwAppTxEx :: Exception exc
          => DB
          -> (exc -> AppError)
          -> (Tx 'RW -> ValidatorT IO a)
          -> ValidatorT IO a
rwAppTxEx db err f = appTxEx db err f withWriteTx

appTxEx :: Exception exc
        => DB
        -> (exc -> AppError)
        -> (Tx mode -> ValidatorT IO a)
        -> (DB -> (Tx mode -> IO (Either AppError a, ValidationState))
               -> IO (Either AppError a, ValidationState))
        -> ValidatorT IO a
appTxEx db err f txF = do
    scopes <- ask
    embedValidatorT $
        txF db (\tx -> do
            z@(r, vs) <- runValidatorT scopes (f tx)
            case r of
                Left e  -> throwIO (TxRollbackException e vs)
                Right _ -> pure z)
        `catches`
            [ Handler $ \(TxRollbackException e vs) -> pure (Left e, vs)
            , Handler $ \e                           -> pure (Left (err e), mempty)
            ]

data TxRollbackException = TxRollbackException AppError ValidationState
    deriving stock (Show, Eq, Ord, Generic)

data StorageCorruptedException = StorageCorruptedException Text
    deriving stock (Show, Eq, Ord, Generic)

instance Exception TxRollbackException
instance Exception StorageCorruptedException

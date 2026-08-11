{-# LANGUAGE OverloadedStrings   #-}
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
    MftShortcutMeta(..), MftShortcutChildren(..),
    -- * Query functions
    getKeyByHash, getObjectKey, getByHash, getKeyedByHash,
    getByUri, getKeysByUri,
    getObjectByKey, getLocatedByKey,
    getLocationCountByKey, getLocationsByKey,
    saveObject, saveOriginal,
    getOriginalBlob, getOriginalBlobByHash,
    getObjectMeta, linkObjectToUrl,
    hashExists, deleteObjectByHash, deleteObjectByKey,
    getMftsForAKI, findAllMftsByAKI, getMftByKey,
    getMftShorcut, saveMftShorcutMeta, saveMftShorcutChildren,
    deleteMftShortcut, getBySKI, getFirstCaCertBySKI,
    markAsValidated,
    saveTA, deleteTA, getTA, getTAs,
    versionsBackwards, previousVersion, getLatestVersion,
    getVersionMeta, getPayloadsForTas,
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
import qualified Data.Set                 as Set
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Data.Map.Strict          as Map
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import           Data.Int                 (Int64)
import           Data.Ord
import           Data.Tuple.Strict

import           GHC.Generics
import           GHC.Natural
import           Text.Read

import           Database.SQLite.Simple
import           Data.Bits                (shiftR, (.&.))
import           Data.Store               (Store)
import qualified Data.ByteString       as BS

import           RPKI.Domain  hiding (object)
import           RPKI.Reporting
import           RPKI.TAL
import           RPKI.RRDP.Types
import           RPKI.SLURM.Types
import           RPKI.Repository

import           RPKI.Store.Base.Serialisation
import           RPKI.Store.Base.Storable
import           RPKI.Store.SQLite            (Tx(..), SqliteDB(..), TxMode(..))
import qualified RPKI.Store.SQLite            as SQLite
import           RPKI.Store.Types
import           RPKI.Validation.Types

import           RPKI.Util                (ifJustM, fmtEx, parseRpkiURL)
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
    withReadTx db (\tx -> f tx db)

rwTxT :: MonadIO m => TVar DB -> (Tx 'RW -> DB -> IO a) -> m a
rwTxT tdb f = liftIO $ do
    db <- readTVarIO tdb
    withWriteTx db (\tx -> f tx db)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- Increment whenever any serialised type changes incompatibly.
currentDatabaseVersion :: Integer
currentDatabaseVersion = 49

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

newtype MftShortcutChildren = MftShortcutChildren
    { nonCrlEntries :: Map.Map ObjectKey MftEntry }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)


-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

onlyValue :: [Only a] -> Maybe a
onlyValue []          = Nothing
onlyValue (Only v : _) = Just v

-- | Encode StorableObject using AsStorable (pre-serialised bytes + compression).
encodeSO :: AsStorable a => a -> BS.ByteString
encodeSO = unStorable . toStorable . Compressed

-- | Decode a StorableObject from compressed bytes.
decodeSO :: AsStorable a => BS.ByteString -> a
decodeSO bs = unCompressed (fromStorable (Storable bs))

insertCompressed :: AsStorable a => Connection -> Query -> a -> IO ArtificialKey
insertCompressed conn q val = do
    execute conn q (Only (serialiseCompressed val))
    SQLite.fromInt64 <$> lastInsertRowId conn

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


-- ---------------------------------------------------------------------------
-- Object functions
-- ---------------------------------------------------------------------------

getKeyByHash :: MonadIO m => Tx mode -> DB -> Hash -> m (Maybe ObjectKey)
getKeyByHash (Tx conn) _ h = liftIO $ do
    rows <- query conn
        "SELECT object_key FROM objects WHERE hash = ?"
        (Only (SQLite.hashToBlob h))
    pure $ SQLite.fromInt64 <$> onlyValue rows

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
    let uriText = toText uri
    rows <- query conn
        "SELECT ou.object_key \
        \FROM object_urls ou JOIN urls u USING(url_key) \
        \WHERE u.url = ?"
        (Only uriText)
    pure $ map (SQLite.fromInt64 . fromOnly) rows

getObjectByKey :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe RpkiObjectLifecycle)
getObjectByKey (Tx conn) _ k = liftIO $ do
    rows <- query conn
        "SELECT data FROM objects WHERE object_key = ? AND data IS NOT NULL"
        (Only (SQLite.toInt64 k))
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
        (Only (SQLite.toInt64 k))
    pure $ maybe 0 fromOnly (listToMaybe rows)

getLocationsByKey :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe Locations)
getLocationsByKey (Tx conn) _ k = liftIO $ do
    rows <- query conn
        "SELECT u.url FROM urls u \
        \JOIN object_urls ou USING(url_key) \
        \WHERE ou.object_key = ?"
        (Only (SQLite.toInt64 k))
    let urls = map fromOnly rows
    pure $ case urls of
        [] -> Nothing
        _  -> case mapM parseRpkiURL urls of
                Left _   -> Nothing
                Right us -> Locations <$> toNESet us        

saveObject :: MonadIO m
           => Tx 'RW
           -> DB
           -> RpkiObjectLifecycle
           -> WorldVersion
           -> m ObjectKey
saveObject (Tx conn) _ lifecycle wv = liftIO $ do
    let so     = toStorableObject lifecycle
        hBs    = SQLite.hashToBlob (getHash lifecycle)
        typ    = show (getRpkiObjectType lifecycle)
        dataBs = encodeSO so
        originalBs = case lifecycle of
            OriginalRO (ObjectOriginal blob) _ _ _ -> Just blob
            _                                      -> Nothing
        wvInt  = SQLite.toInt64 wv

    execute conn
        "INSERT OR IGNORE INTO objects(hash, type, data, original, world_version) \
        \VALUES (?, ?, ?, ?, ?)"
        (hBs, typ, dataBs, originalBs, wvInt)

    rows <- query conn "SELECT object_key FROM objects WHERE hash = ?" (Only hBs)
    let objectKey = SQLite.fromInt64 (fromOnly (head rows)) :: ObjectKey

    affectedRows <- changes conn
    when (affectedRows > 0) $ case lifecycle of
        WellStructuredRO (CerRO c) ->
            execute conn
                "INSERT OR IGNORE INTO certificates(object_key, ski, aki) VALUES (?, ?, ?)"
                ( SQLite.toInt64 objectKey
                , SQLite.skiToBlob (getSKI c)
                , SQLite.akiToBlob <$> getAKI c )
        WellStructuredRO (MftRO mft) ->
            forM_ (getAKI mft) $ \aki_ ->
                let meta = getMftMetaFromWellStructured mft objectKey
                in execute conn
                    "INSERT OR IGNORE INTO manifest_meta(object_key, aki, manifest_number, meta) \
                    \VALUES (?, ?, ?, ?)"
                    ( SQLite.toInt64 objectKey
                    , SQLite.akiToBlob aki_
                    , let Serial mftNum = meta ^. #mftNumber in serialToBlob mftNum
                    , serialiseField meta )
        _ -> pure ()

    pure objectKey

saveOriginal :: MonadIO m
             => Tx 'RW
             -> DB
             -> ObjectOriginal
             -> Hash
             -> ObjectMeta
             -> m ()
saveOriginal (Tx conn) _ (ObjectOriginal blob) h (ObjectMeta wv typ) = liftIO $ do
    let so = toStorableObject $ OriginalRO (ObjectOriginal blob) mempty h typ
    execute conn
        "INSERT OR IGNORE INTO objects(hash, type, data, original, world_version) \
        \VALUES (?, ?, ?, ?, ?)"
        (SQLite.hashToBlob h, show typ, encodeSO so, blob, SQLite.toInt64 wv)

getOriginalBlob :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe ObjectOriginal)
getOriginalBlob (Tx conn) _ k = liftIO $ do
    rows <- query conn
        "SELECT original FROM objects WHERE object_key = ? AND original IS NOT NULL"
        (Only (SQLite.toInt64 k))
    pure $ case rows of
        [Only bs] -> Just (ObjectOriginal bs)
        _         -> Nothing

getOriginalBlobByHash :: MonadIO m => Tx mode -> DB -> Hash -> m (Maybe ObjectOriginal)
getOriginalBlobByHash tx db h =
    getKeyByHash tx db h >>= \case
        Nothing  -> pure Nothing
        Just key -> getOriginalBlob tx db key

getObjectMeta :: MonadIO m => Tx mode -> DB -> ObjectKey -> m (Maybe ObjectMeta)
getObjectMeta (Tx conn) _ k = liftIO $ do
    rows <- query conn
        "SELECT world_version, type FROM objects WHERE object_key = ?"
        (Only (SQLite.toInt64 k))
    pure $ case rows of
        [(wvInt, typText)] -> case readMaybe typText of
            Just typ -> Just $ ObjectMeta (SQLite.fromInt64 wvInt) typ
            Nothing  -> Nothing
        _ -> Nothing

linkObjectToUrl :: MonadIO m => Tx 'RW -> DB -> RpkiURL -> ObjectKey -> m ()
linkObjectToUrl (Tx conn) _ rpkiURL objectKey = liftIO $ do
    let uriText = toText rpkiURL
    execute conn "INSERT OR IGNORE INTO urls(url) VALUES (?)" (Only uriText)
    ukRows <- query conn "SELECT url_key FROM urls WHERE url = ?" (Only uriText)
    forM_ (listToMaybe ukRows) $ \(Only ukInt) ->
        execute conn
            "INSERT OR IGNORE INTO object_urls(object_key, url_key) VALUES (?, ?)"
            (SQLite.toInt64 objectKey, ukInt :: Int64)

hashExists :: MonadIO m => Tx mode -> DB -> Hash -> m Bool
hashExists (Tx conn) _ h = liftIO $ do
    rows <- query conn "SELECT 1 FROM objects WHERE hash = ?"
                (Only (SQLite.hashToBlob h))
    pure $ not (null (rows :: [Only Int]))

deleteObjectByHash :: MonadIO m => Tx 'RW -> DB -> Hash -> m ()
deleteObjectByHash tx db h = liftIO $
    ifJustM (getKeyByHash tx db h) (deleteObjectByKey tx db)

-- | ON DELETE CASCADE handles certificates, manifest_meta, and object_urls.
deleteObjectByKey :: MonadIO m => Tx 'RW -> DB -> ObjectKey -> m ()
deleteObjectByKey (Tx conn) _ k = liftIO $
    execute conn "DELETE FROM objects WHERE object_key = ?" (Only (SQLite.toInt64 k))

getMftMetaFromWellStructured :: WellStructuredCms Manifest -> ObjectKey -> MftMeta
getMftMetaFromWellStructured WellStructuredCms { content = Manifest {..} } key = MftMeta {..}


-- ---------------------------------------------------------------------------
-- Manifest / Certificate index functions
-- ---------------------------------------------------------------------------

getMftsForAKI :: MonadIO m => Tx mode -> DB -> AKI -> m [MftMeta]
getMftsForAKI (Tx conn) _ aki_ = liftIO $ do
    rows <- query conn
        "SELECT meta FROM manifest_meta WHERE aki = ? ORDER BY manifest_number DESC"
        (Only (SQLite.akiToBlob aki_))
    pure $ map (deserialiseField . fromOnly) rows

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

getMftShorcut :: MonadIO m => Tx mode -> DB -> AKI -> m (Maybe MftShortcut)
getMftShorcut (Tx conn) _ aki = liftIO $ runMaybeT $ do
    MftShortcutMeta{..} <- MaybeT $ do
        rows <- query conn
            "SELECT data FROM mft_shortcut_meta WHERE aki = ?" (Only (SQLite.akiToBlob aki))
        pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)    
    MftShortcutChildren{..} <- MaybeT $ do
        rows <- query conn
            "SELECT data FROM mft_shortcut_children WHERE aki = ?" (Only (SQLite.akiToBlob aki))
        pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)
    pure $! MftShortcut {..}

saveMftShorcutMeta :: MonadIO m => Tx 'RW -> DB -> AKI -> Verbatim (Compressed MftShortcutMeta) -> m ()
saveMftShorcutMeta (Tx conn) _ aki meta = liftIO $
    execute conn
        "INSERT OR REPLACE INTO mft_shortcut_meta(aki, data) VALUES (?, ?)"
    (SQLite.akiToBlob aki, unStorable $ unVerbatim meta)

saveMftShorcutChildren :: MonadIO m => Tx 'RW -> DB -> AKI -> Verbatim (Compressed MftShortcutChildren) -> m ()
saveMftShorcutChildren (Tx conn) _ aki children = liftIO $
    execute conn
        "INSERT OR REPLACE INTO mft_shortcut_children(aki, data) VALUES (?, ?)"
    (SQLite.akiToBlob aki, unStorable $ unVerbatim children)

deleteMftShortcut :: MonadIO m => Tx 'RW -> DB -> AKI -> m ()
deleteMftShortcut (Tx conn) _ aki = liftIO $ do
    let akiBs = SQLite.akiToBlob aki
    execute conn "DELETE FROM mft_shortcut_meta     WHERE aki = ?" (Only akiBs)
    execute conn "DELETE FROM mft_shortcut_children WHERE aki = ?" (Only akiBs)

-- | Returns all candidates for the SKI; callers must verify signatures.
getBySKI :: MonadIO m => Tx mode -> DB -> SKI -> m [Located WellStructuredCaCert]
getBySKI tx@(Tx conn) db ski = liftIO $ do
    rows <- query conn
        "SELECT object_key FROM certificates WHERE ski = ?"
        (Only (SQLite.skiToBlob ski))
    let objectKeys = map (SQLite.fromInt64 . fromOnly) rows
    fmap catMaybes $ forM objectKeys $ \k ->
        getLocatedByKey tx db k >>= \case
            Just (Located loc (WellStructuredRO (CerRO c))) ->
                pure $ Just (Located loc c)
            _ -> pure Nothing

-- | Backward-compat wrapper: returns the first CA cert matching the SKI.
getFirstCaCertBySKI :: MonadIO m => Tx mode -> DB -> SKI -> m (Maybe (Located WellStructuredCaCert))
getFirstCaCertBySKI tx db ski =
    listToMaybe <$> getBySKI tx db ski

markAsValidated :: MonadIO m
                => Tx 'RW -> DB -> Set.Set ObjectKey -> WorldVersion -> m ()
markAsValidated tx db allKeys worldVersion =
    liftIO $ void $ updateValidatedByVersionMap tx db $ \m ->
        foldr (`Map.insert` worldVersion) (fromMaybe mempty m) allKeys

-- ---------------------------------------------------------------------------
-- TA functions
-- ---------------------------------------------------------------------------

saveTA :: MonadIO m => Tx 'RW -> DB -> StorableTA -> m ()
saveTA (Tx conn) _ ta = liftIO $
    execute conn
        "INSERT OR REPLACE INTO trust_anchors(ta_name, data) VALUES (?, ?)"
        (unTaName (getTaName (tal ta)), serialiseField ta)

deleteTA :: MonadIO m => Tx 'RW -> DB -> TAL -> m ()
deleteTA (Tx conn) _ t = liftIO $
    execute conn "DELETE FROM trust_anchors WHERE ta_name = ?" (Only (unTaName (getTaName t)))

getTA :: MonadIO m => Tx mode -> DB -> TaName -> m (Maybe StorableTA)
getTA (Tx conn) _ name = liftIO $ do
    rows <- query conn "SELECT data FROM trust_anchors WHERE ta_name = ?" (Only (unTaName name))
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

getTAs :: MonadIO m => Tx mode -> DB -> m [StorableTA]
getTAs (Tx conn) _ = liftIO $ do
    rows <- query_ conn "SELECT data FROM trust_anchors"
    pure $ map (deserialiseField . fromOnly) rows

-- ---------------------------------------------------------------------------
-- Version / Validation payload functions
-- ---------------------------------------------------------------------------

versionsBackwards :: MonadIO m => Tx mode -> DB -> m [(WorldVersion, VersionMeta)]
versionsBackwards (Tx conn) _ = liftIO $ do
    rows <- query_ conn "SELECT key, value FROM versions"
    pure $ List.sortOn (Down . fst)
        [ (deserialiseField k, deserialiseField v) | (k, v) <- rows ]

previousVersion :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe WorldVersion)
previousVersion tx db version = liftIO $ do
    vs <- versionsBackwards tx db
    pure $ case filter (\(v, _) -> v < version) vs of
        [] -> Nothing
        xs -> Just $ maximum (map fst xs)

getLatestVersion :: MonadIO m => Tx mode -> DB -> m (Maybe WorldVersion)
getLatestVersion tx db = listToMaybe . map fst <$> versionsBackwards tx db

getVersionMeta :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe VersionMeta)
getVersionMeta (Tx conn) _ wv = liftIO $ do
    rows <- query conn "SELECT value FROM versions WHERE key = ?"
                (Only (serialiseField wv))
    pure $ fmap (deserialiseField . fromOnly) (listToMaybe rows)

getPayloadsForTas :: MonadIO m
                  => Tx mode
                  -> DB
                  -> WorldVersion
                  -> (Tx mode -> DB -> ValidationVersion -> IO (Maybe payload))
                  -> m (PerTA payload)
getPayloadsForTas tx db version f = liftIO $
    fmap (toPerTA . catMaybes) $
        getVersionMeta tx db version >>= \case
            Nothing -> pure []
            Just versionMeta ->
                forM (perTA $ versionMeta ^. typed) $ \(ta, vv) ->
                    fmap (ta,) <$> f tx db vv

getValidationsPerTA :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Validations)
getValidationsPerTA tx db version = liftIO $
    getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} -> do
        rows <- query conn "SELECT value FROM validations WHERE key = ?"
                    (Only (SQLite.toInt64 validationsKey))
        pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getMetricsPerTA :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Metrics)
getMetricsPerTA tx db version = liftIO $
    getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} -> do
        rows <- query conn "SELECT value FROM metrics WHERE key = ?"
                    (Only (SQLite.toInt64 metricsKey))
        pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getCommonMetrics :: MonadIO m => Tx mode -> DB -> WorldVersion -> m Metrics
getCommonMetrics tx db version = liftIO $
    fmap (fromMaybe mempty) $ runMaybeT $ do
        VersionMeta{..} <- MaybeT $ getVersionMeta tx db version
        let Tx conn = tx
        rows <- liftIO $ query conn "SELECT value FROM metrics WHERE key = ?"
                    (Only (SQLite.toInt64 commonMetricsKey))
        MaybeT $ pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getValidationOutcomes :: MonadIO m
                      => Tx mode
                      -> DB
                      -> WorldVersion
                      -> m (Validations, Metrics, PerTA (Validations, Metrics))
getValidationOutcomes tx db version = liftIO $ do
    (commonV, commonM) <-
        fmap (fromMaybe mempty) $ runMaybeT $ do
            VersionMeta{..} <- MaybeT $ getVersionMeta tx db version
            let Tx conn = tx
            v <- MaybeT $ fmap (deserialiseCompressed . fromOnly) . listToMaybe
                    <$> query conn "SELECT value FROM validations WHERE key = ?"
                            (Only (SQLite.toInt64 commonValidationKey))
            m <- MaybeT $ fmap (deserialiseCompressed . fromOnly) . listToMaybe
                    <$> query conn "SELECT value FROM metrics WHERE key = ?"
                            (Only (SQLite.toInt64 commonMetricsKey))
            pure (v, m)
    perTAOutcomes <-
        getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} ->
            runMaybeT $ do
                v <- MaybeT $ fmap (deserialiseCompressed . fromOnly) . listToMaybe
                        <$> query conn "SELECT value FROM validations WHERE key = ?"
                                (Only (SQLite.toInt64 validationsKey))
                m <- MaybeT $ fmap (deserialiseCompressed . fromOnly) . listToMaybe
                        <$> query conn "SELECT value FROM metrics WHERE key = ?"
                                (Only (SQLite.toInt64 metricsKey))
                pure (v, m)
    pure (commonV, commonM, perTAOutcomes)

getVrps :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Vrps)
getVrps tx db version = fmap toVrps <$> getRoas tx db version

getVrpsForTA :: MonadIO m => Tx mode -> DB -> WorldVersion -> TaName -> m Vrps
getVrpsForTA tx db version taName = liftIO $
    fmap (toVrps . maybe mempty id) $ runMaybeT $ do
        VersionMeta{..} <- MaybeT $ getVersionMeta tx db version
        ValidationVersion{..} <- MaybeT $ pure $ getForTA perTa taName
        let Tx conn = tx
        rows <- liftIO $ query conn "SELECT value FROM roas WHERE key = ?"
                    (Only (SQLite.toInt64 roasKey))
        MaybeT $ pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getRoas :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (PerTA Roas)
getRoas tx db version = liftIO $
    getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} -> do
        rows <- query conn "SELECT value FROM roas WHERE key = ?"
                    (Only (SQLite.toInt64 roasKey))
        pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getAspas :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set Aspa))
getAspas tx db version =
    liftIO $ fmap (Just . allTAs) $
        getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} -> do
            rows <- query conn "SELECT value FROM aspas WHERE key = ?"
                        (Only (SQLite.toInt64 aspasKey))
            pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getGbrs :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set (T2 Hash Gbr)))
getGbrs tx db version =
    liftIO $ fmap (Just . allTAs) $
        getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} -> do
            rows <- query conn "SELECT value FROM gbrs WHERE key = ?"
                        (Only (SQLite.toInt64 gbrsKey))
            pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getBgps :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set BGPSecPayload))
getBgps tx db version =
    liftIO $ fmap (Just . allTAs) $
        getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} -> do
            rows <- query conn "SELECT value FROM bgps WHERE key = ?"
                        (Only (SQLite.toInt64 bgpCertsKey))
            pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getSpls :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe (Set.Set SplN))
getSpls tx db version =
    liftIO $ fmap (Just . allTAs) $
        getPayloadsForTas tx db version $ \(Tx conn) _ ValidationVersion{..} -> do
            rows <- query conn "SELECT value FROM spls WHERE key = ?"
                        (Only (SQLite.toInt64 splsKey))
            pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

saveValidationVersion :: MonadIO m
                      => Tx 'RW
                      -> DB
                      -> WorldVersion
                      -> [TaName]
                      -> PerTA (Payloads, ValidationState)
                      -> ValidationState
                      -> m ()
saveValidationVersion (Tx conn) db validatedBy allTaNames results@(PerTA perTAResults) commonVS =
    liftIO $ do
    commonValidationKey <- insertCompressed conn "INSERT INTO validations(value) VALUES (?)"
                               (commonVS ^. typed @Validations)
    commonMetricsKey    <- insertCompressed conn "INSERT INTO metrics(value) VALUES (?)"
                               (commonVS ^. typed @Metrics)

    addedResults <- forM (perTA results) $ \(taName, (Payloads{..}, vs)) -> do
        roasKey        <- insertCompressed conn "INSERT INTO roas(value)        VALUES (?)" roas
        aspasKey       <- insertCompressed conn "INSERT INTO aspas(value)       VALUES (?)" aspas
        splsKey        <- insertCompressed conn "INSERT INTO spls(value)        VALUES (?)" spls
        gbrsKey        <- insertCompressed conn "INSERT INTO gbrs(value)        VALUES (?)" gbrs
        bgpCertsKey    <- insertCompressed conn "INSERT INTO bgps(value)        VALUES (?)" bgpCerts
        validationsKey <- insertCompressed conn "INSERT INTO validations(value) VALUES (?)"
                              (vs ^. typed @Validations)
        metricsKey     <- insertCompressed conn "INSERT INTO metrics(value)     VALUES (?)"
                              (vs ^. typed @Metrics)
        pure (taName, ValidationVersion{..})

    let notPresentTAs = filter (`MonoidalMap.notMember` perTAResults) allTaNames
    earlierResults <-
        case notPresentTAs of
            [] -> pure []
            _  -> do
                versions <- versionsBackwards (Tx conn) db
                pure [ (ta, r) | (ta, Just r) <- fillUpEarlierTAData versions notPresentTAs mempty ]

    execute conn "INSERT OR REPLACE INTO versions(key, value) VALUES (?, ?)"
        ( serialiseField validatedBy
        , serialiseField $
            VersionMeta { perTa = toPerTA (addedResults <> earlierResults)
                        , .. } )
  where
    fillUpEarlierTAData [] _ acc = acc
    fillUpEarlierTAData _ [] acc = acc
    fillUpEarlierTAData ((_, vmeta) : versions) tasToFind acc =
        let (found, notFound) = List.partition (maybe False (const True) . snd)
                [ (ta, MonoidalMap.lookup ta (unPerTA $ vmeta ^. typed)) | ta <- tasToFind ]
        in fillUpEarlierTAData versions (map fst notFound) (acc <> found)

deleteValidationVersion :: MonadIO m => Tx 'RW -> DB -> WorldVersion -> m ()
deleteValidationVersion (Tx conn) db worldVersion = liftIO $
    ifJustM (getVersionMeta (Tx conn) db worldVersion) $ \vmeta -> do
        let del tbl k = execute conn
                (Query $ "DELETE FROM " <> tbl <> " WHERE key = ?")
                (Only (SQLite.toInt64 k))
        del "validations" (vmeta ^. #commonValidationKey)
        del "metrics"     (vmeta ^. #commonMetricsKey)
        forM_ (perTA $ vmeta ^. typed) $ \(_, ValidationVersion{..}) -> do
            del "roas"        roasKey
            del "aspas"       aspasKey
            del "spls"        splsKey
            del "gbrs"        gbrsKey
            del "bgps"        bgpCertsKey
            del "metrics"     metricsKey
            del "validations" validationsKey
        execute conn "DELETE FROM slurm    WHERE key = ?" (Only (serialiseField worldVersion))
        execute conn "DELETE FROM versions WHERE key = ?" (Only (serialiseField worldVersion))

saveSlurm :: MonadIO m => Tx 'RW -> DB -> WorldVersion -> Slurm -> m ()
saveSlurm (Tx conn) _ version slurm = liftIO $
    execute conn "INSERT OR REPLACE INTO slurm(key, value) VALUES (?, ?)"
        (serialiseField version, serialiseCompressed slurm)

getSlurm :: MonadIO m => Tx mode -> DB -> WorldVersion -> m (Maybe Slurm)
getSlurm (Tx conn) _ version = liftIO $ do
    rows <- query conn "SELECT value FROM slurm WHERE key = ?"
                (Only (serialiseField version))
    pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

getLatestVersions :: MonadIO m => Tx mode -> DB -> m (PerTA WorldVersion)
getLatestVersions tx db = liftIO $
    getLatestVersion tx db >>= \case
        Nothing -> pure $ PerTA MonoidalMap.empty
        Just lv ->
            getPayloadsForTas tx db lv $ \_ _ ValidationVersion{..} ->
                pure $ Just validatedBy


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
    forM_ rrdps $ \r ->
        execute conn "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rrdp-pp', ?)"
            (serialiseField (r ^. #uri), serialiseField r)
    saveRsyncRepositories tx db rsyncs
  where
    sep (RrdpR r)  (rs, ss) = (r : rs, ss)
    sep (RsyncR r) (rs, ss) = (rs, r : ss)

saveRepositoryValidationStates :: MonadIO m
                                => Tx 'RW -> DB -> [(Repository, ValidationState)] -> m ()
saveRepositoryValidationStates tx db repos = liftIO $ do
    let (rrdps, rsyncs) = foldr sep ([], []) repos
    let Tx conn = tx
    forM_ rrdps $ \(r, vs) ->
        execute conn
            "INSERT OR REPLACE INTO repositories(key, kind, data) VALUES (?, 'rrdp-vstate', ?)"
            (serialiseField (r ^. #uri), serialiseCompressed vs)
    saveRsyncValidationStates tx db rsyncs
  where
    sep (RrdpR r,  a) (rs, ss) = ((r, a) : rs, ss)
    sep (RsyncR r, a) (rs, ss) = (rs, (r, a) : ss)

saveRsyncRepositories :: MonadIO m => Tx 'RW -> DB -> [RsyncRepository] -> m ()
saveRsyncRepositories (Tx conn) db repos = liftIO $
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

    rrdpResults <- fmap mconcat $ forM rrdps $ \(url, r) ->
        if not (filterF (RrdpU url)) then pure [] else do
            rows <- query conn
                "SELECT data FROM repositories WHERE key = ? AND kind = 'rrdp-vstate'"
                (Only (serialiseField url))
            pure $ case listToMaybe rows of
                Nothing        -> []
                Just (Only bs) -> [(RrdpR r, deserialiseCompressed bs)]

    rsyncResults <- fmap mconcat $ forM rsyncs $ \(host, metas) -> do
        rows <- query conn
            "SELECT data FROM repositories WHERE key = ? AND kind = 'rsync-vstate'"
            (Only (serialiseField host))
        pure $ case listToMaybe rows of
            Nothing        -> []
            Just (Only bs) ->
                let vss = deserialiseCompressed bs :: RsyncTree ValidationState
                in  [ (RsyncR repo, vs)
                    | (RsyncURL _ path, meta) <- flattenTree host metas
                    , let uri  = RsyncURL host path
                    , let repo = RsyncRepository { repoPP = RsyncPublicationPoint uri, .. }
                    , filterF (RsyncU uri)
                    , Just (_, vs) <- [lookupInRsyncTree path vss] ]
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

updateValidatedByVersionMap :: MonadIO m
                            => Tx 'RW
                            -> DB
                            -> (Maybe (Map.Map ObjectKey WorldVersion) -> Map.Map ObjectKey WorldVersion)
                            -> m (Map.Map ObjectKey WorldVersion)
updateValidatedByVersionMap (Tx conn) _ f = liftIO $ do
    rows <- query conn "SELECT value FROM validated_by_version WHERE key = ?"
                (Only validatedByVersionKey)
    let existing = case rows of
            [Only bs] -> Just $ deserialiseCompressed bs
            _         -> Nothing
        updated  = f existing
    execute conn "INSERT OR REPLACE INTO validated_by_version(key, value) VALUES (?, ?)"
        (validatedByVersionKey, serialiseCompressed updated)
    pure updated


-- ---------------------------------------------------------------------------
-- Stats
-- ---------------------------------------------------------------------------

getObjectsStats :: MonadIO m => Tx mode -> DB -> m ObjectStats
getObjectsStats (Tx conn) _ = liftIO $ do
    rows <- query_ conn
        "SELECT type, COUNT(*), SUM(LENGTH(COALESCE(data, original))) \
        \FROM objects GROUP BY type"
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

deleteOldestVersionsIfNeeded :: MonadIO m
                             => Tx 'RW -> DB -> Natural -> m [WorldVersion]
deleteOldestVersionsIfNeeded tx db versionNumberToKeep =
    mapException (AppException . storageError) <$> liftIO $ do
        versions <- versionsBackwards tx db
        let reallyToKeep = max 2 (fromIntegral versionNumberToKeep)
        if length versions > reallyToKeep
            then do
                let versionsToDelete = map fst $ findEnoughForEachTA reallyToKeep versions mempty
                forM_ versionsToDelete $ deleteValidationVersion tx db
                pure versionsToDelete
            else pure []
  where
    findEnoughForEachTA _ [] _ = []
    findEnoughForEachTA n ((_, meta) : versions) acc =
        if any (\v -> Set.size v < fromIntegral n) $ MonoidalMap.elems acc'
            then findEnoughForEachTA n versions acc'
            else versions
      where
        acc' = acc <> mconcat
            [ MonoidalMap.singleton ta (Set.singleton v)
            | (ta, v) <- perTA $ meta ^. #perTa ]

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
        toDelete <- filter versionIsTooOld . map fst <$> versionsBackwards tx db
        forM_ toDelete $ deleteValidationVersion tx db
        pure $ length toDelete

    deleteStaleObjects tx = do
        validatedBy <- fromMaybe mempty <$> do
            let Tx conn = tx
            rows <- query conn "SELECT value FROM validated_by_version WHERE key = ?"
                        (Only validatedByVersionKey)
            pure $ fmap (deserialiseCompressed . fromOnly) (listToMaybe rows)

        let Tx conn = tx
        allObjs <- query_ conn
            "SELECT object_key, world_version, object_type FROM objects"

        deletedPerType <- newTVarIO mempty
        keptTotal      <- newTVarIO (0 :: Int)
        keysToDelete   <- fmap catMaybes $ forM allObjs $
            \(okInt, wvInt, typText) -> case readMaybe typText of
                Nothing  -> pure Nothing
                Just typ -> do
                    let objectKey   = SQLite.fromInt64 okInt :: ObjectKey
                        insertedBy  = SQLite.fromInt64 wvInt :: WorldVersion
                        insertedOld = objectIsTooOld insertedBy typ
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

        forM_ keysToDelete $ deleteObjectByKey tx db

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
    catMaybes <$> forM rows (getLocatedByKey tx db . SQLite.fromInt64 . fromOnly)

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

instance Exception TxRollbackException

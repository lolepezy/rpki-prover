{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent.MVar (withMVar)
import           Control.Concurrent.STM (TVar, newTVarIO)
import           Control.Exception (bracket)
import           Control.Monad (forM_, void)
import           Control.Lens ((^.))

import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Data.IORef
import qualified Data.Text as Text
import           Data.Hourglass (Seconds(..))

import           Criterion.Main

import           RPKI.AppMonad (runValidatorT, vHoist)
import           RPKI.AppState (instantToVersion)
import           RPKI.AppTypes (Size(..), WorldVersion)
import           RPKI.Domain
import           RPKI.Parse.Parse (readObject)
import           RPKI.Reporting (newScopes)
import           RPKI.Store.Base.Storable (Compressed(..), Verbatim(..), toStorable, toStorableObject)
import           RPKI.Store.Database (DB(..), Tx, MftShortcutMeta(..))
import qualified RPKI.Store.Database as DB
import qualified RPKI.Store.SQLite as SQLite
import           RPKI.Time (thisInstant, unNow, momentAfter)
import           RPKI.Util (parseRpkiURL)
import           RPKI.Validation.Types (CrlShortcut(..), MftShortcut)

import           System.Directory (createDirectoryIfMissing, removePathForcibly)
import           System.FilePath ((</>))
import           System.IO.Temp (createTempDirectory)


data BenchDb = BenchDb {
    benchRoot :: FilePath,
    benchDb   :: DB
}

data SaveObjectEnv = SaveObjectEnv {
    saveDb      :: BenchDb,
    saveObject_ :: RpkiObject,
    saveWorldV  :: WorldVersion
}

data ShortcutEnv = ShortcutEnv {
    shortcutDb  :: BenchDb,
    shortcutAki :: AKI,
    missingAki  :: AKI
}

data LinkEnv = LinkEnv {
    linkDb      :: BenchDb,
    linkHash    :: Hash,
    linkUrl     :: RpkiURL,
    missingHash :: Hash
}

data LinkNewUrlEnv = LinkNewUrlEnv {
    linkNewDb      :: BenchDb,
    linkNewHash    :: Hash,
    linkNewUrls    :: [RpkiURL],
    linkNewCounter :: IORef Int
}

data TxOverheadEnv = TxOverheadEnv {
    txOverheadDb   :: BenchDb,
    txOverheadTVar :: TVar DB,
    txOverheadAki  :: AKI
}

main :: IO ()
main =
    bracket mkSaveObjectEnv cleanupSaveObjectEnv $ \saveEnv ->
    bracket mkShortcutEnv cleanupShortcutEnv $ \shortcutEnv ->
    bracket mkLinkEnv cleanupLinkEnv $ \linkEnv ->
    bracket mkLinkNewUrlEnv cleanupLinkNewUrlEnv $ \linkNewEnv ->
    bracket (mkTxOverheadEnv 1) cleanupTxOverheadEnv $ \txOverheadP1 ->
    bracket (mkTxOverheadEnv 4) cleanupTxOverheadEnv $ \txOverheadP4 ->
    bracket (mkTxOverheadEnv 8) cleanupTxOverheadEnv $ \txOverheadP8 ->
    bracket (mkTxOverheadMmapEnv 1 64) cleanupTxOverheadEnv $ \txOverheadMmap64P1 ->
    bracket (mkTxOverheadMmapEnv 4 64) cleanupTxOverheadEnv $ \txOverheadMmap64P4 ->
    bracket (mkTxOverheadMmapEnv 8 64) cleanupTxOverheadEnv $ \txOverheadMmap64P8 ->
    bracket (mkTxOverheadMmapEnv 1 256) cleanupTxOverheadEnv $ \txOverheadMmap256P1 ->
    bracket (mkTxOverheadMmapEnv 4 256) cleanupTxOverheadEnv $ \txOverheadMmap256P4 ->
    bracket (mkTxOverheadMmapEnv 8 256) cleanupTxOverheadEnv $ \txOverheadMmap256P8 ->
    bracket (mkTxOverheadMmapEnv 1 1024) cleanupTxOverheadEnv $ \txOverheadMmap1024P1 ->
    bracket (mkTxOverheadMmapEnv 4 1024) cleanupTxOverheadEnv $ \txOverheadMmap1024P4 ->
    bracket (mkTxOverheadMmapEnv 8 1024) cleanupTxOverheadEnv $ \txOverheadMmap1024P8 ->
        defaultMain
            [ bgroup "saveObject"
                [ bench "replace-existing-row" $ whnfIO (benchSaveObject saveEnv)
                , bench "existing-hash-hit" $ whnfIO (benchSaveObjectExisting saveEnv)
                ]
            , bgroup "getMftShorcut"
                [ bench "hit" $ whnfIO (benchGetMftShortcut shortcutEnv)
                , bench "miss" $ whnfIO (benchGetMftShortcutMiss shortcutEnv)
                ]
            , bgroup "linkObjectToUrl"
                [ bench "existing-url-existing-link" $ whnfIO (benchLinkObjectToUrl linkEnv)
                , bench "missing-object-hash" $ whnfIO (benchLinkObjectToUrlMissingObject linkEnv)
                , bench "new-url-growing-index" $ whnfIO (benchLinkObjectToUrlNewUrl linkNewEnv)
                ]
            , bgroup "roTxT-overhead"
                [ txOverheadBenchGroup "pool-1" txOverheadP1
                , txOverheadBenchGroup "pool-4" txOverheadP4
                , txOverheadBenchGroup "pool-8" txOverheadP8
                ]
            , bgroup "roTxT-overhead-mmap-64mb"
                [ txOverheadBenchGroup "pool-1" txOverheadMmap64P1
                , txOverheadBenchGroup "pool-4" txOverheadMmap64P4
                , txOverheadBenchGroup "pool-8" txOverheadMmap64P8
                ]
            , bgroup "roTxT-overhead-mmap-256mb"
                [ txOverheadBenchGroup "pool-1" txOverheadMmap256P1
                , txOverheadBenchGroup "pool-4" txOverheadMmap256P4
                , txOverheadBenchGroup "pool-8" txOverheadMmap256P8
                ]
            , bgroup "roTxT-overhead-mmap-1024mb"
                [ txOverheadBenchGroup "pool-1" txOverheadMmap1024P1
                , txOverheadBenchGroup "pool-4" txOverheadMmap1024P4
                , txOverheadBenchGroup "pool-8" txOverheadMmap1024P8
                ]
            ]

txOverheadBenchGroup :: String -> TxOverheadEnv -> Benchmark
txOverheadBenchGroup poolName txEnv =
    bgroup poolName
        [ bench "per-call-roTxT-1000" $ whnfIO (benchRoTxTPerCall txEnv)
        , bench "single-roTxT-batched-1000" $ whnfIO (benchRoTxTBatched txEnv)
        , bench "per-call-roTxT-topdown-like-1000" $ whnfIO (benchRoTxTPerCallTopDownLike txEnv)
        , bench "single-roTxT-topdown-like-batched-1000" $ whnfIO (benchRoTxTBatchedTopDownLike txEnv)
        , bench "single-noTx-topdown-like-batched-1000" $ whnfIO (benchNoTxTBatchedTopDownLike txEnv)
        ]

mkBenchDb :: IO BenchDb
mkBenchDb = mkBenchDbWithPool 1

mkBenchDbWithPool :: Int -> IO BenchDb
mkBenchDbWithPool poolSize = do
    mkBenchDbWithPoolAndMmap poolSize Nothing

mkBenchDbWithPoolAndMmap :: Int -> Maybe Size -> IO BenchDb
mkBenchDbWithPoolAndMmap poolSize mmapSizeMb = do
    root <- createTempDirectory "/tmp" "rpki-prover-bench"
    createDirectoryIfMissing True root
    let dbPath = root </> "bench.sqlite"

    sqliteDb <- SQLite.createDB dbPath 10_000 poolSize
    withMVar (SQLite.writeConn sqliteDb) (SQLite.initSchema . SQLite.rawConn)

    let db = DB sqliteDb
    DB.rwTx db $ \tx -> DB.saveCurrentDatabaseVersion tx db

    pure BenchDb {
        benchRoot = root,
        benchDb = db
    }

cleanupBenchDb :: BenchDb -> IO ()
cleanupBenchDb BenchDb{..} = do
    SQLite.closeDB (unDB benchDb)
    removePathForcibly benchRoot

loadObjectFromFixture :: FilePath -> IO (RpkiURL, RpkiObject)
loadObjectFromFixture path = do
    bs <- BS.readFile path
    let urlText = Text.pack ("rsync://bench.local/" <> path)
    url <- either (fail . show) pure (parseRpkiURL urlText)

    (result, _) <- runValidatorT (newScopes "bench-read") $ vHoist $ readObject url bs
    either (fail . show) (pure . (url,)) result

mkSaveObjectEnv :: IO SaveObjectEnv
mkSaveObjectEnv = do
    saveDb <- mkBenchDb
    (_, saveObject_) <- loadObjectFromFixture "test/data/afrinic_mft1.mft"
    now <- unNow <$> thisInstant
    let saveWorldV = instantToVersion now

    DB.rwTx (benchDb saveDb) $ \tx -> do
        void $ DB.saveObject tx (benchDb saveDb) (toStorableObject saveObject_) saveWorldV

    pure SaveObjectEnv {..}

cleanupSaveObjectEnv :: SaveObjectEnv -> IO ()
cleanupSaveObjectEnv = cleanupBenchDb . saveDb

benchSaveObject :: SaveObjectEnv -> IO ObjectKey
benchSaveObject SaveObjectEnv{..} =
    DB.rwTx (benchDb saveDb) $ \tx -> do
        DB.deleteObjectByHash tx (benchDb saveDb) (getHash saveObject_)
        DB.saveObject tx (benchDb saveDb) (toStorableObject saveObject_) saveWorldV

benchSaveObjectExisting :: SaveObjectEnv -> IO ObjectKey
benchSaveObjectExisting SaveObjectEnv{..} =
    DB.rwTx (benchDb saveDb) $ \tx ->
        DB.saveObject tx (benchDb saveDb) (toStorableObject saveObject_) saveWorldV

mkShortcutEnv :: IO ShortcutEnv
mkShortcutEnv = do
    shortcutDb <- mkBenchDb
    (_, mftObject) <- loadObjectFromFixture "test/data/afrinic_mft1.mft"
    shortcutAki <- case getAKI mftObject of
        Nothing  -> fail "Manifest fixture has no AKI"
        Just aki -> pure aki
    let missingAki = AKI (KI "missing-aki")

    now <- unNow <$> thisInstant
    let later = momentAfter now (Seconds 3600)
        saveDb_ = benchDb shortcutDb

    DB.rwTx saveDb_ $ \tx -> do
        key <- DB.saveObject tx saveDb_ (toStorableObject mftObject) (instantToVersion now)
        let crlShortcut = CrlShortcut {
                key = key,
                notBefore = now,
                notAfter = later
            }
            meta = MftShortcutMeta {
                key = key,
                notBefore = now,
                notAfter = later,
                serial = Serial 1,
                manifestNumber = Serial 1,
                crlShortcut = crlShortcut
            }

        DB.saveMftShorcutMeta tx saveDb_ shortcutAki (Verbatim $ toStorable $ Compressed meta)

    pure ShortcutEnv {..}

cleanupShortcutEnv :: ShortcutEnv -> IO ()
cleanupShortcutEnv = cleanupBenchDb . shortcutDb

benchGetMftShortcut :: ShortcutEnv -> IO (Maybe MftShortcut)
benchGetMftShortcut ShortcutEnv{..} =
    DB.roTx (benchDb shortcutDb) $ \tx -> DB.getMftShorcut tx (benchDb shortcutDb) shortcutAki

benchGetMftShortcutMiss :: ShortcutEnv -> IO (Maybe MftShortcut)
benchGetMftShortcutMiss ShortcutEnv{..} =
    DB.roTx (benchDb shortcutDb) $ \tx -> DB.getMftShorcut tx (benchDb shortcutDb) missingAki

mkLinkEnv :: IO LinkEnv
mkLinkEnv = do
    linkDb <- mkBenchDb
    (linkUrl, obj) <- loadObjectFromFixture "test/data/afrinic_mft1.mft"
    now <- unNow <$> thisInstant

    let db = benchDb linkDb
    DB.rwTx db $ \tx -> do
        void $ DB.saveObject tx db (toStorableObject obj) (instantToVersion now)
        DB.linkObjectToUrl tx db linkUrl (getHash obj)

    let linkHash = getHash obj
        missingHash = Hash "missing-hash"
    pure LinkEnv {..}

cleanupLinkEnv :: LinkEnv -> IO ()
cleanupLinkEnv = cleanupBenchDb . linkDb

benchLinkObjectToUrl :: LinkEnv -> IO ()
benchLinkObjectToUrl LinkEnv{..} =
    DB.rwTx (benchDb linkDb) $ \tx -> DB.linkObjectToUrl tx (benchDb linkDb) linkUrl linkHash

benchLinkObjectToUrlMissingObject :: LinkEnv -> IO ()
benchLinkObjectToUrlMissingObject LinkEnv{..} =
    DB.rwTx (benchDb linkDb) $ \tx -> DB.linkObjectToUrl tx (benchDb linkDb) linkUrl missingHash

mkLinkNewUrlEnv :: IO LinkNewUrlEnv
mkLinkNewUrlEnv = do
    linkNewDb <- mkBenchDb
    (_, obj) <- loadObjectFromFixture "test/data/afrinic_mft1.mft"
    now <- unNow <$> thisInstant

    let db = benchDb linkNewDb
    DB.rwTx db $ \tx ->
        void $ DB.saveObject tx db (toStorableObject obj) (instantToVersion now)

    linkNewUrls <- mapM mkUrl [1 .. 5000 :: Int]
    linkNewCounter <- newIORef 0
    let linkNewHash = getHash obj
    pure LinkNewUrlEnv {..}
  where
    mkUrl i = do
        let urlText = Text.pack ("rsync://bench.local/mft-" <> show i <> ".mft")
        either (fail . show) pure (parseRpkiURL urlText)

cleanupLinkNewUrlEnv :: LinkNewUrlEnv -> IO ()
cleanupLinkNewUrlEnv = cleanupBenchDb . linkNewDb

benchLinkObjectToUrlNewUrl :: LinkNewUrlEnv -> IO ()
benchLinkObjectToUrlNewUrl LinkNewUrlEnv{..} = do
    i <- atomicModifyIORef' linkNewCounter $ \x -> let y = x + 1 in (y, x)
    let url = linkNewUrls !! (i `mod` length linkNewUrls)
    DB.rwTx (benchDb linkNewDb) $ \tx ->
        DB.linkObjectToUrl tx (benchDb linkNewDb) url linkNewHash

mkTxOverheadEnv :: Int -> IO TxOverheadEnv
mkTxOverheadEnv poolSize = do
    txOverheadDb <- mkBenchDbWithPoolAndMmap poolSize Nothing
    mkTxOverheadEnvFromDb txOverheadDb

mkTxOverheadMmapEnv :: Int -> Int64 -> IO TxOverheadEnv
mkTxOverheadMmapEnv poolSize mmapMb = do
    txOverheadDb <- mkBenchDbWithPoolAndMmap poolSize (Just (Size mmapMb))
    mkTxOverheadEnvFromDb txOverheadDb

mkTxOverheadEnvFromDb :: BenchDb -> IO TxOverheadEnv
mkTxOverheadEnvFromDb txOverheadDb = do
    (_, mftObject) <- loadObjectFromFixture "test/data/afrinic_mft1.mft"
    txOverheadAki <- case getAKI mftObject of
        Nothing  -> fail "Manifest fixture has no AKI"
        Just aki -> pure aki
    now <- unNow <$> thisInstant
    let later = momentAfter now (Seconds 3600)
        db = benchDb txOverheadDb

    DB.rwTx db $ \tx -> do
        key <- DB.saveObject tx db (toStorableObject mftObject) (instantToVersion now)
        let crlShortcut = CrlShortcut {
                key = key,
                notBefore = now,
                notAfter = later
            }
            meta = MftShortcutMeta {
                key = key,
                notBefore = now,
                notAfter = later,
                serial = Serial 1,
                manifestNumber = Serial 1,
                crlShortcut = crlShortcut
            }

        DB.saveMftShorcutMeta tx db txOverheadAki (Verbatim $ toStorable $ Compressed meta)

    txOverheadTVar <- newTVarIO db
    pure TxOverheadEnv {..}

cleanupTxOverheadEnv :: TxOverheadEnv -> IO ()
cleanupTxOverheadEnv = cleanupBenchDb . txOverheadDb

benchRoTxTPerCall :: TxOverheadEnv -> IO ()
benchRoTxTPerCall TxOverheadEnv{..} =
    forM_ [1 .. 1000 :: Int] $ \_ ->
        DB.roTxT txOverheadTVar $ \tx db ->
            void $ DB.getMftShorcut tx db txOverheadAki

benchRoTxTBatched :: TxOverheadEnv -> IO ()
benchRoTxTBatched TxOverheadEnv{..} =
    DB.roTxT txOverheadTVar $ \tx db ->
        forM_ [1 .. 1000 :: Int] $ \_ ->
            void $ DB.getMftShorcut tx db txOverheadAki

topDownLikeReadPass :: Tx mode -> DB -> AKI -> IO ()
topDownLikeReadPass tx db aki = do
    mfts <- DB.getMftsForAKI tx db aki
    void $ DB.getMftShorcut tx db aki
    case mfts of
        []    -> pure ()
        (m:_) -> void $ DB.getMftByKey tx db (m ^. #key)

benchRoTxTPerCallTopDownLike :: TxOverheadEnv -> IO ()
benchRoTxTPerCallTopDownLike TxOverheadEnv{..} =
    forM_ [1 .. 1000 :: Int] $ \_ ->
        DB.roTxT txOverheadTVar $ \tx db ->
            topDownLikeReadPass tx db txOverheadAki

benchRoTxTBatchedTopDownLike :: TxOverheadEnv -> IO ()
benchRoTxTBatchedTopDownLike TxOverheadEnv{..} =
    DB.roTxT txOverheadTVar $ \tx db ->
        forM_ [1 .. 1000 :: Int] $ \_ ->
            topDownLikeReadPass tx db txOverheadAki

benchNoTxTBatchedTopDownLike :: TxOverheadEnv -> IO ()
benchNoTxTBatchedTopDownLike TxOverheadEnv{..} =    
    forM_ [1 .. 1000 :: Int] $ \_ ->
        DB.noTx (benchDb txOverheadDb) $ \tx ->
            topDownLikeReadPass tx (benchDb txOverheadDb) txOverheadAki

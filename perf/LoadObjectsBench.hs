-- | Standalone benchmark for `loadObjectsFromFS`: reading, parsing and saving
-- every object of an rsync-ed tree on local disk, what the rsync fetcher does
-- after downloading.
--
-- Usage:
--   cabal run load-objects-bench -- DIR RSYNC_URL [repeats] [--dir=PATH] [--cpus=N] [--keep]
--   cabal run load-objects-bench -- ~/rpki/rsync/rpki.ripe.net/repository \
--        rsync://rpki.ripe.net/repository 3 +RTS -N8 -A4m -AL4m -Fd1 -RTS
--
-- DIR is loaded as the repository RSYNC_URL, the way the rsync fetcher loads
-- it.
--
-- Each repeat gets a fresh SQLite cache in a temporary directory under
-- `--dir` (default /tmp, which is often tmpfs), so nothing is found in the
-- cache already, removed afterwards unless there's `--keep`. The cache is
-- opened the way a fetcher opens it, leaving WAL checkpoints to the main
-- process.
--
-- The configured CPU count is the number of capabilities, or `--cpus`.
-- Loading sets the capabilities to it, but not above the cores available.
--
-- The runs are bracketed by eventlog markers "load start" and "load end".
module Main where

import           Control.Concurrent.STM  (newTVarIO, readTVarIO)
import           Control.Exception       (bracket)
import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad           (forM_)
import           Control.Monad.IO.Class  (liftIO)

import qualified Data.List               as List
import           Data.Maybe              (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text               as Text

import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Conc                (getNumCapabilities)
import           GHC.Stats               (RTSStats (..), getRTSStats, getRTSStatsEnabled)

import           Debug.Trace             (traceMarkerIO)
import           Database.SQLite.Simple  (Only (..))
import           System.CPUTime          (getCPUTime)
import           System.Directory        (createDirectoryIfMissing, removePathForcibly)
import           System.Environment      (getArgs)
import           System.FilePath         ((</>))
import           System.IO.Temp          (createTempDirectory)
import           Text.Printf             (printf)

import           RPKI.AppContext
import           RPKI.AppMonad           (runValidatorIO)
import           RPKI.AppState           (instantToVersion, newAppState)
import           RPKI.AppTypes           (Size (..))
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Meta.UniqueId      (thisExecutableVersion)
import           RPKI.Reporting          (newScopes, Validations (..))
import           RPKI.Rsync              (loadRsyncRepository)
import           RPKI.Store.AppSqliteStorage
import           RPKI.Store.Database     (roTx, Tx (..))
import qualified RPKI.Store.Database     as DB
import qualified RPKI.Store.SQLite       as SQLite
import           RPKI.Time               (thisInstant, unNow)
import           RPKI.Util               (parseRsyncURL)


main :: IO ()
main = do
    allArgs <- getArgs
    let (flags, args) = List.partition ("--" `List.isPrefixOf`) allArgs
        baseDir = fromMaybe "/tmp" $ listToMaybe $ mapMaybe (List.stripPrefix "--dir=") flags
        keep    = "--keep" `elem` flags
        cpus    = readMaybe =<< listToMaybe (mapMaybe (List.stripPrefix "--cpus=") flags)
    (rootPath, url, repeats) <- case args of
        dir : u : rest ->
            case parseRsyncURL (Text.pack u) of
                Left e  -> error $ "Bad rsync URL " <> u <> ": " <> show e
                Right r -> pure (dir, r, maybe 3 id (readMaybe =<< listToMaybe rest))
        _ -> error "Usage: load-objects-bench DIR RSYNC_URL [repeats] [--dir=PATH] [--cpus=N] [--keep]"

    caps <- getNumCapabilities
    printf "loading %s as %s, capabilities: %d, caches under %s\n\n"
        rootPath (show url) caps baseDir

    withLogger (newLogConfig ErrorL MainLog) $ \logger ->
        forM_ [1 .. repeats] $ \i ->
            bracket (mkBenchContext baseDir cpus logger) (cleanup keep . fst) $ \(_, appContext) ->
                runIteration i appContext rootPath url
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing

    cleanup keep dir
        | keep      = printf "          cache kept in %s\n" dir
        | otherwise = removePathForcibly dir


mkBenchContext :: FilePath -> Maybe Int -> AppLogger -> IO (FilePath, AppContext SqliteBackend)
mkBenchContext baseDir cpus logger = do
    dir <- createTempDirectory baseDir "rpki-load-objects-bench"
    let cacheDir = dir </> "cache"
        tmpDir   = dir </> "tmp"
        talDir   = dir </> "tals"
    mapM_ (createDirectoryIfMissing True) [cacheDir, tmpDir, talDir]

    cpuCount_ <- fromIntegral . flip fromMaybe cpus <$> getNumCapabilities
    let config = defaultConfig
            & #rootDirectory   .~ Public dir
            & #tmpDirectory    .~ Public tmpDir
            & #talDirectory    .~ Public talDir
            & #cacheDirectory  .~ Public cacheDir
            & #parallelism     .~ newParallelism cpuCount_

    -- Creates the schema, then open it again as a fetcher would
    _  <- createSqliteDatabase cacheDir config True False
    db <- openExistingSqliteDatabase cacheDir config

    appState <- newAppState
    database <- newTVarIO db
    let executableVersion = thisExecutableVersion
    pure (dir, AppContext {..})


runIteration :: Int -> AppContext SqliteBackend -> FilePath -> RsyncURL -> IO ()
runIteration i appContext rootPath url = do
    worldVersion <- instantToVersion . unNow <$> thisInstant

    statsEnabled <- getRTSStatsEnabled
    statsBefore  <- if statsEnabled then Just <$> getRTSStats else pure Nothing

    traceMarkerIO "load start"
    cpu0  <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    (result, vs) <- runValidatorIO (newScopes "load-objects-bench") $
        loadRsyncRepository appContext worldVersion url rootPath
    wall1 <- getMonotonicTimeNSec
    cpu1  <- getCPUTime
    traceMarkerIO "load end"

    caps <- getNumCapabilities
    let wallSec = fromIntegral (wall1 - wall0) / 1e9 :: Double
        cpuSec  = fromIntegral (cpu1 - cpu0) / 1e12 :: Double
    printf "[run %d] wall=%.3fs cpu=%.3fs utilisation=%.0f%% (capabilities at the end: %d)\n"
        i wallSec cpuSec (cpuSec / wallSec * 100) caps

    case statsBefore of
        Just before -> do
            after <- getRTSStats
            let mb n = fromIntegral n / (1024 * 1024) :: Double
            printf "          gc_cpu=%.3fs mutator_cpu=%.3fs allocated=%.0fMB max_live=%.0fMB max_mem_in_use=%.0fMB\n"
                (fromIntegral (gc_cpu_ns after - gc_cpu_ns before) / 1e9 :: Double)
                (fromIntegral (mutator_cpu_ns after - mutator_cpu_ns before) / 1e9 :: Double)
                (mb $ allocated_bytes after - allocated_bytes before)
                (mb $ max_live_bytes after)
                (mb $ max_mem_in_use_bytes after)
        Nothing -> pure ()

    db <- readTVarIO $ appContext ^. #database
    (stats, linkCount) <- roTx db $ \tx@(Tx conn) -> do
        stats <- DB.getObjectsStats tx
        [Only n] <- liftIO $ SQLite.query_ conn "SELECT COUNT(*) FROM object_urls"
        pure (stats, n :: Int)
    let Size objectCount = stats ^. #totalObjects
        Validations problems = vs ^. #validations
    printf "          %s, stored %d objects and %d url links, %d scopes with issues\n"
        (either (("FAILED: " <>) . show) (const "ok") result)
        objectCount linkCount (length problems)

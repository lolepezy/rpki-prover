{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Standalone (non-criterion) benchmark for `saveSnapshot`: the
-- parse-prevalidate-store pipeline that runs when an RRDP snapshot is
-- processed. Criterion isn't a good fit here since we care about wall-clock
-- vs total-CPU-time (i.e. how many cores actually get used), not statistical
-- sampling of a pure function.
--
-- Usage:
--   cabal run save-snapshot-bench -- path/to/snapshot.xml [repeats]
--   cabal run save-snapshot-bench -- path/to/snapshot.xml 5 +RTS -N8 -RTS
--
-- Each repeat gets a fresh temporary SQLite cache directory, so every run
-- pays the full "nothing cached yet" cost instead of hitting the
-- hash-already-exists shortcut on the 2nd+ repeat.
module Main where

import           Control.Concurrent.STM  (newTVarIO)
import           Control.Exception       (bracket, evaluate)
import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad           (forM_)

import qualified Data.ByteString         as BS
import qualified Data.List               as List
import qualified Data.Text               as Text

import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Conc                (getNumCapabilities)
import           GHC.Stats               (RTSStats (..), getRTSStats, getRTSStatsEnabled)

import           System.CPUTime          (getCPUTime)
import           System.Directory        (createDirectoryIfMissing, removePathForcibly)
import           System.Environment      (getArgs)
import           System.FilePath         ((</>))
import           System.IO.Temp          (createTempDirectory)
import           Text.Printf             (printf)

import           RPKI.AppContext
import           RPKI.AppMonad           (runValidatorT)
import           RPKI.AppState           (instantToVersion, newAppState)
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Meta.UniqueId      (thisExecutableVersion)
import           RPKI.Messages           (formatValidations)
import           RPKI.Reporting          (newScopes, Validations (..))
import           RPKI.RRDP.Parse         (parseSnapshot)
import           RPKI.RRDP.RrdpFetch     (saveSnapshot)
import           RPKI.RRDP.Types
import           RPKI.Store.AppSqliteStorage
import           RPKI.Time               (thisInstant, unNow)
import qualified RPKI.Util               as U


-- | Placeholder snapshot; replace with a real (large) RRDP snapshot.xml to
-- get meaningful numbers.
defaultSnapshotPath :: FilePath
defaultSnapshotPath = "/Users/mpuzanov/tmp/arin-snapshot.xml"

defaultRepeats :: Int
defaultRepeats = 3


main :: IO ()
main = do
    args <- getArgs
    let (snapshotPath, repeats) = case args of
            []        -> (defaultSnapshotPath, defaultRepeats)
            [p]       -> (p, defaultRepeats)
            (p : n : _) -> (p, maybe defaultRepeats id (readMaybe n))

    content <- BS.readFile snapshotPath

    -- Time the XML/RRDP parse in isolation (single-threaded, no ASN.1
    -- object parsing, no DB) to get a floor for "is the XML parser the
    -- bottleneck". Force every publish element's base64 payload (not just
    -- the list spine) so this actually pays the full parsing cost --
    -- `parseSnapshot`'s accumulator builds each payload via repeated
    -- `BS.concat`, and those thunks would otherwise only get forced lazily
    -- wherever the pipeline first touches them.
    parseWall0 <- getMonotonicTimeNSec
    Snapshot _ sessionId serial snapshotItems <-
        either (\e -> error $ "Failed to parse " <> snapshotPath <> ": " <> show e) pure $
            parseSnapshot content
    let totalBase64Bytes = List.foldl'
            (\acc (SnapshotPublish _ (EncodedBase64 b)) -> acc + BS.length b)
            0 snapshotItems
    _ <- evaluate totalBase64Bytes
    parseWall1 <- getMonotonicTimeNSec

    caps <- getNumCapabilities
    statsEnabled <- getRTSStatsEnabled
    printf "snapshot: %s (%d bytes, %d publish elements)\n"
        snapshotPath (BS.length content) (length snapshotItems)
    printf "xml parse only: %.3fs (%d base64 bytes across all publishes)\n"
        (fromIntegral (parseWall1 - parseWall0) / 1e9 :: Double) totalBase64Bytes
    printf "capabilities (RTS -N): %d, +RTS -T stats enabled: %s\n" caps (show statsEnabled)
    printf "repeats: %d\n\n" repeats

    withLogger (newLogConfig ErrorL MainLog) $ \logger ->
        forM_ [1 .. repeats] $ \i ->
            bracket (mkBenchContext logger) cleanupBenchContext $ \appContext ->
                runIteration i appContext sessionId serial content
  where
    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing


mkBenchContext :: AppLogger -> IO (FilePath, AppContext SqliteBackend)
mkBenchContext logger = do
    dir <- createTempDirectory "/tmp" "rpki-save-snapshot-bench"
    let cacheDir = dir </> "cache"
        tmpDir   = dir </> "tmp"
        talDir   = dir </> "tals"
    createDirectoryIfMissing True cacheDir
    createDirectoryIfMissing True tmpDir
    createDirectoryIfMissing True talDir

    -- Mirror what `Main.hs` does at startup: base the configured CPU count
    -- on whatever capabilities the RTS was actually started with (`+RTS -N`),
    -- so `cabal run ... -- file.xml 3 +RTS -N4 -RTS` lets you sweep core
    -- counts without recompiling.
    cpuCount_ <- fromIntegral <$> getNumCapabilities
    let config = defaultConfig
            & #rootDirectory   .~ Public dir
            & #tmpDirectory    .~ Public tmpDir
            & #talDirectory    .~ Public talDir
            & #cacheDirectory  .~ Public cacheDir
            & #parallelism     .~ newParallelism cpuCount_

    (dbResult, _) <- runValidatorT (newScopes "save-snapshot-bench-db") $
        setupSqliteCache Reset logger cacheDir config
    db <- either (\e -> error $ "Failed to set up SQLite cache: " <> show e) pure dbResult

    appState <- newAppState
    database <- newTVarIO db
    let executableVersion = thisExecutableVersion
    pure (dir, AppContext {..})


cleanupBenchContext :: (FilePath, AppContext SqliteBackend) -> IO ()
cleanupBenchContext (dir, _) = removePathForcibly dir


runIteration :: Int -> (FilePath, AppContext SqliteBackend) -> SessionId -> RrdpSerial -> BS.ByteString -> IO ()
runIteration i (_, appContext) sessionId serial content = do
    now <- unNow <$> thisInstant
    let worldVersion = instantToVersion now
        repoUri       = RrdpURL (URI "https://bench.invalid/rrdp/notification.xml")
        notification  = Notification {
                version      = Version 1,
                sessionId    = sessionId,
                serial       = serial,
                snapshotInfo = SnapshotInfo (URI "https://bench.invalid/rrdp/snapshot.xml") (U.sha256s content),
                deltas       = []
            }

    statsEnabled <- getRTSStatsEnabled
    statsBefore <- if statsEnabled then Just <$> getRTSStats else pure Nothing

    cpu0  <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    (result, vs) <- runValidatorT (newScopes "save-snapshot-bench") $
        saveSnapshot appContext worldVersion repoUri notification content
    wall1 <- getMonotonicTimeNSec
    cpu1  <- getCPUTime

    caps <- getNumCapabilities
    let wallSec = fromIntegral (wall1 - wall0) / 1e9 :: Double
        cpuSec  = fromIntegral (cpu1 - cpu0) / 1e12 :: Double
        util    = if wallSec == 0 then 0 else cpuSec / wallSec

    printf "[run %d] wall=%.3fs cpu=%.3fs utilisation=%.0f%% (of %d cores = %.0f%% max)\n"
        i wallSec cpuSec (util * 100) caps (fromIntegral caps * 100 :: Double)

    case statsBefore of
        Just before -> do
            after <- getRTSStats
            let bytesToMb n = fromIntegral n / (1024 * 1024) :: Double
                allocatedThisRun = allocated_bytes after - allocated_bytes before
                gcCpuThisRun     = gc_cpu_ns after - gc_cpu_ns before
                mutatorCpuThisRun = mutator_cpu_ns after - mutator_cpu_ns before
            printf "          gc_cpu=%.3fs mutator_cpu=%.3fs allocated_this_run=%.0fMB max_live=%.0fMB max_mem_in_use=%.0fMB (process peak so far)\n"
                (fromIntegral gcCpuThisRun / 1e9 :: Double)
                (fromIntegral mutatorCpuThisRun / 1e9 :: Double)
                (bytesToMb allocatedThisRun)
                (bytesToMb (max_live_bytes after))
                (bytesToMb (max_mem_in_use_bytes after))
        Nothing -> pure ()

    case result of
        Left e  -> printf "          FAILED: %s\n" (show e)
        Right _ -> pure ()

    let Validations problems = vs ^. #validations
    if null problems
        then pure ()
        else putStrLn $ "          validation issues:\n" <> Text.unpack (formatValidations (vs ^. #validations))

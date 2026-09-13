{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Standalone (non-criterion) benchmark for `validateMutlipleTAs`: the
-- top-down validation entry point that fetches, parses and validates the
-- whole RPKI tree for a set of TAs. Criterion isn't a good fit here for the
-- same reason as in SaveSnapshotBench: we care about wall-clock vs
-- total-CPU-time (how many cores actually get used) and GC/allocation
-- behaviour across a long, network-bound run, not statistical sampling of a
-- pure function.
--
-- Unlike SaveSnapshotBench, this benchmark reuses the SAME SQLite cache
-- across repeats (it never wipes it): the interesting comparison is exactly
-- a cold first pass (nothing cached yet, everything gets freshly fetched,
-- parsed and validated) against subsequent warm passes (manifest shortcuts
-- and publication point state let most of the tree be skipped). Point
-- `defaultBenchRoot` (or the first CLI argument) at the root of a real,
-- already-populated rpki-prover instance to profile against real-world
-- data instead of a cold, empty cache.
--
-- Usage:
--   cabal run validate-tas-bench -- [benchRoot] [repeats]
--   cabal run validate-tas-bench -- /path/to/bench-root 3 +RTS -N8 -RTS
module Main where

import           Control.Concurrent.STM  (newTVarIO)
import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad           (forM, forM_, unless)

import qualified Data.ByteString.Lazy     as LBS
import           Data.Either              (rights)
import           Data.Int                 (Int64)
import qualified Data.List                as List
import qualified Data.Map.Strict          as Map
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Text                as Text

import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Conc                (getNumCapabilities)
import           GHC.Stats               (RTSStats (..), getRTSStats, getRTSStatsEnabled)

import           System.Directory        (createDirectoryIfMissing, doesFileExist)
import           System.CPUTime          (getCPUTime)
import           System.Environment      (getArgs)
import           System.FilePath         ((</>))
import           Text.Printf             (printf)

import           RPKI.AppContext
import           RPKI.AppMonad           (runValidatorT)
import           RPKI.AppState           (instantToVersion, newAppState)
import           RPKI.AppTypes           (Size (..))
import           RPKI.Config
import           RPKI.Domain             (TaName (..), URI (..), estimateVrpCountRoas)
import           RPKI.Logging
import           RPKI.Meta.UniqueId      (thisExecutableVersion)
import           RPKI.Messages           (formatValidations)
import           RPKI.Reporting          (Validations (..), newScopes, MetricMap (..), Count (..))
import           RPKI.RRDP.Http          (downloadToFile)
import           RPKI.Store.AppSqliteStorage
import           RPKI.TAL                (TAL, getTaName, parseTAL)
import           RPKI.Time               (thisInstant, unNow, TimeMs (..))
import           RPKI.Util               (convert, parseRsyncURL)
import           RPKI.Validation.TopDown (TopDownResult (..), validateMutlipleTAs)


-- | Placeholder bench root; replace with the root directory of a real,
-- already-populated rpki-prover instance (the one containing `cache/`,
-- `tals/`, `rsync/`, `tmp/`) to benchmark against real-world data. Left as
-- a fresh empty directory, the first repeat pays the full cold-cache cost.
defaultBenchRoot :: FilePath
defaultBenchRoot = "/Users/mpuzanov/tmp/rpki/sqlite-bench"

defaultRepeats :: Int
defaultRepeats = 3


main :: IO ()
main = do
    args <- getArgs
    let (benchRoot, repeats) = case args of
            []          -> (defaultBenchRoot, defaultRepeats)
            [r]         -> (r, defaultRepeats)
            (r : n : _) -> (r, maybe defaultRepeats id (readMaybe n))

        cacheDir = benchRoot </> "cache"
        talDir   = benchRoot </> "tals"
        rsyncDir = benchRoot </> "rsync"
        tmpDir   = benchRoot </> "tmp"

    mapM_ (createDirectoryIfMissing True) [cacheDir, talDir, rsyncDir, tmpDir]

    withLogger (newLogConfig InfoL MainLog) $ \logger -> do
        tals <- ensureTals logger talDir

        cpuCount_ <- fromIntegral <$> getNumCapabilities
        let prefetchUrls = rights $ map (parseRsyncURL . Text.pack) defaultPrefetchURLs
            config = defaultConfig
                    & #rootDirectory          .~ Public benchRoot
                    & #talDirectory           .~ Public talDir
                    & #tmpDirectory           .~ Public tmpDir
                    & #cacheDirectory         .~ Public cacheDir
                    & #rsyncConf . #rsyncRoot .~ Public rsyncDir
                    & #rsyncConf . #rsyncPrefetchUrls .~ prefetchUrls
                    & #rrdpConf  . #tmpRoot   .~ Public tmpDir
                    & #parallelism            .~ newParallelism cpuCount_

        (dbResult, _) <- runValidatorT (newScopes "validate-tas-bench-db") $
            setupSqliteCache UseExisting logger cacheDir config
        db <- either (\e -> error $ "Failed to set up SQLite cache: " <> show e) pure dbResult

        appState <- newAppState
        database <- newTVarIO db
        let executableVersion = thisExecutableVersion
            appContext = AppContext {..}

        caps         <- getNumCapabilities
        statsEnabled <- getRTSStatsEnabled
        printf "bench root: %s (SQLite cache under cache/ is reused across repeats)\n" benchRoot
        printf "TALs: %s\n" (List.intercalate ", " (map (Text.unpack . unTaName . getTaName) tals))
        printf "capabilities (RTS -N): %d, +RTS -T stats enabled: %s\n" caps (show statsEnabled)
        printf "repeats: %d\n\n" repeats

        forM_ [1 .. repeats] $ \i ->
            runIteration i appContext tals
  where
    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing


-- | Download the 5 standard RIR TALs into `talDir` (if not already present
-- there from a previous run) and parse them.
ensureTals :: AppLogger -> FilePath -> IO [TAL]
ensureTals logger talDir =
    forM defaultTalUrls $ \(talFileName, talUrl) -> do
        let talFilePath = talDir </> talFileName
            taName      = Text.pack (dropTalExtension talFileName)
        exists <- doesFileExist talFilePath
        unless exists $ do
            logInfo logger $ "Downloading " <> Text.pack talUrl <> " to " <> Text.pack talFilePath
            _ <- downloadToFile (URI (Text.pack talUrl)) talFilePath (Size 10_000)
            pure ()
        content <- LBS.readFile talFilePath
        case parseTAL (convert content) taName of
            Left e  -> error $ "Failed to parse TAL " <> talFilePath <> ": " <> show e
            Right t -> pure t
  where
    dropTalExtension f = take (length f - 4) f


runIteration :: Int -> AppContext SqliteBackend -> [TAL] -> IO ()
runIteration i appContext tals = do
    now <- unNow <$> thisInstant
    let worldVersion = instantToVersion now

    statsEnabled <- getRTSStatsEnabled
    statsBefore  <- if statsEnabled then Just <$> getRTSStats else pure Nothing

    cpu0  <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    results <- validateMutlipleTAs appContext worldVersion tals
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
                allocatedThisRun  = allocated_bytes after - allocated_bytes before
                gcCpuThisRun      = gc_cpu_ns after - gc_cpu_ns before
                mutatorCpuThisRun = mutator_cpu_ns after - mutator_cpu_ns before
            printf "          gc_cpu=%.3fs mutator_cpu=%.3fs allocated_this_run=%.0fMB max_live=%.0fMB max_mem_in_use=%.0fMB (process peak so far)\n"
                (fromIntegral gcCpuThisRun / 1e9 :: Double)
                (fromIntegral mutatorCpuThisRun / 1e9 :: Double)
                (bytesToMb allocatedThisRun)
                (bytesToMb (max_live_bytes after))
                (bytesToMb (max_mem_in_use_bytes after))
        Nothing -> pure ()

    let totalVrps = sum
            [ estimateVrpCountRoas roas
            | TopDownResult {..} <- Map.elems results ]

        allValidations = mconcat
            [ topDownValidations
            | TopDownResult {..} <- Map.elems results ]

        Validations problems = allValidations ^. #validations

    printf "          %d TAs validated, %d total VRPs\n" (Map.size results) totalVrps

    -- Aggregate wall-clock time metrics the app already tracks per fetch/validation
    -- phase (see RPKI.Reporting: RrdpMetric.downloadTimeMs/saveTimeMs, RsyncMetric,
    -- ValidationMetric.totalTimeMs), summed across all repositories/TAs. These are
    -- wall-clock, and phases run concurrently (across repos, and within a TA's
    -- object tree), so sums don't add up to the run's wall time -- but the relative
    -- split (fetch/network vs parse+store vs the rest of top-down, i.e. signature
    -- verification and tree walking) is exactly what tells us where to look next.
    let topDownMetric = allValidations ^. #topDownMetric
        rrdpMs   = MonoidalMap.elems $ unMetricMap (topDownMetric ^. #rrdpMetrics)
        rsyncMs  = MonoidalMap.elems $ unMetricMap (topDownMetric ^. #rsyncMetrics)
        validMs  = MonoidalMap.elems $ unMetricMap (topDownMetric ^. #validationMetrics)

        totalDownloadMs = sum [ unTimeMs (m ^. #downloadTimeMs) | m <- rrdpMs ]  :: Int64
        totalRrdpSaveMs = sum [ unTimeMs (m ^. #saveTimeMs)     | m <- rrdpMs ]  :: Int64
        totalRsyncMs    = sum [ unTimeMs (m ^. #totalTimeMs)    | m <- rsyncMs ] :: Int64
        totalValidateMs = sum [ unTimeMs (m ^. #totalTimeMs)    | m <- validMs ] :: Int64

        totalCerts     = sum [ unCount (m ^. #validCertNumber)   | m <- validMs ] :: Int64
        totalRoas      = sum [ unCount (m ^. #validRoaNumber)    | m <- validMs ] :: Int64
        totalMfts      = sum [ unCount (m ^. #validMftNumber)    | m <- validMs ] :: Int64
        totalCrls      = sum [ unCount (m ^. #validCrlNumber)    | m <- validMs ] :: Int64
        totalShortcuts = sum [ unCount (m ^. #mftShortcutNumber) | m <- validMs ] :: Int64

    printf "          [aggregate wall-ms, summed across concurrent repos/TAs] rrdp_download=%dms rrdp_save=%dms rsync=%dms validate_total=%dms\n"
        totalDownloadMs totalRrdpSaveMs totalRsyncMs totalValidateMs
    printf "          [object counts] certs=%d roas=%d mfts=%d crls=%d mft_shortcuts_hit=%d\n"
        totalCerts totalRoas totalMfts totalCrls totalShortcuts

    unless (null problems) $
        putStrLn $ "          validation issues:\n" <> Text.unpack (formatValidations (allValidations ^. #validations))

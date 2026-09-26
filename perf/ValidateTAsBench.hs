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
--   cabal run validate-tas-bench -- [benchRoot] [repeats] [--now=UNIX_SECONDS]
--   cabal run validate-tas-bench -- /path/to/bench-root 3 +RTS -N8 -RTS
--
-- `--now` validates as of that moment instead of the current one, so that
-- runs over a copy of a cache see the same objects valid or expired and
-- their results can be compared.
--
-- `--dump=DIR` writes what the validation produced into DIR (DIR/<run> for
-- more than one run) as sorted text, one item per line and without timings,
-- so that two builds can be compared with `diff`; see perf/topdown-diff.sh.
--
-- `--debug` logs TopDown's counters of the paths it took.
--
-- `--full`, `--reconsidered` and `--strict-manifests` are what
-- `--no-incremental-validation`, `--allow-overclaiming` and
-- `--strict-manifest-validation` are to the program.
module Main where

import           Control.Concurrent.STM  (newTVarIO)
import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad           (forM, forM_, unless, (<=<))

import qualified Data.ByteString.Lazy     as LBS
import           Data.Either              (rights)
import           Data.Int                 (Int64)
import           Data.Maybe               (listToMaybe)
import qualified Data.List                as List
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.Set                 as Set
import qualified Data.Text                as Text

import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Conc                (getNumCapabilities)
import           GHC.Stats               (RTSStats (..), getRTSStats, getRTSStatsEnabled)

import           System.Directory        (createDirectoryIfMissing, doesFileExist)
import           System.CPUTime          (getCPUTime)
import           System.Environment      (getArgs)
import           System.FilePath         ((</>))
import           Debug.Trace             (traceMarkerIO)
import           Text.Printf             (printf)

import           RPKI.AppContext
import           RPKI.AppState           (instantToVersion, newAppState)
import           RPKI.AppTypes           (Count (..), Size (..), WorldVersion)
import           RPKI.Config
import           RPKI.Domain             (Roas (..), TaName (..), URI (..), ValidationRFC (..), estimateVrpCountRoas)
import           RPKI.Logging
import           RPKI.Meta.UniqueId      (thisExecutableVersion)
import           RPKI.Messages           (formatValidations)
import           RPKI.Fetch.Http         (downloadToFile)
import           RPKI.Reporting          (Validations (..), MetricMap (..))
import           RPKI.Store.AppSqliteStorage
import qualified RPKI.Store.Database     as DB
import           RPKI.TAL                (TAL, getTaName, parseTAL)
import           RPKI.Time               (Instant (..), thisInstant, unNow, TimeMs (..))
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
    (flags, args) <- List.partition ("--" `List.isPrefixOf`) <$> getArgs
    let knownFlag f = f `elem` ["--full", "--reconsidered", "--strict-manifests", "--debug"]
                   || any (`List.isPrefixOf` f) ["--now=", "--dump="]
    -- A mistyped flag would quietly compare the wrong things
    case filter (not . knownFlag) flags of
        []      -> pure ()
        unknown -> error $ "Unknown flags: " <> unwords unknown

    let fixedNow = listToMaybe
            [ Instant (seconds * 1_000_000_000)
            | Just seconds <- map (readMaybe <=< List.stripPrefix "--now=") flags ]
        dumpDir = listToMaybe [ d | Just d <- map (List.stripPrefix "--dump=") flags ]
        hasFlag f = f `elem` flags
        (benchRoot, repeats) = case args of
            []          -> (defaultBenchRoot, defaultRepeats)
            [r]         -> (r, defaultRepeats)
            (r : n : _) -> (r, maybe defaultRepeats id (readMaybe n))

        cacheDir = benchRoot </> "cache"
        talDir   = benchRoot </> "tals"
        rsyncDir = benchRoot </> "rsync"
        tmpDir   = benchRoot </> "tmp"

    mapM_ (createDirectoryIfMissing True) [cacheDir, talDir, rsyncDir, tmpDir]

    -- At debug level validation only adds TopDown's counters of what it did
    let logLevel = if hasFlag "--debug" then DebugL else InfoL
    withLogger (newLogConfig logLevel MainLog) $ \logger -> do
        tals <- ensureTals logger talDir

        cpuCount_ <- fromIntegral <$> getNumCapabilities
        let prefetchUrls = rights $ map (parseRsyncURL . Text.pack) defaultPrefetchURLs
            config = defaultConfig
                    & #rootDirectory          .~ Public benchRoot
                    & #talDirectory           .~ Public talDir
                    & #tmpDirectory           .~ Public tmpDir
                    & #cacheDirectory         .~ Public cacheDir
                    & #rsyncConf . #rsyncRoot .~ Public rsyncDir
                    & #rsyncConf . #prefetchUrls .~ prefetchUrls
                    & #rrdpConf  . #tmpRoot   .~ Public tmpDir
                    & #parallelism            .~ newParallelism cpuCount_
                    -- What the program runs with unless --no-incremental-validation
                    -- is given, `defaultConfig` has the other one
                    & #validationConfig . #validationAlgorithm .~
                        (if hasFlag "--full" then FullEveryIteration else Incremental)
                    & #validationConfig . #validationRFC .~
                        (if hasFlag "--reconsidered" then ReconsideredRFC else StrictRFC)
                    & #validationConfig . #manifestProcessing .~
                        (if hasFlag "--strict-manifests" then RFC6486_Strict else RFC9286)

        -- An incompatible cache gets wiped, which would quietly turn this into
        -- a benchmark of validating nothing.
        (_, dbCheck) <- createSqliteDatabase cacheDir config False True
        case dbCheck of
            WasIncompatible -> error $ "The cache in " <> cacheDir <> " is of another version and was wiped."
            _               -> pure ()

        -- Opened as the validation worker opens it: workers leave checkpointing 
        -- the WAL to the main process, and a checkpoint on commit would stall 
        -- the writer of manifest shortcuts for up to a second.
        db <- openExistingSqliteDatabase cacheDir config

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

        forM_ [1 .. repeats] $ \i -> do
            let dumpTo d = if repeats == 1 then d else d </> show i
            runIteration i appContext fixedNow (dumpTo <$> dumpDir) tals
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


runIteration :: Int -> AppContext SqliteBackend -> Maybe Instant -> Maybe FilePath -> [TAL] -> IO ()
runIteration i appContext fixedNow dumpDir tals = do
    now <- maybe (unNow <$> thisInstant) pure fixedNow
    let worldVersion = instantToVersion now

    statsEnabled <- getRTSStatsEnabled
    statsBefore  <- if statsEnabled then Just <$> getRTSStats else pure Nothing

    -- Markers bracket the run in the eventlog (`+RTS -l`)
    traceMarkerIO "validation start"
    cpu0  <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    results <- validateMutlipleTAs appContext worldVersion tals
    wall1 <- getMonotonicTimeNSec
    cpu1  <- getCPUTime
    traceMarkerIO "validation end"

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

    -- Validation works with the cache only, so the one time metric here is
    -- ValidationMetric.totalTimeMs, summed across TAs. TAs are validated
    -- concurrently, so the sum doesn't add up to the run's wall time.
    let topDownMetric = allValidations ^. #topDownMetric
        validMs  = MonoidalMap.elems $ unMetricMap (topDownMetric ^. #validationMetrics)

        totalValidateMs = sum [ unTimeMs (m ^. #totalTimeMs)    | m <- validMs ] :: Int64

        totalCerts     = sum [ unCount (m ^. #validCertNumber)   | m <- validMs ] :: Int64
        totalRoas      = sum [ unCount (m ^. #validRoaNumber)    | m <- validMs ] :: Int64
        totalMfts      = sum [ unCount (m ^. #validMftNumber)    | m <- validMs ] :: Int64
        totalCrls      = sum [ unCount (m ^. #validCrlNumber)    | m <- validMs ] :: Int64
        totalShortcuts = sum [ unCount (m ^. #mftShortcutNumber) | m <- validMs ] :: Int64

    printf "          [aggregate wall-ms, summed across concurrent TAs] validate_total=%dms\n"
        totalValidateMs
    printf "          [object counts] certs=%d roas=%d mfts=%d crls=%d mft_shortcuts_hit=%d\n"
        totalCerts totalRoas totalMfts totalCrls totalShortcuts

    unless (null problems) $
        putStrLn $ "          validation issues:\n" <> Text.unpack (formatValidations (allValidations ^. #validations))

    forM_ dumpDir $ \dir -> do
        dumpResults dir appContext worldVersion results
        printf "          dumped to %s\n" dir


-- | Everything a validation produced, as sorted text with one item per line
-- and without timings, so that runs of two builds can be compared with `diff`.
dumpResults :: FilePath -> AppContext SqliteBackend -> WorldVersion -> Map TaName TopDownResult -> IO ()
dumpResults dir AppContext {..} worldVersion results = do
    createDirectoryIfMissing True dir
    writeLines "vrps.txt"
        [ line [ta, show k, show vrps]
        | (ta, r) <- tas
        , (k, vrps) <- MonoidalMap.toList $ unRoas $ r ^. #payloads . #roas ]
    writeLines "payloads.txt" $ concat
        [ [ line [ta, "spl",    show x] | x <- Set.toList $ r ^. #payloads . #spls ] <>
          [ line [ta, "aspa",   show x] | x <- Set.toList $ r ^. #payloads . #aspas ] <>
          [ line [ta, "gbr",    show x] | x <- Set.toList $ r ^. #payloads . #gbrs ] <>
          [ line [ta, "bgpsec", show x] | x <- Set.toList $ r ^. #payloads . #bgpCerts ]
        | (ta, r) <- tas ]
    writeLines "issues.txt"
        [ line [ta, show scope, show issue]
        | (ta, r) <- tas
        , let Validations vs = r ^. #topDownValidations . #validations
        , (scope, issues) <- Map.toList vs
        , issue <- Set.toList issues ]
    writeLines "metrics.txt" $ concat
        [ [ line [ta, show scope, show (m & #totalTimeMs .~ TimeMs 0)]
          | (scope, m) <- MonoidalMap.toList $ unMetricMap $ metrics ^. #validationMetrics ] <>
          [ line [ta, "vrpCounts", show $ metrics ^. #vrpCounts]
          , line [ta, "traces", show $ r ^. #topDownValidations . #traces] ]
        | (ta, r) <- tas
        , let metrics = r ^. #topDownValidations . #topDownMetric ]
    writeLines "repositories.txt"
        [ line [ta, show (r ^. #discoveredRepositories), show (r ^. #earliestNotValidAfter)]
        | (ta, r) <- tas ]

    -- The objects marked as used by this run, i.e. the ones the cleanup would
    -- keep. It only reads the map, writing it back unchanged.
    validatedBy <- DB.rwTxT database $ \tx -> DB.updateValidatedByVersionMap tx id
    writeLines "visited.txt"
        [ show k | (k, v) <- Map.toList validatedBy, v == worldVersion ]
  where
    tas = [ (Text.unpack $ unTaName ta, r) | (ta, r) <- Map.toList results ]
    line = List.intercalate "\t"
    writeLines f = writeFile (dir </> f) . unlines

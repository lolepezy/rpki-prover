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
--   cabal run save-snapshot-bench -- path/to/snapshot.xml [repeats] [--stages] [--cpu-only] [--dir=PATH] [--delta]
--   cabal run save-snapshot-bench -- path/to/snapshot.xml 5 +RTS -N8 -RTS
--
-- Each repeat gets a fresh temporary SQLite cache directory, so every run
-- pays the full "nothing cached yet" cost instead of hitting the
-- hash-already-exists shortcut on the 2nd+ repeat.
--
-- `--dir` sets where those directories go (default /tmp). /tmp is often
-- tmpfs, where SQLite never waits for a disk; point it at a real
-- filesystem to see what a production cache directory would.
--
-- `--stages` first runs every step of the per-object work one at a time,
-- single-threaded, over the whole snapshot, and reports time and
-- allocation for each, then the SQLite writes on their own. The parallel
-- steps divided by the core count, against the SQLite writes (which are
-- serialised on one connection), is the floor on what the pipeline can do.
--
-- `--cpu-only` runs that per-object work on the same worker pool as
-- `saveSnapshot` (one worker per capability but the first), with no database
-- at all: the ceiling for the whole pipeline on the machine. Sweep `+RTS -N`
-- with it to see where extra workers stop helping.
--
-- `--delta` takes an RRDP delta instead and runs `saveDelta` on it. A snapshot
-- becomes a delta that adds all of its objects with
--   sed 's/<snapshot /<delta /; s/<\/snapshot>/<\/delta>/' snapshot.xml > delta.xml
--
-- The runs are bracketed by eventlog markers, so `+RTS -l` shows what the
-- threads were doing (running or stopped, and why) inside `saveSnapshot` only.
module Main where

import           Control.Concurrent.STM  (newTVarIO, readTVarIO)
import           Control.Exception       (bracket, evaluate)
import           Control.Lens            ((&), (.~), (^.), (<&>))
import           Control.Monad           (forM, forM_, when)
import           Control.Monad.IO.Class  (liftIO)

import qualified Data.ByteString         as BS
import           Data.Int                (Int64)
import           Data.IORef              (newIORef, readIORef)
import qualified Data.List               as List
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import           Data.Maybe              (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text               as Text

import           Codec.Compression.LZ4   (compress)

import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Conc                (getNumCapabilities)
import           GHC.Stats               (RTSStats (..), getRTSStats, getRTSStatsEnabled)

import           Debug.Trace             (traceMarkerIO)
import           System.CPUTime          (getCPUTime)
import           System.Directory        (createDirectoryIfMissing, removePathForcibly)
import           System.Environment      (getArgs)
import           System.Mem              (getAllocationCounter)
import           System.FilePath         ((</>))
import           System.IO.Temp          (createTempDirectory)
import           Text.Printf             (printf)

import           RPKI.AppContext
import           RPKI.AppMonad           (runValidatorIO, runValidatorPure, inSubLocationScope)
import           RPKI.AppState           (instantToVersion, newAppState)
import           RPKI.AppTypes           (Size (..), WorldVersion (..))
import           RPKI.Config
import           RPKI.Cpu                (getCgroupCpuLimit, getPhysicalCpuCount)
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Meta.UniqueId      (thisExecutableVersion)
import           RPKI.Messages           (formatValidations)
import           RPKI.Reporting          (newScopes, Validations (..))
import           RPKI.Parallel           (ResultOrder (..), txPoolPipeline)
import           RPKI.Parse.Parse        (readObjectOfType, urlObjectType)
import           RPKI.RRDP.Parse         (parseDelta, parseSnapshot)
import           RPKI.RRDP.RrdpFetch     (saveDelta, saveSnapshot)
import           RPKI.RRDP.Types
import           RPKI.Store.AppSqliteStorage
import           RPKI.Store.Base.Serialisation (serialise_)
import           RPKI.Store.Base.Storable (Compressed (..), toStorableObject)
import           RPKI.Store.Database     (rwAppTx, roTx, Tx (..))
import qualified RPKI.Store.SQLite       as SQLite
import           Database.SQLite.Simple  (Only (..))
import qualified RPKI.Store.Database     as DB
import           RPKI.Store.Types        (RpkiObjectLifecycle (..))
import           RPKI.Time               (thisInstant, unNow)
import           RPKI.Validation.ObjectValidation (prevalidateObject)
import qualified RPKI.Util               as U


-- | Placeholder snapshot; replace with a real (large) RRDP snapshot.xml to
-- get meaningful numbers.
defaultSnapshotPath :: FilePath
defaultSnapshotPath = "/Users/mpuzanov/tmp/arin-snapshot.xml"

defaultRepeats :: Int
defaultRepeats = 3


main :: IO ()
main = do
    allArgs <- getArgs
    let (flags, args) = List.partition ("--" `List.isPrefixOf`) allArgs
        stages  = "--stages" `elem` flags
        delta   = "--delta" `elem` flags
        -- A delta's publish elements are what the rest of the bench looks at
        parseInput bs
            | delta     = parseDelta bs <&> \(Delta _ sessionId serial items) -> 
                            (sessionId, serial, [ SnapshotPublish uri encoded | DP (DeltaPublish uri _ encoded) <- items ])
            | otherwise = parseSnapshot bs <&> \(Snapshot _ sessionId serial items) -> 
                            (sessionId, serial, items)
        baseDir = fromMaybe "/tmp" $ listToMaybe $ mapMaybe (List.stripPrefix "--dir=") flags
    let (snapshotPath, repeats) = case args of
            []        -> (defaultSnapshotPath, defaultRepeats)
            [p]       -> (p, defaultRepeats)
            (p : n : _) -> (p, maybe defaultRepeats id (readMaybe n))

    content <- BS.readFile snapshotPath

    (sessionId, serial, snapshotItems) <-
        either (\e -> error $ "Failed to parse " <> snapshotPath <> ": " <> show e) pure $
            parseInput content

    caps <- getNumCapabilities
    statsEnabled <- getRTSStatsEnabled
    printf "snapshot: %s (%d bytes, %d publish elements)\n"
        snapshotPath (BS.length content) (length snapshotItems)

    -- Time the XML/RRDP parse in isolation (single-threaded, no ASN.1
    -- object parsing, no DB) to get a floor for "is the XML parser the
    -- bottleneck". Force every publish element's base64 payload (not just
    -- the list spine) so this actually pays the full parsing cost.
    --
    -- Every timed repeat reads its input from an IORef, so that GHC can't
    -- share one evaluated result between repeats (or with the parse above).
    contentRef <- newIORef content
    parseSec <- bestOf 3 $ do
        content' <- readIORef contentRef
        (_, _, items) <-
            either (\e -> error $ "Failed to parse " <> snapshotPath <> ": " <> show e) pure $
                parseInput content'
        evaluate $ List.foldl'
            (\acc (SnapshotPublish _ (EncodedBase64 b)) -> acc + BS.length b) (0 :: Int) items
    printf "xml parse only: %.3fs\n" parseSec

    -- Base64-decode the already parsed payloads, still single-threaded.
    -- Where the whitespace stripping happens (parser or decoder) is an
    -- implementation detail, so parse + decode is the sum to compare
    -- across parser changes.
    itemsRef <- newIORef snapshotItems
    decodeSec <- bestOf 3 $ do
        items <- readIORef itemsRef
        evaluate $ List.foldl'
            (\acc (SnapshotPublish uri encoded) ->
                case U.decodeBase64 encoded uri of
                    Left e                     -> error $ show e
                    Right (DecodedBase64 blob) -> acc + BS.length blob)
            (0 :: Int) items
    printf "base64 decode only: %.3fs\n" decodeSec

    -- What a correct run leaves in the cache: every distinct object once,
    -- and a link from every publish element's URL to its object.
    let expected = Expected {
            objects = Set.size $ Set.fromList
                [ U.sha256s blob | SnapshotPublish uri encoded <- snapshotItems,
                                   Right (DecodedBase64 blob) <- [U.decodeBase64 encoded uri] ],
            links   = length snapshotItems
        }
    printf "xml parse + base64 decode: %.3fs\n" (parseSec + decodeSec)

    printf "capabilities (RTS -N): %d, +RTS -T stats enabled: %s\n" caps (show statsEnabled)
    physical <- getPhysicalCpuCount
    quota    <- getCgroupCpuLimit
    printf "physical cores: %s, cgroup CPU quota: %s (saveSnapshot uses at most the smaller)\n" 
        (maybe "unknown" show physical) (maybe "none" show quota)
    printf "cache directories under: %s\n" baseDir
    printf "repeats: %d\n\n" repeats

    withLogger (newLogConfig ErrorL MainLog) $ \logger -> do
        when stages $
            bracket (mkBenchContext baseDir logger) cleanupBenchContext $ \appContext ->
                stageBreakdown appContext snapshotItems

        when ("--cpu-only" `elem` flags) $ cpuOnly snapshotItems

        forM_ [1 .. repeats] $ \i ->
            bracket (mkBenchContext baseDir logger) cleanupBenchContext $ \appContext ->
                runIteration i appContext delta expected sessionId serial content
  where
    bestOf :: Int -> IO a -> IO Double
    bestOf n action = fmap minimum $ forM [1 .. n] $ \_ -> do
        t0 <- getMonotonicTimeNSec
        _  <- action
        t1 <- getMonotonicTimeNSec
        pure (fromIntegral (t1 - t0) / 1e9)

    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing


mkBenchContext :: FilePath -> AppLogger -> IO (FilePath, AppContext SqliteBackend)
mkBenchContext baseDir logger = do
    dir <- createTempDirectory baseDir "rpki-save-snapshot-bench"
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

    (db, _) <- createSqliteDatabase cacheDir config True False

    appState <- newAppState
    database <- newTVarIO db
    let executableVersion = thisExecutableVersion
    pure (dir, AppContext {..})


cleanupBenchContext :: (FilePath, AppContext SqliteBackend) -> IO ()
cleanupBenchContext (dir, _) = removePathForcibly dir


data Expected = Expected { objects :: Int, links :: Int }

runIteration :: Int -> (FilePath, AppContext SqliteBackend) -> Bool -> Expected -> SessionId -> RrdpSerial -> BS.ByteString -> IO ()
runIteration i (_, appContext) delta expected sessionId serial content = do
    now <- unNow <$> thisInstant
    -- In production the hash comes out of the download; hashing 700MB here
    -- inside the timed region would add seconds of single-threaded work.
    snapshotHash <- evaluate $ U.sha256s content
    let worldVersion = instantToVersion now
        repoUri       = RrdpURL (URI "https://bench.invalid/rrdp/notification.xml")
        notification  = Notification {
                version      = Version 1,
                sessionId    = sessionId,
                serial       = serial,
                snapshotInfo = SnapshotInfo (URI "https://bench.invalid/rrdp/snapshot.xml") snapshotHash,
                deltas       = []
            }

    statsEnabled <- getRTSStatsEnabled
    statsBefore <- if statsEnabled then Just <$> getRTSStats else pure Nothing

    -- Markers bracket the run in the eventlog (`+RTS -l`), so the setup
    -- before it can be left out when looking at what the threads did.
    traceMarkerIO "saveSnapshot start"
    cpu0  <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    (result, vs) <- runValidatorIO (newScopes "save-snapshot-bench") $
        if delta 
            then saveDelta appContext worldVersion repoUri notification serial content
            else saveSnapshot appContext worldVersion repoUri notification content
    wall1 <- getMonotonicTimeNSec
    cpu1  <- getCPUTime
    traceMarkerIO "saveSnapshot end"

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
        Right _ -> do
            db <- readTVarIO $ appContext ^. #database
            (stats, linkCount) <- roTx db $ \tx@(Tx conn) -> do
                stats <- DB.getObjectsStats tx
                [Only n] <- liftIO $ SQLite.query_ conn "SELECT COUNT(*) FROM object_urls"
                pure (stats, n :: Int)
            let Size objectCount = stats ^. #totalObjects
                ok = fromIntegral objectCount == expected.objects && linkCount == expected.links
            printf "          stored %d objects and %d url links (expected %d and %d)%s\n"
                objectCount linkCount expected.objects expected.links
                (if ok then "" else " -- MISMATCH" :: String)

    let Validations problems = vs ^. #validations
    if null problems
        then pure ()
        else putStrLn $ "          validation issues:\n" <> Text.unpack (formatValidations (vs ^. #validations))


-- | Every step `saveSnapshot` does per object, one at a time over the whole
-- snapshot on this thread, each step's output kept for the next. Most of
-- the types are StrictData, so `evaluate` forces nearly all of a step's
-- work into that step; whatever it doesn't is picked up by `serialise`.
stageBreakdown :: (FilePath, AppContext SqliteBackend) -> [SnapshotPublish] -> IO ()
stageBreakdown (_, appContext) items = do
    putStrLn "per-object steps, single-threaded:"
    let scopes = newScopes "stages"

    decoded <- stage "url + base64" $ forM items $ \(SnapshotPublish uri encoded) ->
        case U.parseRpkiURL (unURI uri) of
            Left e        -> error $ show e
            Right rpkiURL -> case U.decodeBase64 encoded rpkiURL of
                Left e                     -> error $ show e
                Right (DecodedBase64 blob) -> do
                    let Just type_ = urlObjectType rpkiURL
                    evaluate blob >> pure (uri, rpkiURL, type_, blob)

    hashes <- stage "sha256" $ forM decoded $ \(_, _, _, blob) -> evaluate $ U.sha256s blob

    parsed <- stage "asn.1 parse" $ forM decoded $ \(uri, _, type_, blob) ->
        case runValidatorPure scopes $ inSubLocationScope uri $ readObjectOfType type_ blob of
            (Right ro, _) -> evaluate ro
            (Left e, _)   -> error $ show e

    valid <- stage "prevalidate" $ forM parsed $ \ro ->
        case runValidatorPure scopes $ prevalidateObject ro of
            (Right vro, _) -> evaluate $ WellStructuredRO vro
            (Left e, _)    -> error $ show e

    serialised <- stage "serialise" $ forM valid $ evaluate . serialise_
    _          <- stage "lz4" $ forM serialised $ evaluate . compress

    -- What the parsing threads actually hand to the writer.
    storables <- forM valid $ \lifecycle -> evaluate $ toStorableObject (Compressed lifecycle)

    let rawBytes = sum $ map (\(_, _, _, b) -> BS.length b) decoded
    printf "  (%d objects, %.0fMB DER, %.0fMB serialised)\n"
        (length items) (mb rawBytes) (mb $ sum $ map BS.length serialised)

    -- Where the per-object allocation goes, by object type.
    putStrLn "per type, asn.1 parse + prevalidate + serialise + lz4:"
    perType <- fmap (Map.fromListWith add4) $ forM decoded $ \(uri, _, type_, blob) -> do
        a0 <- getAllocationCounter
        t0 <- getMonotonicTimeNSec
        case runValidatorPure scopes $ inSubLocationScope uri $
                prevalidateObject =<< readObjectOfType type_ blob of
            (Right vro, _) -> () <$ evaluate (toStorableObject (Compressed (WellStructuredRO vro)))
            (Left e, _)    -> error $ show e
        t1 <- getMonotonicTimeNSec
        a1 <- getAllocationCounter
        pure (type_, (1 :: Int, BS.length blob, t1 - t0, a0 - a1))
    forM_ (Map.toList perType) $ \(type_, (n, bytes, ns, alloc)) ->
        printf "  %-5s n=%7d avg %6.0fB  %7.3fs  %8.0fMB alloc  %6.0fKB alloc/object\n"
            (show type_) n (fromIntegral bytes / fromIntegral n :: Double)
            (fromIntegral ns / 1e9 :: Double) (mb alloc)
            (fromIntegral alloc / 1024 / fromIntegral n :: Double)

    -- The writer's side alone: exactly the calls `saveStorable` makes for a
    -- new object, all in one transaction the way `saveSnapshot` does it.
    db <- readTVarIO $ appContext ^. #database
    let worldVersion = WorldVersion 1
    putStrLn "sqlite, single-threaded:"
    _ <- stage "insert, 1 tx" $ runValidatorIO scopes $ rwAppTx db $ \tx ->
        forM_ (zip decoded storables) $ \((_, rpkiURL, _, _), so) -> do
            key <- DB.saveStorableObject tx so worldVersion
            DB.linkObjectToUrl tx rpkiURL key worldVersion

    -- What every parsing thread does before parsing: a read transaction
    -- per object, here against a DB that has them all.
    _ <- stage "lookup, 1 roTx each" $ forM_ hashes $ \h -> roTx db (\tx -> DB.getObjectKey tx h)
    putStrLn ""
  where
    mb :: Integral n => n -> Double
    mb n = fromIntegral n / (1024 * 1024)

    add4 (a, b, c, d) (a', b', c', d') = (a + a', b + b', c + c', d + d')

    stage :: String -> IO a -> IO a
    stage name action = do
        a0 <- getAllocationCounter
        t0 <- getMonotonicTimeNSec
        r  <- action
        t1 <- getMonotonicTimeNSec
        a1 <- getAllocationCounter
        printf "  %-22s %7.3fs  %8.0fMB alloc\n" name
            (fromIntegral (t1 - t0) / 1e9 :: Double) (mb (a0 - a1 :: Int64))
        pure r


-- | The per-object CPU work alone -- base64, ASN.1 parse, prevalidation,
-- serialisation and compression -- spread over all capabilities with no
-- database and no writer. Whatever throughput this reaches is the ceiling
-- for the real pipeline on this machine.
cpuOnly :: [SnapshotPublish] -> IO ()
cpuOnly items = do
    caps <- getNumCapabilities
    let scopes = newScopes "cpu-only"
    let work (SnapshotPublish uri encoded) =
            case U.parseRpkiURL (unURI uri) of
                Left e        -> error $ show e
                Right rpkiURL -> case U.decodeBase64 encoded rpkiURL of
                    Left e                     -> error $ show e
                    Right (DecodedBase64 blob) -> do
                        let Just type_ = urlObjectType rpkiURL
                        _ <- evaluate $ U.sha256s blob
                        case runValidatorPure scopes $ inSubLocationScope uri $
                                prevalidateObject =<< readObjectOfType type_ blob of
                            (Right vro, _) -> () <$ evaluate (toStorableObject (Compressed (WellStructuredRO vro)))
                            (Left e, _)    -> error $ show e
    cpu0  <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    (_, _) <- runValidatorIO scopes $
        txPoolPipeline CompletionOrder items (liftIO . work) ($ ()) (\_ _ -> pure ())
    wall1 <- getMonotonicTimeNSec
    cpu1  <- getCPUTime
    let wallSec = fromIntegral (wall1 - wall0) / 1e9 :: Double
        cpuSec  = fromIntegral (cpu1 - cpu0) / 1e12 :: Double
    printf "cpu only, %d caps: wall=%.3fs cpu=%.3fs utilisation=%.0f%% %.0f objects/s %.0fus cpu/object\n\n"
        caps wallSec cpuSec (cpuSec / wallSec * 100)
        (fromIntegral (length items) / wallSec)
        (cpuSec / fromIntegral (length items) * 1e6)

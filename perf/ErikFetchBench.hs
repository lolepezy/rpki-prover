{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Standalone benchmark for a cold Erik fetch of one FQDN: download from the
-- relays, parse, and store everything into a fresh SQLite cache. It runs
-- 'fetchErik' in-process with the same RTS options the Erik worker gets, so
-- the memory figures are the ones a worker would show.
--
-- Usage:
--   cabal run erik-fetch-bench -- FQDN [RELAY ...]
--   /usr/bin/time -l $(cabal list-bin erik-fetch-bench) rpki.ripe.net https://miso.sobornost.net
--
-- A live relay makes the download time noisy and the snapshot a moving target,
-- so for before/after comparisons point it at a caching proxy that has been
-- filled once (ERIK_BENCH_PARALLELISM=N raises the download threads for that).
--
-- The Erik fetcher's own log goes to stdout at info level; the line that
-- matters for the write transaction is "Stored downloaded Erik objects".
module Main where

import           Control.Concurrent.STM  (newTVarIO)
import           Control.Exception       (bracket)
import           Control.Lens            ((&), (.~), (^.))

import qualified Data.Map.Strict         as Map
import qualified Data.Text               as Text

import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Conc                (getNumProcessors)
import           GHC.Stats               (RTSStats (..), getRTSStats, getRTSStatsEnabled)

import           System.CPUTime          (getCPUTime)
import           System.Directory        (createDirectoryIfMissing, removePathForcibly)
import           System.Environment      (getArgs, lookupEnv)
import           System.FilePath         ((</>))
import           System.IO.Temp          (createTempDirectory)
import           Text.Printf             (printf)
import           Text.Read               (readMaybe)

import           RPKI.AppContext
import           RPKI.AppMonad           (runValidatorIO)
import           RPKI.AppState           (instantToVersion, newAppState)
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Fetch.Erik.ErikRelay (fetchErik)
import           RPKI.Logging
import           RPKI.Meta.UniqueId      (thisExecutableVersion)
import           RPKI.Reporting
import           RPKI.Store.AppSqliteStorage
import           RPKI.Time               (thisInstant, unNow)


defaultRelays :: [URI]
defaultRelays = [ URI "https://miso.sobornost.net" ]


main :: IO ()
main = do
    getArgs >>= \case
        []              -> putStrLn "usage: erik-fetch-bench FQDN [RELAY ...]"
        (fqdn : relays) -> do
            let relayUris = if null relays then defaultRelays else map (URI . Text.pack) relays
            withLogger (newLogConfig InfoL MainLog) $ \logger ->
                bracket (mkBenchContext logger) cleanupBenchContext $ \ctx ->
                    runFetch ctx (FQDN $ Text.pack fqdn) relayUris


mkBenchContext :: AppLogger -> IO (FilePath, AppContext SqliteBackend)
mkBenchContext logger = do
    dir <- createTempDirectory "/tmp" "rpki-erik-fetch-bench"
    let cacheDir = dir </> "cache"
        tmpDir   = dir </> "tmp"
        talDir   = dir </> "tals"
    mapM_ (createDirectoryIfMissing True) [cacheDir, tmpDir, talDir]

    -- The worker starts at -N1 and raises its capabilities to the configured
    -- CPU count itself, so the count has to come from the machine, not -N.
    cpuCount_ <- fromIntegral <$> getNumProcessors
    -- Only for filling a cache ahead of the real runs, which should use the
    -- defaults the worker would.
    downloadThreads <- (>>= readMaybe) <$> lookupEnv "ERIK_BENCH_PARALLELISM"
    let config = defaultConfig
            & #rootDirectory   .~ Public dir
            & #tmpDirectory    .~ Public tmpDir
            & #talDirectory    .~ Public talDir
            & #cacheDirectory  .~ Public cacheDir
            & #parallelism     .~ newParallelism cpuCount_
            & maybe id (\n -> (#erikConf . #parallelism .~ n) . (#erikConf . #relayParallelism .~ n))
                    downloadThreads

    (db, _) <- createSqliteDatabase cacheDir config True False

    appState <- newAppState
    database <- newTVarIO db
    let executableVersion = thisExecutableVersion
    pure (dir, AppContext {..})


cleanupBenchContext :: (FilePath, AppContext SqliteBackend) -> IO ()
cleanupBenchContext (dir, _) = removePathForcibly dir


runFetch :: (FilePath, AppContext SqliteBackend) -> FQDN -> [URI] -> IO ()
runFetch (_, appContext) fqdn relayUris = do
    worldVersion <- instantToVersion . unNow <$> thisInstant

    cpu0  <- getCPUTime
    wall0 <- getMonotonicTimeNSec
    (result, vs) <- runValidatorIO (newScopes "erik-fetch-bench") $
        fetchErik appContext worldVersion relayUris fqdn
    wall1 <- getMonotonicTimeNSec
    cpu1  <- getCPUTime

    let wallSec = fromIntegral (wall1 - wall0) / 1e9 :: Double
        cpuSec  = fromIntegral (cpu1 - cpu0) / 1e12 :: Double
    printf "\nwall=%.1fs cpu=%.1fs\n" wallSec cpuSec

    statsEnabled <- getRTSStatsEnabled
    if statsEnabled
        then do
            s <- getRTSStats
            let mb n = fromIntegral n / (1024 * 1024) :: Double
            printf "allocated=%.0fMB max_live=%.0fMB max_mem_in_use=%.0fMB gc_cpu=%.1fs major_gcs=%d\n"
                (mb $ allocated_bytes s) (mb $ max_live_bytes s)
                (mb $ max_mem_in_use_bytes s)
                (fromIntegral (gc_cpu_ns s) / 1e9 :: Double) (major_gcs s)
        else putStrLn "RTS stats disabled, run with +RTS -T"

    let Validations issues = vs ^. #validations
    printf "validation scopes with issues: %d\n" (Map.size issues)
    case result of
        Left e  -> printf "FAILED: %s\n" (show e)
        Right r -> printf "result: %s\n" (show r)

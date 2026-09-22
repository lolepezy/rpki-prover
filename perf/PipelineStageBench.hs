{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase          #-}

-- | Per-stage allocation/time breakdown for the RRDP object-processing
-- pipeline (the part of `saveSnapshot` that runs on the parsing/async
-- threads): base64 decode, SHA-256 hash, ASN.1 parse + prevalidate +
-- serialise, and LZ4 compress.
--
-- `save-snapshot-bench` showed the pipeline allocates ~400-500x the input
-- snapshot size (128GB / 300GB per run for 274MB / 728MB inputs) despite
-- modest peak residency, and that heap profiling (`+RTS -h*`) needs a
-- `-prof` rebuild of every dependency on this GHC version. This benchmark
-- gets the same answer more directly: it runs each stage standalone (no DB,
-- no async pipeline, single-threaded) over real objects from a snapshot and
-- measures GHC.Stats allocation deltas around each one, so there's no need
-- to profile the whole dependency tree.
--
-- Each stage's result is forced enough to pay its real cost and no more:
-- stage 3 forces evaluation by serialising (`toStorable`), and stage 4
-- reuses exactly those bytes (via the `Compressed Storable` instance, which
-- is `id` beneath the compression) so compression is measured in isolation
-- without re-serialising.
--
-- Usage:
--   cabal run pipeline-stage-bench -- path/to/snapshot.xml [object-limit]
module Main where

import           Control.Exception       (SomeException, evaluate, catch)
import           Control.Monad           (forM_)

import qualified Data.ByteString         as BS
import           Data.IORef              (IORef, modifyIORef', newIORef, readIORef)
import qualified Data.Map.Strict         as Map
import           Data.Map.Strict         (Map)
import           Data.Word               (Word64)

import           GHC.Clock               (getMonotonicTimeNSec)
import           GHC.Stats               (allocated_bytes, getRTSStats, getRTSStatsEnabled)

import           System.Environment      (getArgs)
import           Text.Printf             (printf)

import           RPKI.AppMonad           (runValidatorPure)
import           RPKI.Domain
import           RPKI.Parse.Parse        (readObjectOfType, urlObjectType)
import           RPKI.Reporting          (Scopes, newScopes, hasValidationErrors)
import           RPKI.RRDP.Parse         (parseSnapshot)
import           RPKI.RRDP.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Types        (RpkiObjectLifecycle (..), ObjectOriginal (..))
import           RPKI.Validation.ObjectValidation (prevalidateObject)
import qualified RPKI.Util               as U


-- Parsed and well-structured objects have no NFData instances, so parsing
-- and prevalidation are forced by serialising their result. The same result
-- is then serialised a second time, which (now that every thunk has been
-- forced) measures serialisation alone, so e.g.
--   parse cost = ParseAndSerialise - SerialiseParsed
data Stage = Base64Decode 
           | Sha256Hash 
           | ParseAndSerialise 
           | SerialiseParsed
           | PrevalidateAndSerialise 
           | SerialiseWellStructured
           | Compress
    deriving (Eq, Ord, Show, Enum, Bounded)

data Totals = Totals { tCount :: !Int, tNs :: !Word64, tAlloc :: !Word64 }

instance Semigroup Totals where
    Totals c1 n1 a1 <> Totals c2 n2 a2 = Totals (c1 + c2) (n1 + n2) (a1 + a2)

instance Monoid Totals where
    mempty = Totals 0 0 0


main :: IO ()
main = do
    args <- getArgs
    let (snapshotPath, objectLimit) = case args of
            []        -> (defaultSnapshotPath, Nothing)
            [p]       -> (p, Nothing)
            (p : n : _) -> (p, readMaybe n)

    statsEnabled <- getRTSStatsEnabled
    if not statsEnabled
        then putStrLn "RTS stats not enabled; run with +RTS -T -RTS (should be on by default for this executable)."
        else pure ()

    content <- BS.readFile snapshotPath
    Snapshot _ _ _ snapshotItems <-
        either (\e -> error $ "Failed to parse " <> snapshotPath <> ": " <> show e) pure $
            parseSnapshot content

    let items = maybe snapshotItems (`take` snapshotItems) objectLimit
    printf "processing %d of %d publish elements from %s\n\n"
        (length items) (length snapshotItems) snapshotPath

    totalsRef <- newIORef Map.empty
    let scopes = newScopes "pipeline-stage-bench"
    forM_ items $ \(SnapshotPublish uri encodedb64) ->
        processOne totalsRef scopes uri encodedb64
            `catch` (\(_ :: SomeException) -> pure ())

    totals <- readIORef totalsRef
    report totals
  where
    readMaybe s = case reads s of
        [(n, "")] -> Just n
        _         -> Nothing


defaultSnapshotPath :: FilePath
defaultSnapshotPath = "/Users/mpuzanov/tmp/ripe-snapshot.xml"


processOne :: IORef (Map (Stage, String) Totals) -> Scopes -> URI -> EncodedBase64 -> IO ()
processOne totalsRef scopes uri encodedb64 =
    case U.parseRpkiURL (unURI uri) of
        Left _         -> pure ()
        Right rpkiURL  ->
            case urlObjectType rpkiURL of
                Nothing    -> pure ()
                Just type_ -> do
                    decoded <- measure totalsRef (show type_) Base64Decode $
                        pure $! case U.decodeBase64 encodedb64 rpkiURL of
                            Left _                     -> Nothing
                            Right (DecodedBase64 blob) -> Just blob
                    case decoded of
                        Nothing   -> pure ()
                        Just blob -> do
                            let hash_ = U.sha256s blob
                            _ <- measure totalsRef (show type_) Sha256Hash $ evaluate hash_

                            let measure' stage = measure totalsRef (show type_) stage
                            let (parsedZ, parseVs) = runValidatorPure scopes $ readObjectOfType type_ blob
                            _ <- measure' ParseAndSerialise $ serialiseParsed parsedZ
                            _ <- measure' SerialiseParsed $ serialiseParsed parsedZ

                            let lifecycle = case parsedZ of
                                    Left _ -> OriginalRO (ObjectOriginal blob) parseVs hash_ type_
                                    Right parsed -> 
                                        let (z, vs) = runValidatorPure scopes $ prevalidateObject parsed
                                        in case z of
                                            Left _ ->
                                                OriginalRO (ObjectOriginal blob) (parseVs <> vs) hash_ type_
                                            Right vro
                                                | hasValidationErrors vs ->
                                                    OriginalRO (ObjectOriginal blob) (parseVs <> vs) hash_ type_
                                                | otherwise ->
                                                    WellStructuredRO vro
                            _ <- measure' PrevalidateAndSerialise $ serialiseLifecycle lifecycle
                            serialisedBytes <- measure' SerialiseWellStructured $ serialiseLifecycle lifecycle

                            _ <- measure totalsRef (show type_) Compress $ do
                                let Storable compressed =
                                        toStorable (Compressed (Storable serialisedBytes))
                                evaluate compressed
                            pure ()
  where
    serialiseParsed = \case
        Left _       -> evaluate BS.empty
        Right parsed -> let Storable bytes = toStorable parsed in evaluate bytes
    serialiseLifecycle lifecycle = 
        let Storable bytes = toStorable lifecycle in evaluate bytes


-- | Time + allocation-delta wrapper around one stage. Uses `GHC.Stats`
-- (needs `+RTS -T`, on by default for this executable) rather than a
-- separate allocation-counter API so it works the same whether or not RTS
-- stats happen to be enabled.
measure :: IORef (Map (Stage, String) Totals) -> String -> Stage -> IO a -> IO a
measure ref objectType stage act = do
    before <- getRTSStats
    t0 <- getMonotonicTimeNSec
    !r <- act
    t1 <- getMonotonicTimeNSec
    after <- getRTSStats
    let elapsed = t1 - t0
        allocDelta = allocated_bytes after - allocated_bytes before
    modifyIORef' ref (Map.insertWith (<>) (stage, objectType) (Totals 1 elapsed allocDelta))
    pure r


report :: Map (Stage, String) Totals -> IO ()
report totalsByType = do
    let byStage = Map.mapKeysWith (<>) fst totalsByType
    table byStage show
    putStrLn ""
    table totalsByType (\(stage, type_) -> type_ <> " " <> show stage)
  where
    table :: Map k Totals -> (k -> String) -> IO ()
    table totals name = do
        let grandNs    = sum [tNs t | t <- Map.elems totals]
            grandAlloc = sum [tAlloc t | t <- Map.elems totals]
        printf "%-32s %8s %10s %12s %10s  %8s %8s\n"
            ("stage" :: String) ("count" :: String) ("wall(s)" :: String)
            ("alloc(MB)" :: String) ("KB/obj" :: String) ("%time" :: String) ("%alloc" :: String)
        forM_ (Map.toList totals) $ \(k, Totals cnt ns alloc) ->
            printf "%-32s %8d %10.3f %12.1f %10.1f  %7.1f%% %7.1f%%\n"
                (name k) cnt (nsToSec ns) (bytesToMb alloc)
                (bytesToMb alloc * 1024 / fromIntegral (max 1 cnt))
                (100 * nsToSec ns / nsToSec grandNs)
                (100 * bytesToMb alloc / bytesToMb grandAlloc)
        printf "%-32s %8s %10.3f %12.1f\n" ("TOTAL" :: String) ("" :: String) (nsToSec grandNs) (bytesToMb grandAlloc)
    nsToSec n = fromIntegral n / 1e9 :: Double
    bytesToMb n = fromIntegral n / (1024 * 1024) :: Double

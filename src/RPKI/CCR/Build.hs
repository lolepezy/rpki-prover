{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{- | Building CCR files in the main process (with --with-ccr).

A CCR is built for every new state of the validated cache, i.e. every time the
world version in AppState changes. If validations finish faster than CCRs are
built, the builder skips to the latest one. The files are written to
<root>/ccr/ through a temporary file and a rename, so a reader never sees a
partial file, and the previous CCR stays when a build fails.
-}
module RPKI.CCR.Build (
    runCcrBuilder,
    buildCcr
) where

import           Control.Concurrent.STM
import qualified Control.Exception                as IOExc
import           Control.Lens
import qualified Codec.Compression.GZip           as GZip
import qualified Data.ByteString.Lazy             as LBS
import           Data.Maybe                       (mapMaybe)
import qualified Data.Set                         as Set
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as Text
import           System.Directory                 (createDirectoryIfMissing, renameFile)
import           System.FilePath                  ((</>))

import           RPKI.AppContext
import           RPKI.AppTypes
import           RPKI.CCR
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.RTR.Types                   (RtrPayloads(..))
import           RPKI.Store.Database              (roTxT)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types                 (StorableTA(..))
import           RPKI.TAL                         (getTaName)
import           RPKI.Time
import           RPKI.Util                        (hashHex, sha256, decodeBase64)


-- | Build a CCR every time there's a new state of the validated cache.
runCcrBuilder :: AppContext s -> IO ()
runCcrBuilder appContext@AppContext {..} = go Nothing
  where
    go lastBuilt = do
        (version, payloads) <- atomically $ do
            world <- readTVar $ appState ^. #world
            case world of
                Just v | Just v /= lastBuilt -> (v, ) <$> readTVar (appState ^. #validated)
                _                           -> retry

        (z, elapsed) <- timedMS $ IOExc.try $ buildCcr appContext version payloads
        case z of
            Left (e :: IOExc.SomeException)
                | Just (_ :: IOExc.SomeAsyncException) <- IOExc.fromException e -> IOExc.throwIO e
                | otherwise -> logError logger [i|Failed to build a CCR for #{version}: #{e}|]
            Right (Left message) ->
                logInfo logger [i|Not building a CCR for #{version}: #{message}|]
            Right (Right file) -> do
                atomically $ writeTVar (appState ^. #ccrFile) (Just file)
                let plainSize   = file ^. #plain . #size
                    gzippedSize = file ^. #gzipped . #size
                logInfo logger $ [i|Built a CCR for #{version}, |] <>
                                 [i|#{plainSize} bytes, #{gzippedSize} gzipped, took #{elapsed}ms.|]
        go (Just version)


-- | Build the CCR of this version of the validated cache and write it to the disk.
-- `payloads` are the VRPs, ASPAs and router keys before SLURM: a CCR
-- describes the validated cache, SLURM is local policy.
buildCcr :: AppContext s -> WorldVersion -> RtrPayloads -> IO (Either Text CcrFile)
buildCcr AppContext {..} version RtrPayloads {..} = do
    (states, activeTas) <- roTxT database $ \tx ->
        (,) <$> DB.getCcrStates tx version <*> DB.getTAs tx

    let walked  = Set.fromList $ map fst $ perTA states
    let missing = [ taName | StorableTA { tal } <- activeTas
                           , let taName = getTaName tal
                           , taName `Set.notMember` walked ]
    case missing of
        -- A CCR without some TA's manifests would look as if the TA had vanished
        _ : _ -> pure $ Left [i|there's no CCR data for #{map unTaName missing} yet.|]
        []    -> do
            Now producedAt <- thisInstant
            let taStates = map snd $ perTA states
            let ccr = Ccr {
                    producedAt   = producedAt,
                    manifests    = concatMap (^. #manifests) taStates,
                    vrps         = vrpsToList uniqueVrps,
                    aspas        = Set.toList aspas,
                    trustAnchors = mapMaybe (^. #trustAnchor) taStates,
                    routerKeys   = concatMap routerKeysOf $ Set.toList bgpSec
                }
            Right <$> writeCcr producedAt (encodeCcr ccr)
  where
    ccrDirectory = configValue (config ^. #rootDirectory) </> "ccr"

    writeCcr producedAt bytes = do
        createDirectoryIfMissing True ccrDirectory
        plain   <- writeAtomically "rpki.ccr" bytes
        gzipped <- writeAtomically "rpki.ccr.gz" $ GZip.compress bytes
        pure CcrFile {..}

    writeAtomically name bytes = do
        let path = ccrDirectory </> name
        let tmp  = path <> ".tmp"
        LBS.writeFile tmp bytes
        renameFile tmp path
        pure CcrFileVariant {
                path = path,
                etag = Text.decodeUtf8 $ hashHex $ sha256 bytes,
                size = LBS.length bytes
            }

    routerKeysOf BGPSecPayload {..} =
        case decodeBase64 (unSPKI bgpSecSpki) ("router key SPKI" :: Text) of
            Left _                    -> []
            Right (DecodedBase64 spki) -> [ RouterKey asn bgpSecSki spki | asn <- bgpSecAsns ]

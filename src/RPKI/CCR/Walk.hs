{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}

{- | The part of a CCR that comes from one TA, read from the manifest shortcuts.

After validation the shortcuts describe the tree as validation found it: every
CA with an accepted manifest has a shortcut naming that manifest, and the CA
certificates on it are in `mft_shortcut_ca_children` with whether they are
valid. So the walk starts from the TA certificate, emits a ManifestInstance for
every CA with a current manifest, and descends into its valid subordinates.

It has to run right after the TA's validation, once all its shortcut writes
are flushed: validations of different TAs can run in parallel, so at any other
moment another TA's tree may be half written.
-}
module RPKI.CCR.Walk (
    walkShortcuts
) where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Short            as BSS
import qualified Data.Set                         as Set
import           Data.Set                         (Set)
import           Data.String.Interpolate.IsString
import           Data.Text                        (Text)

import           RPKI.CCR
import           RPKI.Domain
import           RPKI.Store.Database              (Tx)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types                 (MftMeta(..))
import           RPKI.Time
import           RPKI.Validation.Types


-- | Walk the shortcuts of the tree under the TA certificate stored under
-- `taCertKey`, as of `now`. CAs deeper than `maxDepth` are not validated,
-- so they are not walked either. Returns what was found and the problems,
-- i.e. inconsistencies of the cache, that made the walk skip a CA.
walkShortcuts :: MonadIO m => Tx mode -> Now -> Int -> ObjectKey -> m (CcrTaState, [Text])
walkShortcuts tx now maxDepth taCertKey = do
    DB.getCertificateSkiAndValidity tx taCertKey >>= \case
        Nothing ->
            pure (mempty, [[i|TA certificate #{taCertKey} is not in the cache.|]])
        Just (taSki, taValidity)
            -- Nothing is valid under an invalid TA certificate
            | not (isCurrent taValidity) -> pure (mempty, [])
            | otherwise -> do
                (manifests, problems) <- go Set.empty [(taSki, 0)] [] []
                pure (CcrTaState (Just taSki) manifests, problems)
  where
    isCurrent :: Maybe ValidityPeriod -> Bool
    isCurrent = \case
        Just ValidityPeriod { notBefore, notAfter } -> notBefore <= unNow now && unNow now < notAfter
        Nothing -> False

    -- Depth-first, the order doesn't matter since the encoder sorts everything.
    -- The visited set is what stops cycles, the same way validation stops
    -- validating the same key identifier twice.
    go :: MonadIO m => Set SKI -> [(SKI, Int)] -> [ManifestInstance] -> [Text] -> m ([ManifestInstance], [Text])
    go _ [] manifests problems = pure (manifests, problems)
    go visited ((ski, depth) : rest) manifests problems
        | ski `Set.member` visited || depth > maxDepth = go visited rest manifests problems
        | otherwise = do
            let visited' = Set.insert ski visited
            manifestOf ski >>= \case
                Left problem   -> go visited' rest manifests (problem : problems)
                Right Nothing  -> go visited' rest manifests problems
                Right (Just mi) -> do
                    let children = [ (s, depth + 1) | s <- Set.toList mi.subordinates ]
                    go visited' (children <> rest) (mi : manifests) problems

    manifestOf :: MonadIO m => SKI -> m (Either Text (Maybe ManifestInstance))
    manifestOf ski = do
        let aki = toAKI ski
        DB.getMftShorcutMeta tx aki >>= \case
            -- No accepted manifest
            Nothing   -> pure $ Right Nothing
            Just meta -> do
                let mftKey = meta.key
                manifest    <- DB.getCcrManifest tx mftKey
                crlValidity <- DB.getObjectValidity tx meta.crlShortcut.key
                case manifest of
                    Nothing ->
                        pure $ Left [i|Manifest #{mftKey} of #{ski} is not in the cache.|]
                    Just (hash, size, validity, MftMeta { mftNumber, thisTime }, eeSia)
                        | not (isCurrent validity && isCurrent crlValidity) ->
                            pure $ Right Nothing
                        | otherwise ->
                            case (size, eeSia) of
                                (Just size_, Just locations) -> do
                                    children <- DB.getCaChildren tx aki
                                    let subordinates = Set.fromList
                                            [ childSki
                                            | (childSki, childValidity, ValidCaChild) <- children
                                            , isCurrent childValidity ]
                                    pure $ Right $ Just ManifestInstance {
                                            hash           = hash,
                                            size           = size_,
                                            aki            = aki,
                                            manifestNumber = mftNumber,
                                            thisUpdate     = thisTime,
                                            locations      = BSS.toShort locations,
                                            subordinates   = subordinates
                                        }
                                _ ->
                                    pure $ Left [i|Manifest #{mftKey} of #{ski} has no size or SIA stored.|]

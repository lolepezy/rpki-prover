{-# LANGUAGE StrictData #-}

{- | Whether what a fetch stored is a reason to revalidate the TAs that use
     the repository, and when.

     Validation goes by manifests. A new one is compared with the manifest
     shortcut of its CA, i.e. with the manifest that the last validation
     accepted: if it lists the same objects, the validated tree doesn't change
     and there's nothing to do. Otherwise, the TA is revalidated once the
     manifest is current, i.e. at its thisUpdate.

     A manifest and the objects it lists don't always come in the same fetch.
     A manifest that lists objects not stored yet can't be used, validation
     falls back to an older one, so it is no reason to revalidate. The fetch
     that stores the rest of the objects is: any new object other than a
     manifest or a CRL is a reason to revalidate.
-}
module RPKI.Workflow.Revalidation where

import           GHC.Generics

import           RPKI.Config
import           RPKI.Domain
import           RPKI.Store.Database               (MftShortcutMeta)
import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Validation.TopDown.Shortcuts (shortcutIsUsable)


-- | How a new manifest compares with what the last validation accepted.
data ManifestChange
    -- | There are no shortcuts, every manifest counts
    = FullValidation
    -- | A new CA, or one without an accepted manifest
    | NoShortcut
    -- | The shortcut is past its validity, or its CRL is: the CA is
    -- validated without it and the new manifest may be what fixes it
    | ShortcutUnusable
    -- | The manifest of the shortcut is gone from the cache, there's
    -- nothing to compare with
    | ShortcutManifestGone
    | ContentChanged
    | SameContent
    deriving stock (Show, Eq, Ord, Generic)

-- | What is known about the shortcut of an AKI: the shortcut, and the content
-- digest of its manifest, `Nothing` when that manifest is gone from the cache.
data ShortcutView = ShortcutView {
        shortcut :: MftShortcutMeta,
        digest   :: Maybe Hash
    }
    deriving stock (Show, Eq, Generic)

data RevalidationDecision = RevalidateNow
                          | RevalidateAt Instant
                          | NoRevalidation
    deriving stock (Show, Eq, Ord, Generic)


-- | A new manifest against the shortcut of its AKI: how it compares, and
-- when to revalidate because of it, if at all. It is not before the
-- manifest is current, i.e. its thisUpdate.
manifestRevalidation :: ValidationAlgorithm
                     -> Instant
                     -> NewManifest
                     -> Maybe ShortcutView
                     -> (ManifestChange, Maybe Instant)
manifestRevalidation algorithm now newManifest shortcutView =
    (change, case change of
                SameContent -> Nothing
                _           -> Just $ max now newManifest.thisTime)
  where
    change =
        case algorithm of
            FullEveryIteration -> FullValidation
            Incremental ->
                case shortcutView of
                    Nothing -> NoShortcut
                    Just ShortcutView {..}
                        | not (shortcutIsUsable (Now now) shortcut) -> ShortcutUnusable
                        | otherwise ->
                            case digest of
                                Nothing -> ShortcutManifestGone
                                Just d
                                    | d == newManifest.contentDigest -> SameContent
                                    | otherwise                      -> ContentChanged


-- | One decision for one fetch: the earliest moment over its manifests.
-- In one-off mode every fetch counts, otherwise the run could not finish.
-- A fetch with too many manifests to remember revalidates without comparing
-- anything, so many manifests almost always change something. So does one
-- that stored a new object other than a manifest or a CRL.
fetchRevalidation :: ProverRunMode
                  -> Instant
                  -> FetchUpdates
                  -> [(ManifestChange, Maybe Instant)]
                  -> RevalidationDecision
fetchRevalidation runMode now updates changes =
    case (runMode, updates) of
        (OneOffMode _, _)                              -> RevalidateNow
        (_, FetchUpdates { newObjects = True })        -> RevalidateNow
        (_, FetchUpdates { manifests = ManyManifests }) -> RevalidateNow
        _ ->
            case [ t | (_, Just t) <- changes ] of
                [] -> NoRevalidation
                ts | earliest <= now -> RevalidateNow
                   | otherwise       -> RevalidateAt earliest
                  where
                    earliest = minimum ts

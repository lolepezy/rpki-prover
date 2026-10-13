{-# LANGUAGE StrictData #-}

{- | Whether what a fetch stored is a reason to revalidate the TAs that use
     the repository, and when.

     Validation goes by manifests. A new one is compared with the manifest
     shortcut of its CA, i.e. with the manifest that the last validation
     accepted: if it lists the same objects, the validated tree doesn't change
     and there's nothing to do. Otherwise, the TA is revalidated. A manifest
     that isn't current yet is no exception: validation falls back from it,
     and revalidates once it is (see `becomingValid`).

     A manifest of a CA that no certificate in the cache has the key of is no 
     reason either: validation can't get to it. Neither is a manifest that the 
     CA didn't sign: anyone can publish one with the AKI of any CA, and 
     validation doesn't use it.

     A manifest and the objects it lists don't always come in the same fetch.
     A manifest that lists objects not stored yet can't be used, validation
     falls back to an older one, so it is no reason to revalidate. The fetch
     that stores the rest of the objects is: any new object other than a
     manifest or a CRL is a reason to revalidate.
-}
module RPKI.Workflow.Revalidation where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           GHC.Generics

import qualified Data.Map.Strict                   as Map
import           Data.Set                          (Set)
import qualified Data.Set                          as Set

import           RPKI.Config
import           RPKI.Domain
import           RPKI.Store.Database               (MftShortcutMeta, Tx)
import qualified RPKI.Store.Database               as DB
import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Validation.TopDown           (signedByCa)
import           RPKI.Validation.TopDown.Shortcuts (shortcutIsUsable)


-- | How a new manifest compares with what the last validation accepted.
data ManifestChange
    -- | No certificate in the cache has the key of the CA, so validation can't 
    -- get to the manifest. The certificate is a new object when it comes, and 
    -- that is a reason to revalidate. Until then, there's nothing to do: some 
    -- publishers keep re-issuing manifests of CAs that have no certificate.
    = UnknownCa
    -- | There are no shortcuts, every manifest counts
    | FullValidation
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

-- | What the cache knows about the CA of a new manifest.
data CaView = NoCertificate
            | WithoutShortcut
            | WithShortcut ShortcutView
    deriving stock (Show, Eq, Generic)

data RevalidationDecision = RevalidateNow
                          | NoRevalidation
    deriving stock (Show, Eq, Ord, Generic)

-- | New manifests that validation won't use, by key.
data UnusableManifests = UnusableManifests {
        -- | Their CA didn't sign them
        unsigned :: Set ObjectKey,
        -- | They list objects that the cache doesn't have yet
        waiting  :: Set ObjectKey
    }
    deriving stock (Show, Eq, Generic)

instance Semigroup UnusableManifests where
    UnusableManifests u1 w1 <> UnusableManifests u2 w2 = UnusableManifests (u1 <> u2) (w1 <> w2)

instance Monoid UnusableManifests where
    mempty = UnusableManifests mempty mempty


-- | A new manifest against what the cache knows about its CA.
manifestRevalidation :: ValidationAlgorithm
                     -> Instant
                     -> NewManifest
                     -> CaView
                     -> ManifestChange
manifestRevalidation algorithm now newManifest ca =
    case (algorithm, ca) of
        (_, NoCertificate)                   -> UnknownCa
        (FullEveryIteration, _)              -> FullValidation
        (Incremental, WithoutShortcut)       -> NoShortcut
        (Incremental, WithShortcut ShortcutView {..})
            | not (shortcutIsUsable (Now now) shortcut) -> ShortcutUnusable
            | otherwise ->
                case digest of
                    Nothing -> ShortcutManifestGone
                    Just d
                        | d == newManifest.contentDigest -> SameContent
                        | otherwise                      -> ContentChanged

-- | Whether validation can come out different because of the new manifest.
isReason :: ManifestChange -> Bool
isReason = \case
    SameContent -> False
    UnknownCa   -> False
    _           -> True


-- | One decision for one fetch, over the changes of its manifests.
-- In one-off mode every fetch counts, otherwise the run could not finish.
-- A fetch with too many manifests to remember revalidates without comparing
-- anything, so many manifests almost always change something. So does one
-- that stored a new object other than a manifest or a CRL.
fetchRevalidation :: ProverRunMode
                  -> FetchUpdates
                  -> [ManifestChange]
                  -> RevalidationDecision
fetchRevalidation runMode updates changes =
    case (runMode, updates) of
        (OneOffMode _, _)                              -> RevalidateNow
        (_, FetchUpdates { newObjects = True })        -> RevalidateNow
        (_, FetchUpdates { manifests = ManyManifests }) -> RevalidateNow
        _ | any isReason changes                       -> RevalidateNow
          | otherwise                                  -> NoRevalidation


-- | Of the new manifests that would be a reason to revalidate, the ones that
-- validation won't use. A manifest waiting for its objects waits for the fetch
-- that stores them, that fetch is the one to revalidate for.
--
-- Validation only tries the manifests that their CA signed (`signedByCa`).
-- When there's no certificate of the CA to check that with, the manifest is
-- taken as signed, so that a reason to revalidate is never lost. For the same
-- reason it's enough that any of the certificates with the SKI of the CA signed
-- it. They all have the same key anyway (`validateSKIMatchesPublicKey`), unless 
-- some way into the cache skips that check, and then the first one found could
-- make the manifest of the CA look unsigned.
unusableManifests :: MonadIO m => Tx mode -> [NewManifest] -> m UnusableManifests
unusableManifests tx manifests = do
    caCerts <- fmap Map.fromList $
        forM (Set.toList $ Set.fromList [ m.aki | m <- manifests ]) $ \aki ->
            (aki, ) <$> DB.getBySKIUpTo tx (SKI $ unAKI aki) maxCertificatesPerCa
    fmap mconcat $ forM manifests $ \m ->
        DB.getMftByKey tx m.key >>= \case
            Nothing  -> pure mempty
            Just mft -> do
                let payload = mft ^. #object . #payload
                case Map.findWithDefault [] m.aki caCerts of
                    cas@(_ : _) | not (any (`signedByCa` payload) cas) ->
                        pure $ mempty { unsigned = Set.singleton m.key }
                    _ -> do
                        stored <- DB.manifestObjectsStored tx payload
                        pure $ if stored then mempty else mempty { waiting = Set.singleton m.key }

-- | How many of the certificates with the SKI of a CA a manifest is checked
-- against, the most recent ones. Real CAs have one or two. Anyone can publish 
-- any number of them, with the key of the CA, so a few tell as much as all.
maxCertificatesPerCa :: Int
maxCertificatesPerCa = 16

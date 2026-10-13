{-# LANGUAGE StrictData #-}

{- | Whether what a fetch stored is a reason to revalidate the TAs that use
     the repository.

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
import           Control.Monad.IO.Class
import           GHC.Generics

import           RPKI.Config
import           RPKI.Domain
import           RPKI.Store.Database               (MftShortcutMeta, Tx)
import qualified RPKI.Store.Database               as DB
import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Validation.TopDown           (signedByCa)
import           RPKI.Validation.TopDown.Shortcuts (shortcutIsUsable)


-- | Whether a new manifest is a reason to revalidate, and if it isn't, why.
data ManifestVerdict
    -- | Validation can come out different because of it
    = Revalidate
    -- | It lists the same objects as the manifest the last validation accepted
    | SameAsShortcut
    -- | No certificate in the cache has the key of the CA, so validation 
    -- can't get to the manifest. The certificate is a new object when it 
    -- comes, and that is a reason to revalidate. Until then, there's nothing 
    -- to do: some publishers keep re-issuing manifests of CAs that have no 
    -- certificate.
    | UnknownCa
    -- | Validation only tries the manifests that their CA signed
    | NotSignedByCa
    -- | It lists objects that the cache doesn't have yet. It waits for the
    -- fetch that stores them, that fetch is the one to revalidate for.
    | WaitingForObjects
    deriving stock (Show, Eq, Ord, Generic)

data RevalidationDecision = RevalidateNow
                          | NoRevalidation
    deriving stock (Show, Eq, Ord, Generic)


-- | A new manifest against what the cache knows about its CA, the cheapest 
-- checks first.
--
-- It's enough that any of the certificates with the SKI of the CA signed the
-- manifest. They all have the same key anyway (`validateSKIMatchesPublicKey`),
-- unless some way into the cache skips that check, and then the first one 
-- found could make the manifest of the CA look unsigned.
manifestVerdict :: MonadIO m
                => Tx mode -> ValidationAlgorithm -> Now -> NewManifest -> m ManifestVerdict
manifestVerdict tx algorithm now newManifest = do
    same <- case algorithm of
        FullEveryIteration -> pure False
        Incremental        ->
            DB.getMftShorcutMeta tx newManifest.aki >>= \case
                Nothing       -> pure False
                Just shortcut -> sameAsShortcut now newManifest shortcut . fmap (.contentDigest)
                                    <$> DB.getMftMetaByKey tx shortcut.key
    if same
        then pure SameAsShortcut
        else do
            cas <- DB.getBySKIUpTo tx (SKI $ unAKI newManifest.aki) maxCertificatesPerCa
            if null cas
                then pure UnknownCa
                else DB.getMftByKey tx newManifest.key >>= \case
                    -- Gone already, validation will find out what that means
                    Nothing  -> pure Revalidate
                    Just mft -> do
                        let payload = mft ^. #object . #payload
                        if not (any (`signedByCa` payload) cas)
                            then pure NotSignedByCa
                            else do
                                stored <- DB.manifestObjectsStored tx payload
                                pure $ if stored then Revalidate else WaitingForObjects

-- | Whether the manifest lists what the manifest of the shortcut lists, given 
-- the content digest of the latter, `Nothing` when it's gone from the cache. 
-- A shortcut past its validity, or with its CRL past it, doesn't count: the 
-- CA is validated without it and the new manifest may be what fixes it.
sameAsShortcut :: Now -> NewManifest -> MftShortcutMeta -> Maybe Hash -> Bool
sameAsShortcut now newManifest shortcut digest =
    shortcutIsUsable now shortcut && digest == Just newManifest.contentDigest

-- | How many of the certificates with the SKI of a CA a manifest is checked
-- against, the most recent ones. Real CAs have one or two. Anyone can publish 
-- any number of them, with the key of the CA, so a few tell as much as all.
maxCertificatesPerCa :: Int
maxCertificatesPerCa = 16


-- | One decision for one fetch, over the verdicts on its manifests.
-- In one-off mode every fetch counts, otherwise the run could not finish.
-- A fetch with too many manifests to remember revalidates without comparing
-- anything, so many manifests almost always change something. So does one
-- that stored a new object other than a manifest or a CRL.
fetchRevalidation :: ProverRunMode
                  -> FetchUpdates
                  -> [ManifestVerdict]
                  -> RevalidationDecision
fetchRevalidation runMode updates verdicts =
    case (runMode, updates) of
        (OneOffMode _, _)                              -> RevalidateNow
        (_, FetchUpdates { newObjects = True })        -> RevalidateNow
        (_, FetchUpdates { manifests = ManyManifests }) -> RevalidateNow
        _ | Revalidate `elem` verdicts                 -> RevalidateNow
          | otherwise                                  -> NoRevalidation

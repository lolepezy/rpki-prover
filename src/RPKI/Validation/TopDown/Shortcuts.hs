{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StrictData           #-}

-- | Manifest shortcuts: making them from validated objects, deciding how to
-- validate a manifest with them, and writing them to the database.
module RPKI.Validation.TopDown.Shortcuts where

import           Effectful
import           Control.Concurrent.STM
import           Control.Monad

import           Data.Foldable
import qualified Data.Map.Strict                  as Map
import qualified Data.List                        as List
import           Data.Text                        (Text)
import           Data.Tuple.Strict
import qualified Data.ByteString                  as BS

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Parallel
import           RPKI.Repository

import           RPKI.Store.Base.Storable
import           RPKI.Store.Database    (rwTxT)
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Validation.Types
import           RPKI.Validation.ObjectValidation


-- Making shortcuts -----------------------------------------------------------

makeCaShortcut :: ObjectKey -> Validated WellStructuredCaCert -> PublicationPointAccess -> MftChild
makeCaShortcut key (Validated certificate) ppas = let 
        ValidityPeriod {..} = getValidityPeriod certificate            
        ski = getSKI certificate
        serial = getSerial certificate
        resources = getResources certificate
    in CaChild (CaShortcut {..}) serial

makeRoaShortcut :: ObjectKey -> Validated WellStructuredRoa -> VrpsPerAs -> MftChild
makeRoaShortcut key (Validated roa) roaPayload = let
        ValidityPeriod {..} = getValidityPeriod roa    
        serial = getSerial roa
        resources = getResources roa
    in RoaChild (RoaShortcut {..}) serial

makeSplShortcut :: ObjectKey -> Validated WellStructuredSpl -> SplPayload -> MftChild
makeSplShortcut key (Validated spl) splPayload = let 
        ValidityPeriod {..} = getValidityPeriod spl
        serial = getSerial spl
        resources = getResources spl
    in SplChild (SplShortcut {..}) serial

makeAspaShortcut :: ObjectKey -> Validated WellStructuredAspa -> Aspa -> MftChild
makeAspaShortcut key (Validated aspaObject) aspa = let 
        ValidityPeriod {..} = getValidityPeriod aspaObject            
        serial = getSerial aspaObject
        resources = getResources aspaObject
    in AspaChild (AspaShortcut {..}) serial

makeGbrShortcut :: ObjectKey -> Validated WellStructuredGbr -> T2 Hash Gbr -> MftChild
makeGbrShortcut key (Validated gbrObject) gbr = let 
        ValidityPeriod {..} = getValidityPeriod gbrObject    
        serial = getSerial gbrObject
        resources = getResources gbrObject
    in GbrChild (GbrShortcut {..}) serial

makeBgpSecShortcut :: ObjectKey -> Validated WellStructuredBgpCert -> BGPSecPayload -> MftChild
makeBgpSecShortcut key (Validated bgpCert) bgpSec = let         
        ValidityPeriod {..} = getValidityPeriod bgpCert                  
        serial = getSerial bgpCert
        resources = getResources bgpCert
    in BgpSecChild (BgpSecShortcut {..}) serial

makeMftShortcut :: ObjectKey 
                -> Validated WellStructuredMft -> [(ObjectKey, MftEntry)] 
                -> Keyed (Validated CrlObject) 
                -> MftShortcut   
makeMftShortcut key 
    (Validated mftObject) (Map.fromList -> nonCrlEntries) 
    (Keyed (Validated validCrl) crlKey) = 
  let
    ValidityPeriod {..} = manifestValidityPeriod mftObject
    serial = getSerial mftObject
    manifestNumber = mftObject.content.mftNumber
    crlShortcut = let 
        SignCRL {..} = validCrl.signCrl
        in CrlShortcut {
            key = crlKey,
            notBefore = thisUpdateTime,
            notAfter = nextUpdateTime
        }            
    in MftShortcut { .. }

-- | The period in which a manifest can be used: its EE certificate has to be
-- valid and the manifest itself has to be current, i.e. between thisUpdate and
-- nextUpdate (https://www.rfc-editor.org/rfc/rfc9286.html#section-6.3).
-- `getValidityPeriod` of a manifest is only the EE certificate's.
manifestValidityPeriod :: WellStructuredMft -> ValidityPeriod
manifestValidityPeriod mft =
    let ValidityPeriod eeNotBefore eeNotAfter = getValidityPeriod mft
        Manifest { thisTime, nextTime } = mft.content
    in ValidityPeriod (max eeNotBefore thisTime) (min eeNotAfter nextTime)

makeChildWithIssues :: ObjectKey -> Text -> MftEntry
makeChildWithIssues childKey fileName = 
    MftEntry { child = TroubledChild childKey, .. }     


-- Using shortcuts ------------------------------------------------------------

-- | How to validate the manifest of a CA.
data MftPlan
    = NoManifest
    -- | There's no shortcut that can be used: validate the manifests in full,
    -- one after another until one of them is valid.
    | InFull [MftMeta]
    -- | The manifest of the shortcut is still the latest one, so the shortcut
    -- has everything.
    | UseShortcut DB.MftShortcutMeta
    -- | There's no manifest to use but the shortcut is still valid. It is the
    -- cached data of the last successful fetch, which is to be used until it
    -- becomes stale (RFC 9286, 6.6).
    | OnlyShortcut DB.MftShortcutMeta
    -- | There's a newer manifest than the one of the shortcut: validate only
    -- what changed, and fall back to the shortcut if the manifest is not valid.
    | DiffWithShortcut DB.MftShortcutMeta MftMeta
    deriving stock (Show, Eq)

-- | Given the manifests of a CA, newest first, and its manifest shortcut
-- (`Nothing` when shortcuts are not used), decide how to validate it.
--
-- Also returns the manifests from the future that are passed over for
-- older data, since they are failed fetches to report (RFC 9286, 6.3).
planManifests :: Now -> [MftMeta] -> Maybe DB.MftShortcutMeta -> (MftPlan, [MftMeta])
planManifests now mfts shortcut =
    case shortcut of
        Just meta | not (shortcutExpired meta) ->
            let plan = case current of
                    -- A shortcut is only made for a manifest that is not in the
                    -- future, so the manifest of this one is gone from the cache
                    []                        -> OnlyShortcut meta
                    m : _ | m.key == meta.key -> UseShortcut meta
                          | otherwise         -> DiffWithShortcut meta m
            in (plan, premature)
        _   | null mfts    -> (NoManifest, [])
            -- If there are only manifests from the future, validate
            -- them anyway to have a meaningful error message
            | null current -> (InFull premature, [])
            | otherwise    -> (InFull current, premature)
  where
    (current, premature) = List.partition (\m -> m.thisTime <= unNow now) mfts

    -- Shortcuts stored before `manifestValidityPeriod` only carry the validity
    -- of the manifest's EE certificate. A manifest that's past its nextUpdate
    -- stays unchanged, and so does its shortcut, so the manifest's own
    -- nextUpdate is checked here.
    shortcutExpired meta =
        not (isWithinValidityPeriod now meta) ||
        not (isWithinValidityPeriod now meta.crlShortcut) ||
        maybe False (< unNow now) (shortcutMftNextUpdate mfts meta)

-- | nextUpdate of the manifest of the shortcut, if it's still in the cache.
shortcutMftNextUpdate :: [MftMeta] -> DB.MftShortcutMeta -> Maybe Instant
shortcutMftNextUpdate mfts meta =
    (.nextTime) <$> List.find ((== meta.key) . (.key)) mfts

-- Either a full manifest entry (file_name known, from the diff-path's full read)
-- or just the child's shortcut payload (from the hot, file_name-free light read).
data ChildData = ChildWithEntry MftEntry | ChildLight MftChild

childOf :: ChildData -> MftChild
childOf (ChildWithEntry MftEntry {..}) = child
childOf (ChildLight c)                 = c

-- Calculate difference bentween a manifest shortcut
-- and the list of children of the new manifest object.
manifestDiff :: MftShortcut
            -> [T3 Text a ObjectKey]
            -> ([T3 Text a ObjectKey], [T3 Text a ObjectKey], [ObjectKey])
manifestDiff mftShortcut newMftChidlren =
    (List.reverse newOnes, List.reverse overlapping, Map.keys deletedEntries)
  where
    (newOnes, overlapping, deletedEntries) =
        foldl' go ([], [], mftShortcut.nonCrlEntries) newMftChidlren

    -- If we delete everything from mftShortcut.nonCrlEntries that is present in
    -- newMftChidlren, we only have the entries that are not present on the new manifest,
    -- i.e. the deleted ones.
    go (!newOnes_, !overlapping_, !remaining) t3@(T3 fileName _ key_) =
        case Map.lookup key_ mftShortcut.nonCrlEntries of
            -- it's not in the map of shortcut children -- new entry
            Nothing -> (t3 : newOnes_, overlapping_, remaining)
            Just e
                | e.fileName == fileName ->
                    (newOnes_, t3 : overlapping_, Map.delete key_ remaining)
                -- it has changed its name (very unlikely but can happen in theory)
                -- -- new entry, and the old one under the same key stays "deleted"
                | otherwise ->
                    (t3 : newOnes_, overlapping_, remaining)

revokedShortcutChildren :: MftShortcut 
                        -> Validated CrlObject
                        -> [T3 Text Hash ObjectKey]
                        -> [(ObjectKey, MftEntry)]
revokedShortcutChildren mftShortcut validCrl children = 
    [ (childKey, makeChildWithIssues childKey fileName)
    | T3 fileName _ childKey <- children
    , Just MftEntry { child } <- [ Map.lookup childKey mftShortcut.nonCrlEntries ]
    , Just childSerial        <- [ getMftChildSerial child ]
    , isRevoked childSerial validCrl ]


-- Writing shortcuts ----------------------------------------------------------

updateMftShortcut :: MonadIO m => ClosableQueue MftShortcutOp -> AKI -> MftShortcut -> m ()
updateMftShortcut shortcutQueue aki MftShortcut {..} = 
    liftIO $ do 
        let !raw = Verbatim $ toStorable $ Compressed $ DB.MftShortcutMeta {..}
        atomically $ writeCQueue shortcutQueue $ UpdateMftShortcut aki raw

-- Replace the whole shortcut, i.e. the meta and all the children, with this one.
replaceMftShortcut :: MonadIO m => ClosableQueue MftShortcutOp -> AKI -> MftShortcut -> m ()
replaceMftShortcut shortcutQueue aki MftShortcut {..} =
    liftIO $ do
        let !raw = Verbatim $ toStorable $ Compressed $ DB.MftShortcutMeta {..}
        let !children = shortcutChildRows $ Map.toList nonCrlEntries
        atomically $ writeCQueue shortcutQueue $ ReplaceMftShortcut aki raw children

-- Only the new children get inserted (into `shortcuts` and `mft_shortcut_children`)
-- and only the deleted/revoked keys get removed.
updateMftShortcutChildren :: MonadIO m => ClosableQueue MftShortcutOp -> AKI -> [(ObjectKey, MftEntry)] -> [ObjectKey] -> m ()
updateMftShortcutChildren shortcutQueue aki newEntries deletedKeys =
    liftIO $ do
        let !inserts = shortcutChildRows newEntries
        unless (null inserts && null deletedKeys) $
            atomically $ writeCQueue shortcutQueue $ UpdateMftShortcutChildren aki inserts deletedKeys

-- Pre-serialise each child's data so the heavy lifting happens on this
-- (validation) thread, not on the DB-writer thread.
shortcutChildRows :: [(ObjectKey, MftEntry)] -> [(ObjectKey, Text, BS.ByteString)]
shortcutChildRows entries =
    [ (k, fileName, unStorable $ toStorable $ Compressed child)
    | (k, MftEntry {..}) <- entries ]

deleteMftShortcut :: MonadIO m => ClosableQueue MftShortcutOp -> AKI -> m ()
deleteMftShortcut shortcutQueue aki = 
    liftIO $ atomically $ writeCQueue shortcutQueue $ DeleteMftShortcut aki

storeShortcuts :: (MonadIO m) => 
                AppContext s 
             -> ClosableQueue MftShortcutOp -> m ()
storeShortcuts AppContext {..} shortcutQueue = liftIO $
    readQueueChunked shortcutQueue 1000 $ \shotcutOps ->
        rwTxT database $ \tx ->
            for_ shotcutOps $ \case
                UpdateMftShortcut aki s ->
                    DB.saveMftShorcutMeta tx aki s
                UpdateMftShortcutChildren aki inserts deletedKeys -> do
                    unless (null inserts)     $ DB.insertMftShortcutChildren tx aki inserts
                    unless (null deletedKeys) $ DB.deleteMftShortcutChildren tx aki deletedKeys
                ReplaceMftShortcut aki s children -> do
                    DB.deleteMftShortcut tx aki
                    DB.saveMftShorcutMeta tx aki s
                    DB.insertMftShortcutChildren tx aki children
                DeleteMftShortcut aki ->
                    DB.deleteMftShortcut tx aki

data MftShortcutOp = UpdateMftShortcut AKI (Verbatim (Compressed DB.MftShortcutMeta))
                   | UpdateMftShortcutChildren AKI [(ObjectKey, Text, BS.ByteString)] [ObjectKey]
                   | ReplaceMftShortcut AKI (Verbatim (Compressed DB.MftShortcutMeta)) [(ObjectKey, Text, BS.ByteString)]
                   | DeleteMftShortcut AKI

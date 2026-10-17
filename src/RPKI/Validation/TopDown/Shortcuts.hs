{-# LANGUAGE StrictData #-}

-- | Manifest shortcuts: making them from validated objects, deciding how to
-- validate a manifest with them, and writing them to the database.
module RPKI.Validation.TopDown.Shortcuts where

import           Effectful
import           Control.Concurrent.STM
import           Control.Monad

import qualified Crypto.Hash.SHA256               as S256

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Short            as BSS
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Builder.Extra    as BB (toLazyByteStringWith, untrimmedStrategy, defaultChunkSize)
import           Data.Foldable
import qualified Data.Map.Strict                  as Map
import qualified Data.List                        as List
import           Data.List.NonEmpty               (NonEmpty (..))
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as Text
import           Data.Tuple.Strict
import qualified Data.Vector                      as V
import           Data.Word                        (Word8, Word32, Word64)

import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Parallel
import           RPKI.Repository
import           RPKI.Resources.Resources         (toPrefixesAndAsns)
import           RPKI.Resources.Types

import           RPKI.Store.Base.Storable
import           RPKI.Store.Database    (rwTxT)
import qualified RPKI.Store.Database    as DB
import           RPKI.Store.Types
import           RPKI.Time
import           RPKI.Util                        (mkHash)
import           RPKI.Validation.Types
import           RPKI.Validation.ObjectValidation


-- Making shortcuts -----------------------------------------------------------

makeCaShortcut :: ObjectKey -> Validated WellStructuredCaCert -> PublicationPointAccess -> MftChild
makeCaShortcut key (Validated certificate) ppas = let 
        ValidityPeriod {..} = getValidityPeriod certificate            
        ski = getSKI certificate
        serial = getSerial certificate
        resources = getResources certificate
        mftUri = certificate.certUris.manifestUri
        ownResourcesHash = certResourcesHash resources
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
                -> Bool
                -> Hash
                -> MftShortcut   
makeMftShortcut key 
    (Validated mftObject) (Map.fromList -> nonCrlEntries) 
    (Keyed (Validated validCrl) crlKey) hasIssues validatedUnder = 
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

-- | What the children of a CA are validated under, apart from its key: its 
-- verified resources and the location of its manifest, by its SIA. Several 
-- certificates can have the same key and different ones of these, e.g. a 
-- certificate re-issued with fewer resources, or two parents issuing one 
-- each. A manifest shortcut is used only under what its children were 
-- validated under, otherwise they are validated again.
validationContextHash :: Hash -> Maybe URI -> Hash
validationContextHash resourcesHash mftUri = 
    hashSized $ shortBytes (unHash resourcesHash) <> mftUriBytes
  where
    mftUriBytes = case mftUri of
        Nothing      -> word8 0
        Just (URI u) -> word8 1 <> bytes (Text.encodeUtf8 u)

-- | The hash of the verified resources of a CA, given the resources of its 
-- certificate and their hash (`certResourcesHash`), and the hash of the 
-- verified resources of its parent.
--
-- It's done for every CA in every validation, so it's not the verified 
-- resources themselves that are hashed: they are computed when they are 
-- needed, and that costs more than taking all of the CA from its shortcut.
-- It's what gives them: the resources of the certificate and, if it inherits 
-- some, the hash of the parent's. That's the same for the same verified 
-- resources, whatever the path to the CA. Only the resources the CA overclaims 
-- (with the reconsidered algorithm) are not verified, then they are computed.
verifiedResourcesHash :: AllResources 
                      -> Hash 
                      -> Hash 
                      -> (VerifiedRS PrefixesAndAsns, Maybe (Overclaiming PrefixesAndAsns))
                      -> Hash
verifiedResourcesHash (AllResources ipv4 ipv6 asns) ownHash parentHash = \case
    (_, Nothing) -> 
        hashSized $ word8 1 <> shortBytes (unHash ownHash) <> inherited
    (VerifiedRS (PrefixesAndAsns v4 v6 va), Just _) -> 
        hashSized $ word8 2 <> elements ipv4Bytes v4 <> elements ipv6Bytes v6 <> elements asnBytes va
  where
    inherited = case (ipv4, ipv6, asns) of
        (RS _, RS _, RS _) -> mempty
        _                  -> shortBytes (unHash parentHash)

-- | The hash of the resources on a certificate, a CA shortcut keeps it
certResourcesHash :: AllResources -> Hash
certResourcesHash (AllResources ipv4 ipv6 asns) = 
    hashSized $ resourceSet ipv4Bytes ipv4 <> resourceSet ipv6Bytes ipv6 <> resourceSet asnBytes asns

-- | The hash of the verified resources of a TA certificate, which has 
-- no parent and can't inherit
taResourcesHash :: AllResources -> Hash
taResourcesHash resources = 
    verifiedResourcesHash resources (certResourcesHash resources) noResourcesHash 
        (VerifiedRS $ toPrefixesAndAsns resources, Nothing)

-- | When there are no verified resources
noResourcesHash :: Hash
noResourcesHash = hashSized $ word8 0

resourceSet :: (a -> Sized) -> RSet (IntervalSet a) -> Sized
resourceSet element = \case
    Inherit -> word8 0
    RS s    -> word8 1 <> elements element s

-- | With the number of them, so that where one set ends is known
elements :: (a -> Sized) -> IntervalSet a -> Sized
elements element (IntervalSet v) = 
    word64 (fromIntegral $ V.length v) <> V.foldl' (\acc e -> acc <> element e) mempty v

ipv4Bytes :: Ipv4Prefix -> Sized
ipv4Bytes p = let (w, m) = ipv4PrefixWords p in word32 w <> word8 m

ipv6Bytes :: Ipv6Prefix -> Sized
ipv6Bytes p = let (hi, lo, m) = ipv6PrefixWords p in word64 hi <> word64 lo <> word8 m

asnBytes :: AsResource -> Sized
asnBytes = \case 
    AS (ASN a)              -> word8 0 <> word32 a
    ASRange (ASN a) (ASN b) -> word8 1 <> word32 a <> word32 b

-- | Bytes to hash and how many of them there are, so that they are written 
-- into one buffer of that size: the default ones are a lot bigger
data Sized = Sized BB.Builder Int

instance Semigroup Sized where
    Sized a n <> Sized b m = Sized (a <> b) (n + m)

instance Monoid Sized where
    mempty = Sized mempty 0

hashSized :: Sized -> Hash
hashSized (Sized builder size) = 
    mkHash $ S256.hashlazy $ 
        BB.toLazyByteStringWith (BB.untrimmedStrategy size BB.defaultChunkSize) mempty builder

word8 :: Word8 -> Sized
word8 w = Sized (BB.word8 w) 1

word32 :: Word32 -> Sized
word32 w = Sized (BB.word32BE w) 4

word64 :: Word64 -> Sized
word64 w = Sized (BB.word64BE w) 8

bytes :: BS.ByteString -> Sized
bytes bs = Sized (BB.byteString bs) (BS.length bs)

shortBytes :: BSS.ShortByteString -> Sized
shortBytes bs = Sized (BB.shortByteString bs) (BSS.length bs)

makeChildWithIssues :: ObjectKey -> Text -> Maybe CaChildValidity -> MftEntry
makeChildWithIssues childKey fileName caCert =
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
    -- | There are manifests newer than the one of the shortcut, newest first:
    -- validate only what changed, with the first of them that is valid, and
    -- fall back to the shortcut if none of them is.
    | DiffWithShortcut DB.MftShortcutMeta (NonEmpty MftMeta)
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
                    [] -> OnlyShortcut meta
                    -- The ones before the manifest of the shortcut are newer than
                    -- it. When it is gone from the cache, all of them are tried.
                    _  -> case List.takeWhile ((/= meta.key) . (.key)) current of
                            []     -> UseShortcut meta
                            m : ms -> DiffWithShortcut meta (m :| ms)
            in (plan, premature)
        _   | null mfts    -> (NoManifest, [])
            -- If there are only manifests from the future, validate
            -- them anyway to have a meaningful error message
            | null current -> (InFull premature, [])
            | otherwise    -> (InFull current, premature)
  where
    (current, premature) = List.partition (\m -> m.thisTime <= unNow now) mfts

    shortcutExpired meta =
        not (shortcutIsUsable now meta) ||
        maybe False (< unNow now) (shortcutMftNextUpdate mfts meta)

-- | The shortcut and its CRL are valid at this moment.
shortcutIsUsable :: Now -> DB.MftShortcutMeta -> Bool
shortcutIsUsable now meta =
    isWithinValidityPeriod now meta && isWithinValidityPeriod now meta.crlShortcut

-- | nextUpdate of the manifest of the shortcut, if it's still in the cache.
shortcutMftNextUpdate :: [MftMeta] -> DB.MftShortcutMeta -> Maybe Instant
shortcutMftNextUpdate mfts meta =
    (.nextTime) <$> List.find ((== meta.key) . (.key)) mfts

-- Either a full manifest entry (file_name known, from the diff-path's full read)
-- or just the child's shortcut payload (from the hot, file_name-free light read).
data ChildData = ChildWithEntry MftEntry | ChildLight MftChild (Maybe CaChildValidity)

childOf :: ChildData -> MftChild
childOf (ChildWithEntry MftEntry {..}) = child
childOf (ChildLight c _)               = c

caCertOf :: ChildData -> Maybe CaChildValidity
caCertOf (ChildWithEntry MftEntry {..}) = caCert
caCertOf (ChildLight _ caCert)          = caCert

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

-- | What to write for a manifest that's accepted.
data ShortcutWrites
    -- | There was no shortcut to validate the manifest against, so all the
    -- children are new. The whole shortcut is replaced: an expired one can
    -- still have children that are not on this manifest.
    = ReplaceShortcut
    | UpdateShortcut {
        -- | The shortcut is for another manifest now, the manifest's 
        -- issues came or went, or the children were validated under 
        -- something else
        writeMeta       :: Bool,
        -- | Children that are new on the manifest or validated to something else
        changedChildren :: [(ObjectKey, MftEntry)],
        -- | Children that are not on the manifest anymore
        deletedChildren :: [ObjectKey]
    }
    deriving stock (Show, Eq)

-- | Given the shortcut a manifest was validated against, if any, and the one
-- made from the manifest with the entries of the children validated in full,
-- what is to be written.
shortcutWrites :: Maybe MftShortcut -> MftShortcut -> [ObjectKey] -> ShortcutWrites
shortcutWrites current next deletedKeys =
    case current of
        Nothing -> ReplaceShortcut
        Just shortcut -> UpdateShortcut {
                writeMeta       = shortcut.key /= next.key || 
                                  shortcut.hasIssues /= next.hasIssues ||
                                  shortcut.validatedUnder /= next.validatedUnder,
                -- Children validated again come out the same as they 
                -- are on the shortcut most of the time
                changedChildren = [ e | e@(k, entry) <- Map.toList next.nonCrlEntries,
                                        Map.lookup k shortcut.nonCrlEntries /= Just entry ],
                deletedChildren = deletedKeys
            }

revokedShortcutChildren :: MftShortcut 
                        -> Validated CrlObject
                        -> [T3 Text Hash ObjectKey]
                        -> [(ObjectKey, MftEntry)]
revokedShortcutChildren mftShortcut validCrl children = 
    [ (childKey, makeChildWithIssues childKey fileName (InvalidCaChild <$ caCert))
    | T3 fileName _ childKey          <- children
    , Just MftEntry { child, caCert } <- [ Map.lookup childKey mftShortcut.nonCrlEntries ]
    , Just childSerial                <- [ getMftChildSerial child ]
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

-- Only the new children get inserted (into `shortcuts` and one of the children
-- tables) and only the deleted/revoked keys get removed.
updateMftShortcutChildren :: MonadIO m => ClosableQueue MftShortcutOp -> AKI -> [(ObjectKey, MftEntry)] -> [ObjectKey] -> m ()
updateMftShortcutChildren shortcutQueue aki newEntries deletedKeys =
    liftIO $ do
        let !inserts = shortcutChildRows newEntries
        unless (null inserts && null deletedKeys) $
            atomically $ writeCQueue shortcutQueue $ UpdateMftShortcutChildren aki inserts deletedKeys

-- Pre-serialise each child's data so the heavy lifting happens on this
-- (validation) thread, not on the DB-writer thread.
shortcutChildRows :: [(ObjectKey, MftEntry)] -> [DB.ShortcutChildRow]
shortcutChildRows entries =
    [ DB.ShortcutChildRow {
            childKey = k,
            fileName = fileName,
            caCert   = caCert,
            shortcut = unStorable $ toStorable $ Compressed child
        }
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
                   | UpdateMftShortcutChildren AKI [DB.ShortcutChildRow] [ObjectKey]
                   | ReplaceMftShortcut AKI (Verbatim (Compressed DB.MftShortcutMeta)) [DB.ShortcutChildRow]
                   | DeleteMftShortcut AKI

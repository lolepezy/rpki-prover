{-# LANGUAGE OverloadedStrings #-}

module RPKI.CCR.WalkSpec where

import           Control.Concurrent.STM           (readTVarIO)
import           Control.Lens                     ((^.))
import           Control.Monad                    (void)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Short            as BSS
import           Data.Int                         (Int64)
import qualified Data.Set                         as Set
import           Data.Word                        (Word8)

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppState                    (instantToVersion)
import           RPKI.AppTypes                    (WorldVersion)
import           RPKI.CCR
import           RPKI.CCR.Walk
import           RPKI.Domain
import           RPKI.Store.Base.Storable
import           RPKI.Store.Database              (DB, Tx(..), rwTx, roTx)
import qualified RPKI.Store.Database              as DB
import qualified RPKI.Store.SQLite                as SQLite
import           RPKI.Store.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (sha256s)
import           RPKI.Validation.Types


walkGroup :: TestTree
walkGroup = testGroup "CCR walk of manifest shortcuts"
    [ HU.testCase "Follows valid and current CA certificates only" shouldWalkValidTree
    , HU.testCase "Stops at the maximum certificate path depth" shouldStopAtMaxDepth
    , HU.testCase "Finds nothing under an expired TA certificate" shouldNotWalkUnderExpiredTa
    ]


{- The tree:

  TA (1) -- valid, current manifest
    CA 2 -- valid, current manifest
      CA 5 -- valid, but its manifest is expired
      TA 1 -- a certificate with the TA's key, i.e. a cycle
    CA 3 -- invalid (e.g. revoked), has a current manifest
    CA 4 -- valid when validated, expired now, has a current manifest
    CA 6 -- valid, current manifest without a stored SIA
-}
shouldWalkValidTree :: HU.Assertion
shouldWalkValidTree = withTree $ \db taKey -> do
    (state, problems) <- roTx db $ \tx -> walkShortcuts tx now 32 taKey
    HU.assertEqual "The TA" (Just $ skiOf 1) state.trustAnchor
    HU.assertEqual "Manifests and their subordinates"
        (Set.fromList [ (akiOf 1, mftHash 1, Set.fromList [skiOf 2, skiOf 6])
                      , (akiOf 2, mftHash 2, Set.fromList [skiOf 5, skiOf 1]) ])
        (Set.fromList [ (mi.aki, mi.hash, mi.subordinates) | mi <- state.manifests ])
    HU.assertEqual "One problem, the missing SIA" 1 (length problems)


shouldStopAtMaxDepth :: HU.Assertion
shouldStopAtMaxDepth = withTree $ \db taKey -> do
    (state, _) <- roTx db $ \tx -> walkShortcuts tx now 0 taKey
    HU.assertEqual "Only the TA's manifest"
        [akiOf 1] [ mi.aki | mi <- state.manifests ]


shouldNotWalkUnderExpiredTa :: HU.Assertion
shouldNotWalkUnderExpiredTa = withTree $ \db taKey -> do
    let later = Now $ seconds 5000
    (state, problems) <- roTx db $ \tx -> walkShortcuts tx later 32 taKey
    HU.assertEqual "Nothing" mempty state
    HU.assertEqual "No problems" [] problems


withTree :: (DB -> ObjectKey -> IO a) -> IO a
withTree f = withTestContext $ \appContext -> do
    db <- readTVarIO $ appContext ^. #database
    let wv = instantToVersion $ unNow now

    taKey <- caCertificate db wv 1 1 current
    ca2   <- caCertificate db wv 2 2 current
    ca3   <- caCertificate db wv 3 3 current
    ca4   <- caCertificate db wv 4 4 expired
    ca5   <- caCertificate db wv 5 5 current
    ca6   <- caCertificate db wv 6 6 current
    cycle_ <- caCertificate db wv 7 1 current

    shortcut db wv 1 current (Just sia)
        [(ca2, ValidCaChild), (ca3, InvalidCaChild), (ca4, ValidCaChild), (ca6, ValidCaChild)]
    shortcut db wv 2 current (Just sia) [(ca5, ValidCaChild), (cycle_, ValidCaChild)]
    shortcut db wv 3 current (Just sia) []
    shortcut db wv 4 current (Just sia) []
    shortcut db wv 5 expired (Just sia) []
    shortcut db wv 6 current Nothing    []
    f db taKey
  where
    sia = signedObjectLocations ["rsync://example.net/repo/manifest.mft"]


now :: Now
now = Now $ seconds 1000

current, expired :: (Instant, Instant)
current = (seconds 0, seconds 2000)
expired = (seconds 0, seconds 500)

seconds :: Int64 -> Instant
seconds s = Instant $ s * 1_000_000_000

skiOf :: Word8 -> SKI
skiOf n = SKI $ KI $ BSS.toShort $ BS.replicate 20 n

akiOf :: Word8 -> AKI
akiOf = toAKI . skiOf

mftHash :: Word8 -> Hash
mftHash n = sha256s $ blobOf (100 + n)

blobOf :: Word8 -> BS.ByteString
blobOf n = BS.replicate 3 n

-- | An object with this blob, type and validity window
storeObject :: DB -> WorldVersion -> Word8 -> RpkiObjectType -> (Instant, Instant) -> IO ObjectKey
storeObject db wv n type_ (notBefore, notAfter) = do
    let blob = blobOf n
    key <- rwTx db $ \tx ->
        DB.saveObject tx (OriginalRO (ObjectOriginal blob) mempty (sha256s blob) type_) Nothing wv
    rwTx db $ \(Tx conn) ->
        SQLite.execute conn
            "UPDATE objects SET size = ?, not_before = ?, not_after = ? WHERE object_key = ?"
            (1000 + fromIntegral n :: Int64, toNanoseconds notBefore, toNanoseconds notAfter, key)
    pure key

-- | A CA certificate stored as object `n`, with the key `skiN`
caCertificate :: DB -> WorldVersion -> Word8 -> Word8 -> (Instant, Instant) -> IO ObjectKey
caCertificate db wv n skiN window = do
    key <- storeObject db wv n CER window
    rwTx db $ \(Tx conn) ->
        SQLite.execute conn "INSERT INTO certificates(object_key, ski, aki) VALUES (?, ?, NULL)" (key, skiOf skiN)
    pure key

-- | The shortcut of the CA with the key `skiN`: its manifest, CRL and CA children
shortcut :: DB -> WorldVersion -> Word8 -> (Instant, Instant) -> Maybe BS.ByteString
         -> [(ObjectKey, CaChildValidity)] -> IO ()
shortcut db wv skiN window@(notBefore, notAfter) sia children = do
    mftKey <- storeObject db wv (100 + skiN) MFT window
    crlKey <- storeObject db wv (200 + skiN) CRL current
    let meta = MftMeta { key = mftKey, mftNumber = Serial (fromIntegral skiN), thisTime = notBefore, nextTime = notAfter }
    rwTx db $ \tx@(Tx conn) -> do
        SQLite.execute conn
            "INSERT INTO manifest_meta(object_key, aki, manifest_number, meta, ee_sia) VALUES (?, ?, ?, ?, ?)"
            (mftKey, akiOf skiN, serialiseField (Serial 1), serialiseField meta, sia)
        DB.saveMftShorcutMeta tx (akiOf skiN) $ Verbatim $ toStorable $ Compressed DB.MftShortcutMeta {
                key            = mftKey,
                notBefore      = notBefore,
                notAfter       = notAfter,
                serial         = Serial 1,
                manifestNumber = Serial (fromIntegral skiN),
                crlShortcut    = CrlShortcut crlKey (fst current) (snd current),
                hasIssues      = False
            }
        void $ DB.insertMftShortcutChildren tx (akiOf skiN)
            [ DB.ShortcutChildRow childKey "child.cer" (Just validity)
                    (unStorable $ toStorable $ Compressed $ TroubledChild childKey)
            | (childKey, validity) <- children ]

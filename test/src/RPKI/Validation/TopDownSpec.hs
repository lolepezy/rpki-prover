{-# LANGUAGE OverloadedStrings #-}

module RPKI.Validation.TopDownSpec where

import           Effectful
import           Control.Concurrent.Async         (async, wait)
import           Control.Concurrent.STM
import           Control.Monad                    (forM, unless)
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Short            as BSS
import           Data.Int                         (Int64)
import qualified Data.Map.Strict                  as Map
import qualified Data.Set                         as Set
import qualified Data.Text                        as Text
import           Data.Tuple.Strict
import qualified Data.X509                        as X509

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppState                    (instantToVersion)
import           RPKI.AppMonad
import           RPKI.AppTypes                    (WorldVersion)
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting                   (newScopes)
import           RPKI.Resources.Types
import           RPKI.Store.Database              (DB)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (parseRpkiURL)
import           RPKI.Validation.ObjectValidation (prevalidateObject)
import           RPKI.Validation.Types
import           RPKI.Validation.TopDown
                ( TroubledChildLoadPath (..)
                , resolveTroubledChildByKey
                , revokedShortcutChildren
                )


topDownRegressionGroup :: TestTree
topDownRegressionGroup =
    testGroup "TopDown regressions"
        [ HU.testCase "Resolves troubled child key from well-structured object" shouldResolveTroubledFromWellStructured
        , HU.testCase "Resolves troubled child key from original object" shouldResolveTroubledFromOriginal
        , HU.testCase "Replaces revoked shortcut children with troubled entries" shouldReplaceRevokedShortcutChildren        
        ]


shouldResolveTroubledFromWellStructured :: HU.Assertion
shouldResolveTroubledFromWellStructured =
    withTestContext $ \appContext -> do
        db <- readTVarIO $ appContext ^. #database
        worldVersion <- instantToVersion . unNow <$> thisInstant

        (Right (url, _, parsedObject), _) <- runValidatorIO (newScopes "fixture-ws") $ readFixtureObject fixturePath
        (Right expectedObject, _) <- runValidatorIO (newScopes "prevalidate-ws") $ prevalidateObject parsedObject

        key <- storeLifecycle db worldVersion (WellStructuredRO expectedObject) url

        (resolved, _) <- runValidatorIO (newScopes "resolve-ws") $
            DB.roAppTx db $ \tx -> resolveTroubledChildByKey tx db key

        case resolved of
            Right (Just (TroubledFromParsed, Keyed (Located _ actualObject) actualKey)) -> do
                HU.assertEqual "Resolved key mismatch" key actualKey
                HU.assertEqual "Resolved object mismatch" expectedObject actualObject
            other ->
                HU.assertFailure $ "Expected TroubledFromParsed resolution, got: " <> show other


shouldResolveTroubledFromOriginal :: HU.Assertion
shouldResolveTroubledFromOriginal =
    withTestContext $ \appContext -> do
        db <- readTVarIO $ appContext ^. #database
        worldVersion <- instantToVersion . unNow <$> thisInstant

        (Right (url, blob, parsedObject), _) <- runValidatorIO (newScopes "fixture-orig") $ readFixtureObject fixturePath
        (Right expectedObject, _) <- runValidatorIO (newScopes "prevalidate-orig") $ prevalidateObject parsedObject

        let lifecycle =
                OriginalRO
                    (ObjectOriginal blob)
                    mempty
                    (getHash parsedObject)
                    (getRpkiObjectType parsedObject)

        key <- storeLifecycle db worldVersion lifecycle url

        (resolved, _) <- runValidatorIO (newScopes "resolve-orig") $
            DB.roAppTx db $ \tx -> resolveTroubledChildByKey tx db key

        case resolved of
            Right (Just (TroubledFromOriginal, Keyed (Located _ actualObject) actualKey)) -> do
                HU.assertEqual "Resolved key mismatch" key actualKey
                HU.assertEqual "Resolved object mismatch" expectedObject actualObject
            other ->
                HU.assertFailure $ "Expected TroubledFromOriginal resolution, got: " <> show other


storeLifecycle :: DB -> WorldVersion -> RpkiObjectLifecycle -> RpkiURL -> IO ObjectKey
storeLifecycle db worldVersion lifecycle url =
    DB.rwTx db $ \tx -> do
        key <- DB.saveObject tx db lifecycle worldVersion
        DB.linkObjectToUrl tx db url key worldVersion
        pure key


readFixtureObject :: ValidatorIO es => FilePath -> Eff es (RpkiURL, BS.ByteString, ParsedRpkiObject)
readFixtureObject path = do
    blob <- liftIO $ BS.readFile path
    -- Drop the "./" prefix of the fixture path: `parseRpkiURL` (rightly) rejects 
    -- dot-segments, since rsync URLs are mapped onto local filesystem paths.
    let urlPath = Text.dropWhile (== '/') $ Text.replace "./" "" $ Text.pack path
    let url =
            case parseRpkiURL $ "rsync://host/" <> urlPath of
                Right parsedUrl -> parsedUrl
                Left err -> error $ "Failed to parse fixture URL: " <> Text.unpack err
    object <- readObject url blob
    pure (url, blob, object)


fixturePath :: FilePath
fixturePath = "./test/data/afrinic_mft1.mft"


-- | A child that is covered by the manifest shortcut (i.e. is not going to be
-- re-validated in this round) and whose serial appears on the new CRL must be
-- reported back, so that its shortcut entry is replaced with a troubled one and it
-- stops contributing payloads. See `revokedShortcutChildren` for why replacing the
-- entry (rather than just skipping the child for this round) is what is needed.
shouldReplaceRevokedShortcutChildren :: HU.Assertion
shouldReplaceRevokedShortcutChildren = do
    let revokedRoaKey = objectKey 1
        liveRoaKey    = objectKey 2
        revokedGbrKey = objectKey 3
        troubledKey   = objectKey 4
        notOnShortcut = objectKey 5

    let mftShortcut = testMftShortcut
            [ (revokedRoaKey, MftEntry "revoked.roa"  (RoaChild (testRoaShortcut revokedRoaKey) (Serial 100)))
            , (liveRoaKey,    MftEntry "live.roa"     (RoaChild (testRoaShortcut liveRoaKey)    (Serial 200)))
            , (revokedGbrKey, MftEntry "revoked.gbr"  (GbrChild (testGbrShortcut revokedGbrKey) (Serial 300)))
            -- A troubled child carries no serial, it is re-validated in full anyway
            , (troubledKey,   MftEntry "troubled.roa" (TroubledChild troubledKey))
            ]

    let mftChildren =
            [ T3 "revoked.roa"  (testHash "h1") revokedRoaKey
            , T3 "live.roa"     (testHash "h2") liveRoaKey
            , T3 "revoked.gbr"  (testHash "h3") revokedGbrKey
            , T3 "troubled.roa" (testHash "h4") troubledKey
            -- Not on the shortcut at all, so not this function's business
            , T3 "new.roa"      (testHash "h5") notOnShortcut
            ]

    -- The new CRL revokes the ROA and the GBR, plus a serial that belongs to nothing here
    HU.assertEqual "Wrong set of revoked children"
        [ (revokedRoaKey, MftEntry "revoked.roa" (TroubledChild revokedRoaKey))
        , (revokedGbrKey, MftEntry "revoked.gbr" (TroubledChild revokedGbrKey))
        ]
        (revokedShortcutChildren mftShortcut (testCrl [Serial 100, Serial 300, Serial 999]) mftChildren)

    HU.assertEqual "Nothing may be revoked by a CRL that lists none of these serials"
        []
        (revokedShortcutChildren mftShortcut (testCrl [Serial 999]) mftChildren)


objectKey :: Int64 -> ObjectKey
objectKey = ObjectKey . asKey

testHash :: BS.ByteString -> Hash
testHash = Hash . BSS.toShort

testMftShortcut :: [(ObjectKey, MftEntry)] -> MftShortcut
testMftShortcut entries = MftShortcut {
        key            = objectKey 100,
        nonCrlEntries  = Map.fromList entries,
        notBefore      = Instant 0,
        notAfter       = Instant 1,
        serial         = Serial 1,
        manifestNumber = Serial 1,
        crlShortcut    = CrlShortcut (objectKey 101) (Instant 0) (Instant 1)
    }

testRoaShortcut :: ObjectKey -> RoaShortcut
testRoaShortcut key = RoaShortcut {
        key        = key,
        roaPayload = VrpsPerAs (ASN 64496) [] [],
        notBefore  = Instant 0,
        notAfter   = Instant 1,
        resources  = AllResources Inherit Inherit Inherit
    }

testGbrShortcut :: ObjectKey -> GbrShortcut
testGbrShortcut key = GbrShortcut {
        key       = key,
        gbr       = T2 (testHash "gbr") (Gbr $ BSS.toShort "gbr"),
        notBefore = Instant 0,
        notAfter  = Instant 1,
        resources = AllResources Inherit Inherit Inherit
    }

-- Only `revokedSerials` matters for the revocation check, the rest is filler.
testCrl :: [Serial] -> Validated CrlObject
testCrl serials = Validated CrlObject {
        hash    = testHash "crl",
        aki     = AKI (mkKI "0123456789012345678"),
        signCrl = SignCRL {
            thisUpdateTime     = Instant 0,
            nextUpdateTime     = Instant 1,
            signatureAlgorithm = SignatureAlgorithmIdentifier
                                    (X509.SignatureALG X509.HashSHA256 X509.PubKeyALG_RSA),
            signatureValue     = SignatureValue (BSS.toShort "signature"),
            encodedValue       = BSS.toShort "encoded",
            crlNumber          = Serial 1,
            revokedSerials     = Set.fromList serials
        }
    }

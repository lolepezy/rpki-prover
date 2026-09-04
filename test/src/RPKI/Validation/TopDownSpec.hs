{-# LANGUAGE OverloadedStrings #-}

module RPKI.Validation.TopDownSpec where

import           Control.Concurrent.STM           (readTVarIO)
import           Control.Lens
import           Control.Monad.IO.Class           (liftIO)

import qualified Data.ByteString                  as BS
import qualified Data.Text                        as Text

import           Test.Tasty
import qualified Test.Tasty.HUnit                 as HU

import           RPKI.AppState                    (instantToVersion)
import           RPKI.AppMonad
import           RPKI.AppTypes                    (WorldVersion)
import           RPKI.Domain
import           RPKI.Parse.Parse
import           RPKI.Reporting                   (newScopes)
import           RPKI.Store.Database              (DB)
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Types
import           RPKI.TestCommons
import           RPKI.Time
import           RPKI.Util                        (parseRpkiURL)
import           RPKI.Validation.ObjectValidation (prevalidateObject)
import           RPKI.Validation.TopDown
                ( TroubledChildLoadPath (..)
                , resolveTroubledChildByKey
                )


topDownRegressionGroup :: TestTree
topDownRegressionGroup =
    testGroup "TopDown regressions"
        [ HU.testCase "Resolves troubled child key from well-structured object" shouldResolveTroubledFromWellStructured
        , HU.testCase "Resolves troubled child key from original object" shouldResolveTroubledFromOriginal
        ]


shouldResolveTroubledFromWellStructured :: HU.Assertion
shouldResolveTroubledFromWellStructured =
    withTestContext $ \appContext -> do
        db <- readTVarIO $ appContext ^. #database
        worldVersion <- instantToVersion . unNow <$> thisInstant

        (Right (url, _, parsedObject), _) <- runValidatorT (newScopes "fixture-ws") $ readFixtureObject fixturePath
        (Right expectedObject, _) <- runValidatorT (newScopes "prevalidate-ws") $ vHoist $ prevalidateObject parsedObject

        key <- storeLifecycle db worldVersion (WellStructuredRO expectedObject) url

        (resolved, _) <- runValidatorT (newScopes "resolve-ws") $
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

        (Right (url, blob, parsedObject), _) <- runValidatorT (newScopes "fixture-orig") $ readFixtureObject fixturePath
        (Right expectedObject, _) <- runValidatorT (newScopes "prevalidate-orig") $ vHoist $ prevalidateObject parsedObject

        let lifecycle =
                OriginalRO
                    (ObjectOriginal blob)
                    mempty
                    (getHash parsedObject)
                    (getRpkiObjectType parsedObject)

        key <- storeLifecycle db worldVersion lifecycle url

        (resolved, _) <- runValidatorT (newScopes "resolve-orig") $
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
        DB.linkObjectToUrl tx db url key
        pure key


readFixtureObject :: FilePath -> ValidatorT IO (RpkiURL, BS.ByteString, ParsedRpkiObject)
readFixtureObject path = do
    blob <- liftIO $ BS.readFile path
    let url =
            case parseRpkiURL $ "rsync://host/" <> Text.pack path of
                Right parsedUrl -> parsedUrl
                Left err -> error $ "Failed to parse fixture URL: " <> Text.unpack err
    object <- vHoist $ readObject url blob
    pure (url, blob, object)


fixturePath :: FilePath
fixturePath = "./test/data/afrinic_mft1.mft"

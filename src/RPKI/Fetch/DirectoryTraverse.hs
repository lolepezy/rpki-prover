{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Fetch.DirectoryTraverse where

import           Data.Generics.Product.Typed

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS
import           Data.Bifunctor
import           Data.Foldable                    (for_)
import qualified Data.Map.Strict                  as Map
import           Data.String.Interpolate.IsString

import           GHC.Generics

import qualified Streaming.Prelude                as S
import           System.FilePath
import           System.Directory                 (doesDirectoryExist, getDirectoryContents)
import           System.IO

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Store.Types
import           RPKI.Store.Base.Storable (StorableObject(..), Compressed(..), toStorableObject)
import           RPKI.Store.Database      (DB, roTx)
import qualified RPKI.Store.Database      as DB
import qualified RPKI.Util                as U
import           RPKI.Validation.ObjectValidation


-- | Recursively traverse a directory and save every parseable object into the
-- storage.
--
-- Both rsync and Erik end up with a tree of RPKI objects on local disk and need
-- exactly the same work done on it; the only thing they differ in is how a file
-- maps back to the URL the object is published at. `restoreUrl` gets the path
-- first and -- for callers that cannot tell from the path alone, i.e. Erik,
-- which names files by hash -- a second chance once the object has been parsed
-- and a URL can be read out of it.
--
-- Is not supposed to throw exceptions.
loadObjectsFromFS :: AppContext s
                  -> WorldVersion
                  -> (FilePath -> Maybe ParsedRpkiObject -> Maybe RsyncURL)
                  -> FilePath
                  -> ValidatorT IO ()
loadObjectsFromFS AppContext{..} worldVersion restoreUrl rootPath = do
    db <- liftIO $ readTVarIO database
    doLoad db
  where
    doLoad db =
        txFoldPipeline
            (2 * cpuParallelism)
            traverseFS
            (DB.rwAppTx db)
            saveStorable
      where
        cpuParallelism = config ^. typed @Parallelism . #cpuParallelism

        traverseFS =
            mapException (AppException . RsyncE . FileReadError . U.fmtEx) <$>
                traverseDirectory rootPath

        traverseDirectory currentPath = do
            names <- liftIO $ getDirectoryContents currentPath
            let properNames = filter (`notElem` [".", ".."]) names
            forM_ properNames $ \name -> do
                let path = currentPath </> name
                liftIO (doesDirectoryExist path) >>= \case
                    True  -> traverseDirectory path
                    False ->
                        when (supportedExtension name) $ do
                            let !uri = restoreUrl path Nothing
                            s <- askScopes
                            let task = runValidatorT s (readAndParseObject path (RsyncU <$> uri))
                            a <- liftIO $ async $ evaluate =<< task
                            S.yield (a, uri)
          where
            readAndParseObject filePath rpkiURL =
                liftIO (getSizeAndContent (config ^. typed) filePath) >>= \case
                    Left e          -> pure $! CantReadFile rpkiURL filePath $ VErr e
                    Right (_, blob) ->
                        -- The file name is the only thing both callers can rely on:
                        -- Erik names files by hash, so there is no URL to take the
                        -- extension from.
                        case nameObjectType (takeFileName filePath) of
                            Just type_ -> do
                                -- Check if the object is already in the storage
                                -- before parsing ASN1 and serialising it.
                                let hash = U.sha256s blob
                                liftIO (roTx db $ \tx -> DB.getObjectKey tx db hash) >>= \case
                                    Just key -> pure $! HashExists rpkiURL hash key
                                    Nothing  -> tryToParse hash blob type_
                            Nothing ->
                                pure $! UknownObjectType rpkiURL filePath

              where
                tryToParse hash blob type_ =
                    doParse scopes `catchSync` onError scopes
                  where
                    scopes =
                        case rpkiURL of
                            Just u  -> newScopes' LocationFocus $ getURL u
                            Nothing -> newScopes' HashFocus hash

                    inObjectScope =
                        case rpkiURL of
                            Just u  -> inSubLocationScope (getURL u)
                            Nothing -> vFocusOn HashFocus hash

                    doParse scopes_ = do
                        z <- liftIO $ runValidatorT scopes_ $ do
                                parsed <- vHoist $ readObjectOfType type_ blob
                                vro    <- inObjectScope $ vHoist $ prevalidateObject parsed
                                pure (parsed, vro)
                        evaluate $!
                            case z of
                                (Left _, vs) ->
                                    mkSaveObject Nothing $ OriginalRO (ObjectOriginal blob) vs hash type_
                                (Right (parsed, vro), vs)
                                    | hasValidationErrors vs ->
                                        mkSaveObject (Just parsed) $ OriginalRO (ObjectOriginal blob) vs hash type_
                                    | otherwise ->
                                        mkSaveObject (Just parsed) $ WellStructuredRO vro

                    onError scopes_ e = do
                        (_, vs) <- runValidatorT scopes_ $
                            vHoist $ fromEither @() $ Left $ RsyncE $ RsyncFailedToParseObject $ U.fmtEx e
                        pure $! mkSaveObject Nothing $ OriginalRO (ObjectOriginal blob) vs hash type_

                    -- Encode/compress the object here, on the parsing (async) thread,
                    -- so the single-threaded DB-writer only has to do the INSERT.
                    -- The URL is recovered here too, for the same reason: this is the
                    -- last point at which the parsed object is still in hand.
                    mkSaveObject parsed lifecycle =
                        SaveObject
                            (maybe (RsyncU <$> restoreUrl filePath parsed) Just rpkiURL)
                            (toStorableObject (Compressed lifecycle))

        saveStorable tx (a, _) = do
            (r, vs) <- fromTry (UnspecifiedE "Something bad happened in loadObjectsFromFS" . U.fmtEx) $ wait a
            embedState vs
            case r of
                Left e  -> appWarn e
                Right z -> case z of
                    HashExists rpkiURL _ key ->
                        for_ rpkiURL $ \u -> DB.linkObjectToUrl tx db u key worldVersion

                    CantReadFile rpkiUrl filePath (VErr e) -> do
                        logError logger [i|Cannot read file #{filePath}, error #{e} |]
                        atObject rpkiUrl filePath $ appWarn e

                    UknownObjectType rpkiUrl filePath -> do
                        logError logger [i|Unknown object type: url = #{rpkiUrl}, path = #{filePath}.|]
                        atObject rpkiUrl filePath $
                            appWarn $ RsyncE $ RsyncUnsupportedObjectType $ U.convert filePath

                    SaveObject rpkiUrl so@StorableObject { object = Compressed lifecycle } -> do
                        case lifecycle of
                            OriginalRO _ vs1 _ _ -> do
                                logError logger [i|Object #{rpkiUrl} failed parse/prevalidation.|]
                                embedState vs1
                            WellStructuredRO _ -> pure ()

                        key <- DB.saveStorableObject tx db so worldVersion
                        for_ rpkiUrl $ \u -> DB.linkObjectToUrl tx db u key worldVersion
                        updateMetric @TraverseMetric @_ (#processed %~
                            Map.unionWith (+) (Map.singleton (Just $ getRpkiObjectType lifecycle) 1))
                    other ->
                        logDebug logger [i|Weird thing happened in `saveStorable` #{other}.|]
          where
            atObject rpkiUrl filePath f =
                case rpkiUrl of
                    Just u  -> inSubLocationScope (getURL u) f
                    Nothing -> vFocusOn TextFocus (U.convert filePath) f


getSizeAndContent :: ValidationConfig -> FilePath -> IO (Either AppError (Integer, BS.ByteString))
getSizeAndContent vc path = do
    r <- first (RsyncE . FileReadError . U.fmtEx) <$> readSizeAndContet
    pure $ r >>= \case
                (_, Left e)  -> Left e
                (s, Right b) -> Right (s, b)
  where
    readSizeAndContet = try $
        withFile path ReadMode $ \h -> do
            size <- hFileSize h
            case validateSize vc size of
                Left e  -> pure (size, Left $ ValidationE e)
                Right _ -> do
                    r <- BS.hGetContents h
                    pure (size, Right r)

data ObjectProcessingResult =
          CantReadFile (Maybe RpkiURL) FilePath VIssue
        | HashExists (Maybe RpkiURL) Hash ObjectKey
        | UknownObjectType (Maybe RpkiURL) FilePath
        | SaveObject (Maybe RpkiURL) (StorableObject (Compressed RpkiObjectLifecycle))
    deriving stock (Show, Eq, Generic)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Fetch.DirectoryTraverse where

import           Effectful
import           Data.Generics.Product.Typed

import           Control.Concurrent.STM

import           Effectful.Concurrent.Async
import qualified Control.Exception               as IOExc
import           Effectful.Exception
import           Control.Lens
import           Control.Monad

import qualified Data.ByteString                  as BS
import           Data.Bifunctor
import           Data.Foldable                    (for_)
import qualified Data.Map.Strict                  as Map
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           Streaming                        (lift)
import qualified Streaming.Prelude                as S
import           System.FilePath
import           System.Directory                 (doesDirectoryExist, getDirectoryContents)
import           System.IO

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Cpu                         (useAvailableCpus)
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
-- storage. `restoreUrl` maps a file back to the URL the object is published at.
--
-- Is not supposed to throw exceptions.
loadObjectsFromFS :: (ValidatorIO es, Concurrent :> es) => AppContext s
                  -> WorldVersion
                  -> (FilePath -> Maybe RsyncURL)
                  -> FilePath
                  -> Eff es ()
loadObjectsFromFS AppContext{..} worldVersion restoreUrl rootPath = do
    db <- liftIO $ readTVarIO database
    -- As many capabilities as for an RRDP snapshot
    void $ liftIO $ useAvailableCpus $ config ^. typed @Parallelism . #cpuCount
    scopes <- askScopes
    txPoolPipeline
        CompletionOrder
        (objectFiles rootPath)
        (\(path, uri) -> trySync $ 
            evaluate =<< runValidator scopes (readAndParseObject db path (RsyncU <$> uri)))
        (DB.rwAppTx db)
        saveStorable
  where
    -- Files of supported types in the tree and the URLs their paths tell, 
    -- one directory at a time
    objectFiles currentPath = do
        names <- lift $ getDirectoryContents currentPath
        forM_ (filter (`notElem` [".", ".."]) names) $ \name -> do
            let path = currentPath </> name
            lift (doesDirectoryExist path) >>= \case
                True  -> objectFiles path
                False -> when (supportedExtension name) $ 
                            S.yield (path, restoreUrl path)

    readAndParseObject :: forall es' . ValidatorIO es'
                        => DB -> FilePath -> Maybe RpkiURL -> Eff es' ObjectProcessingResult
    readAndParseObject db filePath rpkiURL =
        liftIO (getSizeAndContent (config ^. typed) filePath) >>= \case
            Left e          -> pure $! CantReadFile rpkiURL filePath $ VErr e
            Right (_, blob) ->
                case nameObjectType (takeFileName filePath) of
                    Just type_ -> do
                        -- Check if the object is already in the storage
                        -- before parsing ASN1 and serialising it.
                        let hash = U.sha256s blob
                        liftIO (roTx db $ \tx -> DB.getObjectKey tx hash) >>= \case
                            Just key -> pure $! HashExists rpkiURL hash key
                            Nothing  -> do
                                (_, lifecycle) <- parseAndPrevalidate type_ hash blob rpkiURL
                                -- Encode/compress the object here, on a worker, so
                                -- the single-threaded DB-writer only has to do the INSERT.
                                pure $! SaveObject rpkiURL (toStorableObject (Compressed lifecycle))
                    Nothing ->
                        pure $! UknownObjectType rpkiURL filePath

    saveStorable tx processed = do
        (r, vs) <- either (appError . UnspecifiedE "Something bad happened in loadObjectsFromFS" . U.fmtEx) pure processed
        embedState vs
        case r of
            Left e  -> appWarn e
            Right z -> case z of
                HashExists rpkiURL _ key ->
                    for_ rpkiURL $ \u -> DB.linkObjectToUrl tx u key worldVersion

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

                    key <- DB.saveStorableObject tx so worldVersion
                    for_ rpkiUrl $ \u -> DB.linkObjectToUrl tx u key worldVersion
                    updateMetric @TraverseMetric @_ (#processed %~
                        Map.unionWith (+) (Map.singleton (Just $ getRpkiObjectType lifecycle) 1))
                other ->
                    logDebug logger [i|Weird thing happened in `saveStorable` #{other}.|]
      where
        atObject rpkiUrl filePath f =
            case rpkiUrl of
                Just u  -> inSubLocationScope (getURL u) f
                Nothing -> vFocusOn TextFocus (U.convert filePath) f


{- | Parse and prevalidate one object, and work out what gets stored for it.

     Bytes that do not make a well-structured object are still stored, as
     'OriginalRO' together with the issues that held them back. The parsed
     object is returned on its own as well, since it can be there even when
     prevalidation failed, and a caller may need it regardless: Erik reads a
     manifest's children from it.
-}
parseAndPrevalidate :: IOE :> es
                    => RpkiObjectType
                    -> Hash
                    -> BS.ByteString
                    -> Maybe RpkiURL
                    -> Eff es (Either AppError ParsedRpkiObject, RpkiObjectLifecycle)
parseAndPrevalidate type_ hash blob rpkiURL =
    doParse `catchSync` onError
  where
    scopes =
        case rpkiURL of
            Just u  -> newScopes' LocationFocus $ getURL u
            Nothing -> newScopes' HashFocus hash

    inObjectScope =
        case rpkiURL of
            Just u  -> inSubLocationScope (getURL u)
            Nothing -> vFocusOn HashFocus hash

    doParse =
        runValidator scopes (readObjectOfType type_ blob) >>= \case
            (Left e, vs) -> pure (Left e, original vs)
            (Right parsed, parseVs) -> do
                (vro, prevalidationVs) <- runValidator scopes $ inObjectScope $ prevalidateObject parsed
                let vs = parseVs <> prevalidationVs
                evaluate $!
                    case vro of
                        Right wellStructured
                            | not (hasValidationErrors vs) -> (Right parsed, WellStructuredRO wellStructured)
                        _                                  -> (Right parsed, original vs)

    onError e = do
        let err = RsyncE $ RsyncFailedToParseObject $ U.fmtEx e
        (_, vs) <- runValidator scopes $ fromEither @() $ Left err
        pure (Left err, original vs)

    original vs = OriginalRO (ObjectOriginal blob) vs hash type_


getSizeAndContent :: ValidationConfig -> FilePath -> IO (Either AppError (Integer, BS.ByteString))
getSizeAndContent vc path = do
    r <- first (RsyncE . FileReadError . U.fmtEx) <$> readSizeAndContet
    pure $ r >>= \case
                (_, Left e)  -> Left e
                (s, Right b) -> Right (s, b)
  where
    readSizeAndContet = IOExc.try $
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

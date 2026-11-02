{-# LANGUAGE StrictData #-}

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
import           Data.IORef
import qualified Data.Map.Strict                  as Map
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           Streaming                        (Of, Stream, lift)
import qualified Streaming.Prelude                as S
import           System.FilePath
import           System.Directory                 (getDirectoryContents)
import           System.IO
import           System.Posix.Files               (getFileStatus, isDirectory, statusChangeTimeHiRes)

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
import           RPKI.Store.Database      (DB, roTx)
import qualified RPKI.Store.Database      as DB
import           RPKI.Time
import qualified RPKI.Util                as U
import           RPKI.Validation.ObjectValidation


-- | What loading a tree ended with.
data LoadOutcome = LoadOutcome {
        -- | When the walk through the tree began
        startedAt    :: Instant,
        filesRead    :: Int,
        -- | Not read because they haven't changed since the mark
        filesSkipped :: Int,
        -- | Read or not, but not in the storage
        filesFailed  :: Int
    }
    deriving stock (Show, Eq, Generic)

-- | The mark for the next load of the same tree, if this load stored
-- every file it had to: every object file that hasn't changed since
-- the walk began is in the storage then.
loadMarkAfter :: LoadOutcome -> Maybe Instant
loadMarkAfter LoadOutcome {..}
    | filesFailed == 0 = Just startedAt
    | otherwise        = Nothing

-- | Recursively traverse a directory and save every parseable object into the
-- storage. `restoreUrl` maps a file back to the URL the object is published at.
--
-- With a mark (see `loadMarkAfter`) only the files that have changed since
-- then are read, every other one is in the storage already.
--
-- Is not supposed to throw exceptions.
loadObjectsFromFS :: (ValidatorIO es, Concurrent :> es) => AppContext
                  -> WorldVersion
                  -> Maybe Instant
                  -> (FilePath -> Maybe RsyncURL)
                  -> FilePath
                  -> Eff es LoadOutcome
loadObjectsFromFS AppContext{..} worldVersion loadMark restoreUrl rootPath = do
    db <- liftIO $ readTVarIO database
    -- As many capabilities as for loading many objects
    cpus <- liftIO $ useAvailableCpus $ config ^. typed @Parallelism . #cpuCount
    logDebug logger [i|Using #{cpus} CPUs for loading objects from #{rootPath}.|]   
    scopes <- askScopes
    Now startedAt <- thisInstant
    readCount    <- liftIO $ newIORef 0
    skippedCount <- liftIO $ newIORef 0
    failedCount  <- liftIO $ newIORef 0
    let since = (`momentAfter` negate (config ^. #rsyncConf . #timestampSlack)) <$> loadMark
    txPoolPipeline
        CompletionOrder
        (objectFiles since skippedCount restoreUrl rootPath
            & S.chain (\_ -> modifyIORef' readCount (+1)))
        (\(path, uri) -> trySync $ 
            evaluate =<< runValidator scopes (readAndParseObject db path (RsyncU <$> uri)))
        (DB.rwAppTx db)
        (saveStorable failedCount)
    filesRead    <- liftIO $ readIORef readCount
    filesSkipped <- liftIO $ readIORef skippedCount
    filesFailed  <- liftIO $ readIORef failedCount
    let skippedText :: String =
            case loadMark of
                Nothing   -> ""
                Just mark -> [i|, skipped #{filesSkipped} unchanged since #{mark}|]
    logInfo logger $ [i|Loaded #{rootPath}: read #{filesRead} files#{skippedText}, |] <>
                     [i|#{filesFailed} not stored.|]
    pure LoadOutcome {..}
  where
    readAndParseObject :: forall es' . ValidatorIO es'
                        => DB -> FilePath -> Maybe RpkiURL -> Eff es' ObjectProcessingResult
    readAndParseObject db filePath rpkiURL =
        liftIO (getSizeAndContent (config ^. typed) filePath) >>= \case
            Left e          -> pure $! CantReadFile rpkiURL filePath $ VErr e
            Right (_, blob) ->
                case nameObjectType (takeFileName filePath) of
                    Just type_ -> do
                        -- Check if the object is already in the storage
                        -- before parsing ASN1 and serialising it. The hash
                        -- goes with the object to the parser.
                        let hashedBlob = U.hashed blob
                        liftIO (roTx db $ \tx -> DB.getObjectKey tx hashedBlob.hash) >>= \case
                            Just key -> pure $! HashExists rpkiURL hashedBlob.hash key
                            Nothing  -> do
                                (_, lifecycle) <- parseAndPrevalidate type_ hashedBlob rpkiURL
                                -- Encode/compress the object here, on a worker, so
                                -- the single-threaded DB-writer only has to do the INSERT.
                                pure $! SaveObject rpkiURL lifecycle (DB.prepareLifecycle lifecycle)
                    Nothing ->
                        pure $! UknownObjectType rpkiURL filePath

    saveStorable failedCount tx processed = do
        (r, vs) <- either 
                    (\e -> appError $ InternalE $ InternalError [i|Failed to load objects from #{rootPath}: #{U.fmtEx e}|]) 
                    pure processed
        embedState vs
        let notStored = liftIO $ modifyIORef' failedCount (+1)
        case r of
            Left e  -> do
                notStored
                appWarn e
            Right z -> case z of
                HashExists rpkiURL _ key ->
                    for_ rpkiURL $ \u -> DB.linkObjectToUrl tx u key worldVersion

                CantReadFile rpkiUrl filePath (VErr e) -> do
                    notStored
                    logError logger [i|Cannot read file #{filePath}, error #{e} |]
                    atObject rpkiUrl filePath $ appWarn e

                UknownObjectType rpkiUrl filePath -> do
                    notStored
                    logError logger [i|Unknown object type: url = #{rpkiUrl}, path = #{filePath}.|]
                    atObject rpkiUrl filePath $
                        appWarn $ RsyncE $ RsyncUnsupportedObjectType $ U.convert filePath

                SaveObject rpkiUrl lifecycle prepared -> do
                    case lifecycle of
                        OriginalRO _ vs1 _ _ -> do
                            logError logger [i|Object #{rpkiUrl} failed parse/prevalidation.|]
                            embedState vs1
                        WellStructuredRO _ -> pure ()

                    key <- DB.insertPreparedObject tx prepared worldVersion
                    for_ rpkiUrl $ \u -> DB.linkObjectToUrl tx u key worldVersion
                    updateMetric @TraverseMetric @_ (#processed %~
                        Map.unionWith (+) (Map.singleton (Just $ getRpkiObjectType lifecycle) 1))
                other -> do
                    notStored
                    logDebug logger [i|Weird thing happened in `saveStorable` #{other}.|]
      where
        atObject rpkiUrl filePath f =
            case rpkiUrl of
                Just u  -> inSubLocationScope (getURL u) f
                Nothing -> vFocusOn TextFocus (U.convert filePath) f


-- | Object files in the tree and the URLs their paths tell, one directory at
-- a time. With `Just since` only the ones that have changed at or after
-- `since`, the others are counted in `skipped`.
--
-- That a file has changed is told by its ctime. rsync can't write a file
-- without setting it.
objectFiles :: Maybe Instant
            -> IORef Int
            -> (FilePath -> Maybe RsyncURL)
            -> FilePath
            -> Stream (Of (FilePath, Maybe RsyncURL)) IO ()
objectFiles since skipped restoreUrl = go
  where
    go currentPath = do
        names <- lift $ getDirectoryContents currentPath
        forM_ (filter (`notElem` [".", ".."]) names) $ \name -> do
            let path = currentPath </> name
            lift (IOExc.try @IOExc.IOException $ getFileStatus path) >>= \case
                Right status
                    | isDirectory status -> go path
                    | otherwise          -> objectFile name path (hasChanged status)
                -- Can't tell, reading it will say what's wrong
                Left _                   -> objectFile name path True

    objectFile name path changed =
        when (supportedExtension name) $
            if changed
                then S.yield (path, restoreUrl path)
                else lift $ modifyIORef' skipped (+1)

    hasChanged status =
        case since of
            Nothing     -> True
            Just moment -> statusChangeInstant status >= moment

    statusChangeInstant = Instant . floor . (* nanosPerSecond) . statusChangeTimeHiRes


{- | Parse and prevalidate one object, and work out what gets stored for it.

     Bytes that do not make a well-structured object are still stored, as
     'OriginalRO' together with the issues that held them back. The parsed
     object is returned on its own as well, since it can be there even when
     prevalidation failed, and a caller may need it regardless: Erik reads a
     manifest's children from it.
-}
parseAndPrevalidate :: IOE :> es
                    => RpkiObjectType
                    -> Hashed BS.ByteString
                    -> Maybe RpkiURL
                    -> Eff es (Either AppError ParsedRpkiObject, RpkiObjectLifecycle)
parseAndPrevalidate type_ hashedBlob@(Hashed blob hash) rpkiURL =
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
        runValidator scopes (readObjectOfType type_ hashedBlob) >>= \case
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
        | SaveObject (Maybe RpkiURL) RpkiObjectLifecycle PreparedObject
    deriving stock (Show, Eq, Generic)

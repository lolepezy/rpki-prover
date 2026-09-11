{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Rsync where
    
import           Effectful
import           Control.Lens
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Control.Concurrent.STM
import qualified Control.Exception               as IOExc

import           Effectful.Concurrent.Async
import           Effectful.Timeout                (Timeout)
import           Effectful.Exception
import           Control.Monad

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict                  as Map
import           Data.Proxy
import           Data.Foldable (for_)
import           Data.List (stripPrefix)
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NonEmpty

import           Data.Hourglass

import           GHC.Generics

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Metrics.System
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Types
import           RPKI.Store.Base.Storable (StorableObject(..), Compressed(..), toStorableObject)
import           RPKI.Store.Database     (DB, roTx)
import qualified RPKI.Store.Database    as DB
import           RPKI.Time
import qualified RPKI.Util                        as U
import           RPKI.Validation.ObjectValidation
import           RPKI.Worker

import           System.Directory                 (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)

import           System.Exit
import           System.IO
import           System.FilePath
import           System.Process.Typed

import           Streaming                        (lift)
import qualified Streaming.Prelude                as S


checkRsyncInPath :: ValidatorIO es => Maybe FilePath -> Eff es ()
checkRsyncInPath rsyncClientPath = do 
    let client = fromMaybe "rsync" rsyncClientPath    
    z <- liftIO $ IOExc.try $ readProcess $ proc client [ "--version" ]
    case z of
        Left (e :: SomeException) -> do 
            let message = maybe 
                    [i|rsync client is not in he $PATH, can't proceed: #{U.fmtEx e}|]
                    (\rc -> [i|rsync client #{rc} is not found, can't proceed: #{U.fmtEx e}|])
                    rsyncClientPath
            appError $ InitE $ InitError message
                    
        Right (exit, stdout', stderr') -> 
            case exit of 
                ExitSuccess -> pure ()
                ExitFailure _ -> do 
                    appError $ InitE $ InitError 
                        [i|#{client} --version returned non-zero exit code #{exit}, 
stdout = [#{U.textual stdout'}], 
stderr = [#{U.textual stderr'}]|]


runRsyncFetchWorker :: ValidatorIO es => AppContext s 
                    -> FetchConfig
                    -> WorldVersion
                    -> RsyncRepository             
                    -> Eff es RsyncRepository
runRsyncFetchWorker appContext@AppContext {..} fetchConfig worldVersion repository = do
        
    -- This is for humans to read in `top` or `ps`, actual parameters
    -- are passed as 'RsyncFetchResult'.
    let (URI u) = getURL repository
    let workerId = WorkerId [i|version:#{worldVersion}:rsync-fetch:#{u}|]    

    let maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount
    let arguments = 
            [ show workerId ] <> 
            rtsArguments [ 
                rtsN maxCpuAvailable, 
                rtsA "4m", 
                rtsAL "4m", 
                "-Fd1",
                "--disable-delayed-os-memory-return",
                rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #rsyncWorkerMemoryMb) ]

    vp <- askScopes
    workerInput <- makeWorkerInput appContext workerId
                        (RsyncFetchParams vp fetchConfig repository worldVersion)                        
                        (Timebox $ fetchConfig ^. #rsyncTimeout)
                        (Just $ asCpuTime $ fetchConfig ^. #cpuLimit) 
    
    workerInfo <- newWorkerInfo RsyncWorker (fetchConfig ^. #rsyncTimeout) (U.convert $ show workerId)

    wr@WorkerResult {..} <- runWorker logger workerInput arguments workerInfo    
    case payload of 
        Left (ErrorResult e) -> do 
            appError $ InternalE $ WorkerError e
        Right (RsyncFetchResult z) -> do     
            logWorkerDone logger workerId wr    
            pushSystem logger $ resourceUsageMetric "rsync-fetch" clockTime stats
            embedValidatorT $ pure z
    

-- | Download one file using rsync
-- | 
-- | This function doesn't throw exceptions.
rsyncRpkiObject :: ValidatorIO es => AppContext s -> 
                FetchConfig -> 
                RsyncURL -> 
                Eff es ParsedRpkiObject
rsyncRpkiObject AppContext{..} fetchConfig uri = do
    let RsyncConf {..} = rsyncConf config
    destination <- rsyncDestination RsyncOneFile (configValue rsyncRoot) uri
    let rsync = rsyncProcess config fetchConfig uri destination RsyncOneFile
    (exitCode, out, err) <- readRsyncProcess logger fetchConfig rsync [i|rsync for #{uri}|]
    case exitCode of  
        ExitFailure errorCode -> do
            logError logger [i|Rsync process failed: #rsync 
                                        with code #{errorCode}, 
                                        stderr = #{err}, 
                                        stdout = #{out}|]        
            appError $ RsyncE $ RsyncProcessError errorCode $ U.convert err  
        ExitSuccess -> do
            fileSize <- fromTry (RsyncE . FileReadError . U.fmtEx) $ getFileSize destination
            void $ validateSizeM (config ^. typed) fileSize
            bs       <- fromTry (RsyncE . FileReadError . U.fmtEx) $ getFileContent destination
            readObject (RsyncU uri) bs


-- | Process the whole rsync repository, download it, traverse the directory and 
-- | add all the relevant objects to the storage.
updateObjectForRsyncRepository :: (ValidatorIO es, Concurrent :> es, Timeout :> es) => 
                                  AppContext s
                               -> FetchConfig 
                               -> WorldVersion 
                               -> RsyncRepository 
                               -> Eff es RsyncRepository
updateObjectForRsyncRepository 
    appContext@AppContext{..} 
    fetchConfig
    worldVersion
    repo@(RsyncRepository (RsyncPublicationPoint uri) _) = 
        
    timedMetric (Proxy :: Proxy RsyncMetric) $ do     
        let rsyncRoot = configValue $ appContext ^. typed @Config . typed @RsyncConf . typed
        db <- liftIO $ readTVarIO database        
        destination <- rsyncDestination RsyncDirectory rsyncRoot uri
        let rsync = rsyncProcess config fetchConfig uri destination RsyncDirectory
            
        logDebug logger [i|Runnning #{U.trimmed rsync}|]

        -- The timeout we are getting here includes the extra timeout for the rsync fetcher 
        -- process to kill itself. So reserve less time specifically for the rsync client.
        let timeout = fetchConfig ^. #rsyncTimeout - Seconds 2

        (exitCode, out, err) <- timeoutVT 
                timeout
                (fromTry  
                    (RsyncE . RsyncRunningError . U.fmtEx) $ 
                    readRsyncProcess logger fetchConfig rsync [i|rsync for #{uri}|])
                (do 
                    logError logger [i|rsync client timed out after #{timeout}}.|]
                    appError $ RsyncE $ RsyncDownloadTimeout timeout)
        logInfo logger [i|Finished rsynching #{getURL uri} to #{destination}.|]
        case exitCode of  
            ExitSuccess -> do                 
                loadRsyncRepository appContext worldVersion uri destination db                             
                pure repo
            ExitFailure errorCode -> do
                logError logger [i|Rsync process failed: #{rsync} 
                                            with code #{errorCode}, 
                                            stderr = #{err}, 
                                            stdout = #{out}|]
                appError $ RsyncE $ RsyncProcessError errorCode $ U.convert err 
  
-- Repeat the readProcess but register PID of the launched rsync process
-- together with its maximal lifetime
readRsyncProcess :: MonadIO m =>
                    AppLogger
                    -> FetchConfig
                    -> ProcessConfig stdin stdout0 stderr0
                    -> Text.Text
                    -> m (ExitCode, LBS.ByteString, LBS.ByteString)
readRsyncProcess logger fetchConfig pc textual = do 
    Now now <- thisInstant
    let endOfLife = momentAfter now (fetchConfig ^. #rsyncTimeout)
    liftIO $ withProcessTerm pc' $ \p -> do 
        mPid <- getPid p
        case mPid of 
            Just pid -> 
                withWorker pid endOfLife $ execute p
            Nothing ->                 
                execute p 
  where
    execute p = atomically $ (,,)
                <$> waitExitCodeSTM p
                <*> getStdout p
                <*> getStderr p

    withWorker pid endOfLife f = do 
        let workerInfo = WorkerInfo pid endOfLife textual RsyncWorker
        IOExc.bracket
            (registerWorker logger workerInfo)
            (\_ -> deregisterWorker logger pid)   
            (const f)

    pc' = setStdout byteStringOutput
        $ setStderr byteStringOutput pc



-- | Recursively traverse given directory and save all the parseable 
-- | objects into the storage.
-- 
-- | Is not supposed to throw exceptions.
loadRsyncRepository :: (ValidatorIO es, Concurrent :> es) => AppContext s 
                    -> WorldVersion 
                    -> RsyncURL 
                    -> FilePath 
                    -> DB
                    -> Eff es ()
loadRsyncRepository AppContext{..} worldVersion repositoryUrl rootPath db = do
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
                        let uri = restoreUriFromPath repositoryUrl rootPath path
                        s <- lift askScopes
                        a <- lift $ async $ evaluate
                                =<< runValidator s (readAndParseObject path (RsyncU uri))
                        S.yield (a, uri)
      where
        -- Explicit `forall es'`: this is run under a nested `runValidator`,
        -- which pushes fresh handlers, so with MonoLocalBinds an unsignatured
        -- (hence monomorphic) binding would not typecheck there.
        readAndParseObject :: forall es' . ValidatorIO es'
                           => FilePath -> RpkiURL -> Eff es' RsyncObjectProcessingResult
        readAndParseObject filePath rpkiURL = 
            liftIO (getSizeAndContent (config ^. typed) filePath) >>= \case                    
                Left e          -> pure $! CantReadFile rpkiURL filePath $ VErr e
                Right (_, blob) ->                     
                    case urlObjectType rpkiURL of 
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
            tryToParse hash blob type_ = do
                scopes <- askScopes
                doParse scopes `catchSync` onError scopes
              where
                doParse scopes = do                     
                    z <- runValidator scopes $ do
                            inSubLocationScope (getURL rpkiURL) $ 
                                prevalidateObject =<< readObjectOfType type_ blob
                    evaluate $!
                        case z of
                            (Left _, vs) ->
                                mkSaveObject $ OriginalRO (ObjectOriginal blob) vs hash type_
                            (Right vro, vs)
                                | hasValidationErrors vs ->
                                    mkSaveObject $ OriginalRO (ObjectOriginal blob) vs hash type_
                                | otherwise ->
                                    mkSaveObject $ WellStructuredRO vro

                onError scopes e = do
                    (_, vs) <- runValidator scopes $
                        fromEither @() $ Left $ RsyncE $ RsyncFailedToParseObject $ U.fmtEx e
                    pure $! mkSaveObject $ OriginalRO (ObjectOriginal blob) vs hash type_

                -- Encode/compress the object here, on the parsing (async) thread,
                -- so the single-threaded DB-writer only has to do the INSERT.
                mkSaveObject lifecycle = SaveObject rpkiURL (toStorableObject (Compressed lifecycle))

    saveStorable tx (a, _) = do 
        (r, vs) <- fromTryM (UnspecifiedE "Something bad happened in loadRsyncRepository" . U.fmtEx) $ wait a                
        embedState vs
        case r of 
            Left e  -> appWarn e
            Right z -> case z of 
                HashExists rpkiURL _ key ->
                    DB.linkObjectToUrl tx db rpkiURL key worldVersion
                CantReadFile rpkiUrl filePath (VErr e) -> do                    
                    logError logger [i|Cannot read file #{filePath}, error #{e} |]
                    inSubLocationScope (getURL rpkiUrl) $ appWarn e                 
                UknownObjectType rpkiUrl filePath -> do
                    logError logger [i|Unknown object type: url = #{rpkiUrl}, path = #{filePath}.|]
                    inSubLocationScope (getURL rpkiUrl) $ 
                        appWarn $ RsyncE $ RsyncUnsupportedObjectType $ U.convert rpkiUrl

                ObjectParsingProblem rpkiUrl (VErr e) original hash objectMeta -> do
                    logError logger [i|Couldn't parse object #{rpkiUrl}, error #{e}, will cache the original object.|]   
                    inSubLocationScope (getURL rpkiUrl) $ appWarn e
                    key <- DB.saveObject tx db (OriginalRO original vs hash objectMeta.objectType) worldVersion
                    DB.linkObjectToUrl tx db rpkiUrl key worldVersion

                SaveObject rpkiUrl so@StorableObject { object = Compressed lifecycle } -> do
                    case lifecycle of
                        OriginalRO _ vs1 _ _ -> do
                            logError logger [i|Object #{rpkiUrl} failed parse/prevalidation.|]
                            embedState vs1
                        WellStructuredRO _ -> pure ()

                    key <- DB.saveStorableObject tx db so worldVersion
                    DB.linkObjectToUrl tx db rpkiUrl key worldVersion
                    updateMetric @RsyncMetric @_ (#processed %~ 
                        Map.unionWith (+) (Map.singleton (Just $ getRpkiObjectType lifecycle) 1))
                other -> 
                    logDebug logger [i|Weird thing happened in `saveStorable` #{other}.|]                    
                  

data RsyncMode = RsyncOneFile | RsyncDirectory

rsyncProcess :: Config -> FetchConfig -> RsyncURL -> FilePath -> RsyncMode -> ProcessConfig () () ()
rsyncProcess Config {..} fetchConfig rsyncURL destination rsyncMode = 
    proc "rsync" $ 
        [ "--update",  "--times" ] <> 
        [ "--timeout=" <> show timeout' ] <>         
        [ "--contimeout=60" ] <>         
        [ "--max-size=" <> show (validationConfig ^. #maxObjectSize) ] <> 
        [ "--min-size=" <> show (validationConfig ^. #minObjectSize) ] <> 
        extraOptions <> 
        [ sourceUrl, destination ]
    where 
        Seconds timeout' = fetchConfig ^. #rsyncTimeout
        source = Text.unpack (unURI $ getURL rsyncURL)        
        (sourceUrl, extraOptions) = case rsyncMode of 
            RsyncOneFile   -> (source, [])
            RsyncDirectory -> (addTrailingPathSeparator source, [ "--recursive", "--delete", "--copy-links" ])

{- | Map an rsync URL onto a local path under the rsync root.

   `parseRsyncURL` already rejects host names and path segments that are not 
   usable as a single path component, so this is defence in depth: whatever 
   happens, never create directories or point the rsync client (which runs 
   with --delete) outside of `root`.
-}
rsyncDestination :: ValidatorIO es => RsyncMode -> FilePath -> RsyncURL -> Eff es FilePath
rsyncDestination rsyncMode root url@(RsyncURL (RsyncHost (RsyncHostName host) port) path) = do 
    let portPath = maybe "" (\p -> "_" <> show p) port
    let fullPath = ((U.convert host :: String) <> portPath) :| map (U.convert . unRsyncPathChunk) path
    let mkPath p = foldl (</>) root p
    let pathWithoutLast = NonEmpty.init fullPath
    let target = mkPath $ NonEmpty.toList fullPath

    for_ (NonEmpty.toList fullPath) $ \segment -> 
        unless (isSafePathSegment segment) $ 
            appError $ RsyncE $ UnknownRsyncProblem 
                [i|Unsafe path segment '#{segment}' derived from rsync URL #{getURL url}.|]

    unless (isUnderRoot root target) $ 
        appError $ RsyncE $ UnknownRsyncProblem 
            [i|Rsync URL #{getURL url} maps to #{target} which is outside of the rsync root #{root}.|]

    liftIO $ case rsyncMode of 
        RsyncOneFile -> do             
            createDirectoryIfMissing True (mkPath pathWithoutLast)
            pure target
        RsyncDirectory -> do            
            createDirectoryIfMissing True target
            pure $ addTrailingPathSeparator target

-- | A path segment must be exactly one non-empty file/directory name.
isSafePathSegment :: FilePath -> Bool
isSafePathSegment segment = 
    not (null segment)
        && segment /= "." && segment /= ".."
        && not (any isPathSeparator segment)
        && not (any (\c -> c == '\0' || c == '\\') segment)
        && not (isAbsolute segment)

-- | Purely lexical containment check on normalised paths.
isUnderRoot :: FilePath -> FilePath -> Bool
isUnderRoot root target = 
    case stripPrefix (splitDirectories (normalise root)) (splitDirectories (normalise target)) of 
        Nothing   -> False
        Just rest -> ".." `notElem` rest
    
                        
getSizeAndContent :: ValidationConfig -> FilePath -> IO (Either AppError (Integer, BS.ByteString))
getSizeAndContent vc path = do 
    r <- first (RsyncE . FileReadError . U.fmtEx) <$> readSizeAndContet
    pure $ r >>= \case 
                (_, Left e)  -> Left e
                (s, Right b) -> Right (s, b)    
  where    
    readSizeAndContet = IOExc.try $ do
        withFile path ReadMode $ \h -> do
            size <- hFileSize h
            case validateSize vc size of
                Left e  -> pure (size, Left $ ValidationE e)
                Right _ -> do
                    r <- BS.hGetContents h                                
                    pure (size, Right r)

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize 

getFileContent :: FilePath -> IO BS.ByteString 
getFileContent = BS.readFile    

restoreUriFromPath :: RsyncURL -> FilePath -> FilePath -> RsyncURL
restoreUriFromPath url@(RsyncURL host rootPath) rsyncRoot filePath = 
    case stripPrefix (splitDirectories rsyncRoot) (splitDirectories filePath) of
        Nothing   -> url
        Just diff -> RsyncURL host (rootPath <> map (RsyncPathChunk . U.convert) diff)
    
data RsyncObjectProcessingResult =           
          CantReadFile RpkiURL FilePath VIssue
        | HashExists RpkiURL Hash ObjectKey
        | UknownObjectType RpkiURL String
        | ObjectParsingProblem RpkiURL VIssue ObjectOriginal Hash ObjectMeta
        | SaveObject RpkiURL (StorableObject (Compressed RpkiObjectLifecycle))
    deriving stock (Show, Eq, Generic)

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Rsync where
    
import           Control.Lens
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.IO.Class           

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict                  as Map
import           Data.Proxy
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
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
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

import qualified Streaming.Prelude                as S


checkRsyncInPath :: Maybe FilePath -> ValidatorT IO ()
checkRsyncInPath rsyncClientPath = do 
    let client = fromMaybe "rsync" rsyncClientPath    
    z <- liftIO $ try $ readProcess $ proc client [ "--version" ]
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


runRsyncFetchWorker :: AppContext s 
                    -> FetchConfig
                    -> WorldVersion
                    -> RsyncRepository             
                    -> ValidatorT IO RsyncRepository
runRsyncFetchWorker appContext@AppContext {..} fetchConfig worldVersion repository = do
        
    -- This is for humans to read in `top` or `ps`, actual parameters
    -- are passed as 'RsyncFetchResult'.
    let (URI u) = getURL repository
    let workerId = WorkerId [i|version:#{worldVersion}:rsync-fetch:#{u}|]    

    let maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount
    let arguments = 
            [ workerIdStr workerId ] <> 
            rtsArguments [ 
                rtsN maxCpuAvailable, 
                rtsA "20m", 
                rtsAL "64m", 
                rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #rsyncWorkerMemoryMb) ]

    vp <- askScopes
    workerInput <- makeWorkerInput appContext workerId
                        (RsyncFetchParams vp fetchConfig repository worldVersion)                        
                        (Timebox $ fetchConfig ^. #rsyncTimeout)
                        (Just $ asCpuTime $ fetchConfig ^. #cpuLimit) 
    
    workerInfo <- newWorkerInfo RsyncWorker (fetchConfig ^. #rsyncTimeout) (U.convert $ workerIdStr workerId)

    wr@WorkerResult {..} <- runWorker logger workerInput arguments workerInfo

    let RsyncFetchResult z = payload        
    logWorkerDone logger workerId wr    
    pushSystem logger $ cpuMemMetric "fetch" cpuTime clockTime maxMemory
    embedValidatorT $ pure z
    

-- | Download one file using rsync
-- | 
-- | This function doesn't throw exceptions.
rsyncRpkiObject :: AppContext s -> 
                FetchConfig -> 
                RsyncURL -> 
                ValidatorT IO RpkiObject
rsyncRpkiObject AppContext{..} fetchConfig uri = do
    let RsyncConf {..} = rsyncConf config
    destination <- liftIO $ rsyncDestination RsyncOneFile (configValue rsyncRoot) uri
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
            void $ vHoist $ validateSizeM (config ^. typed) fileSize
            bs       <- fromTry (RsyncE . FileReadError . U.fmtEx) $ getFileContent destination
            vHoist $ readObject (RsyncU uri) bs


-- | Process the whole rsync repository, download it, traverse the directory and 
-- | add all the relevant objects to the storage.
updateObjectForRsyncRepository :: Storage s => 
                                  AppContext s
                               -> FetchConfig 
                               -> WorldVersion 
                               -> RsyncRepository 
                               -> ValidatorT IO RsyncRepository
updateObjectForRsyncRepository 
    appContext@AppContext{..} 
    fetchConfig
    worldVersion
    repo@(RsyncRepository (RsyncPublicationPoint uri) _) = 
        
    timedMetric (Proxy :: Proxy RsyncMetric) $ do     
        let rsyncRoot = configValue $ appContext ^. typed @Config . typed @RsyncConf . typed
        db <- liftIO $ readTVarIO database        
        destination <- liftIO $ rsyncDestination RsyncDirectory rsyncRoot uri
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
        forM_ mPid $ \pid -> 
            registerWorker logger $ WorkerInfo pid endOfLife textual RsyncWorker

        z <- atomically $ (,,)
            <$> waitExitCodeSTM p
            <*> getStdout p
            <*> getStderr p

        forM_ mPid $ \pid -> 
            deregisterhWorker logger pid

        pure z
  where
    pc' = setStdout byteStringOutput
        $ setStderr byteStringOutput pc



-- | Recursively traverse given directory and save all the parseable 
-- | objects into the storage.
-- 
-- | Is not supposed to throw exceptions.
loadRsyncRepository :: Storage s =>                         
                        AppContext s 
                    -> WorldVersion 
                    -> RsyncURL 
                    -> FilePath 
                    -> DB.DB s 
                    -> ValidatorT IO ()
loadRsyncRepository AppContext{..} worldVersion repositoryUrl rootPath db =    
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
                        s <- askScopes                     
                        let task = runValidatorT s (readAndParseObject path (RsyncU uri))
                        a <- liftIO $ async $ evaluate =<< task
                        S.yield (a, uri)
      where
        readAndParseObject filePath rpkiURL = 
            liftIO (getSizeAndContent (config ^. typed) filePath) >>= \case                    
                Left e          -> pure $! CantReadFile rpkiURL filePath $ VErr e
                Right (_, blob) ->                     
                    case urlObjectType rpkiURL of 
                        Just type_ -> do 
                            -- Check if the object is already in the storage
                            -- before parsing ASN1 and serialising it.
                            let hash = U.sha256s blob  
                            exists <- liftIO $ roTx db $ \tx -> DB.hashExists tx db hash
                            if exists 
                                then pure $! HashExists rpkiURL hash
                                else tryToParse hash blob type_                                    
                        Nothing -> 
                            pure $! UknownObjectType rpkiURL filePath

          where
            tryToParse hash blob type_ = do            
                let scopes = newScopes $ unURI $ getURL rpkiURL
                z <- liftIO $ runValidatorT scopes $ vHoist $ readObjectOfType type_ blob
                (evaluate $! 
                    case z of 
                        (Left e, _) -> 
                            ObjectParsingProblem rpkiURL (VErr e) 
                                (ObjectOriginal blob) hash
                                (ObjectMeta worldVersion type_)                        
                        (Right ro, _) ->                                     
                            SuccessParsed rpkiURL (toStorableObject ro) type_                    
                    ) `catch` 
                    (\(e :: SomeException) -> 
                        pure $! ObjectParsingProblem rpkiURL (VErr $ RsyncE $ RsyncFailedToParseObject $ U.fmtEx e) 
                                (ObjectOriginal blob) hash
                                (ObjectMeta worldVersion type_)
                    )

    saveStorable tx (a, _) = do 
        (r, vs) <- fromTry (UnspecifiedE "Something bad happened in loadRsyncRepository" . U.fmtEx) $ wait a                
        embedState vs
        case r of 
            Left e  -> appWarn e
            Right z -> case z of 
                HashExists rpkiURL hash ->
                    DB.linkObjectToUrl tx db rpkiURL hash
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
                    DB.saveOriginal tx db original hash objectMeta
                    DB.linkObjectToUrl tx db rpkiUrl hash                                  
                SuccessParsed rpkiUrl so@StorableObject {..} type_ -> do 
                    DB.saveObject tx db so worldVersion                    
                    DB.linkObjectToUrl tx db rpkiUrl (getHash object)
                    updateMetric @RsyncMetric @_ (#processed %~ Map.unionWith (+) (Map.singleton (Just type_) 1))
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

rsyncDestination :: RsyncMode -> FilePath -> RsyncURL -> IO FilePath
rsyncDestination rsyncMode root (RsyncURL (RsyncHost (RsyncHostName host) port) path) = do 
    let portPath = maybe "" (\p -> "_" <> show p) port
    let fullPath = ((U.convert host :: String) <> portPath) :| map (U.convert . unRsyncPathChunk) path
    let mkPath p = foldl (</>) root p
    let pathWithoutLast = NonEmpty.init fullPath
    let target = mkPath $ NonEmpty.toList fullPath
    case rsyncMode of 
        RsyncOneFile -> do             
            createDirectoryIfMissing True (mkPath pathWithoutLast)
            pure target
        RsyncDirectory -> do            
            createDirectoryIfMissing True target
            pure $ addTrailingPathSeparator target
    
                        
getSizeAndContent :: ValidationConfig -> FilePath -> IO (Either AppError (Integer, BS.ByteString))
getSizeAndContent vc path = do 
    r <- first (RsyncE . FileReadError . U.fmtEx) <$> readSizeAndContet
    pure $ r >>= \case 
                (_, Left e)  -> Left e
                (s, Right b) -> Right (s, b)    
  where    
    readSizeAndContet = try $ do
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
        | HashExists RpkiURL Hash
        | UknownObjectType RpkiURL String
        | ObjectParsingProblem RpkiURL VIssue ObjectOriginal Hash ObjectMeta
        | SuccessParsed RpkiURL (StorableObject RpkiObject) RpkiObjectType
    deriving stock (Show, Eq, Generic)

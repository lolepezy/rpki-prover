{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module RPKI.Rsync where
    
import           Effectful
import           Control.Lens
import           Data.Generics.Product.Typed

import           Control.Concurrent.STM
import qualified Control.Exception               as IOExc

import           Effectful.Concurrent.Async
import           Effectful.Timeout                (Timeout)
import           Effectful.Exception
import           Control.Monad

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Foldable (for_)
import           Data.List (stripPrefix)
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NonEmpty

import           Data.Hourglass

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppMonadUtil
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Metrics.System
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Types
import           RPKI.Store.Base.Storable (StorableObject(..), Compressed(..), toStorableObject)
import           RPKI.Store.Database     (DB, roTx)
import qualified RPKI.Store.Database    as DB
import           RPKI.Fetch.DirectoryTraverse
import           RPKI.Time
import qualified RPKI.Util                        as U
import           RPKI.Validation.ObjectValidation
import           RPKI.Worker
import           RPKI.Fetch.DirectoryTraverse

import           System.Directory                 (createDirectoryIfMissing)

import           System.Exit
import           System.IO
import           System.FilePath
import           System.Process.Typed


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
            pushSystem logger $ cpuMemMetric "rsync-fetch" cpuTime clockTime maxRtsHeap maxProcessRss
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
        
    timedMetric (Proxy :: Proxy TraverseMetric) $ do     
        let rsyncRoot = configValue $ appContext ^. typed @Config . typed @RsyncConf . typed
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
                loadRsyncRepository appContext worldVersion uri destination                        
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
loadRsyncRepository :: (ValidatorIO es, Concurrent :> es)
                    => AppContext s
                    -> WorldVersion
                    -> RsyncURL
                    -> FilePath
                    -> Eff es ()
loadRsyncRepository appContext worldVersion repositoryUrl rootPath =
    -- An rsync tree mirrors the repository layout, so the path alone determines
    -- the URL and the parsed object is never needed.
    loadObjectsFromFS appContext worldVersion
        (\filePath _ -> Just $ restoreUriFromPath repositoryUrl rootPath filePath)
        rootPath

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
    let mkPath = foldl' (</>) root
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
    
getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize 

getFileContent :: FilePath -> IO BS.ByteString 
getFileContent = BS.readFile    

restoreUriFromPath :: RsyncURL -> FilePath -> FilePath -> RsyncURL
restoreUriFromPath url@(RsyncURL host rootPath) rsyncRoot filePath = 
    case stripPrefix (splitDirectories rsyncRoot) (splitDirectories filePath) of
        Nothing   -> url
        Just diff -> RsyncURL host (rootPath <> map (RsyncPathChunk . U.convert) diff)
    

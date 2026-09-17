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
import           Data.Proxy
import           Data.Foldable (for_)
import           Data.List (stripPrefix)
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NonEmpty

import           Data.Hourglass

import           GHC.Generics (Generic)

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Metrics.System
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Time
import qualified RPKI.Util                        as U
import           RPKI.Validation.ObjectValidation
import           RPKI.Worker
import           RPKI.Sandbox
import           RPKI.Fetch.DirectoryTraverse

import           System.Directory                 (createDirectoryIfMissing, doesFileExist,
                                                   executable, findExecutable,
                                                   getPermissions, makeAbsolute)

import           System.Exit
import           System.IO
import           System.FilePath
import           System.Process.Typed


-- | Find the rsync client and check that it works. Returns its absolute
-- path: it is run through the launcher that needs one and the sandbox
-- needs to know what exactly is allowed to be executed.
findRsyncClient :: ValidatorIO es => Maybe FilePath -> Eff es FilePath
findRsyncClient rsyncClientPath = do 
    client <- liftIO $ case rsyncClientPath of 
        Nothing -> findExecutable "rsync"
        Just path 
            | hasTrailingPathSeparator path || not (any isPathSeparator path) -> 
                findExecutable path
            | otherwise -> do 
                absolute <- makeAbsolute path
                exists <- doesFileExist absolute
                isExecutable <- if exists then executable <$> getPermissions absolute else pure False
                pure $ if isExecutable then Just absolute else Nothing

    case client of 
        Nothing -> appError $ InitE $ InitError $ maybe 
                    "rsync client is not in the $PATH, can't proceed."
                    (\rc -> [i|rsync client #{rc} is not found or is not executable, can't proceed.|])
                    rsyncClientPath
        Just path -> do 
            z <- liftIO $ IOExc.try $ readProcess $ proc path [ "--version" ]
            case z of
                Left (e :: SomeException) -> 
                    appError $ InitE $ InitError [i|Cannot run rsync client #{path}: #{U.fmtEx e}|]
                Right (ExitSuccess, _, _) -> 
                    pure path
                Right (exit, stdout', stderr') -> 
                    appError $ InitE $ InitError 
                        [i|#{path} --version returned non-zero exit code #{exit}, 
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
                rtsMaxMemory $ rtsMemValue (config ^. typed @SystemConfig . #rsyncWorker . #memoryMb) ]

    vp <- askScopes
    workerInput <- makeWorkerInput appContext workerId
                        (RsyncFetchParams vp fetchConfig repository worldVersion)
                        (Timebox $ fetchConfig ^. #rsyncTimeout)
    
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
    let rsync = rsyncCommand config fetchConfig uri destination RsyncOneFile
    (exitCode, out, err) <- readRsyncProcess logger fetchConfig (rsyncProcess rsync) [i|rsync for #{uri}|]
    case exitCode of  
        ExitFailure errorCode -> do
            logError logger $ rsyncFailureMessage rsync exitCode out err
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
        let rsync = rsyncCommand config fetchConfig uri destination RsyncDirectory
            
        logDebug logger [i|Running #{rsyncCommandLine rsync}, sandbox: #{rsyncSandbox rsync}.|]

        -- The timeout we are getting here includes the extra timeout for the rsync fetcher 
        -- process to kill itself. So reserve less time specifically for the rsync client.
        let timeout = fetchConfig ^. #rsyncTimeout - Seconds 2

        (exitCode, out, err) <- timeoutVT 
                timeout
                (fromTry  
                    (RsyncE . RsyncRunningError . U.fmtEx) $ 
                    readRsyncProcess logger fetchConfig (rsyncProcess rsync) [i|rsync for #{uri}|])
                (do 
                    logError logger [i|rsync client timed out after #{timeout}}.|]
                    appError $ RsyncE $ RsyncDownloadTimeout timeout)
        logInfo logger [i|Finished rsynching #{getURL uri} to #{destination}.|]
        case exitCode of  
            ExitSuccess -> do                 
                loadRsyncRepository appContext worldVersion uri destination                        
                pure repo
            ExitFailure errorCode -> do
                logError logger $ rsyncFailureMessage rsync exitCode out err
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

-- | Everything needed to run the rsync client. It doesn't run directly, but
-- through the launcher (see RPKI.Sandbox), that sets resource limits, applies
-- the sandbox and then becomes the rsync client, keeping the PID, stdout,
-- stderr and the exit code.
data RsyncCommand = RsyncCommand {
        -- | The rsync client and its arguments
        rsyncClient  :: FilePath,
        rsyncArgs    :: [String],
        launcher     :: FilePath,
        rsyncSandbox :: Maybe WorkerSandbox,
        required     :: Bool,
        limits       :: ProcessLimits
    }
    deriving stock (Eq, Show, Generic)

rsyncCommand :: Config -> FetchConfig -> RsyncURL -> FilePath -> RsyncMode -> RsyncCommand
rsyncCommand config fetchConfig rsyncURL destination rsyncMode = 
    RsyncCommand {
        rsyncClient  = rsyncClientExecutable config,
        rsyncArgs    = rsyncArguments config fetchConfig rsyncURL destination rsyncMode,
        launcher     = configValue $ config ^. #programBinaryPath,
        rsyncSandbox = case sandboxMode of 
                        NoSandbox -> Nothing
                        _         -> Just $ rsyncClientSandbox config rsyncURL writable,
        required     = sandboxMode == SandboxRequired,
        limits       = ProcessLimits {
            cpuSeconds        = Just $ toInteger cpuLimit,
            addressSpaceBytes = Just $ toInteger (workerLimits ^. #memoryMb) * 1024 * 1024,
            -- rsync doesn't download anything bigger anyway (--max-size)
            fileSizeBytes     = Just $ config ^. typed @ValidationConfig . #maxObjectSize,
            openFiles         = Just 1024
        }
    }
  where
    sandboxMode  = config ^. typed @SystemConfig . #sandboxMode
    workerLimits = config ^. typed @SystemConfig . #rsyncWorker
    Seconds cpuLimit = workerLimits ^. #cpuLimit

    -- rsync writes temporary files next to the ones it downloads
    writable = case rsyncMode of 
        RsyncOneFile   -> takeDirectory destination
        RsyncDirectory -> destination

rsyncArguments :: Config -> FetchConfig -> RsyncURL -> FilePath -> RsyncMode -> [String]
rsyncArguments Config {..} fetchConfig rsyncURL destination rsyncMode = 
    [ "--update",  "--times" ] <> 
    [ "--timeout=" <> show timeout' ] <>         
    [ "--contimeout=60" ] <>         
    [ "--max-size=" <> show (validationConfig ^. #maxObjectSize) ] <> 
    [ "--min-size=" <> show (validationConfig ^. #minObjectSize) ] <> 
    -- These are all defaults without --archive, spelled out to make sure 
    -- nothing but plain files and directories ever gets created locally.
    [ "--no-motd", "--no-devices", "--no-specials", "--no-perms", "--no-owner", "--no-group" ] <>
    extraOptions <> 
    [ sourceUrl, destination ]
  where 
    Seconds timeout' = fetchConfig ^. #rsyncTimeout
    source = Text.unpack (unURI $ getURL rsyncURL)        
    (sourceUrl, extraOptions) = case rsyncMode of 
        RsyncOneFile   -> (source, [])
        RsyncDirectory -> (addTrailingPathSeparator source, [ "--recursive", "--delete", "--copy-links" ])

-- | What the launcher is started with.
rsyncLauncherCommand :: RsyncCommand -> (FilePath, [String], [(String, String)])
rsyncLauncherCommand RsyncCommand {..} = let 
        (program, args) = sandboxedCommand launcher rsyncClient rsyncArgs
    in (program, args, environment)
  where
    -- Nothing from the environment of the worker makes it to the rsync client, 
    -- some RSYNC_* variables would make it run other programs or connect elsewhere.
    environment = 
        [ ("PATH", "/usr/bin:/bin"), ("LC_ALL", "C") ] 
        <> limitsEnvironment limits
        <> maybe [] sandboxEnvironment rsyncSandbox
        <> (if required then requiredEnvironment else [])

rsyncProcess :: RsyncCommand -> ProcessConfig () () ()
rsyncProcess rsync = setEnv environment $ proc program args
  where
    (program, args, environment) = rsyncLauncherCommand rsync

rsyncCommandLine :: RsyncCommand -> Text.Text
rsyncCommandLine RsyncCommand {..} = Text.pack $ unwords $ rsyncClient : rsyncArgs

rsyncFailureMessage :: RsyncCommand -> ExitCode -> LBS.ByteString -> LBS.ByteString -> Text.Text
rsyncFailureMessage rsync exitCode out err 
    | exitCode == launcherSetupFailedExitCode = 
        [i|Could not set up limits or sandbox for #{rsyncCommandLine rsync}, it was not run: #{U.textual err}|]
    | exitCode == launcherExecFailedExitCode = 
        [i|Could not start #{rsyncCommandLine rsync}: #{U.textual err}|]
    | otherwise = 
        [i|Rsync process failed: #{rsyncCommandLine rsync} with #{exitCode}, stderr = #{U.textual err}, stdout = #{U.textual out}|]

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
    

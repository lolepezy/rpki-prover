{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Lens ((^.), (&))
import           Control.Lens.Setter
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Async

import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Foldable
import           Data.Generics.Product.Typed

import           Data.Bifunctor
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Text                        as Text
import qualified Data.Set                         as Set

import           Data.Hourglass
import           Data.Int                         (Int16, Int64)    
import qualified Data.List                        as List
import           Data.Maybe
import           Data.Word                        (Word16)
import           Data.String.Interpolate.IsString
import qualified Network.Wai.Handler.Warp         as Warp

import           System.Directory
import           System.Environment
import           System.FilePath                  ((</>))
import           System.IO                        
import           System.Exit

import           Options.Applicative
import           Numeric.Natural
import           GHC.Generics (Generic)

import           Shower

import           RPKI.AppTypes
import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Messages
import           RPKI.Reporting
import           RPKI.RRDP.Http (downloadToFile)
import           RPKI.Http.HttpServer
import           RPKI.Logging
import           RPKI.Store.Base.Storage
import           RPKI.Store.AppStorage
import           RPKI.Store.AppLmdbStorage
import qualified RPKI.Store.MakeLmdb as Lmdb
import           RPKI.SLURM.SlurmProcessing

import           RPKI.RRDP.RrdpFetch

import           RPKI.Rsync
import           RPKI.TAL
import           RPKI.Util               
import           RPKI.Worker
import           RPKI.Metrics.HeapReport
import           RPKI.Workflow
import           RPKI.RSC.Verifier
import           RPKI.Version
import           RPKI.UniqueId


import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
-- import           Network.HTTP.Simple
import           Network.Connection


main :: IO ()
main = do
    cliOptions@CLIOptions{..} <- execParser $
            info (cliOptionsParser <**> helper)
                 (fullDesc <> progDesc ("RPKI prover, relying party software for RPKI, version " <> Text.unpack rpkiProverVersion))

    if version 
        then 
            -- it is "--version" call, so print the version and exit
            putStrLn $ convert rpkiProverVersion
        else 
            if printConfig
            then printConf cliOptions
            else case worker of
                Nothing ->
                    if verifySignature
                        -- this is a call to RSC verification 
                        then executeVerifier cliOptions
                        -- this is a normal validator daemon launch
                        else executeMainProcess cliOptions
                Just _ ->
                    executeWorkerProcess
  where
    printConf cliOptions = do 
        putStrLn "CLI options:"
        putStrLn $ shower cliOptions
        putStrLn "Configuration:"
        putStrLn $ shower $ applyCliToConfig defaultConfig cliOptions Hidden                            


executeMainProcess :: CLIOptions -> IO ()
executeMainProcess cliOptions@CLIOptions{..} = do     
    -- TODO This doesn't look pretty, come up with something better.
    appStateHolder <- newTVarIO Nothing

    let withAppState f = do 
            z <- readTVarIO appStateHolder
            for_ z f    

    withLogConfig cliOptions $ \logConfig_ -> do
        let logConfig = logConfig_
                & #metricsHandler .~ withAppState . mergeSystemMetrics
                & #workerHandler .~ withAppState . updateRunningWorkers
                & #systemStatusHandler .~ withAppState . updateSystemStatus

        -- This one modifies system metrics in AppState
        -- if appState is actually initialised
        withLogger logConfig $ \logger -> do
            logDebug logger $ if once 
                    then [i|Starting #{rpkiProverVersion} in one-off mode.|]
                    else [i|Starting #{rpkiProverVersion} as a server.|]
            
            (z, validations) <- do
                        runValidatorT (newScopes "Startup") $ do
                            checkPreconditions cliOptions
                            createAppContext cliOptions logger (logConfig ^. #logLevel)
            case z of
                Left _ -> do 
                    logError logger [i|Failure:
#{formatValidations (validations ^. typed)}|]
                    drainLog logger
                    hFlush stdout
                    hFlush stderr
                    threadDelay 100_000
                    exitFailure
                Right appContext -> do 
                    atomically $ writeTVar appStateHolder $ Just $ appContext ^. #appState
                    runMainProcess
                        `finally` 
                        closeStorage appContext
                  where
                    runMainProcess = do                                          
                        tals <- readTALs appContext
                        case appContext ^. #config . #proverRunMode of
                            OneOffMode _ -> 
                                runValidatorWorkflow appContext tals                                                        
                            ServerMode -> 
                                void $ race
                                    (runHttpApi appContext)
                                    (runValidatorWorkflow appContext tals)

executeWorkerProcess :: IO ()
executeWorkerProcess = do
    input <- readWorkerInput    
    let config = adjustWorkerConfig (input ^. typed @Config) (input ^. #workerTimeout)
    let logConfig = newLogConfig (config ^. #logLevel) WorkerLog
                    
    -- turnOffTlsValidation

    appContextRef <- newTVarIO Nothing
    let onExit exitCode = do            
            readTVarIO appContextRef >>= maybe (pure ()) closeStorage
            exitWith exitCode

    executeWork input onExit $ \_ resultHandler -> 
        withLogger logConfig $ \logger -> liftIO $ do
            (z, validations) <- runValidatorT
                                    (newScopes "worker-create-app-context")
                                    (createWorkerAppContext config logger)
            case z of
                Left e ->
                    logError logger [i|Couldn't initialise: #{e}, problems: #{validations}.|]
                Right appContext -> do
                    atomically $ writeTVar appContextRef $ Just appContext
                    let exec :: forall r . (WorkerResult r -> IO ()) -> IO (Either ErrorResult r) -> IO ()
                        exec resultHandler f = do
                            wr <- execWithStats f
                            -- Emit a full heap report to stderr.  The parent process captures
                            -- stderr and forwards it through its own logger, so the report ends
                            -- up in the regular log stream and can be pasted into an AI
                            -- conversation for memory analysis.
                            logHeapReport logger (workerParamLabel $ input ^. #params)
                            resultHandler wr
                    let actuallyExecuteWork = 
                            case input ^. #params of
                                RrdpFetchParams {..} -> 
                                    exec resultHandler $ fmap (Right . RrdpFetchResult) $ runValidatorT scopes $ 
                                        updateRrdpRepository appContext worldVersion rrdpRepository

                                RsyncFetchParams {..} -> 
                                    exec resultHandler $ fmap (Right . RsyncFetchResult) $ runValidatorT scopes $                                     
                                        updateObjectForRsyncRepository appContext fetchConfig 
                                            worldVersion rsyncRepository

                                CompactionParams {..} -> 
                                    exec resultHandler $ 
                                        Right . CompactionResult <$> copyLmdbEnvironment appContext targetLmdbEnv

                                ValidationParams {..} -> 
                                    exec resultHandler $ do 
                                        (vs, discoveredRepositories, slurm) <- 
                                            runValidation appContext worldVersion talsToValidate allTaNames
                                        pure $ Right $ ValidationResult vs discoveredRepositories slurm

                                CacheCleanupParams {..} -> 
                                    exec resultHandler $
                                        Right . CacheCleanupResult <$> runCacheCleanup appContext worldVersion
                    actuallyExecuteWork
                        `catch` (\(t :: TxTimeout) -> 
                                    exec @() resultHandler $ do
                                        pushSystemStatus logger $ SystemStatusMessage $ SystemState { dbState = DbStuck }
                                        pure $ Left $ ErrorResult $ fmtGen t)
                        `finally`                             
                            closeStorage appContext


-- turnOffTlsValidation :: IO ()
-- turnOffTlsValidation = do 
--     manager <- newManager $ mkManagerSettings (TLSSettingsSimple True True True) Nothing 
--     setGlobalManager manager    


readTALs :: (Storage s, MaintainableStorage s) => AppContext s -> IO [TAL]
readTALs AppContext {..} = do
    
    logInfo logger [i|Reading TAL files from #{talDirectory config}|]

    -- Check that TAL names are unique
    let talSourcesDirs = configValue (config ^. #talDirectory) 
                       : configValue (config ^. #extraTalsDirectories)
    talNames <- mconcat <$> mapM listTalFiles talSourcesDirs
    
    when (Set.size (Set.fromList talNames) < length talNames) $ do
        let message = [i|TAL names are not unique #{talNames}, |] <> 
                      [i|TAL are from directories #{talSourcesDirs}.|]
        logError logger message
        throwIO $ AppException $ TAL_E $ TALError message

    (tals, _) <- runValidatorT (newScopes "validation-root") $
        forM talNames $ \(talFilePath, taName) ->
            vFocusOn TAFocus (convert taName) $
                parseTalFromFile talFilePath (Text.pack taName)    
    
    case tals of
        Left e -> do
            logError logger [i|Error reading some of the TALs, e = #{e}.|]
            throwIO $ AppException e
        Right tals' -> do
            logInfo logger [i|Successfully loaded #{length talNames} TALs: #{map snd talNames}|]            
            pure tals'
  where
    parseTalFromFile talFileName taName = do
        talContent <- fromTry (TAL_E . TALError . fmtEx) $ LBS.readFile talFileName
        vHoist $ fromEither $ first TAL_E $ parseTAL (convert talContent) taName            


runHttpApi :: (Storage s, MaintainableStorage s) => AppContext s -> IO ()
runHttpApi appContext@AppContext {..} = do 
    let httpPort = fromIntegral $ appContext ^. typed @Config . typed @HttpApiConfig . #port
    Warp.run httpPort (httpServer appContext) 
        `catch` 
        (\(e :: SomeException) -> logError logger [i|Interrupted HTTP server: #{e}.|])


createAppContext :: CLIOptions -> AppLogger -> LogLevel -> ValidatorT IO AppLmdbEnv
createAppContext cliOptions@CLIOptions{..} logger derivedLogLevel = do

    programPath <- liftIO getExecutablePath

    -- Create (or make sure exist) necessary directories in the root directory
    (root, tald, rsyncd, tmpd, cached) <- fsLayout cliOptions logger

    -- Set capabilities to the values from the CLI or to all available CPUs,
    -- (disregard the HT issue for now it needs more testing).
    let cpuCount' = fromMaybe getRtsCpuCount cpuCount
    liftIO $ setCpuCount cpuCount'

    proverRunMode     <- deriveProverRunMode cliOptions
    rsyncPrefetchUrls <- rsyncPrefetches cliOptions

    let apiSecured :: a -> ApiSecured a
        apiSecured a = if showHiddenConfig then Public a else Hidden a

    -- Build a base config with IO-derived values (paths, run mode, etc.),
    -- then apply the pure CLI overrides on top.
    let baseConfig = defaultConfig
            & #programBinaryPath .~ apiSecured programPath
            & #rootDirectory .~ apiSecured root
            & #talDirectory .~ apiSecured tald
            & #tmpDirectory .~ apiSecured tmpd
            & #cacheDirectory .~ apiSecured cached
            & #extraTalsDirectories .~ apiSecured extraTalsDirectory
            & #proverRunMode .~ proverRunMode
            & #parallelism . #cpuCount .~ cpuCount'
            & #rsyncConf . #rsyncRoot .~ apiSecured rsyncd
            & #rsyncConf . #rsyncPrefetchUrls .~ rsyncPrefetchUrls
            & #rrdpConf . #tmpRoot .~ apiSecured tmpd
            & #logLevel .~ derivedLogLevel

    let config = applyCliToConfig baseConfig cliOptions apiSecured           
        
    let readSlurms files = do
            logDebug logger [i|Reading SLURM files: #{files}.|]
            readSlurmFiles files

    -- Read the files first to fail fast
    unless (null localExceptions) $
        void $ readSlurms localExceptions
    
    appState <- createAppState logger localExceptions    
    
    lmdbEnv <- setupLmdbCache
                    (if resetCache then Reset else UseExisting)
                    logger
                    cached
                    config

    (db, dbCheck) <- fromTry (InitE . InitError . fmtEx) $ 
                Lmdb.createDatabase lmdbEnv logger config Lmdb.CheckVersion

    database <- liftIO $ newTVarIO db    
    
    let executableVersion = thisExecutableVersion
    let appContext = AppContext {..}

    case dbCheck of 
        -- If the DB version wasn't compatible with the expected version, 
        -- all the data was erased and it makes a lot of sense to perform 
        -- compaction. Not performing it may potentially bloat the database 
        -- (not sure why exactly but it was reproduced multiple times)
        -- until the next compaction that will happen probably in weeks.
        Lmdb.WasIncompatible -> liftIO $ runMaintenance appContext

        -- It may mean two different cases
        --   * empty db
        --   * old non-empty cache
        -- In practice there hardly ever be a non-empty old cache, 
        -- and even if it will be there, it will be compacted in 
        -- a week or so. So don't compact,
        Lmdb.DidntHaveVersion -> pure ()

        -- Nothing special, the cache has the version as expected
        Lmdb.WasCompatible    -> pure ()

    logInfo logger [i|Created application context with configuration: 
#{shower (config)}|]
    pure appContext

      
fsLayout :: CLIOptions
        -> AppLogger
        -> ValidatorT IO (FilePath, FilePath, FilePath, FilePath, FilePath)
fsLayout cliOptions@CLIOptions {..} logger = do
    root <- getRoot cliOptions    
    
    logInfo logger $ case root of        
        Left d  -> [i|Root directory is not set, using default #{d}, i.e. ${HOME}/.rpki|]
        Right d -> [i|Root directory is set to #{d}.|]
    
    let rootDir = either id id root

    rootExists <- liftIO $ doesDirectoryExist rootDir
    unless rootExists $ do
        let message = [i|Root directory #{rootDir} doesn't exist.|]
        logError logger message
        appError $ InitE $ InitError message

    -- For each sub-directory create it if it doesn't exist
    [cached, rsyncd, tald, tmpd] <- 
        fromTryM 
            (\e -> InitE $ InitError [i|Error verifying/creating directories: #{fmtEx e}|])
            $ forM [cacheDirName, rsyncDirName, talsDirName, tmpDirName] $ \dir -> 
                fromEitherM $ first (InitE . InitError) <$> 
                    createSubDirectoryIfNeeded rootDir dir    

    if refetchRirTals then do 
        if noRirTals then
            logInfo logger $ "Will re-fetch and overwrite TAL files, but not use them. " <> 
                             "Maybe only one of  `--refetch-rir-tals` and `--no-rir-tals` should be set?"
        else 
            logInfo logger "Will re-fetch and overwrite TAL files."
        downloadTals tald
    else do         
        tals <- liftIO $ listTalFiles tald
        when (null tals) $         
            if noRirTals then 
                -- We don't know what you wanted here, but we respect your choice
                logWarn logger $ "There are no TAL files and RIR TALs download is disabled, " <> 
                                 "so the validator is not going to do anything useful."
            else
                -- Assume the reason is the very first start of a typical installation 
                case extraTalsDirectory of 
                    [] -> do
                        -- Since there are no extra TAL locations set do the most reasonable thing, download TALs
                        logInfo logger [i|No TAL files found in #{tald}, assuming it is the first launch and downloading them.|]
                        downloadTals tald
                    _ -> 
                        -- Assume the user knows better and there are some TALs in the extraTalsDirectory
                        logInfo logger [i|No TAL files found in #{tald}, assuming there are some TALs in #{extraTalsDirectory}.|]
 
    -- Do not do anything with `tmp` and `rsync`, they most likely are going 
    -- to be okay for either a new installation or an existing one. Otherwise,
    -- they will be cleaned up by one of the periodic maintenance tasks.

    pure (rootDir, tald, rsyncd, tmpd, cached)
  where
    downloadTals tald =     
        fromTryM
            (\e -> InitE $ InitError [i|Error downloading TALs: #{fmtEx e}|])
            $ do                
                httpStatuses <- liftIO $ forConcurrently defaultTalUrls $ \(talName, Text.pack -> talUrl) -> do                    
                        logDebug logger [i|Downloading #{talUrl} to #{tald </> talName}.|]                    
                        fmap (talName, talUrl, ) $ try $ downloadToFile (URI talUrl) (tald </> talName) (Size 10_000)                        
                
                let talText = \case 
                        (talName, talUrl, Left (e :: SomeException)) -> 
                            Just [i|Failed to download TAL #{talName} from #{talUrl}, error: #{e}.|]
                        (talName, talUrl, Right status) 
                            | isHttpSuccess status -> Nothing
                            | otherwise -> Just 
                                [i|Failed to download TAL #{talName} from #{talUrl}, HTTP status: #{status}.|]

                let anyFailures = any (\(_, _, s) -> either (const True) (not . isHttpSuccess) s) httpStatuses
                when anyFailures $  
                    appError $ InitE $ InitError $ 
                        Text.intercalate "\n" $ mapMaybe talText httpStatuses                                        


getRoot :: CLIOptions -> ValidatorT IO (Either FilePath FilePath)
getRoot cliOptions = do    
    case getRootDirectory cliOptions of 
        Nothing -> do 
            home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
            Left <$> makeSureRootExists (home </> ".rpki")
        Just root -> 
            Right <$> makeSureRootExists root            
  where
    makeSureRootExists root = do 
        absoluteRoot <- liftIO $ makeAbsolute root
        rootExists <- liftIO $ doesDirectoryExist absoluteRoot
        if rootExists
            then pure absoluteRoot
            else appError $ InitE $ InitError [i|Root directory #{absoluteRoot} doesn't exist.|]

orDefault :: Maybe a -> a -> a
m `orDefault` d = fromMaybe d m

maybeSet :: ASetter s s a b -> Maybe b -> s -> s
maybeSet lenz newValue big = maybe big (\val -> big & lenz .~ val) newValue


listTalFiles :: FilePath -> IO [(FilePath, FilePath)]
listTalFiles talDirectory = do
    names <- getDirectoryContents talDirectory
    pure $ map (\f -> (talDirectory </> f, cutOffTalExtension f)) $
            filter (".tal" `List.isSuffixOf`) $
            filter (`notElem` [".", ".."]) names
  where
    cutOffTalExtension s = List.take (List.length s - 4) s


cacheDirName, rsyncDirName, talsDirName, tmpDirName :: FilePath
cacheDirName = "cache"
rsyncDirName = "rsync"
talsDirName  = "tals"
tmpDirName   = "tmp"

checkSubDirectory :: FilePath -> FilePath -> IO (Either Text.Text FilePath)
checkSubDirectory root sub = do
    let subDirectory = root </> sub
    doesDirectoryExist subDirectory >>= \case
        False -> pure $ Left [i|Directory #{subDirectory} doesn't exist|]
        True  -> pure $ Right subDirectory

createSubDirectoryIfNeeded :: FilePath -> FilePath -> IO (Either Text.Text FilePath)
createSubDirectoryIfNeeded root sub = do
    let subDirectory = root </> sub
    exists <- doesDirectoryExist subDirectory
    unless exists $ createDirectory subDirectory
    pure $ Right subDirectory

getRootDirectory :: CLIOptions -> Maybe FilePath
getRootDirectory CLIOptions{..} =
    case rpkiRootDirectory of
        [] -> Nothing
        s  -> Just $ Prelude.last s

-- Set rsync prefetch URLs
rsyncPrefetches :: CLIOptions -> ValidatorT IO [RsyncURL]
rsyncPrefetches CLIOptions {..} = do
    let urlsToParse =
            case rsyncPrefetchUrl of
                [] -> defaultPrefetchURLs
                _  -> rsyncPrefetchUrl

    forM urlsToParse $ \u ->
        case parseRsyncURL (convert u) of
            Left e         -> appError $ UnspecifiedE [i|Rsync URL #{u} is invalid|] (convert $ show e)
            Right rsyncURL -> pure rsyncURL


createWorkerAppContext :: Config -> AppLogger -> ValidatorT IO AppLmdbEnv
createWorkerAppContext config logger = do
    lmdbEnv <- setupWorkerLmdbCache
                    logger
                    (configValue $ config ^. #cacheDirectory)
                    config

    (db, _) <- fromTry (InitE . InitError . fmtEx) $ 
                Lmdb.createDatabase lmdbEnv logger config Lmdb.DontCheckVersion

    appState <- createAppState logger (configValue $ config ^. #localExceptions)
    database <- liftIO $ newTVarIO db
    let executableVersion = thisExecutableVersion

    pure AppContext {..}

createAppState :: MonadIO m => AppLogger -> [String] -> m AppState
createAppState logger localExceptions = do
    appState <- liftIO newAppState    

    -- Set the function that re-reads SLURM files with every re-validation.
    pure $ case localExceptions of
            []         -> appState
            slurmFiles -> appState & #readSlurm ?~ readSlurms slurmFiles    
  where
    readSlurms files = do
        logDebug logger [i|Reading SLURM files: #{files}.|]
        readSlurmFiles files


-- | Check some crucial things before running the validator
checkPreconditions :: CLIOptions -> ValidatorT IO ()
checkPreconditions CLIOptions {..} = checkRsyncInPath rsyncClientPath

deriveProverRunMode :: CLIOptions -> ValidatorT IO ProverRunMode
deriveProverRunMode CLIOptions {..} = 
    case (once, vrpOutput) of 
        (False, Nothing) -> pure ServerMode  
        (True, Just vo)  -> pure $ OneOffMode vo
        _                -> appError $ UnspecifiedE "options"
            [i|Options `--once` and `--vrp-output` must be either both set or both not set|]
            

-- | Run rpki-prover in a CLI mode for verifying RSC signature (*.sig file).
executeVerifier :: CLIOptions -> IO ()
executeVerifier cliOptions@CLIOptions {..} = do
    withLogConfig cliOptions $ \logConfig ->
        withLogger logConfig $ \logger ->
            withVerifier logger $ \verifyPath rscFile -> do
                logDebug logger [i|Verifying #{verifyPath} with RSC #{rscFile}.|]
                (ac, vs) <- runValidatorT (newScopes "Verify RSC") $ do
                                appContext <- createVerifierContext cliOptions logger
                                rscVerify appContext rscFile verifyPath
                case ac of
                    Left _ -> do
                        let report = formatValidations $ vs ^. #validations
                        logError logger [i|Verification failed: 
#{report}|]
                    Right _ ->
                        logInfo logger [i|Verification succeeded.|]
  where
    withVerifier logger f =
        case signatureFile of
            Nothing      -> logError logger "RSC file is not set."
            Just rscFile ->
                case (verifyDirectory, verifyFiles) of
                    (Nothing, [])  -> logError logger "Neither files nor directory for files to verify with RSC are not set."
                    (Nothing, vfs) -> f (FileList vfs) rscFile
                    (Just vd, [])  -> f (Directory vd) rscFile
                    _              -> logError logger "Both directory and list of files are set, leave just one of them to verify."


createVerifierContext :: CLIOptions -> AppLogger -> ValidatorT IO AppLmdbEnv
createVerifierContext cliOptions logger = do
    rootDir <- either id id <$> getRoot cliOptions
    cached <- fromEitherM $ first (InitE . InitError) <$> checkSubDirectory rootDir cacheDirName

    let config = defaultConfig
    lmdbEnv <- setupWorkerLmdbCache logger cached config

    (db, _) <- fromTry (InitE . InitError . fmtEx) $ 
                Lmdb.createDatabase lmdbEnv logger config Lmdb.DontCheckVersion

    appState <- liftIO newAppState
    database <- liftIO $ newTVarIO db
    let executableVersion = thisExecutableVersion

    pure AppContext {..}


-- CLI Options-related machinery
data CLIOptions = CLIOptions {
        -- Internal: identifies the worker process; not shown in --help
        worker                   :: Maybe String,
        version                  :: Bool,
        once                     :: Bool,
        vrpOutput                :: Maybe FilePath,
        noRirTals                :: Bool,
        refetchRirTals           :: Bool,
        rpkiRootDirectory        :: [FilePath],
        extraTalsDirectory       :: [FilePath],
        verifySignature          :: Bool,
        signatureFile            :: Maybe FilePath,
        verifyDirectory          :: Maybe FilePath,
        verifyFiles              :: [FilePath],
        cpuCount                 :: Maybe Natural,
        fetcherCount             :: Maybe Natural,
        resetCache               :: Bool,
        revalidationInterval     :: Maybe Int64,
        cacheLifetimeHours       :: Maybe Int64,
        rrdpRefreshInterval      :: Maybe Int64,
        rsyncRefreshInterval     :: Maybe Int64,
        rrdpTimeout              :: Maybe Int64,
        rsyncTimeout             :: Maybe Int64,
        rsyncClientPath          :: Maybe String,
        httpApiPort              :: Maybe Word16,
        lmdbSize                 :: Maybe Int64,
        withRtr                  :: Bool,
        rtrAddress               :: Maybe String,
        rtrPort                  :: Maybe Int16,
        rtrLogFile               :: Maybe String,
        logLevel                 :: Maybe String,
        strictManifestValidation :: Bool,
        allowOverclaiming        :: Bool,
        localExceptions          :: [String],
        maxTaRepositories        :: Maybe Int,
        maxCertificatePathDepth  :: Maybe Int,
        maxTotalTreeSize         :: Maybe Int,
        maxObjectSize            :: Maybe Integer,
        minObjectSize            :: Maybe Integer,
        topDownTimeout           :: Maybe Int64,
        noRrdp                   :: Bool,
        noRsync                  :: Bool,
        rsyncPrefetchUrl         :: [String],
        metricsPrefix            :: Maybe String,
        maxRrdpFetchMemory       :: Maybe Int,
        maxRsyncFetchMemory      :: Maybe Int,
        maxValidationMemory      :: Maybe Int,
        noIncrementalValidation  :: Bool,
        showHiddenConfig         :: Bool,
        withValidityApi          :: Bool,
        printConfig              :: Bool,
        heapProfileDir           :: Maybe FilePath
    }
    deriving stock (Show, Generic)

cliOptionsParser :: Parser CLIOptions
cliOptionsParser = CLIOptions
    <$> optional (strOption
            (  long "worker"
            <> internal
            <> metavar "WORKER-ID"))
    <*> switch
            (  long "version"
            <> help "Print program version and exit.")
    <*> switch
            (  long "once"
            <> help ("Run a single validation cycle and exit. "
                  <> "The HTTP API will not start, and the result will be written to the file "
                  <> "specified by --vrp-output (which must also be set)."))
    <*> optional (strOption
            (  long "vrp-output"
            <> metavar "FILE"
            <> help ("Path to the file where VRPs will be written. "
                  <> "Only used when --once is set. "
                  <> "VRPs are written in CSV format with TA names, duplicates are not removed.")))
    <*> switch
            (  long "no-rir-tals"
            <> help "If set, RIR TAL files will not be downloaded.")
    <*> switch
            (  long "refetch-rir-tals"
            <> help "If set, RIR TAL files will be forced to re-download.")
    <*> many (strOption
            (  long "rpki-root-directory"
            <> metavar "DIR"
            <> help ("Root directory (default: ${HOME}/.rpki/). "
                  <> "Can be passed multiple times; only the last value is used.")))
    <*> many (strOption
            (  long "extra-tals-directory"
            <> metavar "DIR"
            <> help ("Additional directories to search for TAL files. "
                  <> "By default, TALs are only loaded from $rpkiRootDirectory/tals.")))
    <*> switch
            (  long "verify-signature"
            <> help ("Run as a one-off RSC signature verifier instead of a server. "
                  <> "Requires the cache of validated RPKI objects to be present."))
    <*> optional (strOption
            (  long "signature-file"
            <> metavar "FILE"
            <> help "Path to the RSC signature file."))
    <*> optional (strOption
            (  long "verify-directory"
            <> metavar "DIR"
            <> help "Path to the directory containing files to verify using an RSC signature file."))
    <*> many (strOption
            (  long "verify-files"
            <> metavar "FILE"
            <> help "Files to verify using an RSC signature file. Can be specified multiple times."))
    <*> optional (option auto
            (  long "cpu-count"
            <> metavar "N"
            <> help ("Number of CPUs available to the program (default: " <> show defCpuCount <> "). "
                  <> "It is recommended to use the number of physical CPU cores rather than hyper-threads.")))
    <*> optional (option auto
            (  long "fetcher-count"
            <> metavar "N"
            <> help ("Maximum number of concurrent fetchers (default: " <> show defFetcherCount <> ", i.e. cpu-count * 2).")))
    <*> switch
            (  long "reset-cache"
            <> help "Reset the LMDB cache, removing ~/.rpki/cache/*.mdb files.")
    <*> optional (option auto
            (  long "revalidation-interval"
            <> metavar "SECONDS"
            <> help ("Interval between validation cycles in seconds (default: " <> show defRevalidation <> "). "
                  <> "Each cycle traverses the RPKI tree and gathers payloads (VRPs, ASPAs, BGPSec certificates). "
                  <> "Revalidations are also triggered by repository updates and object expirations.")))
    <*> optional (option auto
            (  long "cache-lifetime-hours"
            <> metavar "HOURS"
            <> help ("Lifetime of objects in the local cache in hours (default: " <> show defCacheLifetimeHours <> ").")))
    <*> optional (option auto
            (  long "rrdp-refresh-interval"
            <> metavar "SECONDS"
            <> help ("Time interval for updating RRDP repositories in seconds (default: " <> show defRrdpRefresh <> ").")))
    <*> optional (option auto
            (  long "rsync-refresh-interval"
            <> metavar "SECONDS"
            <> help ("Time interval for updating rsync repositories in seconds (default: " <> show defRsyncRefresh <> ").")))
    <*> optional (option auto
            (  long "rrdp-timeout"
            <> metavar "SECONDS"
            <> help ("Timeout for RRDP repository fetching in seconds (default: " <> show defRrdpTimeout <> "). "
                  <> "If a repository cannot be fetched within this period, it is considered unavailable.")))
    <*> optional (option auto
            (  long "rsync-timeout"
            <> metavar "SECONDS"
            <> help ("Timeout for rsync repository fetching in seconds (default: " <> show defRsyncTimeout <> "). "
                  <> "If a repository cannot be fetched within this period, it is considered unavailable.")))
    <*> optional (strOption
            (  long "rsync-client-path"
            <> metavar "PATH"
            <> help "Path to the rsync executable. Defaults to whatever is found in $PATH."))
    <*> optional (option auto
            (  long "http-api-port"
            <> metavar "PORT"
            <> help ("Port for the HTTP API (default: " <> show defHttpApiPort <> ").")))
    <*> optional (option auto
            (  long "lmdb-size"
            <> metavar "MB"
            <> help ("Maximum LMDB cache size in MB (default: " <> show defLmdbSize <> ", i.e. " <> show (defLmdbSize `div` 1024) <> "GB). "
                  <> "This is an upper limit; actual usage may be less. "
                  <> "About 1GB of cache is needed for each additional 24 hours of cache lifetime.")))
    <*> switch
            (  long "with-rtr"
            <> help "Start the RTR server (default: false).")
    <*> optional (strOption
            (  long "rtr-address"
            <> metavar "ADDRESS"
            <> help ("Address to bind the RTR server to (default: " <> defRtrAddress <> ").")))
    <*> optional (option auto
            (  long "rtr-port"
            <> metavar "PORT"
            <> help ("Port for the RTR server (default: " <> show defRtrPort <> ").")))
    <*> optional (strOption
            (  long "rtr-log-file"
            <> metavar "FILE"
            <> help "Path for the RTR log file (default: stdout, shared with general output)."))
    <*> optional (strOption
            (  long "log-level"
            <> metavar "LEVEL"
            <> help "Log level: 'error', 'warn', 'info', or 'debug' (case-insensitive, default: info)."))
    <*> switch
            (  long "strict-manifest-validation"
            <> help ("Use strict RFC 6486 manifest validation "
                  <> "(https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/, section 6.4). "
                  <> "Default is RFC 9286."))
    <*> switch
            (  long "allow-overclaiming"
            <> help ("Use the 'validation reconsidered' algorithm for certificate resource sets "
                  <> "instead of the strict version. Default: false (strict version is used)."))
    <*> many (strOption
            (  long "local-exceptions"
            <> metavar "FILE"
            <> help ("Path to a local exception file in SLURM format (RFC 8416). "
                  <> "Can be specified multiple times; contents are merged before application.")))
    <*> optional (option auto
            (  long "max-ta-repositories"
            <> metavar "N"
            <> help ("Maximum number of new repositories per TA added during a validation run (default: " <> show defMaxTaRepos <> ").")))
    <*> optional (option auto
            (  long "max-certificate-path-depth"
            <> metavar "N"
            <> help ("Maximum depth of the certificate path from the TA certificate (default: " <> show defMaxCertDepth <> ").")))
    <*> optional (option auto
            (  long "max-total-tree-size"
            <> metavar "N"
            <> help ("Maximum total number of objects in the tree for a single TA (default: " <> show defMaxTotalTree <> ").")))
    <*> optional (option auto
            (  long "max-object-size"
            <> metavar "BYTES"
            <> help ("Maximum size of any object in bytes (default: " <> show defMaxObjSize <> ", i.e. " <> show (defMaxObjSize `div` (1024*1024)) <> "MB).")))
    <*> optional (option auto
            (  long "min-object-size"
            <> metavar "BYTES"
            <> help ("Minimum size of any object in bytes (default: " <> show defMinObjSize <> ").")))
    <*> optional (option auto
            (  long "top-down-timeout"
            <> metavar "SECONDS"
            <> help ("Timeout for validating a single TA in seconds (default: " <> show defTopDownTimeout <> ", i.e. 1 hour).")))
    <*> switch
            (  long "no-rrdp"
            <> help "Disable fetching RRDP repositories (default: false).")
    <*> switch
            (  long "no-rsync"
            <> help "Disable fetching rsync repositories (default: false).")
    <*> many (strOption
            (  long "rsync-prefetch-url"
            <> metavar "URL"
            <> help ("rsync repositories to fetch instead of their children. "
                  <> "Defaults include common RPKI rsync URLs.")))
    <*> optional (strOption
            (  long "metrics-prefix"
            <> metavar "PREFIX"
            <> help ("Prefix for Prometheus metrics (default: '" <> Text.unpack defMetricsPrefix <> "').")))
    <*> optional (option auto
            (  long "max-rrdp-fetch-memory"
            <> metavar "MB"
            <> help ("Maximum memory for the RRDP fetcher process in MB (default: " <> show defMaxRrdpMem <> ").")))
    <*> optional (option auto
            (  long "max-rsync-fetch-memory"
            <> metavar "MB"
            <> help ("Maximum memory for the rsync fetcher process in MB (default: " <> show defMaxRsyncMem <> ").")))
    <*> optional (option auto
            (  long "max-validation-memory"
            <> metavar "MB"
            <> help ("Maximum memory for the validation process in MB (default: " <> show defMaxValidMem <> ").")))
    <*> switch
            (  long "no-incremental-validation"
            <> help ("Disable the incremental validation algorithm. "
                  <> "Incremental validation is enabled by default."))
    <*> switch
            (  long "show-hidden-config"
            <> help ("Show all configuration values in the /api/system HTTP API call. "
                  <> "May expose local filesystem paths. Default: false."))
    <*> switch
            (  long "with-validity-api"
            <> help ("Enable the VRP index used by the REST API endpoint at /api/validity (default: false). "
                  <> "Increases memory usage (about 200-300MB in the main process) "
                  <> "and CPU usage (about 2 seconds per validation cycle)."))
    <*> switch
            (  long "print-config"
            <> help "Print the effective configuration derived from CLI options and exit.")
    <*> optional (strOption
            (  long "heap-profile-dir"
            <> metavar "DIR"
            <> help ("When set, each worker subprocess writes a GHC heap-profiling eventlog "
                  <> "(.eventlog) to this directory.  The binary must be compiled with "
                  <> "-finfo-table-map (the default).  Convert the result with: "
                  <> "eventlog2html <file>.eventlog")))
  where
    cfg    = defaultConfig
    rtrCfg = defaultRtrConfig
    defCpuCount               = cfg ^. #parallelism . #cpuCount
    defFetcherCount           = cfg ^. #parallelism . #fetchParallelism
    Seconds defRevalidation   = cfg ^. #validationConfig . #revalidationInterval
    Seconds defCacheLifetime  = cfg ^. #longLivedCacheLifeTime
    defCacheLifetimeHours     = defCacheLifetime `div` 3600
    Seconds defRrdpRefresh    = cfg ^. #validationConfig . #rrdpRepositoryRefreshInterval
    Seconds defRsyncRefresh   = cfg ^. #validationConfig . #rsyncRepositoryRefreshInterval
    Seconds defRrdpTimeout    = cfg ^. #rrdpConf . #rrdpTimeout
    Seconds defRsyncTimeout   = cfg ^. #rsyncConf . #rsyncTimeout
    defHttpApiPort            = cfg ^. #httpApiConf . #port
    defLmdbSize               = unSize $ cfg ^. #lmdbSizeMb
    defRtrAddress             = rtrCfg ^. #rtrAddress
    defRtrPort                = rtrCfg ^. #rtrPort
    defMaxTaRepos             = cfg ^. #validationConfig . #maxTaRepositories
    defMaxCertDepth           = cfg ^. #validationConfig . #maxCertificatePathDepth
    defMaxTotalTree           = cfg ^. #validationConfig . #maxTotalTreeSize
    defMaxObjSize             = cfg ^. #validationConfig . #maxObjectSize
    defMinObjSize             = cfg ^. #validationConfig . #minObjectSize
    Seconds defTopDownTimeout = cfg ^. #validationConfig . #topDownTimeout
    defMetricsPrefix          = cfg ^. #metricsPrefix
    defMaxRrdpMem             = cfg ^. #systemConfig . #rrdpWorkerMemoryMb
    defMaxRsyncMem            = cfg ^. #systemConfig . #rsyncWorkerMemoryMb
    defMaxValidMem            = cfg ^. #systemConfig . #validationWorkerMemoryMb


-- | Apply CLI option overrides to a base Config. The base config should
-- already contain any IO-derived values (paths, run mode, etc.).
-- The 'cpuCount' field of 'parallelism' in the base config is used as
-- the fallback when '--cpu-count' is not supplied.
applyCliToConfig :: Config -> CLIOptions -> (forall a . a -> ApiSecured a) -> Config
applyCliToConfig baseConfig CLIOptions{..} apiSecured = 
    adjustConfig $ baseConfig
        & #parallelism .~ parallelism
        & #rsyncConf . #rsyncClientPath .~ fmap apiSecured rsyncClientPath
        & #rsyncConf . #enabled .~ not noRsync
        & maybeSet (#rsyncConf . #rsyncTimeout) (Seconds <$> rsyncTimeout)
        & #rrdpConf . #enabled .~ not noRrdp
        & maybeSet (#rrdpConf . #rrdpTimeout) (Seconds <$> rrdpTimeout)
        & maybeSet (#validationConfig . #revalidationInterval) (Seconds <$> revalidationInterval)
        & maybeSet (#validationConfig . #rrdpRepositoryRefreshInterval) (Seconds <$> rrdpRefreshInterval)
        & maybeSet (#validationConfig . #rsyncRepositoryRefreshInterval) (Seconds <$> rsyncRefreshInterval)
        & #validationConfig . #manifestProcessing .~
                (if strictManifestValidation then RFC6486_Strict else RFC9286)
        & #validationConfig . #validationAlgorithm .~
                (if noIncrementalValidation then FullEveryIteration else Incremental)
        & #validationConfig . #validationRFC .~
                (if allowOverclaiming then ReconsideredRFC else StrictRFC)
        & maybeSet (#validationConfig . #topDownTimeout) (Seconds <$> topDownTimeout)
        & maybeSet (#validationConfig . #maxTaRepositories) maxTaRepositories
        & maybeSet (#validationConfig . #maxCertificatePathDepth) maxCertificatePathDepth
        & maybeSet (#validationConfig . #maxTotalTreeSize) maxTotalTreeSize
        & maybeSet (#validationConfig . #maxObjectSize) maxObjectSize
        & maybeSet (#validationConfig . #minObjectSize) minObjectSize
        & maybeSet (#httpApiConf . #port) httpApiPort
        & #rtrConfig .~ rtrConfig
        & maybeSet #longLivedCacheLifeTime ((\hours -> Seconds (hours * 60 * 60)) <$> cacheLifetimeHours)
        & #lmdbSizeMb .~ lmdbRealSize
        & #localExceptions .~ apiSecured localExceptions
        & #withValidityApi .~ withValidityApi
        & maybeSet #metricsPrefix (convert <$> metricsPrefix)
        & maybeSet (#systemConfig . #rsyncWorkerMemoryMb) maxRsyncFetchMemory
        & maybeSet (#systemConfig . #rrdpWorkerMemoryMb) maxRrdpFetchMemory
        & maybeSet (#systemConfig . #validationWorkerMemoryMb) maxValidationMemory
        & #systemConfig . #heapProfileDir .~ heapProfileDir
  where
    lmdbRealSize = (Size <$> lmdbSize) `orDefault` (baseConfig ^. #lmdbSizeMb)
    cpuCount'    = fromMaybe (baseConfig ^. #parallelism . #cpuCount) cpuCount
    parallelism  = case fetcherCount of
        Nothing -> newParallelism cpuCount'
        Just fc -> makeParallelismF cpuCount' fc
    rtrConfig = if withRtr
        then Just $ defaultRtrConfig
                    & maybeSet #rtrPort rtrPort
                    & maybeSet #rtrAddress rtrAddress
                    & #rtrLogFile .~ rtrLogFile
        else Nothing    

withLogConfig :: CLIOptions -> (LogConfig -> IO ()) -> IO ()
withLogConfig CLIOptions{..} f =
    case logLevel of
        Nothing -> run defaultsLogLevel
        Just s  ->
            case Text.toLower $ Text.pack s of
                "error" -> run ErrorL
                "warn"  -> run WarnL
                "info"  -> run InfoL
                "debug" -> run DebugL
                other   -> hPutStrLn stderr $ "Invalid log level: " <> Text.unpack other
  where
    run logLev = f $ newLogConfig logLev logType
      where
        logType = case (rtrLogFile, worker) of 
            (Just fs, Nothing) -> MainLogWithRtr fs
            (Nothing, Nothing) -> MainLog
            (_,        Just _) -> WorkerLog
            
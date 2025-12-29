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
import           GHC.TypeLits

import qualified Network.Wai.Handler.Warp         as Warp

import           System.Directory
import           System.Environment
import           System.FilePath                  ((</>))
import           System.IO                        
import           System.Exit

import           Options.Generic

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
    cliOptions@CLIOptions{..} <- unwrapRecord $ 
            "RPKI prover, relying party software for RPKI, version " <> rpkiProverVersion

    if version        
        then do
            -- it is "--version" call, so print the version and exit
            putStrLn $ convert rpkiProverVersion
        else do
            case worker of
                Nothing ->
                    if verifySignature
                        -- this is a call to RSC verification 
                        then executeVerifier cliOptions
                        -- this is a normal validator daemon launch
                        else executeMainProcess cliOptions
                Just _ ->
                    executeWorkerProcess


executeMainProcess :: CLIOptions Unwrapped -> IO ()
executeMainProcess cliOptions@CLIOptions{..} = do     
    -- TODO This doesn't look pretty, come up with something better.
    appStateHolder <- newTVarIO Nothing

    let withApp f = do 
            z <- readTVarIO appStateHolder
            for_ z f    

    withLogConfig cliOptions $ \logConfig_ -> do
        let logConfig = logConfig_
                & #metricsHandler .~ withApp . mergeSystemMetrics
                & #workerHandler .~ withApp . updateRunningWorkers
                & #systemStatusHandler .~ withApp . updateSystemStatus

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
    let config = input ^. typed @Config    
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
                    let actuallyExecuteWork = 
                            case input ^. #params of
                                RrdpFetchParams {..} -> 
                                    exec resultHandler $ fmap RrdpFetchResult $ runValidatorT scopes $ 
                                        updateRrdpRepository appContext worldVersion rrdpRepository

                                RsyncFetchParams {..} -> 
                                    exec resultHandler $ fmap RsyncFetchResult $ runValidatorT scopes $                                     
                                        updateObjectForRsyncRepository appContext fetchConfig 
                                            worldVersion rsyncRepository

                                CompactionParams {..} -> 
                                    exec resultHandler $ 
                                        CompactionResult <$> copyLmdbEnvironment appContext targetLmdbEnv

                                ValidationParams {..} -> 
                                    exec resultHandler $ do 
                                        (vs, discoveredRepositories, slurm) <- 
                                            runValidation appContext worldVersion talsToValidate allTaNames
                                        pure $ ValidationResult vs discoveredRepositories slurm

                                CacheCleanupParams {..} -> 
                                    exec resultHandler $
                                        CacheCleanupResult <$> runCacheCleanup appContext worldVersion
                    actuallyExecuteWork
                        `catch` (\(_ :: TxTimeout) -> 
                                    pushSystemStatus logger $ SystemStatusMessage $ SystemState { dbState = DbStuck })
                        `finally` 
                            -- There's a short window between opening LMDB and not yet having AppContext 
                            -- constructed when an exception will not result in the database closed. It is not good, 
                            -- but we are trying to solve the problem of interrupted RW transactions leaving the DB 
                            -- in broken/locked state, and no transactions are possible within this window.                                
                            closeStorage appContext                                    
                        
  where    
    exec resultHandler f = resultHandler =<< execWithStats f                    


turnOffTlsValidation :: IO ()
turnOffTlsValidation = do 
    manager <- newManager $ mkManagerSettings (TLSSettingsSimple True True True) Nothing 
    setGlobalManager manager    


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


createAppContext :: CLIOptions Unwrapped -> AppLogger -> LogLevel -> ValidatorT IO AppLmdbEnv
createAppContext cliOptions@CLIOptions{..} logger derivedLogLevel = do

    programPath <- liftIO getExecutablePath

    let defaults = defaultConfig
    let lmdbRealSize = (Size <$> lmdbSize) `orDefault` (defaults ^. #lmdbSizeMb)    

    -- Create (or make sure exist) necessary directories in the root directory
    (root, tald, rsyncd, tmpd, cached) <- fsLayout cliOptions logger

    -- Set capabilities to the values from the CLI or to all available CPUs,
    -- (disregard the HT issue for now it needs more testing).
    let cpuCount' = fromMaybe getRtsCpuCount cpuCount
    liftIO $ setCpuCount cpuCount'    
    let parallelism = 
            case fetcherCount of 
                Nothing -> makeParallelism cpuCount'
                Just fc -> makeParallelismF cpuCount' fc

    let rtrConfig = if withRtr
            then Just $ defaultRtrConfig
                        & maybeSet #rtrPort rtrPort
                        & maybeSet #rtrAddress rtrAddress
                        & #rtrLogFile .~ rtrLogFile
            else Nothing        

    proverRunMode     <- deriveProverRunMode cliOptions  
    rsyncPrefetchUrls <- rsyncPrefetches cliOptions                

    let apiSecured :: a -> ApiSecured a
        apiSecured a = if showHiddenConfig then Public a else Hidden a

    let config = adjustConfig $ defaults
            & #programBinaryPath .~ apiSecured programPath
            & #rootDirectory .~ apiSecured root
            & #talDirectory .~ apiSecured tald
            & #tmpDirectory .~ apiSecured tmpd
            & #cacheDirectory .~ apiSecured cached
            & #extraTalsDirectories .~ apiSecured extraTalsDirectory
            & #proverRunMode .~ proverRunMode
            & #parallelism .~ parallelism
            & #rsyncConf . #rsyncRoot .~ apiSecured rsyncd
            & #rsyncConf . #rsyncClientPath .~ fmap apiSecured rsyncClientPath
            & #rsyncConf . #enabled .~ not noRsync
            & #rsyncConf . #rsyncPrefetchUrls .~ rsyncPrefetchUrls
            & maybeSet (#rsyncConf . #rsyncTimeout) (Seconds <$> rsyncTimeout)
            & #rrdpConf . #tmpRoot .~ apiSecured tmpd
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
            & #logLevel .~ derivedLogLevel
            & #withValidityApi .~ withValidityApi
            & maybeSet #metricsPrefix (convert <$> metricsPrefix)
            & maybeSet (#systemConfig . #rsyncWorkerMemoryMb) maxRsyncFetchMemory
            & maybeSet (#systemConfig . #rrdpWorkerMemoryMb) maxRrdpFetchMemory
            & maybeSet (#systemConfig . #validationWorkerMemoryMb) maxValidationMemory            
        
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

      
fsLayout :: CLIOptions Unwrapped
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


getRoot :: CLIOptions Unwrapped -> ValidatorT IO (Either FilePath FilePath)
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

checkSubDirectory :: FilePath -> FilePath -> IO (Either Text FilePath)
checkSubDirectory root sub = do
    let subDirectory = root </> sub
    doesDirectoryExist subDirectory >>= \case
        False -> pure $ Left [i|Directory #{subDirectory} doesn't exist|]
        True  -> pure $ Right subDirectory

createSubDirectoryIfNeeded :: FilePath -> FilePath -> IO (Either Text FilePath)
createSubDirectoryIfNeeded root sub = do
    let subDirectory = root </> sub
    exists <- doesDirectoryExist subDirectory
    unless exists $ createDirectory subDirectory
    pure $ Right subDirectory

getRootDirectory :: CLIOptions Unwrapped -> Maybe FilePath
getRootDirectory CLIOptions{..} =
    case rpkiRootDirectory of
        [] -> Nothing
        s  -> Just $ Prelude.last s

-- Set rsync prefetch URLs
rsyncPrefetches :: CLIOptions Unwrapped -> ValidatorT IO [RsyncURL]
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
checkPreconditions :: CLIOptions Unwrapped -> ValidatorT IO ()
checkPreconditions CLIOptions {..} = checkRsyncInPath rsyncClientPath

deriveProverRunMode :: CLIOptions Unwrapped -> ValidatorT IO ProverRunMode
deriveProverRunMode CLIOptions {..} = 
    case (once, vrpOutput) of 
        (False, Nothing) -> pure ServerMode  
        (True, Just vo)  -> pure $ OneOffMode vo
        _                -> appError $ UnspecifiedE "options"
            [i|Options `--once` and `--vrp-output` must be either both set or both not set|]
            

-- | Run rpki-prover in a CLI mode for verifying RSC signature (*.sig file).
executeVerifier :: CLIOptions Unwrapped -> IO ()
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


createVerifierContext :: CLIOptions Unwrapped -> AppLogger -> ValidatorT IO AppLmdbEnv
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
data CLIOptions wrapped = CLIOptions {

    -- It is not documented since it's for internal machinery
    worker :: wrapped ::: Maybe String,

    version :: wrapped ::: Bool <?> "Program version.",

    once :: wrapped ::: Bool <?>
        ("If set, runs a single validation cycle and exits." +++
         " The HTTP API will not start, and the result will be written to the file" +++
         " specified by the --vrp-output option (which must also be set)."),

    vrpOutput :: wrapped ::: Maybe FilePath <?> 
        ("Path to the file where VRPs will be written." +++
         " This is only used when the --once option is set." +++
         " VRPs are written in CSV format with TA names and included 'as is'," +++
         " meaning duplicates within or between TAs are not removed."),

    noRirTals :: wrapped ::: Bool <?> 
        "If set, RIR TAL files will not be downloaded.",

    refetchRirTals :: wrapped ::: Bool <?> 
        "If set, RIR TAL files will be forced to re-download.",

    rpkiRootDirectory :: wrapped ::: [FilePath] <?>
        ("Root directory (default is ${HOME}/.rpki/)." +++
         " This option can be passed multiple times, but only the last value is used." +++
         " This allows for easy overriding, for example, in Docker."),

    extraTalsDirectory :: wrapped ::: [FilePath] <?>
        ("Additional directories to search for TAL files." +++
         " By default, no extra directories are used;" +++
         " TALs are only loaded from the $rpkiRootDirectory/tals directory."),

    verifySignature :: wrapped ::: Bool <?>
        ("Runs as a one-off RSC signature file verifier instead of a server." +++
         " Requires the cache of validated RPKI objects and VRPs to be present." +++
         " This should be run alongside a running daemon instance of rpki-prover."),

    signatureFile :: wrapped ::: Maybe FilePath <?>
        "Path to the RSC signature file.",

    verifyDirectory :: wrapped ::: Maybe FilePath <?>
        "Path to the directory containing files to be verified using an RSC signature file.",

    verifyFiles :: wrapped ::: [FilePath] <?>
        "Files to verify using an RSC signature file. Multiple files may be specified.",

    cpuCount :: wrapped ::: Maybe Natural <?>
        ("Number of CPUs available to the program (default is 2). Note that higher values lead to larger " +++ 
        "memory usage. It's recommended to specify the number of physical CPU cores rather than (hyper-)threads, " +++
        "as the latter may degrade performance."),

    fetcherCount :: wrapped ::: Maybe Natural <?>
        "Maximum number of concurrent fetchers (default is --cpu-count * 3).",

    resetCache :: wrapped ::: Bool <?>
        "Reset the LMDB cache, that is, remove ~/.rpki/cache/*.mdb files.",

    revalidationInterval :: wrapped ::: Maybe Int64 <?>
        ("Interval between validation cycles in seconds. " +++
         "Each cycle traverses the RPKI tree and gathers payloads (VRPs, ASPAs, BGPSec certificates). " +++
         "Default is 15 minutes (900 seconds). Note that revalidations are also triggered by repository " +++ 
         "updates and object expirations, so this interval is the maximum time between validation cycles."),

    cacheLifetimeHours :: wrapped ::: Maybe Int64 <?>
        "Lifetime of objects in the local cache, in hours (default is 72).",

    rrdpRefreshInterval :: wrapped ::: Maybe Int64 <?>
        "Time interval for updating RRDP repositories in seconds (default is 120).",

    rsyncRefreshInterval :: wrapped ::: Maybe Int64 <?>
        "Time interval for updating rsync repositories in seconds (default is 11 minutes, that is, 660 seconds).",

    rrdpTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timeout for RRDP repository fetching in seconds." +++
         " If a repository cannot be fetched within this period," +++
         " it is considered unavailable and the fetch is aborted."),

    rsyncTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timeout for rsync repository fetching in seconds." +++
         " If a repository cannot be fetched within this period," +++
         " it is considered unavailable and the fetch is aborted."),

    rsyncClientPath :: wrapped ::: Maybe String <?>
        "Path to the rsync executable. Defaults to whatever is found in $PATH.",

    httpApiPort :: wrapped ::: Maybe Word16 <?>
        "Port for the HTTP API (default is 9999).",

    lmdbSize :: wrapped ::: Maybe Int64 <?>
        ("Maximum LMDB cache size in MB (default is 32768, that is, 32GB)." +++
         " Note: (a) this is the upper limit; actual usage may be less;" +++
         " (b) about 1GB of cache is needed for each additional 24 hours of cache lifetime."),

    withRtr :: wrapped ::: Bool <?>
        "Start the RTR server (default is false).",

    rtrAddress :: wrapped ::: Maybe String <?>
        "Address to bind the RTR server to (default is localhost).",

    rtrPort :: wrapped ::: Maybe Int16 <?>
        "Port for the RTR server (default is 8283).",

    rtrLogFile :: wrapped ::: Maybe String <?>
        "Path for the RTR log file (default is stdout, shared with general output).",

    logLevel :: wrapped ::: Maybe String <?>
        "Log level: 'error', 'warn', 'info', or 'debug' (case-insensitive). Default is 'info'.",

    strictManifestValidation :: wrapped ::: Bool <?>
        ("Use strict RFC 6486 (https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/, section 6.4) " +++ 
        "for manifest validation (default is false). The currently used default is defined in " +++
        "RFC 9286 (https://www.rfc-editor.org/rfc/rfc9286.html#name-relying-party-processing-of)."),

    allowOverclaiming :: wrapped ::: Bool <?>
        ("Use the 'validation reconsidered' algorithm to validate certificate resource sets " +++
         "instead of the strict version. Default is false, meaning the strict version is used."),

    localExceptions :: wrapped ::: [String] <?>
        ("Paths to local exception files in SLURM format (RFC 8416)." +++
         " Multiple files can be specified and their contents are merged internally before application."),

    maxTaRepositories :: wrapped ::: Maybe Int <?>
        "Maximum number of new repositories per TA added during a validation run (default is 1000).",

    maxCertificatePathDepth :: wrapped ::: Maybe Int <?>
        "Maximum depth of certificate path from the TA certificate to the RPKI tree (default is 32).",

    maxTotalTreeSize :: wrapped ::: Maybe Int <?>
        ("Maximum total size of the object tree for a single TA, across all object types " +++
        "(default is 5,000,000)."),

    maxObjectSize :: wrapped ::: Maybe Integer <?>
        ("Maximum size of any object (certificate, CRL, MFT, GBR, ROA, ASPA), in bytes" +++
         " (default is 32MB, that is, 33554432 bytes)."),

    minObjectSize :: wrapped ::: Maybe Integer <?>
        "Minimum size of any object, in bytes (default is 300).",

    topDownTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timeout for validating a single TA, in seconds (default is 1 hour, that is, 3600 seconds)."),

    noRrdp :: wrapped ::: Bool <?>
        "Disable fetching RRDP repositories (default is false).",

    noRsync :: wrapped ::: Bool <?>
        "Disable fetching rsync repositories (default is false).",

    rsyncPrefetchUrl :: wrapped ::: [String] <?>
        ("rsync repositories to fetch instead of their children. " +++
         "Defaults include common RPKI rsync URLs. This is an optimization and is rarely needed."),

    metricsPrefix :: wrapped ::: Maybe String <?>
        "Prefix for Prometheus metrics (default is 'rpki_prover').",

    maxRrdpFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximum memory allocation (in MB) for the RRDP fetcher process (default is 1024).",

    maxRsyncFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximum memory allocation (in MB) for the rsync fetcher process (default is 1024).",

    maxValidationMemory :: wrapped ::: Maybe Int <?>
        "Maximum memory allocation (in MB) for the validation process (default is 2048).",

    noIncrementalValidation :: wrapped ::: Bool <?>
        ("Disable the incremental validation algorithm." +++
         " Incremental validation is enabled by default, so the default for this option is false."),

    showHiddenConfig :: wrapped ::: Bool <?>
        ("Show all configuration values in the /api/system HTTP API call. " +++
         "This may pose a security risk, as some values may contain local filesystem paths. " +++
         "Default is false."),

    withValidityApi :: wrapped ::: Bool <?>
        ("Enable the VRP index used by the REST API endpoint at /api/validity (default is false). " +++
         "Note: Enabling this option increases memory usage (about 200-300MB in the main process) " +++
         "and CPU usage (about 2 seconds per validation cycle).")

    }
    deriving (Generic)

instance ParseRecord (CLIOptions Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (CLIOptions Unwrapped)

type (+++) (a :: Symbol) (b :: Symbol) = AppendSymbol a b

withLogConfig :: CLIOptions Unwrapped -> (LogConfig -> IO ()) -> IO ()
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
            
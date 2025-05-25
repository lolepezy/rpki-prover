{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

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

    let bumpSysMetric sm = do 
            z <- readTVarIO appStateHolder
            for_ z $ mergeSystemMetrics sm

    let updateWorkers wi = do 
            z <- readTVarIO appStateHolder
            -- We only keep track of running rsync clients
            for_ z $ updateRsyncClient wi

    withLogConfig cliOptions $ \lc -> do
        let logConfig = lc
                & #metricsHandler .~ bumpSysMetric
                & #workerHandler .~ updateWorkers

        -- This one modifies system metrics in AppState
        -- if appState is actually initialised
        withLogger logConfig $ \logger -> do
            logDebug logger $ if once 
                    then [i|Starting #{rpkiProverVersion} in one-off mode.|]
                    else [i|Starting #{rpkiProverVersion} as a server.|]
            
            (appContext, validations) <- do
                        runValidatorT (newScopes "Startup") $ do
                            checkPreconditions cliOptions
                            createAppContext cliOptions logger (logConfig ^. #logLevel)
            case appContext of
                Left _ -> do 
                    logError logger [i|Failure:
#{formatValidations (validations ^. typed)}|]
                    drainLog logger
                    hFlush stdout
                    hFlush stderr
                    threadDelay 100_000
                    exitFailure
                Right appContext' -> do 
                    -- now we have the appState, set appStateHolder
                    atomically $ writeTVar appStateHolder $ Just $ appContext' ^. #appState
                    tals <- readTALs appContext'
                    if once 
                        then runValidatorServer appContext' tals
                        else do                             
                            void $ race
                                (runHttpApi appContext' tals)
                                (runValidatorServer appContext' tals)

executeWorkerProcess :: IO ()
executeWorkerProcess = do
    input <- readWorkerInput
    let config = input ^. typed @Config    
    let logConfig = makeLogConfig (config ^. #logLevel) WorkerLog
                    
    -- turnOffTlsValidation

    executeWork input $ \_ resultHandler -> 
        withLogger logConfig $ \logger -> liftIO $ do
            (z, validations) <- runValidatorT
                                    (newScopes "worker-create-app-context")
                                    (createWorkerAppContext config logger)
            case z of
                Left e ->
                    logError logger [i|Couldn't initialise: #{e}, problems: #{validations}.|]
                Right appContext ->                    
                    case input ^. #params of
                        RrdpFetchParams {..} -> exec resultHandler $
                            fmap RrdpFetchResult $ runValidatorT scopes $ 
                                updateObjectForRrdpRepository appContext worldVersion rrdpRepository

                        RsyncFetchParams {..} -> exec resultHandler $
                            fmap RsyncFetchResult $ runValidatorT scopes $ 
                                updateObjectForRsyncRepository appContext fetchConfig worldVersion rsyncRepository

                        CompactionParams {..} -> exec resultHandler $
                            CompactionResult <$> copyLmdbEnvironment appContext targetLmdbEnv

                        ValidationParams {..} -> exec resultHandler $ do 
                            (vs, discoveredRepositories, slurm) <- 
                                runValidation appContext worldVersion talsToValidate allTaNames
                            pure $ ValidationResult vs discoveredRepositories slurm

                        CacheCleanupParams {..} -> exec resultHandler $
                            CacheCleanupResult <$> runCacheCleanup appContext worldVersion
  where    
    exec resultHandler f = resultHandler =<< execWithStats f                    


turnOffTlsValidation :: IO ()
turnOffTlsValidation = do 
    manager <- newManager $ mkManagerSettings (TLSSettingsSimple True True True) Nothing 
    setGlobalManager manager    


readTALs :: (Storage s, MaintainableStorage s) => AppContext s -> IO [TAL]
readTALs AppContext {..} = do
    
    logInfo logger [i|Reading TAL files from #{talDirectory config}|]
    -- worldVersion  <- newWorldVersion

    -- Check that TAL names are unique
    let talSourcesDirs = (configValue $ config ^. #talDirectory) 
                       : (configValue $ config ^. #extraTalsDirectories)
    talNames <- fmap mconcat $ mapM listTalFiles talSourcesDirs    
    when (Set.size (Set.fromList talNames) < length talNames) $ do
        let message = [i|TAL names are not unique #{talNames}, |] <> 
                      [i|TAL are from directories #{talSourcesDirs}.|]
        logError logger message
        throwIO $ AppException $ TAL_E $ TALError message

    (tals, vs) <- runValidatorT (newScopes "validation-root") $
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


runValidatorServer :: (Storage s, MaintainableStorage s) => AppContext s -> [TAL] -> IO ()
runValidatorServer appContext tals =     
    runWorkflow appContext tals
        `finally`
        closeStorage appContext


runHttpApi :: (Storage s, MaintainableStorage s) => AppContext s -> [TAL] -> IO ()
runHttpApi appContext@AppContext {..} tals = do 
    let httpPort = fromIntegral $ appContext ^. typed @Config . typed @HttpApiConfig . #port
    (Warp.run httpPort $ httpServer appContext tals) 
        `catch` 
        (\(e :: SomeException) -> logError logger [i|Could not start HTTP server: #{e}.|])


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

    let config = defaults
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
            & #validationConfig . #fetchIntervalCalculation .~ 
                (if noAdaptiveFetchIntervals then Constant else Adaptive)
            & #validationConfig . #fetchTimeoutCalculation .~ 
                (if noAdaptiveFetchTimeouts then Constant else Adaptive)                    
            & maybeSet (#httpApiConf . #port) httpApiPort
            & #rtrConfig .~ rtrConfig
            & maybeSet #cacheLifeTime ((\hours -> Seconds (hours * 60 * 60)) <$> cacheLifetimeHours)
            & maybeSet #versionNumberToKeep versionNumberToKeep
            & #lmdbSizeMb .~ lmdbRealSize
            & #localExceptions .~ apiSecured localExceptions
            & #logLevel .~ derivedLogLevel
            & #withValidityApi .~ not noValidityApi
            & maybeSet #metricsPrefix (convert <$> metricsPrefix)
            & maybeSet (#systemConfig . #rsyncWorkerMemoryMb) maxRsyncFetchMemory
            & maybeSet (#systemConfig . #rrdpWorkerMemoryMb) maxRrdpFetchMemory
            & maybeSet (#systemConfig . #validationWorkerMemoryMb) maxValidationMemory            
        
    -- Do some "common sense" adjustmnents to the config for correctness
    let adjustedConfig = config 
            -- Cache must be cleaned up at least as often as the 
            -- lifetime of the objects in it    
            & #cacheCleanupInterval %~ (`min` (config ^. #cacheLifeTime))

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
                    adjustedConfig

    (db, dbCheck) <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv logger Lmdb.CheckVersion
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

        -- Nothing special, the cache is totally normal
        Lmdb.WasCompatible    -> pure ()

    logInfo logger [i|Created application context with configuration: 
#{shower (adjustedConfig)}|]
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
                        Text.intercalate "\n" $ catMaybes $ map talText httpStatuses                                        


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
        rootExists <- liftIO $ doesDirectoryExist root
        if rootExists
            then pure root
            else appError $ InitE $ InitError [i|Root directory #{root} doesn't exist.|]

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

    (db, _) <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv logger Lmdb.DontCheckVersion

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

    (db, _) <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv logger Lmdb.DontCheckVersion

    appState <- liftIO newAppState
    database <- liftIO $ newTVarIO db
    let executableVersion = thisExecutableVersion

    pure AppContext {..}


-- CLI Options-related machinery
data CLIOptions wrapped = CLIOptions {

    -- It is not documented since it's for internal machinery
    worker :: wrapped ::: Maybe String,

    version :: wrapped ::: Bool <?> "Program version.",

    initialise :: wrapped ::: Bool <?>
        "Deprecated, does nothing, used to initialise FS layout before the first launch.",

    once :: wrapped ::: Bool <?>
        ("If set, will run one validation cycle and exit. Http API will not start, " +++ 
         "result will be written to the file set by --vrp-output option (which must also be set)."),

    vrpOutput :: wrapped ::: Maybe FilePath <?> 
        "Path of the file to write VRPs to. Only effectful when --once option is set.",

    noRirTals :: wrapped ::: Bool <?> 
        "If set, RIR TAL files will not be downloaded.",

    refetchRirTals :: wrapped ::: Bool <?> 
        "If set, RIR TAL files will be re-downloaded.",        

    rpkiRootDirectory :: wrapped ::: [FilePath] <?>
        ("Root directory (default is ${HOME}/.rpki/). This option can be passed multiple times and "
         +++ "the last one will be used, it is done for convenience of overriding this option with dockerised version."),

    extraTalsDirectory :: wrapped ::: [FilePath] <?>
        ("And extra directories where to look for TAL files. By default there is none, " +++ 
         "TALs are picked up only from the $rpkiRootDirectory/tals directory"),

    verifySignature :: wrapped ::: Bool <?>
        ("Work as a one-off RSC signature file verifier, not as a server. To work as a verifier it needs the cache " +++
        "of validated RPKI objects and VRPs to exist and be populateds. So verifier can (and should) run next to " +++
        "the running daemon instance of rpki-prover"),

    signatureFile :: wrapped ::: Maybe FilePath <?> ("Path to the RSC signature file."),

    verifyDirectory :: wrapped ::: Maybe FilePath <?>
        ("Path to the directory with the files to be verified using and RSC signaure file."),

    verifyFiles :: wrapped ::: [FilePath] <?>
        ("Files to be verified using and RSC signaure file, may be multiple files."),

    cpuCount :: wrapped ::: Maybe Natural <?>
        ("CPU number available to the program (default is 2). Note that higher CPU counts result in bigger " +++ 
        "memory allocations. It is also recommended to set real CPU core number rather than the (hyper-)thread " +++ 
        "number, since using the latter does not give much benefit and actually may cause performance degradation."),

    fetcherCount :: wrapped ::: Maybe Natural <?>
        ("Maximal number of concurrent fetchers (default is --cpu-count * 3)."),

    resetCache :: wrapped ::: Bool <?>
        "Reset the LMDB cache i.e. remove ~/.rpki/cache/*.mdb files.",

    revalidationInterval :: wrapped ::: Maybe Int64 <?>
        ("Interval between validation cycles, each consisting of traversing RPKI tree for each TA " +++ 
        "and fetching repositories on the way, in seconds. Default is 7 minutes, i.e. 560 seconds."),

    cacheLifetimeHours :: wrapped ::: Maybe Int64 <?>
        "Lifetime of objects in the local cache, in hours (default is 72 hours)",

    versionNumberToKeep :: wrapped ::: Maybe Natural <?>
        ("Number of versions to keep in the local cache (default is 100). " +++
         "Every re-validation creates a new version and associates resulting data " +++
         "(validation results, metrics, VRPs, etc.) with it."),

    rrdpRefreshInterval :: wrapped ::: Maybe Int64 <?>
        ("Period of time after which an RRDP repository must be updated, " +++ 
         "in seconds (default is 120 seconds)"),

    rsyncRefreshInterval :: wrapped ::: Maybe Int64 <?>
        ("Period of time after which an rsync repository must be updated, " +++ 
         "in seconds (default is 11 minutes, i.e. 660 seconds)"),

    rrdpTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for RRDP repositories, in seconds. If fetching of a repository does not " +++ 
         "finish within this timeout, the repository is considered unavailable and fetching process is interrupted"),
    
    rsyncTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for rsync repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable and fetching process is interrupted."),

    rsyncClientPath :: wrapped ::: Maybe String <?>
        ("Path to rsync client executable. By default rsync client is expected to be in the $PATH."),

    httpApiPort :: wrapped ::: Maybe Word16 <?>
        "Port to listen to for http API (default is 9999)",

    lmdbSize :: wrapped ::: Maybe Int64 <?>
        ("Maximal LMDB cache size in MBs (default is 32768, i.e. 32GB). Note that " +++ 
         "(a) It is the maximal size of LMDB, i.e. it will not claim that much space from the beginning. " +++ 
         "(b) About 1Gb of cache is required for every extra 24 hours of cache life time."),

    withRtr :: wrapped ::: Bool <?>
        "Start RTR server (default is false)",

    rtrAddress :: wrapped ::: Maybe String <?>
        "Address to bind to for the RTR server (default is localhost)",

    rtrPort :: wrapped ::: Maybe Int16 <?>
        "Port to listen to for the RTR server (default is 8283)",

    rtrLogFile :: wrapped ::: Maybe String <?>
        "Path to a file used for RTR log (default is stdout, together with general output).",

    logLevel :: wrapped ::: Maybe String <?>
        "Log level, may be 'error', 'warn', 'info' or 'debug' (case-insensitive). Default is 'info'.",

    strictManifestValidation :: wrapped ::: Bool <?>
        ("Use the strict version of RFC 6486 (https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/" +++ 
         " item 6.4) for manifest handling (default is false). More modern version is here " +++
         "https://www.rfc-editor.org/rfc/rfc9286.html#name-relying-party-processing-of and it is the default."),

    allowOverclaiming :: wrapped ::: Bool <?>
        ("Use validation reconsidered algorithm for validating resource sets on certificates " +++ 
         "(https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-validation-update/) instead of " +++
         "strict version (https://www.rfc-editor.org/rfc/rfc6487.html#section-7). The default if false, " +++
         "i.e. strict version is used by default."),

    localExceptions :: wrapped ::: [String] <?>
        ("Files with local exceptions in the SLURM format (RFC 8416). It is possible " +++ 
         "to set multiple local exception files (i.e. set this option multiple times " +++ 
         "with different arguments), they all will be united into one internally before applying."),

    maxTaRepositories :: wrapped ::: Maybe Int <?>
        "Maximal number of new repositories that validation run can add per TA (default is 1000).",

    maxCertificatePathDepth :: wrapped ::: Maybe Int <?>
        "Maximal depth of the certificate path from the TA certificate to the RPKI tree (default is 32).",

    maxTotalTreeSize :: wrapped ::: Maybe Int <?>
        ("Maximal total size of the object tree for one TA including all currently supported types of " +++
         "objects (default is 5000000)."),

    maxObjectSize :: wrapped ::: Maybe Integer <?>
        ("Maximal size of an object of any type (certificate, CRL, MFT, GRB, ROA, ASPA) " +++
         "in bytes (default is 32mb, i.e. 33554432 bytes)."),

    minObjectSize :: wrapped ::: Maybe Integer <?>
        "Minimal size of an object of any type in bytes (default is 300).",

    topDownTimeout :: wrapped ::: Maybe Int64 <?>
        "Timebox for one TA validation in seconds (default is 1 hours, i.e. 3600 seconds).",

    noRrdp :: wrapped ::: Bool <?> "Do not fetch RRDP repositories (default is false)",
    noRsync :: wrapped ::: Bool <?> "Do not fetch rsync repositories (default is false)",

    rsyncPrefetchUrl :: wrapped ::: [String] <?>
        ("Rsync repositories that will be fetched instead of their children (defaults are " +++
         "'rsync://rpki.afrinic.net/repository', " +++
         "'rsync://rpki.apnic.net/member_repository', " +++
         "'rsync://rpki-repo.registro.br/repo/', " +++
         "'rsync://repo-rpki.idnic.net/repo/', " +++
         "'rsync://0.sb/repo/', " +++
         "'rsync://rpki.co/repo/', " +++
         "'rsync://rpki-rps.arin.net/repository/', " +++
         "'rsync://rpki-repository.nic.ad.jp/ap/', " +++
         "'rsync://rsync.paas.rpki.ripe.net/repository/', " +++
         "'rsync://rpki.sub.apnic.net/repository/', " +++
         "'rsync://rpki.cnnic.cn/rpki/A9162E3D0000/). " +++
         "It is an optimisation and most of the time this option is of no use."),

    metricsPrefix :: wrapped ::: Maybe String <?>
        "Prefix for Prometheus metrics (default is 'rpki_prover').",

    maxRrdpFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for RRDP fetcher process (default is 1024).",

    maxRsyncFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for rsync fetcher process (default is 1024).",

    maxValidationMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for validation process (default is 2048).",

    noIncrementalValidation :: wrapped ::: Bool <?>
        ("Do not use incremental validation algorithm (incremental validation is the default " +++ 
         "so default for this option is false)."),

    noAdaptiveFetchIntervals :: wrapped ::: Bool <?>
        ("Do not use adaptive fetch intervals for repositories (adaptive fetch intervals is the default " +++ 
         "so default for this option is false)."),

    noAdaptiveFetchTimeouts :: wrapped ::: Bool <?>
        ("Do not use adaptive fetch timeouts for repositories (adaptive fetch timeouts is the default " +++ 
         "so default for this option is false)."),
    
    showHiddenConfig :: wrapped ::: Bool <?>
        ("Reveal all config values in the HTTP API call to `/api/system`. " +++ 
         "This is a potential security issue since some of the config values include " +++ 
         "local FS paths (default is false)."),

    noValidityApi :: wrapped ::: Bool <?>
        ("Do not build VRP index for /api/validity calls in the REST API (default is false, i.e. the index " +++ 
         "is built by default). This option is useful for the cases when the VRP index is not needed, " +++ 
         "it will save some memory and CPU spent on the index.") 

} deriving (Generic)

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
    run logLev = f $ makeLogConfig logLev logType
      where
        logType = case (rtrLogFile, worker) of 
            (Just fs, Nothing) -> MainLogWithRtr fs
            (Nothing, Nothing) -> MainLog
            (_,        Just _) -> WorkerLog
            
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
import           RPKI.Store.Database
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

    withLogConfig cliOptions $ \logConfig -> do
        -- This one modifies system metrics in AppState
        -- if appState is actually initialised
        let bumpSysMetric sm = do 
                z <- readTVarIO appStateHolder
                for_ z $ mergeSystemMetrics sm

        withLogger logConfig bumpSysMetric $ \logger -> do
            logDebug logger $ if once 
                    then [i|Starting #{rpkiProverVersion} in one-off mode.|]
                    else [i|Starting #{rpkiProverVersion} as a server.|]
            
            if cliOptions ^. #initialise
                then
                    -- init the FS layout and download TALs
                    void $ liftIO $ initialiseFS cliOptions logger
                else do
                    -- run the validator
                    (appContext, validations) <- do
                                runValidatorT (newScopes "Initialise") $ do
                                    checkPreconditions cliOptions
                                    createAppContext cliOptions logger (logConfig ^. #logLevel)
                    case appContext of
                        Left _ -> do 
                            logError logger [i|Failure:
#{formatValidations (validations ^. typed)}|]
                            drainLog logger
                            hFlush stdout
                            hFlush stderr
                            exitFailure
                        Right appContext' -> do 
                            -- now we have the appState, set appStateHolder
                            atomically $ writeTVar appStateHolder $ Just $ appContext' ^. #appState
                            if once 
                                then runValidatorServer appContext'
                                else void $ race
                                        (runHttpApi appContext')
                                        (runValidatorServer appContext')

executeWorkerProcess :: IO ()
executeWorkerProcess = do
    input <- readWorkerInput
    let config = input ^. typed @Config    
    let logConfig = LogConfig {
                        logLevel = config ^. #logLevel,
                        logSetup = WorkerLog
                    }
                    
    -- turnOffTlsValidation

    executeWork input $ \_ resultHandler -> 
        withLogger logConfig (\_ -> pure ()) $ \logger -> liftIO $ do
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

                        ValidationParams {..} -> exec resultHandler $
                            uncurry ValidationResult <$> runValidation appContext worldVersion tals

                        CacheCleanupParams {..} -> exec resultHandler $
                            CacheCleanupResult <$> runCacheCleanup appContext worldVersion
  where    
    exec resultHandler f = resultHandler =<< execWithStats f                    


turnOffTlsValidation :: IO ()
turnOffTlsValidation = do 
    manager <- newManager $ mkManagerSettings (TLSSettingsSimple True True True) Nothing 
    setGlobalManager manager    


runValidatorServer :: (Storage s, MaintainableStorage s) => AppContext s -> IO ()
runValidatorServer appContext@AppContext {..} = do
    
    logInfo logger [i|Reading TAL files from #{talDirectory config}|]
    worldVersion  <- newWorldVersion
    talNames      <- listTALFiles $ configValue $ config ^. #talDirectory
    extraTalNames <- fmap mconcat $ mapM listTALFiles $ configValue $ config ^. #extraTalsDirectories
    let totalTalsNames = talNames <> extraTalNames
    (tals, vs) <- runValidatorT (newScopes "validation-root") $
        forM totalTalsNames $ \(talFilePath, taName) ->
            vFocusOn TAFocus (convert taName) $
                parseTALFromFile talFilePath (Text.pack taName)    

    db <- readTVarIO database
    rwTx db $ \tx -> do 
        generalWorldVersion tx db worldVersion
        saveValidations tx db worldVersion (vs ^. typed)
    case tals of
        Left e -> do
            logError logger [i|Error reading some of the TALs, e = #{e}.|]
            throwIO $ AppException e
        Right tals' -> do
            logInfo logger [i|Successfully loaded #{length totalTalsNames} TALs: #{map snd totalTalsNames}|]
            -- here it blocks and loops in never-ending re-validation
            runWorkflow appContext tals'
                `finally`
                closeStorage appContext
  where
    parseTALFromFile talFileName taName = do
        talContent <- fromTry (TAL_E . TALError . fmtEx) $ LBS.readFile talFileName
        vHoist $ fromEither $ first TAL_E $ parseTAL (convert talContent) taName


runHttpApi :: (Storage s, MaintainableStorage s) => AppContext s -> IO ()
runHttpApi appContext = let
    httpPort = fromIntegral $ appContext ^. typed @Config . typed @HttpApiConfig . #port
    in Warp.run httpPort $ httpServer appContext


createAppContext :: CLIOptions Unwrapped -> AppLogger -> LogLevel -> ValidatorT IO AppLmdbEnv
createAppContext cliOptions@CLIOptions{..} logger derivedLogLevel = do

    programPath <- liftIO getExecutablePath

    let defaults = defaultConfig
    let lmdbRealSize = (Size <$> lmdbSize) `orDefault` (defaults ^. #lmdbSizeMb)    

    (root, tald, rsyncd, tmpd, cached) <- 
            fromTryM 
                (\e -> UnspecifiedE "Error verifying/creating FS layout: " (fmtEx e))
                $ do 
                    z@(_, _, _, tmpd, _) <- fsLayout cliOptions logger CheckTALsExists    
                    -- clean up tmp directory if it's not empty
                    cleanDir tmpd
                    pure z    

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
            & maybeSet (#rsyncConf . #asyncRsyncTimeout) (Seconds <$> asyncRsyncTimeout)
            & #rrdpConf . #tmpRoot .~ apiSecured tmpd
            & #rrdpConf . #enabled .~ not noRrdp
            & maybeSet (#rrdpConf . #rrdpTimeout) (Seconds <$> rrdpTimeout)
            & maybeSet (#rrdpConf . #asyncRrdpTimeout) (Seconds <$> asyncRrdpTimeout)
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
            & #validationConfig . #fetchMethod .~ 
                (if noAsyncFetch then SyncOnly else SyncAndAsync)        
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


data TALsHandle = CreateTALs | CheckTALsExists

initialiseFS :: CLIOptions Unwrapped -> AppLogger -> IO ()
initialiseFS cliOptions@CLIOptions {..} logger = do
    (r, _) <- runValidatorT
        (newScopes "initialise")
        $ do
            logInfo logger [i|Initialising FS layout...|]

            -- this one checks that "tals" exists
            (_, tald, _, _, _) <- fsLayout cliOptions logger CreateTALs

            -- TODO Change it to 
            let talsUrl :: String = "https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/"
            let talNames = ["afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal"]

            unless noRirTals $ do 
                logInfo logger [i|Downloading TALs from #{talsUrl} to #{tald}.|]
                fromTryM
                    (\e -> UnspecifiedE "Error downloading TALs: " (fmtEx e))
                    $ forM_ talNames
                        $ \tal -> do
                            let talUrl = Text.pack $ talsUrl <> tal
                            logDebug logger [i|Downloading #{talUrl} to #{tald </> tal}.|]
                            httpStatus <- downloadToFile (URI talUrl) (tald </> tal) (Size 10_000)
                            unless (isHttpSuccess httpStatus) $ do
                                appError $ UnspecifiedE
                                    [i|Error downloading TAL #{tal} from #{talUrl}|]
                                    [i|Http status #{unHttpStatus httpStatus}|]
    case r of
        Left e -> do
            logError logger [i|Failed to initialise: #{e}.|]
            logError logger [i|Please read https://github.com/lolepezy/rpki-prover/blob/master/README.md for the instructions on how to fix it manually.|]
        Right _ ->
            logInfo logger [i|Done.|]


fsLayout :: CLIOptions Unwrapped
        -> AppLogger
        -> TALsHandle
        -> ValidatorT IO (FilePath, FilePath, FilePath, FilePath, FilePath)
fsLayout cliOptions logger talsHandle = do
    (root, rootDir) <- getRoot cliOptions

    let message =
            case root of
                Just d  -> [i|Root directory is set to #{d}.|]
                Nothing -> [i|Root directory is not set, using defaul ${HOME}/.rpki|]

    logInfo logger message

    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir talsHandle
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    cached <- fromEitherM $ first (InitE . InitError) <$> cacheDir rootDir
    pure (rootDir, tald, rsyncd, tmpd, cached)


getRoot :: CLIOptions Unwrapped -> ValidatorT IO (Maybe FilePath, FilePath)
getRoot cliOptions = do
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let root = getRootDirectory cliOptions
    pure (root, root `orDefault` (home </> ".rpki"))

orDefault :: Maybe a -> a -> a
m `orDefault` d = fromMaybe d m

maybeSet :: ASetter s s a b -> Maybe b -> s -> s
maybeSet lenz newValue big = maybe big (\val -> big & lenz .~ val) newValue


listTALFiles :: FilePath -> IO [(FilePath, FilePath)]
listTALFiles talDirectory = do
    names <- getDirectoryContents talDirectory
    pure $ map (\f -> (talDirectory </> f, cutOffTalExtension f)) $
            filter (".tal" `List.isSuffixOf`) $
            filter (`notElem` [".", ".."]) names
  where
    cutOffTalExtension s = List.take (List.length s - 4) s


cacheDirN, rsyncDirN, talsDirN, tmpDirN :: FilePath
cacheDirN = "cache"
rsyncDirN = "rsync"
talsDirN  = "tals"
tmpDirN   = "tmp"

talsDir :: FilePath -> TALsHandle -> IO (Either Text FilePath)
talsDir root CreateTALs      = createSubDirectoryIfNeeded root talsDirN
talsDir root CheckTALsExists = checkSubDirectory root talsDirN

rsyncDir, tmpDir, cacheDir :: FilePath -> IO (Either Text FilePath)
rsyncDir root = createSubDirectoryIfNeeded root rsyncDirN
tmpDir root   = createSubDirectoryIfNeeded root tmpDirN
cacheDir root = createSubDirectoryIfNeeded root cacheDirN


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
                [] -> defaulPrefetchURLs
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
        withLogger logConfig (\_ -> pure ()) $ \logger ->
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

    (_, rootDir) <- getRoot cliOptions
    cached <- fromEitherM $ first (InitE . InitError) <$> checkSubDirectory rootDir cacheDirN

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
        "If set, the FS layout will be created and TAL files will be downloaded.",

    once :: wrapped ::: Bool <?>
        ("If set, will run one validation cycle and exit. Http API will not start, " +++ 
         "result will be written to the file set by --vrp-output option (which must also be set)."),

    vrpOutput :: wrapped ::: Maybe FilePath <?> 
        "Path of the file to write VRPs to. Only effectful when --once option is set.",

    noRirTals :: wrapped ::: Bool <?> 
        "If set, RIR TAL files will not be downloaded.",

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

    asyncRrdpTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for RRDP repositories when fetched asynchronously, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable and fetching process is interrupted"),

    rsyncTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for rsync repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable and fetching process is interrupted."),

    asyncRsyncTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for rsync repositories when fetched asynchronously, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable and fetching process is interrupted"),

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

    noAsyncFetch :: wrapped ::: Bool <?>
        ("Do not fetch repositories asynchronously, i.e. only fetch them while validating the RPKI tree " +++ 
        "(default is false, i.e. asynchronous fetches are used by default)."),

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
    run logLev = 
        f LogConfig { 
            logLevel = logLev,
            logSetup = 
                case (rtrLogFile, worker) of 
                    (Just fs, Nothing) -> MainLogWithRtr fs
                    (Nothing, Nothing) -> MainLog
                    (_,        Just _) -> WorkerLog
            }
            
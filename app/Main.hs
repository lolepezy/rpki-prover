{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

module Main where

import           Control.Lens ((^.), (&))
import           Control.Lens.Setter
import           Control.Concurrent.STM
import           Control.Concurrent.Async

import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Generics.Product.Typed

import           Data.Bifunctor
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Text                        as Text

import           Data.Hourglass
import           Data.Int                         (Int16, Int64)
import qualified Data.List                        as List
import           Data.Maybe
import           Data.Word                        (Word16)
import           Numeric.Natural                  (Natural)

import           Data.String.Interpolate.IsString

import           GHC.TypeLits

import qualified Network.Wai.Handler.Warp         as Warp

import           System.Directory
import           System.Environment
import           System.FilePath                  ((</>))
import           System.IO (hPutStrLn, stderr)

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
import           RPKI.Util               (convert, fmtEx)
import           RPKI.Worker
import           RPKI.Workflow
import           RPKI.RSC.Verifier

main :: IO ()
main = do    
    cliOptions@CLIOptions{..} <- unwrapRecord "RPKI prover, relying party software for RPKI"
    case worker of 
        Nothing -> 
            if verifySignature 
                then executeVerifier cliOptions
                else executeMainProcess cliOptions                
        Just _ -> 
            executeWorkerProcess


executeMainProcess :: CLIOptions Unwrapped -> IO ()
executeMainProcess cliOptions = 
    withLogLevel cliOptions $ \logLevel ->
        withLogger MainLogger logLevel $ \logger -> do             
            logDebug logger [i|Main process.|]
            if cliOptions ^. #initialise
                then
                    -- init the FS layout and download TALs
                    void $ liftIO $ initialiseFS cliOptions logger
                else do                                                                                
                    -- run the validator
                    (appContext, validations) <- do
                                runValidatorT (newScopes "initialise") $ do 
                                    checkPreconditions cliOptions
                                    createAppContext cliOptions logger logLevel
                    case appContext of
                        Left _ ->
                            logError logger [i|Couldn't initialise, problems: #{validations}.|]
                        Right appContext' ->
                            void $ race
                                (runHttpApi appContext')
                                (runValidatorServer appContext')

executeWorkerProcess :: IO ()
executeWorkerProcess = do
    input <- readWorkerInput
    let config = input ^. typed @Config
    let logLevel' = config ^. #logLevel
    withLogger WorkerLogger logLevel' $ \logger -> liftIO $ do
        (z, validations) <- runValidatorT 
                                (newScopes "worker-create-app-context")
                                (createWorkerAppContext config logger)
        case z of
            Left e ->                        
                logError logger [i|Couldn't initialise: #{e}, problems: #{validations}.|]
            Right appContext -> 
                executeWorker input appContext     


runValidatorServer :: (Storage s, MaintainableStorage s) => AppContext s -> IO ()
runValidatorServer appContext@AppContext {..} = do
    logInfo logger [i|Reading TAL files from #{talDirectory config}|]
    worldVersion <- newWorldVersion
    talNames <- listTALFiles $ talDirectory config
    let validationContext = newScopes "validation-root"
    (tals, vs) <- runValidatorT validationContext $
        forM talNames $ \(talFilePath, taName) ->
            inSubVScope' TAFocus (convert taName) $ 
                parseTALFromFile talFilePath (Text.pack taName)

    logInfo logger [i|Successfully loaded #{length talNames} TALs: #{map snd talNames}|]

    database' <- readTVarIO database
    rwTx database' $ \tx -> putValidations tx database' worldVersion (vs ^. typed)
    case tals of
        Left e -> do
            logError logger [i|Error reading some of the TALs, e = #{e}.|]
            throwIO $ AppException e
        Right tals' -> do
            -- this is where it blocks and loops in never-ending re-validation
            runWorkflow appContext tals'
                `finally`
                closeStorage appContext
    where
        parseTALFromFile talFileName taName = do
            talContent <- fromTry (TAL_E . TALError . fmtEx) $ LBS.readFile talFileName
            vHoist $ fromEither $ first TAL_E $ parseTAL (convert talContent) taName


runHttpApi :: Storage s => AppContext s -> IO ()
runHttpApi appContext = let
    httpPort = fromIntegral $ appContext ^. typed @Config . typed @HttpApiConfig . #port
    in Warp.run httpPort $ httpApi appContext


createAppContext :: CLIOptions Unwrapped -> AppLogger -> LogLevel -> ValidatorT IO AppLmdbEnv
createAppContext cliOptions@CLIOptions{..} logger derivedLogLevel = do

    programPath <- liftIO getExecutablePath

    (root, tald, rsyncd, tmpd, cached) <- fsLayout cliOptions logger CheckTALsExists

    let defaults = defaultConfig

    let lmdbRealSize = (Size <$> lmdbSize) `orDefault` (defaults ^. #lmdbSizeMb)
    lmdbEnv <- setupLmdbCache 
                    (if resetCache then Reset else UseExisting)
                    logger
                    cached
                    lmdbRealSize

    db <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv

    -- clean up tmp directory if it's not empty
    cleanDir tmpd

    let cpuCount' = fromMaybe getRtsCpuCount cpuCount

    -- Set capabilities to the values from the CLI or to all available CPUs,
    -- (disregard the HT issue for now it needs more testing).
    liftIO $ setCpuCount cpuCount'
    let parallelism = makeParallelism cpuCount'

    let rtrConfig = if withRtr
            then Just $ defaultRtrConfig
                        & maybeSet #rtrPort rtrPort
                        & maybeSet #rtrAddress rtrAddress
            else Nothing

    appState1 <- liftIO newAppState
    database  <- liftIO $ newTVarIO db

    let readSlurms files = do 
            logDebug logger [i|Reading SLURM files: #{files}.|]
            readSlurmFiles files

    -- Read the files first to fail fast
    unless (null localExceptions) $ do 
        void $ readSlurms localExceptions        

    -- Set the function that re-reads SLURM files with every re-validation.
    let appState =
            case localExceptions of
                []         -> appState1
                slurmFiles -> appState1 & #readSlurm ?~ readSlurms slurmFiles

    let config = defaults               
                & #programBinaryPath .~ programPath
                & #rootDirectory .~ root                
                & #talDirectory .~ tald 
                & #tmpDirectory .~ tmpd 
                & #cacheDirectory .~ cached 
                & #parallelism .~ parallelism
                & #rsyncConf . #rsyncRoot .~ rsyncd
                & #rsyncConf . #rsyncClientPath .~ rsyncClientPath
                & #rsyncConf . #enabled .~ not noRsync
                & maybeSet (#rsyncConf . #rsyncTimeout) (Seconds <$> rsyncTimeout)
                & #rrdpConf . #tmpRoot .~ tmpd
                & #rrdpConf . #enabled .~ not noRrdp
                & maybeSet (#rrdpConf . #rrdpTimeout) (Seconds <$> rrdpTimeout)
                & maybeSet (#validationConfig . #revalidationInterval) (Seconds <$> revalidationInterval)
                & maybeSet (#validationConfig . #rrdpRepositoryRefreshInterval) (Seconds <$> rrdpRefreshInterval)
                & maybeSet (#validationConfig . #rsyncRepositoryRefreshInterval) (Seconds <$> rsyncRefreshInterval)
                & #validationConfig . #manifestProcessing .~                
                        (if strictManifestValidation then RFC6486_Strict else RFC6486)                
                & maybeSet (#validationConfig . #topDownTimeout) (Seconds <$> topDownTimeout)
                & maybeSet (#validationConfig . #maxTaRepositories) maxTaRepositories
                & maybeSet (#validationConfig . #maxCertificatePathDepth) maxCertificatePathDepth
                & maybeSet (#validationConfig . #maxTotalTreeSize) maxTotalTreeSize
                & maybeSet (#validationConfig . #maxObjectSize) maxObjectSize
                & maybeSet (#validationConfig . #minObjectSize) minObjectSize
                & maybeSet (#httpApiConf . #port) httpApiPort
                & #rtrConfig .~ rtrConfig
                & maybeSet #cacheLifeTime ((\hours -> Seconds (hours * 60 * 60)) <$> cacheLifetimeHours)
                & maybeSet #oldVersionsLifetime ((\hours -> Seconds (hours * 60 * 60)) <$> oldVersionsLifeTimeHours)
                & #lmdbSizeMb .~ lmdbRealSize            
                & #localExceptions .~ localExceptions    
                & #logLevel .~ derivedLogLevel                
                & maybeSet #metricsPrefix (convert <$> metricsPrefix)
                & maybeSet (#systemConfig . #rsyncWorkerMemoryMb) maxRsyncFetchMemory
                & maybeSet (#systemConfig . #rrdpWorkerMemoryMb) maxRrdpFetchMemory
                & maybeSet (#systemConfig . #validationWorkerMemoryMb) maxValidationMemory

    let appContext = AppContext {..}
    logInfo logger [i|Created application context with configuration: 
#{shower (appContext ^. typed @Config)}|]    
    pure appContext


data TALsHandle = CreateTALs | CheckTALsExists

initialiseFS :: CLIOptions Unwrapped -> AppLogger -> IO ()
initialiseFS cliOptions logger = do

    if cliOptions ^. #agreeWithArinRpa
        then do
            (r, _) <- runValidatorT
                (newScopes "initialise")
                $ do
                    logInfo logger [i|Initialising FS layout...|]

                    -- this one checks that "tals" exists
                    (_, tald, _, _, _) <- fsLayout cliOptions logger CreateTALs

                    let talsUrl :: String = "https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/"
                    let talNames = ["afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal"]

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
        else do
            putStrLn [i|
Before downloading and installing ARIN TAL, you must read
and agree to the ARIN Relying Party Agreement (RPA) available
here:

https://www.arin.net/resources/manage/rpki/rpa.pdf

If you do agree with the RPA, re-run the same command
adding --agree-with-arin-rpa option.
|]


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
    pure $ map (\f -> (talDirectory </> f, cutOfTalExtension f)) $
            filter (".tal" `List.isSuffixOf`) $
            filter (`notElem` [".", ".."]) names
  where
    cutOfTalExtension s = List.take (List.length s - 4) s


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
        False -> pure $ Left [i|Directory #{subDirectory} doesn't exist.|]
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
        

-- This is for worker processes.
executeWorker :: WorkerInput 
            -> AppLmdbEnv 
            -> IO ()
executeWorker input appContext = 
    executeWork input $ \_ resultHandler ->   
        case input ^. #params of
            RrdpFetchParams {..} -> do
                z <- runValidatorT scopes $ 
                            updateObjectForRrdpRepository appContext worldVersion rrdpRepository                            
                resultHandler $ RrdpFetchResult z
            RsyncFetchParams {..} -> do
                z <- runValidatorT scopes $ 
                            updateObjectForRsyncRepository appContext worldVersion rsyncRepository                            
                resultHandler $ RsyncFetchResult z
            CompactionParams {..} -> do 
                z <- copyLmdbEnvironment appContext targetLmdbEnv                
                resultHandler $ CompactionResult z
            ValidationParams {..} -> do 
                (vs, z) <- runValidation appContext worldVersion tals                
                resultHandler $ ValidationResult vs z

createWorkerAppContext :: Config -> AppLogger -> ValidatorT IO AppLmdbEnv
createWorkerAppContext config logger = do    
    lmdbEnv <- setupWorkerLmdbCache                     
                    logger
                    (config ^. #cacheDirectory)
                    (config ^. #lmdbSizeMb)

    db <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv    

    appState <- liftIO newAppState
    database <- liftIO $ newTVarIO db

    pure AppContext {..}       

-- | Check some crucial things before running the validator
checkPreconditions :: CLIOptions Unwrapped -> ValidatorT IO ()
checkPreconditions CLIOptions {..} = checkRsyncInPath rsyncClientPath           


-- | Run rpki-prover in a CLI mode for verifying RSC signature (*.sig file).
executeVerifier :: CLIOptions Unwrapped -> IO ()
executeVerifier cliOptions@CLIOptions {..} = do 
    withLogLevel cliOptions $ \logLevel1 ->        
        withLogger MainLogger logLevel1 $ \logger ->             
            withVerifier logger $ \verifyPath rscFile -> do               
                logDebug logger [i|Verifying #{verifyPath} with RSC #{rscFile}.|]                                 
                (ac, vs) <- runValidatorT (newScopes "verify-rsc") $ do 
                                appContext <- createVerifierContext cliOptions logger
                                rscVerify appContext rscFile verifyPath                                            
                case ac of
                    Left _ -> do
                        let report = formatValidations $ vs ^. #validations
                        logError logger [i|Verification failed: 
#{report}.|]
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
    lmdbEnv <- setupWorkerLmdbCache logger cached (config ^. #lmdbSizeMb)

    db <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv    

    appState <- liftIO newAppState
    database <- liftIO $ newTVarIO db

    pure AppContext {..}             


-- CLI Options-related machinery
data CLIOptions wrapped = CLIOptions {

    -- It is not documented since it's for internal machinery
    worker :: wrapped ::: Maybe String,

    initialise :: wrapped ::: Bool <?>
        ("If set, the FS layout will be created and TAL files will be downloaded." ),

    agreeWithArinRpa :: wrapped ::: Bool <?>
        ("This is to indicate that you do accept (and maybe even have read) ARIN Relying Party Agreement "
        +++ "and would like ARIN TAL to be downloaded."),

    rpkiRootDirectory :: wrapped ::: [FilePath] <?>
        ("Root directory (default is ${HOME}/.rpki/). This option can be passed multiple times and "
         +++ "the last one will be used, it is done for convenience of overriding this option with dockerised version."),

    verifySignature :: wrapped ::: Bool <?>
        ("Work as a one-off RSC signature file executeVerifier, not as a server. To work as a executeVerifier it needs the cache " +++ 
        "of validated RPKI objects and VRPs to exist and be poulateds. So executeVerifier can (and should) run next to " +++
        "the running daemon instance of rpki-prover"),         

    signatureFile :: wrapped ::: Maybe FilePath <?> ("Path to the RSC signature file."),

    verifyDirectory :: wrapped ::: Maybe FilePath <?>
        ("Path to the directory with the files to be verified using and RSC signaure file."),         

    verifyFiles :: wrapped ::: [FilePath] <?>
        ("Files to be verified using and RSC signaure file, may be multiple files."),         

    cpuCount :: wrapped ::: Maybe Natural <?>
        "CPU number available to the program (default is 2). Note that higher CPU counts result in bigger memory allocations.",

    resetCache :: wrapped ::: Bool <?>
        "Reset the LMDB cache i.e. remove ~/.rpki/cache/*.mdb files.",

    revalidationInterval :: wrapped ::: Maybe Int64 <?>
        ("Re-validation interval in seconds, i.e. how often to re-download repositories are "
       +++ "updated and certificate tree is re-validated. "
       +++ "Default is 13 minutes, i.e. 780 seconds."),

    cacheLifetimeHours :: wrapped ::: Maybe Int64 <?>
        "Lifetime of objects in the local cache, in hours (default is 72 hours)",

    oldVersionsLifeTimeHours :: wrapped ::: Maybe Int64 <?>
        ("Lifetime of versions in the local cache, in hours (default is 24 hours). " +++ 
         "Every re-validation creates a new version and associates resulting data " +++ 
         "(validation results, metrics, VRPs, etc.) with it."),

    rrdpRefreshInterval :: wrapped ::: Maybe Int64 <?>
        ("Period of time after which an RRDP repository must be updated, "
       +++ "in seconds (default is 120 seconds)"),

    rsyncRefreshInterval :: wrapped ::: Maybe Int64 <?>
        ("Period of time after which an rsync repository must be updated, "
       +++ "in seconds (default is 11 minutes, i.e. 660 seconds)"),

    rrdpTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for RRDP repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable"),

    rsyncTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for rsync repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable"),

    rsyncClientPath :: wrapped ::: Maybe String <?>
        ("Scope to rsync client. By default rsync client is expected to be in the $PATH."),

    httpApiPort :: wrapped ::: Maybe Word16 <?>
        "Port to listen to for http API (default is 9999)",

    lmdbSize :: wrapped ::: Maybe Int64 <?>
        ("Maximal LMDB cache size in MBs (default is 32768, i.e. 32GB). Note that " 
       +++ "(a) It is the maximal size of LMDB, i.e. it will not claim that much space from the beginning. "
       +++ "(b) About 1Gb of cache is required for every extra 24 hours of cache life time."),

    withRtr :: wrapped ::: Bool <?>
        "Start RTR server (default is false)",

    rtrAddress :: wrapped ::: Maybe String <?>
        "Address to bind to for the RTR server (default is localhost)",

    rtrPort :: wrapped ::: Maybe Int16 <?>
        "Port to listen to for the RTR server (default is 8283)",

    logLevel :: wrapped ::: Maybe String <?>
        "Log level, may be 'error', 'warn', 'info', 'debug' (case-insensitive). Default is 'info'.",

    strictManifestValidation :: wrapped ::: Bool <?>
        "Use the strict version of RFC 6486 (https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/ item 6.4) for manifest handling (default is false).",

    localExceptions :: wrapped ::: [String] <?>
        "Files with local exceptions in the SLURM format (RFC 8416).",

    maxTaRepositories :: wrapped ::: Maybe Int <?>
        "Maximal number of new repositories that TA validation run can add (default is 1000).",

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

    metricsPrefix :: wrapped ::: Maybe String <?> 
        "Prefix for Prometheus metrics (default is 'rpki_prover').",

    maxRrdpFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for RRDP fetcher process (default is 1024).",        

    maxRsyncFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for rsync fetcher process (default is 1024).",        

    maxValidationMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for validation process (default is 2048)."

} deriving (Generic)

instance ParseRecord (CLIOptions Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (CLIOptions Unwrapped)

type (+++) (a :: Symbol) (b :: Symbol) = AppendSymbol a b


withLogLevel :: CLIOptions Unwrapped -> (LogLevel -> IO ()) -> IO ()
withLogLevel CLIOptions{..} f =
    case logLevel of
        Nothing -> f defaultsLogLevel
        Just s  ->
            case Text.toLower $ Text.pack s of
                "error" -> f ErrorL
                "warn"  -> f WarnL
                "info"  -> f InfoL
                "debug" -> f DebugL
                other   -> hPutStrLn stderr $ "Wrong log level: " <> Text.unpack other

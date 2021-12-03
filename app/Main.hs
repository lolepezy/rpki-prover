{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

module Main where

import           Codec.Serialise

import           Control.Lens ((^.), (&))
import           Control.Lens.Setter
import           Control.Concurrent (threadDelay)
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

import           System.IO (hPutStrLn, stderr)
import           System.Directory
import           System.Exit
import           System.Environment
import           System.FilePath                  ((</>))
import           System.IO (stdin, stdout)
import           System.Posix.Process

import           Options.Generic

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.RRDP.Http (downloadToFile)
import           RPKI.Http.HttpServer
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.AppStorage
import           RPKI.Store.AppLmdbStorage
import qualified RPKI.Store.MakeLmdb as Lmdb
import           RPKI.SLURM.SlurmProcessing

import           RPKI.RRDP.RrdpFetch

import           RPKI.Worker
import           RPKI.TAL
import           RPKI.Util               (convert, fmtEx)
import           RPKI.Workflow

main :: IO ()
main = do    
    cliOptions :: CLIOptions Unwrapped <- unwrapRecord
        "RPKI prover, relying party software for RPKI"
    inProcess cliOptions


mainProcess :: CLIOptions Unwrapped -> IO ()
mainProcess cliOptions = do 
    withLogLevel cliOptions $ \logLevel ->
        withMainAppLogger logLevel $ \logger -> liftIO $ do
            logDebug_ logger [i|Main process.|]
            if cliOptions ^. #initialise
                then
                    -- init the FS layout and download TALs
                    void $ liftIO $ initialiseFS cliOptions logger
                else do
                    -- run the validator
                    (appContext, validations) <- do
                                runValidatorT (newValidatorPath "initialise")
                                    $ createAppContext cliOptions logger logLevel
                    case appContext of
                        Left e ->
                            logError_ logger [i|Couldn't initialise: #{e}, problems: #{validations}.|]
                        Right appContext' ->
                            void $ race
                                (runHttpApi appContext')
                                (runValidatorApp appContext')

inProcess :: CLIOptions Unwrapped -> IO ()
inProcess cliOptions@CLIOptions{..} =
    if not (isJust worker)
        then do             
            mainProcess cliOptions
        else do         
            input <- readWorkerInput
            let logLevel' = input ^. typed @Config . #logLevel
            withWorkerLogger logLevel' $ \logger -> liftIO $ do
                (z, validations) <- do
                            runValidatorT (newValidatorPath "worker-create-app-context")
                                $ readWorkerContext input logger
                case z of
                    Left e ->                        
                        logErrorM logger [i|Couldn't initialise: #{e}, problems: #{validations}.|]
                    Right appContext -> 
                        executeWork input appContext

runValidatorApp :: (Storage s, MaintainableStorage s) => AppContext s -> IO ()
runValidatorApp appContext@AppContext {..} = do
    logInfo_ logger [i|Reading TAL files from #{talDirectory config}|]
    worldVersion <- newWorldVersion
    talNames <- listTALFiles $ talDirectory config
    let validationContext = newValidatorPath "validation-root"
    (tals, vs) <- runValidatorT validationContext $
        forM talNames $ \(talFilePath, taName) ->
            inSubVPath (convert taName) $ parseTALFromFile talFilePath (Text.pack taName)

    logInfo_ logger [i|Successfully loaded #{length talNames} TALs: #{map snd talNames}|]

    database' <- readTVarIO database
    rwTx database' $ \tx -> putValidations tx database' worldVersion (vs ^. typed)
    case tals of
        Left e -> do
            logError_ logger [i|Error reading some of the TALs, e = #{e}.|]
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

    (root, tald, rsyncd, tmpd, cached) <- fsLayout cliOptions logger CheckTALsExists

    let defaults = defaultConfig

    let lmdbRealSize = (Size <$> lmdbSize) `orDefault` (defaults ^. #lmdbSizeMb)
    lmdbEnv <- setupLmdbCache 
                    (if resetCache then Reset else UseExisting)
                    logger
                    cached
                    lmdbRealSize

    database <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv

    -- clean up tmp directory if it's not empty
    cleanDir tmpd

    let cpuCount' = fromMaybe getRtsCpuCount cpuCount

    -- Set capabilities to the values from the CLI or to all available CPUs,
    -- (disregard the HT issue for now it needs more testing).
    liftIO $ setCpuCount cpuCount'
    let parallelism = makeParallelism cpuCount'

    appBottlenecks <- liftIO $ AppBottleneck <$>
                        newBottleneckIO (parallelism ^. #cpuParallelism) <*>
                        newBottleneckIO (parallelism ^. #ioParallelism)

    let rtrConfig = if withRtr
            then Just $ defaultRtrConfig
                        & maybeSet #rtrPort rtrPort
                        & maybeSet #rtrAddress rtrAddress
            else Nothing

    appState <- liftIO newAppState
    tvarDatabase <- liftIO $ newTVarIO database

    let readSlurms files = do 
            logDebugM logger [i|Reading SLURM files: #{files}|]
            readSlurmFiles files

    -- Read the files first to fail fast
    unless (null localExceptions) $ do 
        void $ readSlurms localExceptions        

    -- Set the function that re-reads SLURM files with every re-validation.
    let appState' =
            case localExceptions of
                []         -> appState
                slurmFiles -> appState & #readSlurm ?~ readSlurms slurmFiles

    programPath <- liftIO getExecutablePath
    let appContext = AppContext {
        appState = appState',
        database = tvarDatabase,
        appBottlenecks = appBottlenecks,
        logger = logger,        
        config = defaults               
                & #programBinaryPath .~ programPath
                & #rootDirectory .~ root                
                & #talDirectory .~ tald 
                & #tmpDirectory .~ tmpd 
                & #cacheDirectory .~ cached 
                & #parallelism .~ parallelism
                & #rsyncConf . #rsyncRoot .~ rsyncd
                & maybeSet (#rsyncConf . #rsyncTimeout) (Seconds <$> rsyncTimeout)
                & #rrdpConf . #tmpRoot .~ tmpd
                & maybeSet (#rrdpConf . #rrdpTimeout) (Seconds <$> rrdpTimeout)
                & maybeSet (#validationConfig . #revalidationInterval) (Seconds <$> revalidationInterval)
                & maybeSet (#validationConfig . #rrdpRepositoryRefreshInterval) (Seconds <$> rrdpRefreshInterval)
                & maybeSet (#validationConfig . #rsyncRepositoryRefreshInterval) (Seconds <$> rsyncRefreshInterval)
                & #validationConfig . #dontFetch .~ dontFetch
                & #validationConfig . #manifestProcessing .~
                        (if strictManifestValidation then RFC6486_Strict else RFC6486)
                & maybeSet (#httpApiConf . #port) httpApiPort
                & #rtrConfig .~ rtrConfig
                & maybeSet #cacheLifeTime ((\hours -> Seconds (hours * 60 * 60)) <$> cacheLifetimeHours)
                & #lmdbSizeMb .~ lmdbRealSize            
                & #localExceptions .~ localExceptions    
                & #logLevel .~ derivedLogLevel
    }

    logInfoM logger [i|Created application context: #{appContext ^. typed @Config}|]
    pure appContext


data TALsHandle = CreateTALs | CheckTALsExists

initialiseFS :: CLIOptions Unwrapped -> AppLogger -> IO ()
initialiseFS cliOptions logger = do

    if cliOptions ^. #agreeWithArinRpa
        then do
            (r, _) <- runValidatorT
                (newValidatorPath "initialise")
                $ do
                    logInfoM logger [i|Initialising FS layout...|]

                    -- this one checks that "tals" exists
                    (_, tald, _, _, _) <- fsLayout cliOptions logger CreateTALs

                    let talsUrl :: String = "https://raw.githubusercontent.com/NLnetLabs/routinator/master/tals/"
                    let talNames = ["afrinic.tal", "apnic.tal", "arin.tal", "lacnic.tal", "ripe.tal"]

                    logInfoM logger [i|Downloading TALs from #{talsUrl} to #{tald}.|]
                    fromTryM
                        (\e -> UnspecifiedE "Error downloading TALs: " (fmtEx e))
                        $ forM_ talNames
                            $ \tal -> do
                                let talUrl = Text.pack $ talsUrl <> tal
                                logDebugM logger [i|Downloading #{talUrl} to #{tald </> tal}.|]
                                httpStatus <- downloadToFile (URI talUrl) (tald </> tal) (Size 10_000)
                                unless (isHttpSuccess httpStatus) $ do
                                    appError $ UnspecifiedE
                                        [i|Error downloading TAL #{tal} from #{talUrl}|]
                                        [i|Http status #{unHttpStatus httpStatus}|]
            case r of
                Left e ->
                    logErrorM logger [i|Failed to initialise: #{e}. |] <>
                    logErrorM logger [i|Please read https://github.com/lolepezy/rpki-prover/blob/master/README.md for the instructions on how to fix it manually.|]
                Right _ ->
                    logInfoM logger [i|Done.|]
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
fsLayout cli logger talsHandle = do
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let root = getRootDirectory cli
    let rootDir = root `orDefault` (home </> ".rpki")

    let message =
            case root of
                Just d  -> [i|Root directory is set to #{d}.|]
                Nothing -> [i|Root directory is not set, using defaul ${HOME}/.rpki|]

    logInfoM logger message

    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir talsHandle
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    cached <- fromEitherM $ first (InitE . InitError) <$> cacheDir rootDir
    pure (rootDir, tald, rsyncd, tmpd, cached)


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
        False -> pure $ Left [i| Directory #{subDirectory} doesn't exist.|]
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


-- This is for worker processes
executeWork :: WorkerInput 
            -> AppLmdbEnv 
            -> IO ()
executeWork input appContext = 
    void $ race actualWork (race suicideCheck timeoutWait)
  where
    actualWork = do 
        case input ^. #params of
            RrdpFetchParams {..} -> do
                z <- runValidatorT validatorPath $ 
                            updateObjectForRrdpRepository appContext worldVersion rrdpRepository                            
                output $ RrdpFetchResult z
            CompactionParams {..} -> do 
                z <- copyLmdbEnvironment appContext targetLmdbEnv                
                output $ CompactionResult z

    output = LBS.hPut stdout . serialise

    -- Keep track of who's the current process parent: if it is not the same 
    -- as we started with then parent exited/is killed. Exit the worker as well.
    suicideCheck = 
        forever $ do
            parentId <- getParentProcessID                    
            when (parentId /= input ^. #initialParentId) $ 
                exitWith exitParentDied    
            threadDelay 500_000                

    timeoutWait = do
        let Timebox (Seconds s) = input ^. #workerTimeout
        threadDelay $ 1_000_000 * fromIntegral s        
        exitWith exitTimeout
   

readWorkerInput :: (MonadIO m) => m WorkerInput
readWorkerInput = liftIO $ deserialise <$> LBS.hGetContents stdin        

readWorkerContext :: WorkerInput -> AppLogger -> ValidatorT IO AppLmdbEnv
readWorkerContext input logger = do    

    lmdbEnv <- setupWorkerLmdbCache                     
                    logger
                    (input ^. #config . #cacheDirectory)
                    (input ^. #config . #lmdbSizeMb)

    database <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv
    
    let parallelism = input ^. #config . #parallelism
    appBottlenecks <- liftIO $ AppBottleneck <$>
                        newBottleneckIO (parallelism ^. #cpuParallelism) <*>
                        newBottleneckIO (parallelism ^. #ioParallelism)        

    appState     <- liftIO newAppState
    tvarDatabase <- liftIO $ newTVarIO database

    pure AppContext {
            appState = appState,
            database = tvarDatabase,
            appBottlenecks = appBottlenecks,
            logger = logger,        
            config = input ^. #config
        }       


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

    httpApiPort :: wrapped ::: Maybe Word16 <?>
        "Port to listen to for http API (default is 9999)",

    lmdbSize :: wrapped ::: Maybe Int64 <?>
        ("Maximal LMDB cache size in MBs (default is 32768, i.e. 32GB). Note that about 1Gb of cache is "
       +++ "required for every extra 24 hours of cache life time."),

    withRtr :: wrapped ::: Bool <?>
        "Start RTR server (default is false)",

    rtrAddress :: wrapped ::: Maybe String <?>
        "Address to bind to for the RTR server (default is localhost)",

    rtrPort :: wrapped ::: Maybe Int16 <?>
        "Port to listen to for the RTR server (default is 8283)",

    logLevel :: wrapped ::: Maybe String <?>
        "Log level, may be 'error', 'warn', 'info', 'debug' (case-insensitive). Default is 'info'.",

    dontFetch :: wrapped ::: Bool <?>
        "Don't fetch repositories, expect all the objects to be cached (mostly used for testing, default is false).",

    strictManifestValidation :: wrapped ::: Bool <?>
        "Use the strict version of RFC 6486 (https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/ item 6.4) for manifest handling (default is false).",

    localExceptions :: wrapped ::: [String] <?>
        "Files with local exceptions in the SLURM format (RFC 8416)."

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

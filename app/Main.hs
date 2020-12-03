{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}

module Main where
    
import           Colog

import           Control.Lens ((^.))
import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Exception.Lifted

import           Data.Generics.Product.Typed

import           Data.Bifunctor
import qualified Data.ByteString                  as BS
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
import           System.IO               (BufferMode (..), hSetBuffering, stdout)

import           Options.Generic

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Http.HttpServer
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.RRDP.HttpContext
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.AppStorage
import           RPKI.Store.Util
import           RPKI.TAL
import           RPKI.Util               (convert, fmtEx)
import           RPKI.Workflow


main :: IO ()
main = do
    -- load config file and apply command line options    
    logger <- createLogger

    cliOptions :: CLIOptions Unwrapped <- unwrapRecord "RPKI prover, relying party software"

    (appContext, validations) <- 
        runValidatorT (newValidatorPath "configuration") 
        $ createAppContext cliOptions logger
    case appContext of
        Left e ->
            logError_ logger [i|Couldn't initialise: #{e}|]
        Right appContext' -> 
            void $ concurrently
                (runHttpApi appContext')
                (runValidatorApp appContext')


runValidatorApp :: AppEnv -> IO ()
runValidatorApp appContext@AppContext {..} = do    
    logInfo_ logger [i|Reading TAL files from #{talDirectory config}|]
    worldVersion <- updateWorldVerion appState
    talFileNames <- listTALFiles $ talDirectory config
    let validationContext = newValidatorPath "validation-root"
    (tals, validationState) <- runValidatorT validationContext $ 
        forM talFileNames $ \talFileName -> 
            subVPath (convert talFileName) $ parseTALFromFile talFileName

    logInfo_ logger [i|Successfully loaded #{length talFileNames} TALs.|]    
    rwTx database $ \tx -> putValidations tx (validationsStore database) worldVersion (validationState ^. typed)    
    case tals of 
        Left e -> do
            logError_ logger [i|Error reading some of the TALs, e = #{e}.|]    
            throwIO $ AppException e        
        Right tals' ->
            -- this is where it blocks and loops in never-ending re-validation
            runWorkflow appContext tals'
    where
        parseTALFromFile talFileName = do
            talContent <- fromTry (TAL_E . TALError . fmtEx) $ BS.readFile talFileName
            vHoist $ fromEither $ first TAL_E $ parseTAL $ convert talContent


runHttpApi :: AppEnv -> IO ()
runHttpApi appContext = Warp.run 9999 $ httpApi appContext
    

createAppContext :: CLIOptions Unwrapped -> AppLogger -> ValidatorT IO AppEnv
createAppContext cliOoptions@CLIOptions{..} logger = do        
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = rpkiRootDirectory `orDefault` (home </> ".rpki")
    
    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir 
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir            

    (lmdbEnv, cacheDir) <- setupLmdbCache 
                                (if reset then Reset else UseExisting) 
                                logger 
                                rootDir
                                (lmdbSize `orDefault` 2048)
                                
    database <- fromTry (InitE . InitError . fmtEx) $ createDatabase lmdbEnv                

    -- clean up tmp directory if it's not empty
    cleanDir tmpd    

    let cpuCount' = fromMaybe getRtsCpuCount cpuCount

    -- Set capabilities to the values from the CLI or to all available CPUs,
    -- (disregard the HT issue for now it needs more testing).
    liftIO $ setCpuCount cpuCount'
        
    -- BUT: create 2 times more asyncs/tasks than there're capabilities. In most 
    -- tested cases it seems to be beneficial for the CPU utilisation ¯\_(ツ)_/¯.    
    let cpuParallelism = 2 * cpuCount'
    
    -- Hardcoded (not sure it makes sense to make it configurable). Allow for 
    -- that much IO (http downloads, LMDB reads, etc.) operations at once.
    let ioParallelism = 64     

    appBottlenecks <- liftIO $ do 
        cpuBottleneck <- newBottleneckIO cpuParallelism
        ioBottleneck  <- newBottleneckIO ioParallelism
        pure $ AppBottleneck cpuBottleneck ioBottleneck

    -- TODO read stuff from the config, CLI
    httpContext <- liftIO newHttpContext
    
    let rtrConfig = if withRtr
            then Just $ RtrConfig { 
                    rtrAddress = rtrAddress `orDefault` "localhost",
                    rtrPort    = rtrPort `orDefault` 8283
                }
            else Nothing     

    appState <- liftIO newAppState    

    let appContext = AppContext {        
        logger = logger,
        config = Config {
            talDirectory = tald,
            tmpDirectory = tmpd,
            cacheDirectory = cacheDir,
            parallelism  = Parallelism cpuParallelism ioParallelism,
            rsyncConf    = RsyncConf rsyncd (Seconds $ rsyncTimeout `orDefault` (7 * 60)),
            rrdpConf     = RrdpConf { 
                tmpRoot = tmpd,
                -- Do not download files bigger than 1Gb, it's fishy
                maxSize = Size 1024 * 1024 * 1024,
                rrdpTimeout = Seconds $ rsyncTimeout `orDefault` (5 * 60)
            },
            validationConfig = ValidationConfig {                
                repositoryGracePeriod          = Seconds <$> repositoryGracePeriod,
                revalidationInterval           = Seconds $ revalidationInterval `orDefault` (13 * 60),
                rrdpRepositoryRefreshInterval  = Seconds $ rrdpRefreshInterval `orDefault` 120,
                rsyncRepositoryRefreshInterval = Seconds $ rsyncRefreshInterval `orDefault` (11 * 60)
            },
            httpApiConf = HttpApiConfig {
                port = httpApiPort `orDefault` 9999
            },
            rtrConfig = rtrConfig,
            cacheCleanupInterval = 120 * 60,
            cacheLifeTime = Seconds $ 60 * 60 * (cacheLifetimeHours `orDefault` 72),

            -- TODO Think about it, it should be lifetime or we should store N last versions
            oldVersionsLifetime = let twoHours = 2 * 60 * 60 in twoHours,

            storageDefragmentInterval = Seconds $ 60 * 60 * 12
        },
        appState = appState,
        database = database,
        appBottlenecks = appBottlenecks,
        httpContext = httpContext
    }    

    logDebugM logger [i|Created application context: #{config appContext}|]
    pure appContext    


orDefault :: Maybe a -> a -> a
m `orDefault` d = fromMaybe d m

createLogger :: IO AppLogger
createLogger = do 
    -- TODO Use colog-concurrent instead of this
    lock <- newMVar True
    hSetBuffering stdout LineBuffering
    pure $ AppLogger fullMessageAction lock
    where
        fullMessageAction = upgradeMessageAction defaultFieldMap $ 
            cmapM fmtRichMessageDefault logTextStdout  

listTALFiles :: FilePath -> IO [FilePath]
listTALFiles talDirectory = do     
    names <- getDirectoryContents talDirectory
    pure $ map (talDirectory </>) $ 
            filter (".tal" `List.isSuffixOf`) $ 
            filter (`notElem` [".", ".."]) names

            

talsDir, rsyncDir, tmpDir, lmdbDir :: FilePath -> IO (Either Text FilePath)
talsDir root  = checkSubDirectory root "tals"
rsyncDir root = checkSubDirectory root "rsync"
tmpDir root   = checkSubDirectory root "tmp"
lmdbDir root  = checkSubDirectory root "cache"

checkSubDirectory :: FilePath -> FilePath -> IO (Either Text FilePath)
checkSubDirectory root sub = do
    let talDirectory = root </> sub
    doesDirectoryExist talDirectory >>= \case
        False -> pure $ Left [i| Directory #{talDirectory} doesn't exist.|]
        True  -> pure $ Right talDirectory


-- CLI Options-related machinery
data CLIOptions wrapped = CLIOptions {
    rpkiRootDirectory :: wrapped ::: Maybe FilePath <?> 
        "Root directory (default is ${HOME}/.rpki/).",

    cpuCount :: wrapped ::: Maybe Natural <?> 
        "CPU number available to the program (default is all CPUs).",

    reset :: wrapped ::: Bool <?> 
        "Reset the disk cache of (i.e. remove ~/.rpki/cache/*.mdb files.",

    revalidationInterval :: wrapped ::: Maybe Int64 <?>          
        ("Re-validation interval in seconds, i.e. how often to re-download repositories are " 
        `AppendSymbol` "updated and certificate tree is re-validated. "
        `AppendSymbol` "Default is 13 minutes, i.e. 780 seconds."),

    cacheLifetimeHours :: wrapped ::: Maybe Int64 <?> 
        "Lifetime of objects in the local cache, in hours (default is 72 hours)",

    rrdpRefreshInterval :: wrapped ::: Maybe Int64 <?>          
        ("Period of time after which an RRDP repository must be updated," 
        `AppendSymbol` "in seconds (default is 120 seconds)"),

    rsyncRefreshInterval :: wrapped ::: Maybe Int64 <?>         
        ("Period of time after which an rsync repository must be updated, "
        `AppendSymbol`  "in seconds (default is 11 minutes, i.e. 660 seconds)"),

    rrdpTimeout :: wrapped ::: Maybe Int64 <?> 
        ("Timeout for RRDP repositories. If fetching of a repository does not "
        `AppendSymbol` "finish within this timeout, the repository is considered unavailable"),

    rsyncTimeout :: wrapped ::: Maybe Int64 <?> 
        ("Timeout for rsync repositories. If fetching of a repository does not "
        `AppendSymbol` "finish within this timeout, the repository is considered unavailable"),

    repositoryGracePeriod :: wrapped ::: Maybe Int64 <?> 
        ("Period of time in seconds for which a repository is 'allowed' to be unavailable, "
        `AppendSymbol` "before its cached objects are ignored. "
        `AppendSymbol` "The default is zero, so repository objects are ignored immediately "
        `AppendSymbol` "after the repository could not be successfully downloaded."),

    httpApiPort :: wrapped ::: Maybe Word16 <?> 
        "Port to listen to for http API (default is 9999)",

    lmdbSize :: wrapped ::: Maybe Int64 <?> 
        ("Maximal LMDB cache size in MBs (default is 2048mb). Note that about 1Gb of cache is "
        `AppendSymbol` "required for every extra day of cache life time"),    

    withRtr :: wrapped ::: Bool <?> 
        "Start RTR server (default is false)",

    rtrAddress :: wrapped ::: Maybe String <?> 
        "Address to bind to for the RTR server (default is localhost)",

    rtrPort :: wrapped ::: Maybe Int16 <?> 
        "Port to listen to for the RTR server (default is 8282)"

} deriving (Generic)


instance ParseRecord (CLIOptions Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (CLIOptions Unwrapped)

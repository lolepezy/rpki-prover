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
import           Control.Concurrent.STM (readTVarIO)
import           Control.Concurrent.STM.TVar (newTVarIO)

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Concurrent.Async.Lifted
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

import           Options.Generic

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppState
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Http.HttpServer
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.AppStorage
import           RPKI.Store.AppLmdbStorage
import qualified RPKI.Store.MakeLmdb as Lmdb

import           RPKI.TAL
import           RPKI.Util               (convert, fmtEx)
import           RPKI.Workflow

main :: IO ()
main = do
    -- load config file and apply command line options    
    withAppLogger $ \logger -> liftIO $ do 
        cliOptions :: CLIOptions Unwrapped <- unwrapRecord "RPKI prover, relying party software"

        (appContext, validations) <- 
            runValidatorT (newValidatorPath "configuration") 
            $ createAppContext cliOptions logger
        case appContext of
            Left e ->
                logError_ logger [i|Couldn't initialise: #{e}|]
            Right appContext' -> 
                void $ race
                    (runHttpApi appContext')
                    (runValidatorApp appContext')


runValidatorApp :: (Storage s, MaintainableStorage s) => AppContext s -> IO ()
runValidatorApp appContext@AppContext {..} = do    
    logInfo_ logger [i|Reading TAL files from #{talDirectory config}|]
    worldVersion <- updateWorldVerion appState
    talFileNames <- listTALFiles $ talDirectory config
    let validationContext = newValidatorPath "validation-root"
    (tals, validationState) <- runValidatorT validationContext $ 
        forM talFileNames $ \talFileName -> 
            inSubVPath (convert talFileName) $ parseTALFromFile talFileName

    logInfo_ logger [i|Successfully loaded #{length talFileNames} TALs.|]    
    
    database' <- readTVarIO database
    rwTx database' $ \tx -> putValidations tx (validationsStore database') worldVersion (validationState ^. typed)    
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
        parseTALFromFile talFileName = do
            talContent <- fromTry (TAL_E . TALError . fmtEx) $ BS.readFile talFileName
            vHoist $ fromEither $ first TAL_E $ parseTAL $ convert talContent


runHttpApi :: Storage s => AppContext s -> IO ()
runHttpApi appContext = let
    httpPort = fromIntegral $ appContext ^. typed @Config . typed @HttpApiConfig . #port
    in Warp.run httpPort $ httpApi appContext
    

createAppContext :: CLIOptions Unwrapped -> AppLogger -> ValidatorT IO AppLmdbEnv
createAppContext CLIOptions{..} logger = do        
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = rpkiRootDirectory `orDefault` (home </> ".rpki")
    
    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir 
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir            
    cached <- fromEitherM $ first (InitE . InitError) <$> cacheDir rootDir            

    let lmdbRealSize = Size $ lmdbSize `orDefault` 8192
    lmdbEnv <- setupLmdbCache 
                    (if reset then Reset else UseExisting)
                    logger 
                    cached
                    lmdbRealSize
                                
    database <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv                
    -- database <- fromTry (InitE . InitError . fmtEx) Mem.createDatabase

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
    -- that many IO operations (http downloads, LMDB reads, etc.) at once.
    let ioParallelism = 64     

    appBottlenecks <- liftIO $ AppBottleneck <$> 
                        newBottleneckIO cpuParallelism <*>
                        newBottleneckIO ioParallelism        

    -- TODO read stuff from the config, CLI
    -- httpContext <- liftIO newHttpContext
    
    let rtrConfig = if withRtr
            then Just $ defaultRtrConfig
                        & maybeSet #rtrPort rtrPort
                        & maybeSet #rtrAddress rtrAddress 
            else Nothing    
             

    appState <- liftIO newAppState    
    tvarDatabase <- liftIO $ newTVarIO database    

    let appContext = AppContext {        
        appState = appState,
        database = tvarDatabase,        
        appBottlenecks = appBottlenecks,
        logger = logger,        
        config = defaultConfig 
                & #talDirectory .~ tald 
                & #tmpDirectory .~ tmpd 
                & #cacheDirectory .~ cached 
                & #parallelism .~ Parallelism cpuParallelism ioParallelism
                & #rsyncConf . #rsyncRoot .~ rsyncd                
                & maybeSet (#rsyncConf . #rsyncTimeout) (Seconds <$> rsyncTimeout)
                & #rrdpConf . #tmpRoot .~ tmpd
                & maybeSet (#rrdpConf . #rrdpTimeout) (Seconds <$> rrdpTimeout)
                & maybeSet (#validationConfig . #revalidationInterval) (Seconds <$> revalidationInterval)
                & maybeSet (#validationConfig . #rrdpRepositoryRefreshInterval) (Seconds <$> rrdpRefreshInterval)
                & maybeSet (#validationConfig . #rsyncRepositoryRefreshInterval) (Seconds <$> rsyncRefreshInterval)
                & #validationConfig . #dontFetch .~ dontFetch                
                & maybeSet (#httpApiConf . #port) httpApiPort
                & #rtrConfig .~ rtrConfig
                & maybeSet #cacheLifeTime ((\hours -> Seconds (hours * 60 * 60)) <$> cacheLifetimeHours)
                & #lmdbSize .~ lmdbRealSize        
    }

    logDebugM logger [i|Created application context: #{config appContext}|]
    pure appContext    


orDefault :: Maybe a -> a -> a
m `orDefault` d = fromMaybe d m

maybeSet :: ASetter s s a b -> Maybe b -> s -> s
maybeSet lenz newValue big = maybe big (\val -> big & lenz .~ val) newValue

        
listTALFiles :: FilePath -> IO [FilePath]
listTALFiles talDirectory = do     
    names <- getDirectoryContents talDirectory
    pure $ map (talDirectory </>) $ 
            filter (".tal" `List.isSuffixOf`) $ 
            filter (`notElem` [".", ".."]) names


talsDir, rsyncDir, tmpDir, cacheDir :: FilePath -> IO (Either Text FilePath)
talsDir root  = checkSubDirectory root "tals"
rsyncDir root = createSubDirectoryIfNeeded root "rsync"
tmpDir root   = createSubDirectoryIfNeeded root "tmp"
cacheDir root = createSubDirectoryIfNeeded root "cache"


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


-- CLI Options-related machinery
data CLIOptions wrapped = CLIOptions {
    rpkiRootDirectory :: wrapped ::: Maybe FilePath <?> 
        "Root directory (default is ${HOME}/.rpki/).",

    cpuCount :: wrapped ::: Maybe Natural <?> 
        "CPU number available to the program (default is 2).",

    reset :: wrapped ::: Bool <?> 
        "Reset the disk cache of (i.e. remove ~/.rpki/cache/*.mdb files.",

    revalidationInterval :: wrapped ::: Maybe Int64 <?>          
        ("Re-validation interval in seconds, i.e. how often to re-download repositories are " 
       +++ "updated and certificate tree is re-validated. "
       +++ "Default is 13 minutes, i.e. 780 seconds."),

    cacheLifetimeHours :: wrapped ::: Maybe Int64 <?> 
        "Lifetime of objects in the local cache, in hours (default is 72 hours)",

    rrdpRefreshInterval :: wrapped ::: Maybe Int64 <?>          
        ("Period of time after which an RRDP repository must be updated," 
       +++ "in seconds (default is 120 seconds)"),

    rsyncRefreshInterval :: wrapped ::: Maybe Int64 <?>         
        ("Period of time after which an rsync repository must be updated, "
       +++ "in seconds (default is 11 minutes, i.e. 660 seconds)"),

    rrdpTimeout :: wrapped ::: Maybe Int64 <?> 
        ("Timeout for RRDP repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable"),

    rsyncTimeout :: wrapped ::: Maybe Int64 <?> 
        ("Timeout for rsync repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable"),

    httpApiPort :: wrapped ::: Maybe Word16 <?> 
        "Port to listen to for http API (default is 9999)",

    lmdbSize :: wrapped ::: Maybe Int64 <?> 
        ("Maximal LMDB cache size in MBs (default is 8192mb). Note that about 1Gb of cache is "
       +++ "required for every extra 24 hours of cache life time"),    

    withRtr :: wrapped ::: Bool <?> 
        "Start RTR server (default is false)",

    rtrAddress :: wrapped ::: Maybe String <?> 
        "Address to bind to for the RTR server (default is localhost)",

    rtrPort :: wrapped ::: Maybe Int16 <?> 
        "Port to listen to for the RTR server (default is 8283)",

    dontFetch :: wrapped ::: Bool <?> 
        "Don't fetch repositories, expect all the objects to be cached (mostly used for testing, default is false)."

} deriving (Generic)

instance ParseRecord (CLIOptions Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (CLIOptions Unwrapped)

type (+++) (a :: Symbol) (b :: Symbol) = AppendSymbol a b

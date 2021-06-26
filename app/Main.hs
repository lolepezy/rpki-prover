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
import           System.Environment
import           System.FilePath                  ((</>))

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

import           RPKI.TAL
import           RPKI.Util               (convert, fmtEx)
import           RPKI.Workflow

main :: IO ()
main = do
    cliOptions :: CLIOptions Unwrapped <- unwrapRecord 
        "RPKI prover, relying party software for RPKI"

    withLogLevel cliOptions $ \logLevel ->
        -- load config file and apply command line options    
        withAppLogger logLevel $ \logger -> liftIO $ do 
            
            if cliOptions ^. #initialise
                then 
                    -- init the FS layout and download TALs
                    void $ liftIO $ initialiseFS cliOptions logger              
                else do
                    -- run the validator
                    (appContext, validations) <- do              
                                runValidatorT (newValidatorPath "initialise") 
                                    $ createAppContext cliOptions logger                     
                    case appContext of
                        Left e ->
                            logError_ logger [i|Couldn't initialise: #{e}, problems: #{validations}.|]
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
createAppContext cliOptions@CLIOptions{..} logger = do
         
    (tald, rsyncd, tmpd, cached) <- fsLayout cliOptions logger CheckTALsExists

    let lmdbRealSize = Size $ lmdbSize `orDefault` 32768
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

    logInfoM logger [i|Created application context: #{config appContext}|]
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
                    (tald, _, _, _) <- fsLayout cliOptions logger CreateTALs

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
        -> ValidatorT IO (FilePath, FilePath, FilePath, FilePath)
fsLayout cli logger talsHandle = do 
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let root = getRootrDirectory cli
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
    pure (tald, rsyncd, tmpd, cached)


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


talsDir :: FilePath -> TALsHandle -> IO (Either Text FilePath)
talsDir root CreateTALs      = createSubDirectoryIfNeeded root "tals"
talsDir root CheckTALsExists = checkSubDirectory root "tals"

rsyncDir, tmpDir, cacheDir :: FilePath -> IO (Either Text FilePath)
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

getRootrDirectory :: CLIOptions Unwrapped -> Maybe FilePath
getRootrDirectory CLIOptions{..} = 
    case rpkiRootDirectory of 
        [] -> Nothing
        s  -> Just $ Prelude.last s

-- CLI Options-related machinery
data CLIOptions wrapped = CLIOptions {
    initialise :: wrapped ::: Bool <?> 
        ("If set, the FS layout will be created and TAL files will be downloaded." ),

    agreeWithArinRpa :: wrapped ::: Bool <?> 
        ("This is to indicate that you do accept (and maybe even have read) ARIN Relying Party Agreement " 
        +++ "and would like ARIN TAL to be downloaded."),        

    rpkiRootDirectory :: wrapped ::: [FilePath] <?> 
        ("Root directory (default is ${HOME}/.rpki/). This option can be passed multiple times and "
         +++ "the last one will be used, it is done for convenience of overriding this option with dockerised version."),

    cpuCount :: wrapped ::: Maybe Natural <?> 
        "CPU number available to the program (default is 2).",

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
        ("Timeout for RRDP repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable"),

    rsyncTimeout :: wrapped ::: Maybe Int64 <?> 
        ("Timeout for rsync repositories, in seconds. If fetching of a repository does not "
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
        "Don't fetch repositories, expect all the objects to be cached (mostly used for testing, default is false)."

} deriving (Generic)

instance ParseRecord (CLIOptions Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (CLIOptions Unwrapped)

type (+++) (a :: Symbol) (b :: Symbol) = AppendSymbol a b


withLogLevel :: CLIOptions Unwrapped -> (LogLevel -> IO ()) -> IO ()
withLogLevel CLIOptions{..} f =
    case logLevel of 
        Nothing -> f InfoL
        Just s  ->
            case Text.toLower $ Text.pack s of 
                "error" -> f ErrorL
                "warn"  -> f WarnL
                "info"  -> f InfoL
                "debug" -> f DebugL
                other   -> hPutStrLn stderr $ "Wrong log level: " <> Text.unpack other      

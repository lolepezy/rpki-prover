{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where
    
import           Colog

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Lens                     ((^.))
import           Data.Generics.Labels
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed


import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted

import           Data.Bifunctor
import qualified Data.ByteString                  as BS
import qualified Data.List                        as List
import           Data.Text                        (Text)

import           Data.String.Interpolate.IsString

import qualified Network.Wai.Handler.Warp         as Warp

import           System.Directory                 (doesDirectoryExist, getDirectoryContents, listDirectory, removeFile)
import           System.Environment
import           System.FilePath                  ((</>))

import           Options.Generic

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.AppContext
import           RPKI.Http.Server
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.Store.Base.LMDB             hiding (getEnv)
import           RPKI.Store.Util
import           RPKI.TAL
import           RPKI.Time
import           RPKI.TopDown
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.Version
import Data.Hourglass (Seconds)

type AppEnv = AppContext LmdbStorage


main :: IO ()
main = do
    -- load config file and apply command line options    
    logger <- createLogger

    (appContext, validations) <- runValidatorT (vContext $ URI "configuration") $ createAppContext logger
    case appContext of
        Left e ->
            logError_ logger [i|Couldn't initialise: #{e}|]
        Right appContext' ->
            runValidatorApp appContext'
            -- void $ concurrently
            --     (runHttpApi appContext')
            --     (runValidatorApp appContext')


runValidatorApp :: AppEnv -> IO ()
runValidatorApp appContext@AppContext {..} = do    
    worldVersion <- updateWorldVerion versions
    (result, elapsed) <- timedMS $ bootstapAllTAs appContext
    completeWorldVersion appContext worldVersion
    logDebug_ logger [i|Validated all TAs, took #{elapsed}ms.|]


bootstapAllTAs :: AppEnv -> IO [(Either AppError (), Validations)]
bootstapAllTAs appContext@AppContext {..} = do
    worldVersion <- updateWorldVerion versions
    talFileNames <- listTALFiles $ talDirectory config
    -- ttt <- listTALFiles $ talDirectory config
    -- let talFileNames = List.filter ("ripe" `List.isInfixOf`) ttt
    asyncs <- forM talFileNames $ \talFileName -> 
        async $ do
            (talContent, vs) <- runValidatorT (vContext $ URI $ convert talFileName) $ do
                                    t <- fromTry (RsyncE . FileReadError . fmtEx) $ BS.readFile talFileName
                                    vHoist $ fromEither $ first TAL_E $ parseTAL $ convert t    
            case talContent of 
                Left e -> do
                    logError_ logger [i|Error reading TAL #{talFileName}, e = #{e}.|]
                    pure (Left e, vs)
                Right talContent' -> 
                    bootstrapTA appContext talContent' worldVersion

    forM asyncs wait



runHttpApi :: AppEnv -> IO ()
runHttpApi appContext = Warp.run 9999 $ httpApi appContext


runGC :: AppEnv -> IO ()
runGC appContext = do
    Now now <- thisInstant
    let objectStore = appContext ^. #database . #objectStore
    let cacheLifeTime = appContext ^. typed @Config . #cacheLifeTime
    cleanObjectCache objectStore $ \(WorldVersion nanos) -> 
            let validatedAt = fromNanoseconds nanos
            in closeEnoughMoments validatedAt now cacheLifeTime
    

periodically :: Seconds -> IO () -> IO ()
periodically interval action = do
    
    pure ()


createAppContext :: AppLogger -> ValidatorT vc IO AppEnv
createAppContext logger = do    

    -- TODO Make it configurable?
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = home </> ".rpki"
    
    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir 
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    lmdb   <- fromEitherM $ first (InitE . InitError) <$> lmdbDir  rootDir 

    lmdbEnv  <- fromTry (InitE . InitError . fmtEx) $ mkLmdb lmdb 1000
    database <- fromTry (InitE . InitError . fmtEx) $ createDatabase lmdbEnv

    -- clean up tmp directory if it's not empty
    fromTry (InitE . InitError . fmtEx) $ 
        listDirectory tmpd >>= mapM_ (removeFile . (tmpd </>))

    versions <- liftIO createDynamicState

    -- TODO Make it configurable
    let parallelism = Parallelism getParallelism 64

    appBottlenecks <- liftIO $ do 
        cpuBottleneck <- newBottleneckIO $ cpuParallelism parallelism
        ioBottleneck  <- newBottleneckIO $ ioParallelism parallelism
        pure $ AppBottleneck cpuBottleneck ioBottleneck

    -- TODO read stuff from the config, CLI
    pure $ AppContext {        
        logger = logger,
        config = Config {
            talDirectory = tald,
            parallelism  = parallelism,
            rsyncConf    = RsyncConf rsyncd,
            rrdpConf     = RrdpConf { 
                tmpRoot = tmpd,
                -- Do not download files bigger than 1Gb
                -- TODO Make it configurable
                maxSize = Size 1024 * 1024 * 1024
            },
            validationConfig = ValidationConfig {
                rrdpRepositoryRefreshInterval  = 2 * 60,
                rsyncRepositoryRefreshInterval = 10 * 60
            },
            -- run cache GC every 30 minutes
            cacheCleanupInterval = 30 * 60,

            -- allow objects to stay in cache for one day
            cacheLifeTime = let oneDay = 24 * 60 * 60 in oneDay
        },
        versions = versions,
        database = database,
        appBottlenecks = appBottlenecks
    }

createLogger :: IO AppLogger
createLogger = do 
    -- TODO Use colog-concurrent instead of this
    lock <- newMVar True
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
        True ->  pure $ Right talDirectory


data Options = Options {
    rpkiRootDirectory :: FilePath,
    reset :: Bool
} deriving (Eq, Ord, Show, Generic)


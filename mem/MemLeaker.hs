{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes        #-}

import Colog hiding (extract)

import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent.Lifted
import           Data.Bifunctor
import           Data.String.Interpolate.IsString

import           System.Directory                 (removeFile, doesDirectoryExist, getDirectoryContents, listDirectory,
                                                   removePathForcibly)
import           System.Environment
import           System.Directory
import           System.FilePath                  ((</>))
import           System.IO                        (BufferMode (..), hSetBuffering, stdout)

import           Options.Generic

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Config
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.RRDP.RrdpFetch
import           RPKI.TAL
import           RPKI.TopDown
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.AppState

import           Data.Hourglass
import           RPKI.Repository
import           RPKI.Store.Base.LMDB hiding (getEnv)

import qualified RPKI.Store.MakeLmdb as Lmdb
import RPKI.Store.Base.Storage
import RPKI.Store.Repository
import RPKI.Store.Database
import RPKI.Workflow
import RPKI.Time
import Data.Generics.Product.Typed




main :: IO ()
main = do     
    logger <- createLogger
    z <- runValidatorT (newValidatorPath "configuration") $ createAppContext logger    
    case z of 
        (Left e, x) -> do 
            putStrLn $ "Error: " <> show e        
            
        (Right appContext, _) -> do             
            let repository = RrdpRepository {
                    uri      = RrdpURL $ URI "https://rrdp.ripe.net/notification.xml",
                    rrdpMeta = Nothing,
                    status   = Pending
                } 
            runValidatorT (newValidatorPath "download-rrdp") 
                $ updateObjectForRrdpRepository appContext repository   

            let tal = RFC_TAL {
                certificateLocations = newLocation "https://rpki.ripe.net/ta/ripe-ncc-ta.cer",
                publicKeyInfo = EncodedBase64 $
                    "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA0URYSGqUz2myBsOzeW1j" <>
                    "Q6NsxNvlLMyhWknvnl8NiBCs/T/S2XuNKQNZ+wBZxIgPPV2pFBFeQAvoH/WK83Hw" <>
                    "A26V2siwm/MY2nKZ+Olw+wlpzlZ1p3Ipj2eNcKrmit8BwBC8xImzuCGaV0jkRB0G" <>
                    "Z0hoH6Ml03umLprRsn6v0xOP0+l6Qc1ZHMFVFb385IQ7FQQTcVIxrdeMsoyJq9eM" <>
                    "kE6DoclHhF/NlSllXubASQ9KUWqJ0+Ot3QCXr4LXECMfkpkVR2TZT+v5v658bHVs" <>
                    "6ZxRD1b6Uk1uQKAyHUbn/tXvP8lrjAibGzVsXDT2L0x4Edx+QdixPgOji3gBMyL2" <>
                    "VwIDAQAB"
            }
            database' <- readTVarIO (appContext ^. #database)
            worldVersion <- updateWorldVerion (appContext ^. #appState)
            storedPubPoints   <- roTx database' $ \tx -> getPublicationPoints tx (repositoryStore database')
            repositoryContext <- newTVarIO $ newRepositoryContext storedPubPoints

            -- Don't update data
            let appContext' = appContext & typed @Config . typed @ValidationConfig . #dontFetch .~ True
            forever $ do                
                let now = versionToMoment worldVersion
                let cacheLifeTime = appContext ^. typed @Config ^. #cacheLifeTime
                validateTA appContext' tal worldVersion repositoryContext 
                ((deleted, kept), elapsed) <- timedMS $ cleanObjectCache database' $ versionIsOld now cacheLifeTime
                logInfo_ logger [i|Done with cache GC, deleted #{deleted} objects, kept #{kept}, took #{elapsed}ms|]                
            pure ()




-- Auxilliary functions

createAppContext :: AppLogger -> ValidatorT IO (AppContext LmdbStorage)
createAppContext logger = do

    -- TODO Make it configurable?
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = home </> ".mem-test"
        
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    lmdb   <- fromEitherM $ first (InitE . InitError) <$> cacheDir rootDir 

    lmdbEnv  <- fromTry (InitE . InitError . fmtEx) $ Lmdb.mkLmdb lmdb 1000 1000
    database <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv

    -- clean up tmp directory if it's not empty
    fromTry (InitE . InitError . fmtEx) $ 
        listDirectory tmpd >>= mapM_ (removeFile . (tmpd </>))

    let cpuCount' = getRtsCpuCount
    liftIO $ setCpuCount cpuCount'
    let cpuParallelism = 2 * cpuCount'

    -- Hardcoded (not sure it makes sense to make it configurable)
    let ioParallelism = 64  

    appBottlenecks <- liftIO $ do 
        cpuBottleneck <- newBottleneckIO cpuParallelism
        ioBottleneck  <- newBottleneckIO ioParallelism
        pure $ AppBottleneck cpuBottleneck ioBottleneck
    
    appState <- liftIO newAppState    
    tvarDatabase <- liftIO $ newTVarIO database

    let appContext = AppContext {        
        logger = logger,
        config = Config {
            talDirectory = rootDir </> "tals",
            tmpDirectory = tmpd,            
            cacheDirectory = rootDir </> "cache",
            parallelism  = Parallelism cpuParallelism ioParallelism,
            rsyncConf    = RsyncConf rsyncd (Seconds 600),
            rrdpConf     = RrdpConf { 
                tmpRoot = tmpd,
                -- Do not download files bigger than 1Gb, it's fishy
                maxSize = Size 1024 * 1024 * 1024,
                rrdpTimeout = Seconds (5 * 60)
            },
            validationConfig = ValidationConfig {
                revalidationInterval           = Seconds (13 * 60),
                rrdpRepositoryRefreshInterval  = Seconds 120,
                rsyncRepositoryRefreshInterval = Seconds (11 * 60),
                dontFetch = False
            },
            httpApiConf = HttpApiConfig {
                port = 9999
            },
            rtrConfig = Nothing,
            cacheCleanupInterval = 120 * 60,
            cacheLifeTime = Seconds $ 60 * 60 * 2,

            -- TODO Think about it, it should be lifetime or we should store N last versions
            oldVersionsLifetime = let twoHours = 2 * 60 * 60 in twoHours,

            storageDefragmentInterval = Seconds $ 60 * 60 * 12,

            lmdbSize = 2000
        },
        appState = appState,
        database = tvarDatabase,
        appBottlenecks = appBottlenecks
    }   
    pure appContext

createLogger :: IO AppLogger
createLogger = do 
    -- TODO Use colog-concurrent instead of this
    lock <- newMVar True
    pure $ AppLogger fullMessageAction lock
    where
        fullMessageAction = upgradeMessageAction defaultFieldMap $ 
            cmapM fmtRichMessageDefault logTextStdout     


rsyncDir, tmpDir, cacheDir :: FilePath -> IO (Either Text FilePath)
rsyncDir root = createSubDirectoryIfNeeded root "rsync"
tmpDir root   = createSubDirectoryIfNeeded root "tmp"
cacheDir root = createSubDirectoryIfNeeded root "cache"


createSubDirectoryIfNeeded :: FilePath -> FilePath -> IO (Either Text FilePath)
createSubDirectoryIfNeeded root sub = do
    let subDirectory = root </> sub
    exists <- doesDirectoryExist subDirectory
    unless exists $ createDirectory subDirectory   
    pure $ Right subDirectory

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes        #-}

import Colog hiding (extract)

import Control.Concurrent.STM
import           Control.Lens ((^.), (.~), (&))
import Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent.Lifted
import           Data.Bifunctor
import qualified Data.Set as Set
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

import           RPKI.Repository
import           RPKI.Store.Base.LMDB hiding (getEnv)

import qualified RPKI.Store.MakeLmdb as Lmdb
import RPKI.Store.Base.Storage
import RPKI.Store.Repository
import RPKI.Store.Database
import RPKI.Workflow
import RPKI.Time
import Data.Generics.Product.Typed
import Control.Concurrent.Async




main :: IO ()
main = do     
    logger <- createLogger
    z <- runValidatorT (newValidatorPath "configuration") $ createAppContext logger    
    case z of 
        (Left e, _) -> do 
            putStrLn $ "Error: " <> show e        
            
        (Right appContext, _) -> do             

            database' <- readTVarIO (appContext ^. #database)
            worldVersion <- updateWorldVersion (appContext ^. #appState)
            storedPubPoints   <- roTx database' $ \tx -> getPublicationPoints tx (repositoryStore database')

            forConcurrently_ sources $ \(repository, _) -> do
                    runValidatorT (newValidatorPath "download-rrdp") 
                        $ updateObjectForRrdpRepository appContext repository    
                

            -- Don't update data
            let appContext' = appContext & typed @Config . typed @ValidationConfig . #dontFetch .~ True
            replicateM_ 30 $ do                
                let now = versionToMoment worldVersion
                let cacheLifeTime = appContext ^. typed @Config ^. #cacheLifeTime
                (results, elapsed) <- timedMS $
                    inParallel
                        (appContext ^. #appBottlenecks . #cpuBottleneck 
                         <> appContext ^. #appBottlenecks . #ioBottleneck) 
                    -- forConcurrently
                        sources 
                        $ \(_, tal) -> do 
                            repositoryContext <- newTVarIO $ newRepositoryContext storedPubPoints
                            validateTA appContext' tal worldVersion repositoryContext 

                let TopDownResult {..} = mconcat results
                logInfo_ logger [i|Validated GC, #{Set.size vrps} VRPs, took #{elapsed}ms|]
                (cleanup, elapsed2) <- timedMS $ cleanObjectCache database' $ versionIsOld now cacheLifeTime
                logInfo_ logger [i|Done with cache GC, #{cleanup}, took #{elapsed2}ms|]

    where
        sources = let 
            ripeRrdp = RrdpRepository {
                uri      = RrdpURL $ URI "https://rrdp.ripe.net/notification.xml",
                rrdpMeta = Nothing,
                status   = Pending
            } 
            ripeTAL = RFC_TAL {
                certificateLocations = toLocations $ RrdpU $ RrdpURL $ URI "https://rpki.ripe.net/ta/ripe-ncc-ta.cer",
                publicKeyInfo = EncodedBase64 $
                    "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA0URYSGqUz2myBsOzeW1j" <>
                    "Q6NsxNvlLMyhWknvnl8NiBCs/T/S2XuNKQNZ+wBZxIgPPV2pFBFeQAvoH/WK83Hw" <>
                    "A26V2siwm/MY2nKZ+Olw+wlpzlZ1p3Ipj2eNcKrmit8BwBC8xImzuCGaV0jkRB0G" <>
                    "Z0hoH6Ml03umLprRsn6v0xOP0+l6Qc1ZHMFVFb385IQ7FQQTcVIxrdeMsoyJq9eM" <>
                    "kE6DoclHhF/NlSllXubASQ9KUWqJ0+Ot3QCXr4LXECMfkpkVR2TZT+v5v658bHVs" <>
                    "6ZxRD1b6Uk1uQKAyHUbn/tXvP8lrjAibGzVsXDT2L0x4Edx+QdixPgOji3gBMyL2" <>
                    "VwIDAQAB"
            }       
            apnicAs0Rrdp = RrdpRepository {
                uri      = RrdpURL $ URI "https://rrdp-as0.apnic.net/notification.xml",
                rrdpMeta = Nothing,
                status   = Pending
            }             
            apnicAS0TAL = RFC_TAL {
                certificateLocations = toLocations $ RrdpU $ RrdpURL $ 
                    URI "https://rpki-as0-web.apnic.net/repository/APNIC-AS0-AP/apnic-rpki-root-as0-origin.cer",
                publicKeyInfo = EncodedBase64 $
                    "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA7xn+C9dYQDHGaEIqFteu" <>
                    "EnW3r9KJOajc6Jl2ZdgB7qps+dvij1ZAhK/FTKBNGgzM7zLLg2dcDiZRBYd7bgFB" <>
                    "C+nZouOCsm/o6JRSZqk84bNqNcxuWuyt0iIBc9n0rZIo4YoJOh1Xjs1lq6B6MikR" <>
                    "2iTC1aApFC/haZAS1/i1awNcvAb9xfVdp0/MpI0Ip8rmJix33NCWtaORkn21JgTr" <>
                    "E3H0Ov8oAxYfbHLZQ8sI8gI7yrpipCDok8cCVi7+F579ROXvSpZUFF5a/rtWABoN" <>
                    "fXT5nFYMAZJoGoAazBIFBiCUaxUJsaTVChDdAw10qFQu7ZPKyTdoHh+LD0r8Sro7" <>
                    "qwIDAQAB"
            }                     
            apnicRrdp = RrdpRepository {
                uri      = RrdpURL $ URI "https://rrdp.apnic.net/notification.xml",
                rrdpMeta = Nothing,
                status   = Pending
            }             
            apnicTAL = RFC_TAL {
                certificateLocations = toLocations $ RrdpU $ RrdpURL $ URI "https://tal.apnic.net/apnic.cer",
                publicKeyInfo = EncodedBase64 $
                    "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAx9RWSL61YAAYumEiU8z8" <>
                    "qH2ETVIL01ilxZlzIL9JYSORMN5Cmtf8V2JblIealSqgOTGjvSjEsiV73s67zYQI" <>
                    "7C/iSOb96uf3/s86NqbxDiFQGN8qG7RNcdgVuUlAidl8WxvLNI8VhqbAB5uSg/Mr" <>
                    "LeSOvXRja041VptAxIhcGzDMvlAJRwkrYK/Mo8P4E2rSQgwqCgae0ebY1CsJ3Cjf" <>
                    "i67C1nw7oXqJJovvXJ4apGmEv8az23OLC6Ki54Ul/E6xk227BFttqFV3YMtKx42H" <>
                    "cCcDVZZy01n7JjzvO8ccaXmHIgR7utnqhBRNNq5Xc5ZhbkrUsNtiJmrZzVlgU6Ou" <>
                    "0wIDAQAB"
            }                     
            arinRrdp = RrdpRepository {
                uri      = RrdpURL $ URI "https://rrdp.arin.net/notification.xml",
                rrdpMeta = Nothing,
                status   = Pending
            }             
            arinTAL = RFC_TAL {
                certificateLocations = toLocations $ RrdpU $ RrdpURL $ 
                    URI "https://rrdp.arin.net/arin-rpki-ta.cer",
                publicKeyInfo = EncodedBase64 $
                    "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA3lZPjbHvMRV5sDDqfLc/685th5FnreHMJjg8" <>
                    "pEZUbG8Y8TQxSBsDebbsDpl3Ov3Cj1WtdrJ3CIfQODCPrrJdOBSrMATeUbPC+JlNf2SRP3UB+VJFgtTj" <>
                    "0RN8cEYIuhBW5t6AxQbHhdNQH+A1F/OJdw0q9da2U29Lx85nfFxvnC1EpK9CbLJS4m37+RlpNbT1cba+" <>
                    "b+loXpx0Qcb1C4UpJCGDy7uNf5w6/+l7RpATAHqqsX4qCtwwDYlbHzp2xk9owF3mkCxzl0HwncO+sEHH" <>
                    "eaL3OjtwdIGrRGeHi2Mpt+mvWHhtQqVG+51MHTyg+nIjWFKKGx1Q9+KDx4wJStwveQIDAQAB"
            }                     
            in [
                (ripeRrdp, ripeTAL), 
                (apnicAs0Rrdp, apnicAS0TAL), 
                (arinRrdp, arinTAL), 
                (apnicRrdp, apnicTAL)
            ]



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
        config = defaultConfig 
                    & #talDirectory .~ rootDir </> "tals"
                    & #tmpDirectory .~ tmpd 
                    & #cacheDirectory .~ rootDir </> "cache" 
                    & #parallelism .~ Parallelism cpuParallelism ioParallelism            
                    & #rrdpConf . #tmpRoot .~ tmpd, 
        appState = appState,
        database = tvarDatabase,
        appBottlenecks = appBottlenecks
    }   
    pure appContext

createLogger :: IO AppLogger
createLogger = do         
    pure $ AppLogger fullMessageAction
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

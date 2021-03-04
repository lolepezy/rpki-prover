{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes        #-}

import Colog hiding (extract)

import           Colog

import           Control.Monad
import           Control.Monad.IO.Class

import           Control.Lens                     ((^.))
import           Data.Generics.Labels
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Exception.Lifted

import           Data.Bifunctor
import qualified Data.ByteString                  as BS
import qualified Data.List                        as List
import           Data.Maybe
import           Data.Text                        (Text)

import           Data.String.Interpolate.IsString

import           GHC.TypeLits

import qualified Network.Wai.Handler.Warp         as Warp

import           System.Directory                 (removeFile, doesDirectoryExist, getDirectoryContents, listDirectory,
                                                   removePathForcibly)
import           System.Environment
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
import           RPKI.Time
import           RPKI.RRDP.RrdpFetch
import           RPKI.RRDP.Http
import           RPKI.TAL
import           RPKI.TopDown
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.AppState

import           Data.Hourglass
import           Data.Int                         (Int16, Int64)
import           Numeric.Natural                  (Natural)
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Base.LMDB hiding (getEnv)

import qualified System.IO.Posix.MMap.Lazy        as MmapLazy
import RPKI.RRDP.Types
import qualified RPKI.Store.MakeLmdb as Lmdb
import Control.Concurrent.STM


main :: IO ()
main = do 
    -- testDeltaLoad
    testSnapshotLoad
    pure ()


testSnapshotLoad :: IO ()
testSnapshotLoad = do
    logger <- createLogger
    z <- runValidatorT (newValidatorPath "configuration") $ createAppContext logger    
    case z of 
        (Left e, x) -> do 
            putStrLn $ "Error: " <> show e        

        (Right appContext, _) -> do 
            snapshotContent <- readB "../tmp/test-data/snapshot.xml"
            logDebug_ logger [i|Starting |]        
            let notification = makeNotification (SessionId "f8542d84-3d8a-4e5a-aee7-aa87198f61f2") (Serial 673)
            void $ forever $ do 
                (_, t) <- timedMS $ runValidatorT (newValidatorPath "snapshot") $ 
                            saveSnapshotSeq appContext (RrdpURL $ URI "bla.xml") notification snapshotContent    
                logDebug_ logger [i|Saved snapshot in #{t}ms |]  

testDeltaLoad :: IO ()
testDeltaLoad = do
    logger <- createLogger
    (Right appContext, _) <- runValidatorT (newValidatorPath "configuration") $ createAppContext logger    
    deltas <- filter (`notElem` [".", "..", "snapshot.xml"]) <$> listDirectory "../tmp/test-data/"
    deltaContents <- forM deltas $ \d -> MmapLazy.unsafeMMapFile $ "../tmp/test-data/" </> d
    logDebug_ logger [i|Starting |]    
    -- void $ runValidatorT (vContext "snapshot") $ mapM (saveDelta appContext) deltaContents    


newtype HexString = HexString BS.ByteString 
    deriving (Show, Eq, Ord)


type AppLmdbEnv = AppContext LmdbStorage

createAppContext :: AppLogger -> ValidatorT IO AppLmdbEnv
createAppContext logger = do

    -- TODO Make it configurable?
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = home </> ".rpki-rrdp-perf"
    
    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir 
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    lmdb   <- fromEitherM $ first (InitE . InitError) <$> lmdbDir  rootDir 

    lmdbEnv  <- fromTry (InitE . InitError . fmtEx) $ Lmdb.mkLmdb lmdb 1000 1000
    database <- fromTry (InitE . InitError . fmtEx) $ Lmdb.createDatabase lmdbEnv

    -- clean up tmp directory if it's not empty
    fromTry (InitE . InitError . fmtEx) $ 
        listDirectory tmpd >>= mapM_ (removeFile . (tmpd </>))

    appState <- liftIO newAppState    
    tvarDatabase <- liftIO $ newTVarIO database

    let cpuCount' = getRtsCpuCount
    liftIO $ setCpuCount cpuCount'
    let cpuParallelism = 2 * cpuCount'

    -- Hardcoded (not sure it makes sense to make it configurable)
    let ioParallelism = 64  

    appBottlenecks <- liftIO $ do 
        cpuBottleneck <- newBottleneckIO cpuParallelism
        ioBottleneck  <- newBottleneckIO ioParallelism
        pure $ AppBottleneck cpuBottleneck ioBottleneck    

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

            storageCompactionInterval = Seconds $ 60 * 60 * 12,

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



makeNotification :: SessionId -> Serial -> Notification
makeNotification sessionId' serial' = Notification {
    version = Version 1,
    sessionId = sessionId',
    serial = serial',
    snapshotInfo = SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB"),
    deltas = []
  }
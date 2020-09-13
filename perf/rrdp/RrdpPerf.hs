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
import           RPKI.Errors
import           RPKI.Http.Server
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.RRDP.HttpContext
import           RPKI.RRDP.RrdpFetch
import           RPKI.Store.Util
import           RPKI.TAL
import           RPKI.TopDown
import           RPKI.Util                        (convert, fmtEx)
import           RPKI.Version

import           Data.Hourglass
import           Data.Int                         (Int16, Int64)
import           Numeric.Natural                  (Natural)
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import           RPKI.Store.Base.LMDB hiding (getEnv)

import qualified System.IO.Posix.MMap.Lazy        as MmapLazy
import RPKI.RRDP.Types


main :: IO ()
main = do 
    -- testDeltaLoad
    testSnapshotLoad
    pure ()


testSnapshotLoad :: IO ()
testSnapshotLoad = do
    logger <- createLogger
    (Right appContext, _) <- runValidatorT (vContext "configuration") $ createAppContext logger    
    snapshotContent <- MmapLazy.unsafeMMapFile "../tmp/test-data/snapshot.xml"
    logDebug_ logger [i|Starting |]    
    stats <- newRrdpStat
    let notification = makeNotification (SessionId "f8542d84-3d8a-4e5a-aee7-aa87198f61f2") (Serial 673)
    void $ runValidatorT (vContext "snapshot") $ saveSnapshot appContext stats (RrdpURL $ URI "bla.xml") notification snapshotContent    

testDeltaLoad :: IO ()
testDeltaLoad = do
    logger <- createLogger
    (Right appContext, _) <- runValidatorT (vContext "configuration") $ createAppContext logger    
    deltas <- filter (`notElem` [".", "..", "snapshot.xml"]) <$> listDirectory "../tmp/test-data/"
    deltaContents <- forM deltas $ \d -> MmapLazy.unsafeMMapFile $ "../tmp/test-data/" </> d
    logDebug_ logger [i|Starting |]    
    -- void $ runValidatorT (vContext "snapshot") $ mapM (saveDelta appContext) deltaContents    


-- downloadBigFile :: IO ()
-- downloadBigFile = do
--     logger <- createLogger
--     (Right appContext, _) <- runValidatorT (vContext $ URI "configuration") $ createAppContext logger  
--     let rrdpConf = appContext ^. typed @Config . typed 
--     let uri = "https://rrdp.ripe.net/5d507b9b-b7c7-47ef-8d12-2766a07d61d2/2474/snapshot.xml"
--     let hash = makeHash "954DC8E9DF1DF97EF1DD6BE50A93AE50CFA998E9AD5BCEC44B8EC8881177D14B"
--     (rawContent, _) <- downloadHashedLazyBS rrdpConf uri hash
--                                 (RrdpE . CantDownloadSnapshot . show)                 
--                                 (\actualHash -> Left $ RrdpE $ SnapshotHashMismatch hash actualHash)    
--     pure ()

toBytes :: HexString -> BS.ByteString
toBytes (HexString bs) = bs

newtype HexString = HexString BS.ByteString 
    deriving (Show, Eq, Ord)


type AppEnv = AppContext LmdbStorage

createAppContext :: AppLogger -> ValidatorT vc IO AppEnv
createAppContext logger = do

    -- TODO Make it configurable?
    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = home </> ".rpki"
    
    tald   <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir 
    rsyncd <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    lmdb   <- fromEitherM $ first (InitE . InitError) <$> lmdbDir  rootDir 

    lmdbEnv  <- fromTry (InitE . InitError . fmtEx) $ mkLmdb lmdb 1000 1000
    database <- fromTry (InitE . InitError . fmtEx) $ createDatabase lmdbEnv

    -- clean up tmp directory if it's not empty
    fromTry (InitE . InitError . fmtEx) $ 
        listDirectory tmpd >>= mapM_ (removeFile . (tmpd </>))

    versions <- liftIO createDynamicState

    let cpuCount' = getRtsCpuCount
    liftIO $ setCpuCount cpuCount'
    let cpuParallelism = 2 * cpuCount'

    -- Hardcoded (not sure it makes sense to make it configurable)
    let ioParallelism = 64  

    appBottlenecks <- liftIO $ do 
        cpuBottleneck <- newBottleneckIO cpuParallelism
        ioBottleneck  <- newBottleneckIO ioParallelism
        pure $ AppBottleneck cpuBottleneck ioBottleneck

    httpContext <- liftIO newHttpContext

    let appContext = AppContext {        
        logger = logger,
        config = Config {
            talDirectory = tald,
            parallelism  = Parallelism cpuParallelism ioParallelism,
            rsyncConf    = RsyncConf rsyncd (Seconds (7 * 60)),
            rrdpConf     = RrdpConf { 
                tmpRoot = tmpd,
                -- Do not download files bigger than 1Gb, it's fishy
                maxSize = Size 1024 * 1024 * 1024,
                rrdpTimeout = Seconds (5 * 60)
            },
            validationConfig = ValidationConfig {                
                repositoryGracePeriod          = Nothing,
                revalidationInterval           = Seconds $ (13 * 60),
                rrdpRepositoryRefreshInterval  = Seconds $  120,
                rsyncRepositoryRefreshInterval = Seconds $ (11 * 660)
            },
            httpApiConf = HttpApiConf {
                port = 9999
            },
            cacheCleanupInterval = 30 * 60,
            cacheLifeTime = Seconds $ 60 * 60 * ( 72),

            -- TODO Think about it, it should be in lifetime or we should store N last versions
            oldVersionsLifetime = let twoHours = 2 * 60 * 60 in twoHours
        },
        versions = versions,
        database = database,
        appBottlenecks = appBottlenecks,
        httpContext = httpContext
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes        #-}

import Colog hiding (extract)

import           Control.Lens                     ((^.))
import           Control.Monad.Except
import           Data.Generics.Labels
import           Data.Generics.Product.Fields

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as LBS
import           Data.Text                        (Text)
import qualified Data.Text                        as Text

import           Data.String.Interpolate.IsString
import           Data.Time.Clock                  (getCurrentTime)

import           Lmdb.Codec
import           Lmdb.Connection
import           Lmdb.Types
import           RPKI.Store.Base.LMDB             (LmdbEnv, LmdbStorage)

import qualified Data.ByteString.Streaming        as Q
import           Data.ByteString.Streaming.HTTP
import           Streaming

import           Data.List.NonEmpty               (NonEmpty (..))

import           System.IO                        (hClose)
import qualified System.IO.Posix.MMap             as Mmap
import           System.IO.Posix.MMap.Lazy        (unsafeMMapFile)
import           System.IO.Temp                   (withSystemTempFile)

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.AppContext
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.AppContext
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.RRDP.Update
import           RPKI.RRDP.Http
import           RPKI.Rsync
import           RPKI.Store.Database
import           RPKI.Store.Repository            (getTaPublicationPoints)
import           RPKI.TAL
import           RPKI.TopDown
import           RPKI.Validation.Crypto
import           RPKI.Validation.ObjectValidation
import           RPKI.Version

import           RPKI.Store.Base.Storable
import           System.Directory                 (doesDirectoryExist, getDirectoryContents, listDirectory, removeFile)
import           System.Environment
import           System.FilePath                  ((</>))

import           RPKI.Store.Util
import           RPKI.Util

import qualified System.IO.Posix.MMap.Lazy        as MmapLazy


main :: IO ()
main = do 
    testDeltaLoad
    -- testSnapshotLoad
    pure ()


testSnapshotLoad :: IO ()
testSnapshotLoad = do
    logger <- createLogger
    (Right appContext, _) <- runValidatorT (vContext $ URI "configuration") $ createAppContext logger    
    snapshotContent <- MmapLazy.unsafeMMapFile "../tmp/test-data/snapshot.xml"
    logDebug_ logger [i|Starting |]    
    void $ runValidatorT (vContext $ URI "snapshot") $ saveSnapshot appContext snapshotContent    

testDeltaLoad :: IO ()
testDeltaLoad = do
    logger <- createLogger
    (Right appContext, _) <- runValidatorT (vContext $ URI "configuration") $ createAppContext logger    
    deltas <- filter (`notElem` [".", "..", "snapshot.xml"]) <$> listDirectory "../tmp/test-data/"
    deltaContents <- forM deltas $ \d -> MmapLazy.unsafeMMapFile $ "../tmp/test-data/" </> d
    logDebug_ logger [i|Starting |]    
    void $ runValidatorT (vContext $ URI "snapshot") $ mapM (saveDelta appContext) deltaContents    


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
            }
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

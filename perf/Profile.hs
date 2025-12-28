{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Lens ((^.), (&))
import           Control.Lens.Setter
import           Control.Concurrent.STM

import           Control.Monad.IO.Class

import           Data.Bifunctor
import           Data.String.Interpolate.IsString

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
import           RPKI.Logging
import           RPKI.Store.AppStorage
import           RPKI.Store.AppLmdbStorage
import qualified RPKI.Store.MakeLmdb as Lmdb

import           RPKI.RRDP.RrdpFetch

import           RPKI.Time
import           RPKI.Util               
import           RPKI.UniqueId
import           RPKI.RRDP.Types
import           RPKI.RRDP.Http



main :: IO ()
main = do
    testSnapshotLoad
    
testSnapshotLoad :: IO ()
testSnapshotLoad = do
    let logConfig = LogConfig DebugL MainLog
    withLogger logConfig (\_ -> pure ()) $ \logger -> liftIO $ do    
        z <- runValidatorT (newScopes "configuration") $ createAppContext logger
        case z of
            (Left e, _) -> do
                logError logger $ convert $ show e

            (Right appContext, _) -> do
                let fileName =  "../tmp/test-data/snapshot.xml"
                fileSize <- Size . fromIntegral <$> getFileSize fileName                
                let notification = makeNotification (SessionId "4a394319-7460-4141-a416-1addb69284ff") (RrdpSerial 98750)
                version <- newWorldVersion
                snapshotContent <- readB fileName fileSize
                (_, t) <- timedMS $ runValidatorT (newScopes "snapshot") $
                            saveSnapshot appContext version (RrdpURL $ URI "bla.xml") 
                                            notification snapshotContent
                logDebug logger [i|Saved snapshot in #{t}ms |]    



createAppContext :: AppLogger -> ValidatorT IO AppLmdbEnv
createAppContext logger = do

    liftIO $ setCpuCount 6
    
    let parallelism = makeParallelism 8

    let config = defaultConfig & #parallelism .~ parallelism

    home <- fromTry (InitE . InitError . fmtEx) $ getEnv "HOME"
    let rootDir = home </> ".rpki-profiler"

    _ <- fromEitherM $ first (InitE . InitError) <$> talsDir  rootDir
    _ <- fromEitherM $ first (InitE . InitError) <$> rsyncDir rootDir
    tmpd   <- fromEitherM $ first (InitE . InitError) <$> tmpDir   rootDir
    cached <- fromEitherM $ first (InitE . InitError) <$> lmdbDir  rootDir

    lmdbEnv <- setupLmdbCache UseExisting logger cached config

    (db, _) <- fromTry (InitE . InitError . fmtEx) $
                        Lmdb.createDatabase lmdbEnv logger Lmdb.DontCheckVersion

    -- clean up tmp directory if it's not empty
    cleanDir tmpd
    
    appState <- liftIO newAppState
    database <- liftIO $ newTVarIO db    

    let executableVersion = thisExecutableVersion
    pure AppContext {..}    


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


makeNotification :: SessionId -> RrdpSerial -> Notification
makeNotification sessionId' serial' = Notification {
    version = Version 1,
    sessionId = sessionId',
    serial = serial',
    snapshotInfo = SnapshotInfo (URI "http://bla.com/snapshot.xml") (Hash "AABB"),
    deltas = []
  }
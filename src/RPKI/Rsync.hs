{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Rsync where

import Control.DeepSeq (($!!))

import Data.Bifunctor

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Reader
import           UnliftIO.Exception

import qualified Data.ByteString                   as B
import qualified Data.Text                  as T

import           RPKI.Domain
import           RPKI.Store.Base.Storage
import           RPKI.Parse.Parse
import qualified RPKI.Util as U
import           RPKI.Logging

import Data.Has

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan

import System.Directory ( doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing )
import System.FilePath ( (</>) )

import System.Process.Typed
import System.Exit

newtype RsyncConf = RsyncConf {
  rsyncRoot :: FilePath
}

-- | Download one file using rsync
rsyncFile :: (Has RsyncConf conf, Has AppLogger conf) => 
              URI -> 
              ReaderT conf IO (Either SomeError RpkiObject)
rsyncFile (URI uri) = do
  RsyncConf {..} <- asks getter
  let destination = rsyncDestination rsyncRoot uri
  let rsync = rsyncProc (URI uri) destination RsyncOneFile
  logger :: AppLogger   <- asks getter 
  (exitCode, stdout, stderr) <- lift $ readProcess rsync      
  case exitCode of  
    ExitFailure errorCode -> do
      lift $ logError_ logger $ U.convert $ "Rsync process failed: " <> show rsync <> 
        " with code " <> show errorCode <> 
        " sdterr = " <> show stderr <>
        " stdout = " <> show stdout
      pure $ Left $ RsyncE $ RsyncProcessError errorCode stderr  
    ExitSuccess -> do
      read' <- lift $ first (RsyncE . FileReadError . U.fmtEx) <$> 
        try (B.readFile destination)        

      pure $ first ParseE . readObject (U.convert uri) =<< read'


processRsync :: (Has RsyncConf conf, Has AppLogger conf, Storage s) => 
                RsyncRepository -> s -> ReaderT conf IO (Either SomeError ())
processRsync (RsyncRepository (URI uri)) storage = do  
  RsyncConf {..} <- asks getter
  let destination = rsyncDestination rsyncRoot uri
  let rsync = rsyncProc (URI uri) destination RsyncDirectory

  created <- lift $
    first (RsyncE . FileReadError . U.fmtEx) <$> 
      try (createDirectoryIfMissing True destination)
  case created of      
    Left e  -> pure $ Left e
    Right _ -> do
      logger :: AppLogger   <- asks getter 
      (exitCode, stdout, stderr) <- lift $ readProcess rsync      
      case exitCode of  
        ExitSuccess ->
          lift $ loadRsyncRepository logger rsyncRoot storage      
        ExitFailure errorCode -> do
          lift $ logError_ logger $ T.pack $ "Rsync process failed: " <> show rsync <> 
            " with code " <> show errorCode <> 
            " sdterr = " <> show stderr <>
            " stdout = " <> show stdout
          pure $ Left $ RsyncE $ RsyncProcessError errorCode stderr  
    

data RsyncCL = RsyncOneFile | RsyncDirectory

rsyncProc :: URI -> FilePath -> RsyncCL -> ProcessConfig () () ()
rsyncProc (URI uri) destination rsyncCL = 
  let extraOptions = case rsyncCL of 
        RsyncOneFile   -> []
        RsyncDirectory -> ["--recursive", "--delete", "--copy-links" ]
      options = [ "--timeout=300",  "--update",  "--times" ] ++ extraOptions
      in proc "rsync" $ options ++ [ T.unpack uri, destination ]

-- TODO Make it generate shorter filenames
rsyncDestination :: FilePath -> T.Text -> FilePath
rsyncDestination root uri = root </> T.unpack (U.normalizeUri uri)

-- | Recursively traverse given directory and save all the parseabvle 
-- | objects from there into the storage.
loadRsyncRepository :: (Storage s, Logger logger) => 
                        logger ->
                        FilePath -> 
                        s -> 
                        IO (Either SomeError ())
loadRsyncRepository logger topPath storage = do
    (chanIn, chanOut) <- Chan.newChan 12
    (r1, r2) <- concurrently (travelrseFS chanIn) (saveObjects chanOut)
    pure $ first RsyncE r1 >> first StorageE r2
  where 
    travelrseFS chanIn = 
      first (FileReadError . U.fmtEx) <$> try (
          readFiles chanIn topPath 
            `finally` 
            Chan.writeChan chanIn Nothing)        

    readFiles chanIn currentPath = do
      names <- getDirectoryContents currentPath
      let properNames = filter (`notElem` [".", ".."]) names
      forM_ properNames $ \name -> do
        let path = currentPath </> name
        doesDirectoryExist path >>= \case
          True  -> readFiles chanIn path
          False -> 
            when (supportedExtension path) $ do
              a <- async $ do 
                bs <- B.readFile path                
                pure $!! (\ro -> (getHash ro, toStorable ro)) <$> 
                  first ParseE (readObject path bs)
              Chan.writeChan chanIn $ Just a      

    saveObjects chanOut = 
      first (StorageError . U.fmtEx) <$> 
        try (readWriteTx storage go)
      where 
        go tx = Chan.readChan chanOut >>= \case 
            Nothing -> pure ()
            Just a  -> do
              try (wait a) >>= \case
                Left (e :: SomeException) -> 
                  logError_ logger $ U.convert ("An error reading the object: " <> show e)
                Right (Left e) -> 
                  logError_ logger $ U.convert ("An error parsing or serialising the object: " <> show e)
                Right (Right (h, st)) -> 
                  getByHash tx storage h >>= \case 
                      Nothing -> storeObj tx storage (h, st)
                      Just _  ->
                          -- TODO Add location
                          logInfo_ logger $ U.convert ("There's an existing object with hash: " <> show (hexHash h) <> ", ignoring the new one.")
              go tx




              


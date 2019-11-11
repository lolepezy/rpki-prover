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
import           RPKI.Store.Storage
import           RPKI.Parse.Parse
import qualified RPKI.Util as U
import            RPKI.Logging

import Data.Has

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan

import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath ( (</>) )

import System.Process.Typed
import System.Exit

data RsyncConf = RsyncConf {
  rsyncRoot :: FilePath
}

processRsync :: (Has RsyncConf conf, Has AppLogger conf) => 
                Storage s => 
                RsyncRepository -> s -> ReaderT conf IO (Either SomeError ())
processRsync (RsyncRepository (URI uri)) storage = do  
  RsyncConf {..} <- asks getter
  let destination = destinationDirectory rsyncRoot uri
  let rsync = proc "rsync" [
          "--timeout=300", 
          "--update", 
          "--times", 
          "--copy-links", 
          T.unpack uri, 
          destination
        ]

  logger :: AppLogger   <- asks getter
  (exitCode, _, stderr) <- lift $ readProcess rsync
  case exitCode of  
    ExitSuccess ->
      lift $ loadRsyncRepository logger rsyncRoot storage      
    ExitFailure errorCode -> do
      lift $ logError_ logger $ T.pack $ "Rsync process failed: " <> show rsync <> 
        " with code " <> show errorCode <> " and message " <> show stderr
      pure $ Left $ RsyncE $ RsyncProcessError errorCode stderr
  where
    destinationDirectory root uri = root </> T.unpack (U.normalizeUri uri)


loadRsyncRepository :: (Storage s, Logger logger) => 
                        logger ->
                        FilePath -> 
                        s -> 
                        IO (Either SomeError ())
loadRsyncRepository logger topPath storage = do
    (chanIn, chanOut) <- Chan.newChan 12
    -- count <- newIORef (0 :: Int)
    r <- concurrently (travelrseFS chanIn) (saveObjects chanOut)

    -- TODO Implement
    -- case r  of 
    --   (Left e1, Left e2) -> 
    pure $ Right ()
  where 
    travelrseFS chanIn = 
      first (StorageError . U.fmtEx) <$> try (
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
                          logError_ logger $ U.convert ("There's an existing object with hash: " <> show (hexHash h) <> ", ignoring the new one.")
              go tx




              


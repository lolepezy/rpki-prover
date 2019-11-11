{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Rsync where

import Colog
import Control.DeepSeq (($!!))

import Data.Bifunctor

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Reader
import           UnliftIO.Exception

import qualified Data.ByteString                   as B

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

data RsyncConf = RsyncConf {
  rsyncRoot :: FilePath
}

processRsync :: (Has RsyncConf conf, Has AppLogger conf) => 
                Storage s => 
                Repository 'Rsync -> s -> ReaderT conf IO ()
processRsync (RsyncRepo (URI u)) storage = do
  -- proc "rsync" ["/etc/hosts", "-", "/etc/group"]
  RsyncConf {..}   <- asks getter
  logger :: AppLogger <- asks getter
  lift $ loadRsyncRepository logger rsyncRoot storage
  pure ()


loadRsyncRepository :: (Storage s, Logger logger) => 
                        logger ->
                        FilePath -> 
                        s -> 
                        IO ()
loadRsyncRepository logger topPath storage = do
    (chanIn, chanOut) <- Chan.newChan 12
    void $ concurrently 
      (readFiles chanIn topPath `finally` Chan.writeChan chanIn Nothing)
      (saveObjects chanOut)
  where 
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
      readWriteTx storage go
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




              


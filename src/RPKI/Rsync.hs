{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.Rsync where

import Control.DeepSeq (($!!))

import Data.Bifunctor

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           UnliftIO.Exception hiding (fromEither)

import qualified Data.ByteString                   as B
import qualified Data.Text                  as T

import           RPKI.Domain
import           RPKI.AppMonad
import           RPKI.Store.Base.Storage
import           RPKI.Store.Stores
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
              (B.ByteString -> Maybe ValidationError) ->
              ValidatorT conf IO RpkiObject
rsyncFile (URI uri) binaryCheck = do
  RsyncConf {..} <- asks getter
  let destination = rsyncDestination rsyncRoot uri
  let rsync = rsyncProcess (URI uri) destination RsyncOneFile
  logger :: AppLogger        <- asks getter 
  (exitCode, stdout, stderr) <- lift3 $ readProcess rsync      
  case exitCode of  
    ExitFailure errorCode -> do
      lift3 $ logError_ logger $ U.convert $ "Rsync process failed: " <> show rsync <> 
        " with code " <> show errorCode <> 
        " sdterr = " <> show stderr <>
        " stdout = " <> show stdout
      lift $ throwE $ RsyncE $ RsyncProcessError errorCode stderr  
    ExitSuccess -> do
        bs <- fromTry (RsyncE . FileReadError . U.fmtEx) $ B.readFile destination
        case binaryCheck bs of 
          Nothing -> fromEither $ first ParseE $ readObject (U.convert uri) bs
          Just e  -> lift $ throwE $ ValidationE e


processRsync :: (Has RsyncConf conf, Has AppLogger conf, Storage s) => 
                RsyncRepository -> 
                RpkiObjectStore s -> 
                ValidatorT conf IO ()
processRsync (RsyncRepository (URI uri)) objectStore = do 
  RsyncConf {..} <- asks getter
  let destination = rsyncDestination rsyncRoot uri
  let rsync = rsyncProcess (URI uri) destination RsyncDirectory

  _ <- fromTry (RsyncE . FileReadError . U.fmtEx) $ 
    createDirectoryIfMissing True destination
  
  logger :: AppLogger   <- asks getter 
  (exitCode, stdout, stderr) <- lift $ readProcess rsync      
  case exitCode of  
    ExitSuccess ->
      fromIOEither $ loadRsyncRepository logger rsyncRoot objectStore
    ExitFailure errorCode -> do
      lift3 $ logError_ logger $ T.pack $ "Rsync process failed: " <> show rsync <> 
        " with code " <> show errorCode <> 
        " sdterr = " <> show stderr <>
        " stdout = " <> show stdout
      lift $ throwE $ RsyncE $ RsyncProcessError errorCode stderr  
    

data RsyncCL = RsyncOneFile | RsyncDirectory

rsyncProcess :: URI -> FilePath -> RsyncCL -> ProcessConfig () () ()
rsyncProcess (URI uri) destination rsyncCL = 
  let extraOptions = case rsyncCL of 
        RsyncOneFile   -> []
        RsyncDirectory -> ["--recursive", "--delete", "--copy-links" ]
      options = [ "--timeout=300",  "--update",  "--times" ] ++ extraOptions
      in proc "rsync" $ options ++ [ T.unpack uri, destination ]

-- TODO Make it generate shorter filenames
rsyncDestination :: FilePath -> T.Text -> FilePath
rsyncDestination root uri = root </> T.unpack (U.normalizeUri uri)

-- | Recursively traverse given directory and save all the parseable 
-- | objects from there into the storage.
loadRsyncRepository :: (Storage s, Logger logger) => 
                        logger ->
                        FilePath -> 
                        RpkiObjectStore s -> 
                        IO (Either SomeError ())
loadRsyncRepository logger topPath objectStore = do
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
                -- "storableValue ro" has to happen in this thread, as it the way
                -- to force computation of the serialised object
                pure $!! (\ro -> (getHash ro, storableValue ro)) <$> 
                  first ParseE (readObject path bs)
              Chan.writeChan chanIn $ Just a      

    saveObjects chanOut = 
      first (StorageError . U.fmtEx) <$> 
        try (rwTx (storage objectStore) go)
      where 
        go tx = 
          Chan.readChan chanOut >>= \case 
            Nothing -> pure ()
            Just a  -> try (wait a) >>= process tx >> go tx

        process tx = \case
          Left (e :: SomeException) -> 
            logError_ logger $ U.convert ("An error reading the object: " <> show e)
          Right (Left e) -> 
            logError_ logger $ U.convert ("An error parsing or serialising the object: " <> show e)
          Right (Right (h, sValue)) -> 
            getByHash tx objectStore h >>= \case 
                Nothing -> putObject tx objectStore h sValue
                Just _  ->
                    -- TODO Add location
                    logInfo_ logger $ U.convert ("There's an existing object with hash: " <> show (hexHash h) <> ", ignoring the new one.")      
              




              


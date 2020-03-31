{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE NumericUnderscores #-}

module RPKI.Rsync where

import           Data.Bifunctor

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import           UnliftIO.Exception                    hiding (fromEither)

import qualified Data.ByteString                       as B
import           Data.String.Interpolate
import qualified Data.Text                             as T
import           Data.IORef

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parse.Parse
import           RPKI.Validation.ObjectValidation
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Storable
import           RPKI.Store.Stores
import qualified RPKI.Util                             as U

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan

import           System.Directory                      (createDirectoryIfMissing,
                                                        doesDirectoryExist,
                                                        getDirectoryContents)
import           System.FilePath                       ((</>))

import           System.IO
import           System.Exit
import           System.Process.Typed

import System.IO.Posix.MMap (unsafeMMapFile)

-- | Download one file using rsync
rsyncFile :: AppContext -> 
            URI -> 
            ValidatorT vc IO RpkiObject
rsyncFile AppContext{..}(URI uri) = do
    let RsyncConf {..} = rsyncConf
    let destination = rsyncDestination rsyncRoot uri
    let rsync = rsyncProcess (URI uri) destination RsyncOneFile
    (exitCode, out, err) <- readProcess rsync      
    case exitCode of  
        ExitFailure errorCode -> do
            lift3 $ logError_ logger [i|Rsync process failed: #rsync 
                                        with code #{errorCode}, 
                                        stderr = #{err}, 
                                        stdout = #{out}|]        
            throwError $ RsyncE $ RsyncProcessError errorCode err  
        ExitSuccess -> do
            fileSize  <- fromTry (RsyncE . FileReadError . U.fmtEx) $ getFileSize destination
            pureToValidatorT $ validateSizeM fileSize
            bs        <- fromTry (RsyncE . FileReadError . U.fmtEx) $ fileContent destination
            fromEither $ first ParseE $ readObject (U.convert uri) bs


-- | Process the whole rsync repository, download it, traverse the directory and 
-- | add all the relevant objects to the storage.
processRsync :: Storage s => 
                AppContext ->
                RsyncRepository -> 
                RpkiObjectStore s -> 
                ValidatorT conf IO ()
processRsync AppContext{..} (RsyncRepository (URI uri)) objectStore = do 
    let RsyncConf {..} = rsyncConf
    let destination = rsyncDestination rsyncRoot uri
    let rsync = rsyncProcess (URI uri) destination RsyncDirectory

    _ <- fromTry (RsyncE . FileReadError . U.fmtEx) $ 
        createDirectoryIfMissing True destination
        
    lift3 $ logInfo_ logger [i|Going to run #{rsync}|]
    (exitCode, out, err) <- lift $ readProcess rsync
    lift3 $ logInfo_ logger [i|Finished rsynching #{destination}|]
    case exitCode of  
        ExitSuccess -> do
            count <- fromIOEither $ loadRsyncRepository logger config rsyncRoot objectStore      
            lift3 $ logInfo_ logger [i|Finished loading #{count} objects into local storage|]      
        ExitFailure errorCode -> do
            lift3 $ logError_ logger [i|Rsync process failed: #{rsync} 
                                        with code #{errorCode}, 
                                        stderr = #{err}, 
                                        stdout = #{out}|]
            throwError $ RsyncE $ RsyncProcessError errorCode err  
    

-- | Recursively traverse given directory and save all the parseable 
-- | objects into the storage.
loadRsyncRepository :: (Storage s, Logger logger) => 
                        logger ->
                        Config ->
                        FilePath -> 
                        RpkiObjectStore s -> 
                        IO (Either SomeError Integer)
loadRsyncRepository logger config topPath objectStore = do
    counter <- newIORef 0
    (chanIn, chanOut) <- Chan.newChan $ parallelism config
    (r1, r2) <- concurrently (travelrseFS chanIn) (saveObjects counter chanOut)
    c <- readIORef counter
    pure (first RsyncE r1 >> first StorageE r2 >> pure c)
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
                            (_, content)  <- getSizeAndContent path                
                            pure $! case content of 
                                Left e   -> SError $ ValidationE e
                                Right bs -> 
                                    case first ParseE $ readObject path bs of
                                        Left e   -> SError e
                                        -- "toStorableObject ro" has to happen in this thread, as it is the way to 
                                        -- force computation of the serialised object and gain some parallelism
                                        Right ro -> SObject $ toStorableObject ro
                        
                        Chan.writeChan chanIn $ Just a      

    saveObjects counter chanOut = 
        first (StorageError . U.fmtEx) <$> 
            try (rwTx objectStore go)
        where 
            go tx = 
                Chan.readChan chanOut >>= \case 
                    Nothing -> pure ()
                    Just a  -> try (wait a) >>= process tx >> go tx

            process tx = \case
                Left (e :: SomeException) -> 
                    logError_ logger [i|An error reading the object: #{e}|]
                Right (SError e) -> 
                    -- TODO Do something with the validation result here
                    logError_ logger [i|An error parsing or serialising the object: #{e}|]
                Right (SObject so@(StorableObject ro _)) -> do
                    let h = getHash ro
                    getByHash tx objectStore h >>= \case 
                        Nothing -> do
                            putObject tx objectStore so
                            void $ atomicModifyIORef counter $ \c -> (c + 1, ())
                        (Just _ :: Maybe RpkiObject) ->
                            -- TODO Add location
                            logDebug_ logger [i|There's an existing object with hash: #{hexHash h}, ignoring the new one.|]
                

data RsyncMode = RsyncOneFile | RsyncDirectory

rsyncProcess :: URI -> FilePath -> RsyncMode -> ProcessConfig () () ()
rsyncProcess (URI uri) destination rsyncMode = 
    proc "rsync" $ 
        [ "--timeout=300",  "--update",  "--times" ] <> 
        extraOptions <> 
        [ T.unpack uri, destination ]
    where 
        extraOptions = case rsyncMode of 
            RsyncOneFile   -> []
            RsyncDirectory -> [ "--recursive", "--delete", "--copy-links" ]        

-- TODO Make it generate shorter filenames
rsyncDestination :: FilePath -> T.Text -> FilePath
rsyncDestination root uri = root </> T.unpack (U.normalizeUri uri)

-- TODO Do something with proper error handling
fileContent :: FilePath -> IO B.ByteString 
fileContent path = do
    r <- try $ unsafeMMapFile path
    case r of
        Right bs                  -> pure bs
        Left (_ :: SomeException) -> B.readFile path

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize 

-- TODO Refactor that stuff
getSizeAndContent :: FilePath -> IO (Integer, Either ValidationError B.ByteString)
getSizeAndContent path = withFile path ReadMode $ \h -> do
    s <- hFileSize h
    case validateSize s of
        Left e  -> pure (s, Left e)
        Right _ -> do
            c <- if s < 65536 
                then B.hGetContents h          
                else fileContent path
            pure (s, Right c)

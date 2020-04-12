{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns       #-}

module RPKI.Rsync where

import           Data.Bifunctor

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Except
import           UnliftIO.Exception                    hiding (fromEither, fromEitherM)

import qualified Data.ByteString                       as B
import           Data.String.Interpolate
import qualified Data.Text                             as T
import           Data.IORef

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Repository
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Validation.ObjectValidation
import           RPKI.Store.Base.Storage
import           RPKI.Store.Base.Storable
import           RPKI.Store.Stores
import qualified RPKI.Util                             as U

import qualified Control.Concurrent.STM.TBQueue as Q

import           System.Directory                      (createDirectoryIfMissing,
                                                        doesDirectoryExist,
                                                        getDirectoryContents)
import           System.FilePath                       ((</>))

import           System.IO
import           System.Exit
import           System.Process.Typed

import System.IO.Posix.MMap (unsafeMMapFile)

import qualified UnliftIO.Async as Unlift

-- | Download one file using rsync
rsyncFile :: AppContext -> 
            URI -> 
            ValidatorT vc IO RpkiObject
rsyncFile AppContext{..} (URI uri) = do
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
            void $ vHoist $ validateSizeM fileSize
            bs        <- fromTry (RsyncE . FileReadError . U.fmtEx) $ fileContent destination
            fromEitherM $ pure $ first ParseE $ readObject (U.convert uri) bs


-- | Process the whole rsync repository, download it, traverse the directory and 
-- | add all the relevant objects to the storage.
updateObjectForRsyncRepository :: Storage s => 
                AppContext ->
                RsyncRepository -> 
                RpkiObjectStore s -> 
                ValidatorT vc IO (RsyncRepository, Validations)
updateObjectForRsyncRepository 
                appContenxt@AppContext{..} 
                repo@(RsyncRepository (URI uri)) 
                objectStore = do     
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
            (validations, count) <- fromEitherM $ loadRsyncRepository appContenxt (URI uri) destination objectStore
            lift3 $ logInfo_ logger [i|Finished loading #{count} objects into local storage.|]
            pure (repo, validations)
        ExitFailure errorCode -> do
            lift3 $ logError_ logger [i|Rsync process failed: #{rsync} 
                                        with code #{errorCode}, 
                                        stderr = #{err}, 
                                        stdout = #{out}|]
            throwError $ RsyncE $ RsyncProcessError errorCode err 
        
    

-- | Recursively traverse given directory and save all the parseable 
-- | objects into the storage.
-- 
-- | 'accumulator' is a function that does something pretty arbitrary 
-- | with any read and parsed objects (most probably extracts repositories 
-- | from certificates)
loadRsyncRepository :: Storage s => 
                        AppContext ->
                        URI -> 
                        FilePath -> 
                        RpkiObjectStore s -> 
                        IO (Either AppError (Validations, Integer))
loadRsyncRepository AppContext{..} repositoryUrl rootPath objectStore = do        
        counter <- newIORef 0
        (r1, r2) <- bracketChan 
                        (parallelism config)
                        travelrseFS
                        (saveObjects counter)
                        kill
        c <- readIORef counter
        -- TODO That is weird, make somjething reasonable out of it
        pure $ do    
            r1' <- r1
            r2' <- r2
            pure (r2', c)
    where
        kill = maybe (pure ()) (Unlift.cancel . snd)

        travelrseFS queue = 
            first (RsyncE . FileReadError . U.fmtEx) <$> try (
                readFiles queue rootPath 
                    `finally` 
                    (atomically $ Q.writeTBQueue queue Nothing))

        readFiles queue currentPath = do
            names <- getDirectoryContents currentPath
            let properNames = filter (`notElem` [".", ".."]) names
            forM_ properNames $ \name -> do
                let path = currentPath </> name
                doesDirectoryExist path >>= \case
                    True  -> readFiles queue path
                    False -> 
                        when (supportedExtension path) $ do                            
                            a <- Unlift.async $ readAndParseObject path                            
                            let uri = pathToUri repositoryUrl rootPath path
                            atomically $ Q.writeTBQueue queue $ Just (uri, a)
            where
                readAndParseObject filePath = do                                         
                    (_, content)  <- getSizeAndContent filePath                
                    case content of 
                        Left e   -> pure $! SError e
                        Right bs ->                            
                            case first ParseE $ readObject filePath bs of
                                Left e   -> pure $! SError e
                                -- 1) "toStorableObject ro" has to happen in this thread, as it is the way to 
                                -- force computation of the serialised object and gain some parallelism
                                -- 2) "toStorableObject ro" shouldn't happen inside of "atomically" to prevent
                                -- a slow CPU-intensive transaction (verify that it's the case)
                                Right ro -> pure $! SObject $! toStorableObject ro
        
        saveObjects counter queue = 
            first (StorageE . StorageError . U.fmtEx) <$> 
                try (rwTx objectStore $ \tx -> go tx (mempty :: Validations))
            where            
                go tx validations = 
                    (atomically $ Q.readTBQueue queue) >>= \case 
                        Nothing -> pure validations
                        Just (uri, a) -> try (Unlift.wait a) >>= process tx uri validations >>= go tx

                process tx uri validations = \case
                    Left (e :: SomeException) -> do
                        logError_ logger [i|An error reading and parsing the object: #{e}|]
                        pure $ validations <> mError (vContext uri) (UnspecifiedE $ U.fmtEx e)
                    Right (SError e) -> do
                        logError_ logger [i|An error parsing or serialising the object: #{e}|]
                        pure $ validations <> mError (vContext uri) e
                    Right (SObject so@(StorableObject ro _)) -> do                        
                        let h = getHash ro
                        getByHash tx objectStore h >>= \case 
                            Nothing -> do
                                putObject tx objectStore so
                                void $ atomicModifyIORef counter $ \c -> (c + 1, ())
                            (Just _ :: Maybe RpkiObject) -> pure ()
                                -- TODO Add location
                                -- logDebug_ logger [i|There's an existing object with hash: #{hexHash h}, ignoring the new one.|]
                        pure validations
                    

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
rsyncDestination root uri = root </> T.unpack (U.normalizeUri $ T.replace "rsync://" "" uri)

fileContent :: FilePath -> IO B.ByteString 
fileContent path = do
    r <- try $ unsafeMMapFile path
    case r of
        Right bs                  -> pure bs
        Left (_ :: SomeException) -> B.readFile path

getSizeAndContent :: FilePath -> IO (Integer, Either AppError B.ByteString)
getSizeAndContent path = withFile path ReadMode $ \h -> do
    size <- hFileSize h
    case validateSize size of
        Left e  -> pure (size, Left $ ValidationE e)
        Right _ -> do
            -- read small files in memory and mmap bigger ones 
            r <- try $ if size < 10_000 
                        then B.hGetContents h 
                        else fileContent path
            pure (size, first (RsyncE . FileReadError . U.fmtEx) r)                                

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize 


-- | Slightly heuristical 
-- TODO Make it more effectient and simpler, introduce NormalisedURI and NormalisedPath
pathToUri :: URI -> FilePath -> FilePath -> URI
pathToUri (URI rsyncBaseUri) (T.pack -> rsyncRoot) (T.pack -> filePath) = 
    let 
        rsyncRoot' = if T.isSuffixOf "/" rsyncRoot 
            then rsyncRoot
            else rsyncRoot <> "/"

        rsyncBaseUri' = if T.isSuffixOf "/" rsyncBaseUri 
            then rsyncBaseUri
            else rsyncBaseUri <> "/"
        in 
            URI $ T.replace rsyncRoot' rsyncBaseUri' filePath    

    

{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns       #-}

module RPKI.Rsync where
    
import           Control.Lens                     ((^.))
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Control.Concurrent.STM
import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.Except

import qualified Data.ByteString                  as BS
import           Data.IORef
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Execution
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import qualified RPKI.Util                        as U
import           RPKI.Validation.ObjectValidation

import qualified Control.Concurrent.STM.TBQueue   as Q

import           System.Directory                 (createDirectoryIfMissing,
                                                   doesDirectoryExist,
                                                   getDirectoryContents)
import           System.FilePath                  ((</>))

import           System.Exit
import           System.IO
import           System.Process.Typed

import           System.IO.Posix.MMap             (unsafeMMapFile)



-- | Download one file using rsync
rsyncRpkiObject :: AppContext s -> 
                    URI -> 
                    ValidatorT vc IO RpkiObject
rsyncRpkiObject AppContext{..} uri = do
    let RsyncConf {..} = rsyncConf config
    let destination = rsyncDestination rsyncRoot uri
    let rsync = rsyncProcess uri destination RsyncOneFile
    (exitCode, out, err) <- readProcess rsync      
    case exitCode of  
        ExitFailure errorCode -> do
            logErrorM logger [i|Rsync process failed: #rsync 
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
                AppContext s ->
                RsyncRepository -> 
                RpkiObjectStore s -> 
                ValidatorT vc IO (RsyncRepository, Validations)
updateObjectForRsyncRepository 
    appContext@AppContext{..} 
    repo@(RsyncRepository (RsyncPublicationPoint uri) _) 
    objectStore = do     

    let rsyncRoot = appContext ^. typed @Config . typed @RsyncConf . typed @FilePath
    let destination = rsyncDestination rsyncRoot uri
    let rsync = rsyncProcess uri destination RsyncDirectory

    void $ fromTry (RsyncE . FileReadError . U.fmtEx) $ 
        createDirectoryIfMissing True destination
        
    logInfoM logger [i|Going to run #{rsync}|]
    (exitCode, out, err) <- fromTry 
        (RsyncE . RsyncRunningError . U.fmtEx) $ 
        readProcess rsync
    logInfoM logger [i|Finished rsynching #{destination}|]
    case exitCode of  
        ExitSuccess -> do 
            (validations, count) <- fromEitherM $ 
                loadRsyncRepository appContext uri destination objectStore
            logInfoM logger [i|Finished loading #{count} objects into local storage.|]
            pure (repo, validations)
        ExitFailure errorCode -> do
            logErrorM logger [i|Rsync process failed: #{rsync} 
                                        with code #{errorCode}, 
                                        stderr = #{err}, 
                                        stdout = #{out}|]
            throwError $ RsyncE $ RsyncProcessError errorCode err 


-- | Recursively traverse given directory and save all the parseable 
-- | objects into the storage.
-- 
-- | Is not supposed to throw exceptions, only report errors through Either.
loadRsyncRepository :: Storage s => 
                        AppContext s ->
                        URI -> 
                        FilePath -> 
                        RpkiObjectStore s -> 
                        IO (Either AppError (Validations, Integer))
loadRsyncRepository AppContext{..} repositoryUrl rootPath objectStore = do        
        counter <- newIORef 0
        let cpuParallelism = config ^. typed @Parallelism . field @"cpuParallelism"

        (r1, r2) <- bracketChan 
                        cpuParallelism
                        traverseFS
                        (saveObjects counter)
                        kill
        c <- readIORef counter        
        pure $ void r1 >> (, c) <$> r2            
    where
        kill = maybe (pure ()) (cancelTask . snd)

        threads = cpuBottleneck appThreads

        traverseFS queue = 
            first (RsyncE . FileReadError . U.fmtEx) <$> try (
                readFiles queue rootPath 
                    `finally` 
                    atomically (Q.writeTBQueue queue Nothing))

        readFiles queue currentPath = do
            names <- getDirectoryContents currentPath
            let properNames = filter (`notElem` [".", ".."]) names
            forM_ properNames $ \name -> do
                let path = currentPath </> name
                doesDirectoryExist path >>= \case
                    True  -> readFiles queue path
                    False -> 
                        when (supportedExtension path) $ do         
                            task <- (readAndParseObject path) `strictTask` threads                   
                            -- task <- Unlift.async $ readAndParseObject path                            
                            let uri = pathToUri repositoryUrl rootPath path
                            -- logDebugM logger [i|rsync uri = #{uri}|]
                            atomically $ Q.writeTBQueue queue $ Just (uri, task)
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
                    atomically (Q.readTBQueue queue) >>= \case 
                        Nothing       -> pure validations
                        Just (uri, a) -> try (waitTask a) >>= process tx uri validations >>= go tx

                process tx uri validations = \case
                    Left (e :: SomeException) -> do
                        logError_ logger [i|An error reading and parsing the object: #{e}|]
                        pure $ validations <> mError (vContext uri) (UnspecifiedE $ U.fmtEx e)
                    Right (SError e) -> do
                        logError_ logger [i|An error parsing or serialising the object: #{e}|]
                        pure $ validations <> mError (vContext uri) e
                    Right (SObject so@(StorableObject ro _)) -> do                        
                        alreadyThere <- hashExists tx objectStore (getHash ro)
                        if alreadyThere 
                            then do
                                -- complain
                                pure ()
                            else do 
                                putObject tx objectStore so
                                atomicModifyIORef' counter $ \c -> (c + 1, ())                            
                        pure validations
                    

data RsyncMode = RsyncOneFile | RsyncDirectory

rsyncProcess :: URI -> FilePath -> RsyncMode -> ProcessConfig () () ()
rsyncProcess (URI uri) destination rsyncMode = 
    proc "rsync" $ 
        [ "--timeout=300",  "--update",  "--times" ] <> 
        extraOptions <> 
        [ Text.unpack uri, destination ]
    where 
        extraOptions = case rsyncMode of 
            RsyncOneFile   -> []
            RsyncDirectory -> [ "--recursive", "--delete", "--copy-links" ]        

-- TODO Make it generate shorter filenames
rsyncDestination :: FilePath -> URI -> FilePath
rsyncDestination root (URI u) = root </> Text.unpack (U.normalizeUri $ Text.replace "rsync://" "" u)

fileContent :: FilePath -> IO BS.ByteString 
fileContent path = do
    r <- try $ unsafeMMapFile path
    case r of
        Right bs                  -> pure bs
        Left (_ :: SomeException) -> BS.readFile path

getSizeAndContent :: FilePath -> IO (Integer, Either AppError BS.ByteString)
getSizeAndContent path = withFile path ReadMode $ \h -> do
    size <- hFileSize h
    case validateSize size of
        Left e  -> pure (size, Left $ ValidationE e)
        Right _ -> do
            -- read small files in memory and mmap bigger ones 
            r <- try $ if size < 10_000 
                        then BS.hGetContents h 
                        else fileContent path
            pure (size, first (RsyncE . FileReadError . U.fmtEx) r)                                

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize 


-- | Slightly heuristical 
-- TODO Make it more effectient and simpler, introduce NormalisedURI and NormalisedPath 
-- and don't check it here.
pathToUri :: URI -> FilePath -> FilePath -> URI
pathToUri (URI rsyncBaseUri) (Text.pack -> rsyncRoot) (Text.pack -> filePath) = 
    let 
        rsyncRoot' = if Text.isSuffixOf "/" rsyncRoot 
            then rsyncRoot
            else rsyncRoot <> "/"

        rsyncBaseUri' = if Text.isSuffixOf "/" rsyncBaseUri 
            then rsyncBaseUri
            else rsyncBaseUri <> "/"
        in 
            URI $ Text.replace rsyncRoot' rsyncBaseUri' filePath    

    

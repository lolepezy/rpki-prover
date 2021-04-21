{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.Rsync where
    
import           Control.Lens                     ((%~), (&), (+=), (^.))
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Control.Concurrent.STM
import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import           RPKI.Store.Database
import qualified RPKI.Util                        as U
import           RPKI.Validation.ObjectValidation
import           RPKI.AppState

import           System.Directory                 (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)
import           System.FilePath                  ((</>))

import           System.Exit
import           System.IO
import           System.Process.Typed

import           System.Mem                       (performGC)
import Data.Proxy



-- | Download one file using rsync
-- | 
-- | This function doesn't throw exceptions.
rsyncRpkiObject :: AppContext s -> 
                RsyncURL -> 
                ValidatorT IO RpkiObject
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
            appError $ RsyncE $ RsyncProcessError errorCode $ U.convert err  
        ExitSuccess -> do
            fileSize  <- fromTry (RsyncE . FileReadError . U.fmtEx) $ getFileSize destination
            void $ vHoist $ validateSizeM fileSize
            bs        <- fromTry (RsyncE . FileReadError . U.fmtEx) $ fileContent destination
            fromEitherM $ pure $ first ParseE $ readObject (RsyncU uri) bs


-- | Process the whole rsync repository, download it, traverse the directory and 
-- | add all the relevant objects to the storage.
updateObjectForRsyncRepository :: Storage s => 
                                  AppContext s ->
                                  RsyncRepository ->     
                                  ValidatorT IO RsyncRepository
updateObjectForRsyncRepository 
    appContext@AppContext{..} 
    repo@(RsyncRepository (RsyncPublicationPoint uri) _) = 
        
    timedMetric (Proxy :: Proxy RsyncMetric) $ do     
        let rsyncRoot   = appContext ^. typed @Config . typed @RsyncConf . typed @FilePath
        objectStore <- fmap (^. #objectStore) $ liftIO $ readTVarIO database        
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
                -- Try to deallocate all the bytestrings created by mmaps right after they are used, 
                -- they will hold too much files open.            
                loadRsyncRepository appContext uri destination objectStore
                            `finally` liftIO performGC            
                pure repo
            ExitFailure errorCode -> do
                logErrorM logger [i|Rsync process failed: #{rsync} 
                                            with code #{errorCode}, 
                                            stderr = #{err}, 
                                            stdout = #{out}|]
                appError $ RsyncE $ RsyncProcessError errorCode $ U.convert err 


-- | Recursively traverse given directory and save all the parseable 
-- | objects into the storage.
-- 
-- | Is not supposed to throw exceptions, only report errors through Either.
loadRsyncRepository :: Storage s =>                         
                        AppContext s ->
                        RsyncURL -> 
                        FilePath -> 
                        RpkiObjectStore s -> 
                        ValidatorT IO ()
loadRsyncRepository AppContext{..} repositoryUrl rootPath objectStore = do       
    worldVersion <- liftIO $ getWorldVerionIO appState    
    void $ bracketChanClosable 
        -- it makes sense to run slightly more tasks because they 
        -- will spend some time waiting for the file IO to finish.
        (2 * cpuParallelism)
        traverseFS
        (saveObjects worldVersion)
        (cancelTask . snd)        
    where    
        threads = cpuBottleneck appBottlenecks
        cpuParallelism = config ^. typed @Parallelism . #cpuParallelism

        traverseFS queue = 
            mapException (AppException . RsyncE . FileReadError . U.fmtEx) <$> 
                traverseDirectory queue rootPath

        traverseDirectory queue currentPath = do
            objectStore <- fmap (^. #objectStore) $ liftIO $ readTVarIO database
            names <- liftIO $ getDirectoryContents currentPath
            let properNames = filter (`notElem` [".", ".."]) names
            forM_ properNames $ \name -> do
                let path = currentPath </> name
                liftIO (doesDirectoryExist path) >>= \case
                    True  -> traverseDirectory queue path
                    False -> 
                        when (supportedExtension path) $ do         
                            let uri = pathToUri repositoryUrl rootPath path
                            task <- readAndParseObject path (RsyncU uri) `strictTask` threads                                                                      
                            liftIO $ atomically $ writeCQueue queue (uri, task)
            where                
                readAndParseObject :: FilePath 
                                    -> RpkiURL 
                                    -> ValidatorT IO (Maybe (StorableUnit RpkiObject AppError))
                readAndParseObject filePath uri = 
                    liftIO (getSizeAndContent1 filePath) >>= \case                    
                        Left e        -> pure $! Just $! SError e
                        Right (_, bs) -> 
                            liftIO $ roTx objectStore $ \tx -> do
                                -- Check if the object is already in the storage
                                -- before parsing ASN1 and serialising it.
                                exists <- hashExists tx objectStore (U.sha256s bs)
                                pure $! if exists 
                                    then Nothing
                                    else 
                                        case first ParseE $ readObject uri bs of
                                            Left e -> Just $! SError e
                                            -- All these bangs here make sense because
                                            -- 
                                            -- 1) "toStorableObject ro" has to happen in this thread, as it is the way to 
                                            -- force computation of the serialised object and gain some parallelism
                                            -- 2) "toStorableObject ro" shouldn't happen inside of "atomically" to prevent
                                            -- a slow CPU-intensive transaction (verify that it's the case)
                                            Right ro -> Just $! SObject $ toStorableObject ro
        
        saveObjects worldVersion queue = do            
            mapException (AppException . StorageE . StorageError . U.fmtEx) <$> 
                rwAppTx objectStore go
            where
                go tx = 
                    liftIO (atomically (readCQueue queue)) >>= \case 
                        Nothing       -> pure ()
                        Just (uri, a) -> do 
                            r <- try $ waitTask a
                            process tx uri r
                            go tx

                process tx (RsyncURL uri) = \case
                    Left (e :: SomeException) -> 
                        throwIO $ AppException $ UnspecifiedE (unURI uri) (U.fmtEx e)

                    Right Nothing           -> pure ()
                    Right (Just (SError e)) -> do
                        logErrorM logger [i|An error parsing or serialising the object: #{e}|]
                        appWarn e

                    Right (Just (SObject so@(StorableObject ro _))) -> do                        
                        alreadyThere <- hashExists tx objectStore (getHash ro)
                        unless alreadyThere $ do 
                            putObject tx objectStore so worldVersion                            
                            updateMetric @RsyncMetric @_ (& #processed %~ (+1))
                                
                    

data RsyncMode = RsyncOneFile | RsyncDirectory

rsyncProcess :: RsyncURL -> FilePath -> RsyncMode -> ProcessConfig () () ()
rsyncProcess (RsyncURL (URI uri)) destination rsyncMode = 
    proc "rsync" $ 
        [ "--timeout=300",  "--update",  "--times" ] <> 
        extraOptions <> 
        [ Text.unpack uri, destination ]
    where 
        extraOptions = case rsyncMode of 
            RsyncOneFile   -> []
            RsyncDirectory -> [ "--recursive", "--delete", "--copy-links" ]        

-- TODO Make it generate shorter filenames
rsyncDestination :: FilePath -> RsyncURL -> FilePath
rsyncDestination root u = let
    RsyncURL (URI urlText) = u
    in 
        root </> Text.unpack (U.normalizeUri $ Text.replace "rsync://" "" urlText)
    

fileContent :: FilePath -> IO BS.ByteString 
fileContent = BS.readFile

getSizeAndContent :: FilePath -> IO (Integer, Either AppError BS.ByteString)
getSizeAndContent path = 
    withFile path ReadMode $ \h -> do
        size <- hFileSize h
        case validateSize size of
            Left e  -> pure (size, Left $ ValidationE e)
            Right _ -> do
                r <- try $ BS.hGetContents h
                pure (size, first (RsyncE . FileReadError . U.fmtEx) r)                                


getSizeAndContent1 :: FilePath -> IO (Either AppError (Integer, BS.ByteString))
getSizeAndContent1 path = do 
    r <- first (RsyncE . FileReadError . U.fmtEx) <$> readSizeAndContet
    pure $ r >>= \case 
                (_, Left e)  -> Left e
                (s, Right b) -> Right (s, b)    
  where    
    readSizeAndContet = try $ do
        withFile path ReadMode $ \h -> do
            size <- hFileSize h
            case validateSize size of
                Left e  -> pure (size, Left $ ValidationE e)
                Right _ -> do
                    r <- BS.hGetContents h                                
                    pure (size, Right r)



getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize 


-- | Slightly heuristical 
-- TODO Make it more effectient and simpler, introduce NormalisedURI and NormalisedPath 
-- and don't check it here.
pathToUri :: RsyncURL -> FilePath -> FilePath -> RsyncURL
pathToUri (RsyncURL (URI rsyncBaseUri)) (Text.pack -> rsyncRoot) (Text.pack -> filePath) = 
    let 
        rsyncRoot' = if Text.isSuffixOf "/" rsyncRoot 
            then rsyncRoot
            else rsyncRoot <> "/"

        rsyncBaseUri' = if Text.isSuffixOf "/" rsyncBaseUri 
            then rsyncBaseUri
            else rsyncBaseUri <> "/"
        in 
            RsyncURL $ URI $ Text.replace rsyncRoot' rsyncBaseUri' filePath    

    

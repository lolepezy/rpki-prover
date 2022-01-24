{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

module RPKI.Rsync where
    
import           Control.Lens                     ((%~), (&), (^.))
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Control.Concurrent.STM
import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.Except

import qualified Data.ByteString                  as BS
import Data.Maybe (fromMaybe)
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.List.NonEmpty               (NonEmpty(..))
import qualified Data.List.NonEmpty               as NonEmpty
import           Data.Tuple.Strict

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Parse.Parse
import           RPKI.Repository
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database              as DB
import qualified RPKI.Util                        as U
import           RPKI.Validation.ObjectValidation
import RPKI.Time ( timedMS )
import           RPKI.Worker

import           System.Directory                 (createDirectoryIfMissing, doesDirectoryExist, getDirectoryContents)

import           System.Exit
import           System.IO
import           System.FilePath
import           System.Process.Typed

import           System.Mem                       (performGC)
import Data.Proxy
import Data.List (stripPrefix)


checkRsyncInPath :: Maybe FilePath -> ValidatorT IO ()
checkRsyncInPath rsyncClientPath = do 
    let client = fromMaybe "rsync" rsyncClientPath    
    z <- liftIO $ try $ readProcess $ proc client [ "--version" ]
    case z of
        Left (e :: SomeException) -> do 
            let message = maybe 
                    [i|rsync client is not in he $PATH, can't proceed: #{U.fmtEx e}|]
                    (\rc -> [i|rsync client #{rc} is not found, can't proceed: #{U.fmtEx e}|])
                    rsyncClientPath
            appError $ InitE $ InitError message
                    
        Right (exit, stdout', stderr') -> 
            case exit of 
                ExitSuccess -> pure ()
                ExitFailure _ -> do 
                    appError $ InitE $ InitError 
                        [i|#{client} --version returned non-zero exit code #{exit}, 
stdout = [#{U.textual stdout'}], 
stderr = [#{U.textual stderr'}]|]


runRsyncFetchWorker :: AppContext s 
                    -> WorldVersion
                    -> RsyncRepository             
                    -> ValidatorT IO RsyncRepository
runRsyncFetchWorker AppContext {..} worldVersion rsyncRepo = do
        
    -- This is for humans to read in `top` or `ps`, actual parameters
    -- are passed as 'RsyncFetchResult'.
    let workerId = WorkerId $ "rsync-fetch:" <> unURI (getURL rsyncRepo)

    let maxCpuAvailable = fromIntegral $ config ^. typed @Parallelism . #cpuCount
    let arguments = 
            [ worderIdS workerId ] <>
            rtsArguments [ rtsN maxCpuAvailable, rtsA "20m", rtsAL "64m", rtsMaxMemory "1G" ]

    vp <- askEnv
    ((RsyncFetchResult (z, vs), stderr'), elapsed) <- 
                    timedMS $ runWorker 
                                logger
                                config
                                workerId 
                                (RsyncFetchParams vp rsyncRepo worldVersion)                        
                                (Timebox $ config ^. typed @RsyncConf . #rsyncTimeout)
                                arguments                        
    embedState vs
    case z of 
        Left e  -> appError e
        Right r -> do 
            logDebugM logger $ workerLogMessage (U.convert $ worderIdS workerId) stderr' elapsed            
            pure r

    

-- | Download one file using rsync
-- | 
-- | This function doesn't throw exceptions.
rsyncRpkiObject :: AppContext s -> 
                RsyncURL -> 
                ValidatorT IO RpkiObject
rsyncRpkiObject AppContext{..} uri = do
    let RsyncConf {..} = rsyncConf config
    destination <- liftIO $ rsyncDestination RsyncOneFile rsyncRoot uri
    let validationConfig = config ^. typed @Config . typed @ValidationConfig
    let rsync = rsyncProcess validationConfig uri destination RsyncOneFile
    (exitCode, out, err) <- readProcess rsync      
    case exitCode of  
        ExitFailure errorCode -> do
            logErrorM logger [i|Rsync process failed: #rsync 
                                        with code #{errorCode}, 
                                        stderr = #{err}, 
                                        stdout = #{out}|]        
            appError $ RsyncE $ RsyncProcessError errorCode $ U.convert err  
        ExitSuccess -> do
            fileSize <- fromTry (RsyncE . FileReadError . U.fmtEx) $ getFileSize destination
            void $ vHoist $ validateSizeM (config ^. typed) fileSize
            bs       <- fromTry (RsyncE . FileReadError . U.fmtEx) $ getFileContent destination
            fromEitherM $ pure $ first ParseE $ readObject (RsyncU uri) bs


-- | Process the whole rsync repository, download it, traverse the directory and 
-- | add all the relevant objects to the storage.
updateObjectForRsyncRepository :: Storage s => 
                                  AppContext s
                               -> WorldVersion 
                               -> RsyncRepository 
                               -> ValidatorT IO RsyncRepository
updateObjectForRsyncRepository 
    appContext@AppContext{..} 
    worldVersion
    repo@(RsyncRepository (RsyncPublicationPoint uri) _) = 
        
    timedMetric (Proxy :: Proxy RsyncMetric) $ do     
        let rsyncRoot = appContext ^. typed @Config . typed @RsyncConf . typed @FilePath
        let validationConfig = config ^. typed @Config . typed @ValidationConfig
        objectStore <- fmap (^. #objectStore) $ liftIO $ readTVarIO database        
        destination <- liftIO $ rsyncDestination RsyncDirectory rsyncRoot uri
        let rsync = rsyncProcess validationConfig uri destination RsyncDirectory
            
        logDebugM logger [i|Runnning #{U.trimmed rsync}|]
        (exitCode, out, err) <- fromTry 
                (RsyncE . RsyncRunningError . U.fmtEx) $ 
                readProcess rsync
        logInfoM logger [i|Finished rsynching #{destination}.|]
        case exitCode of  
            ExitSuccess -> do 
                -- Try to deallocate all the bytestrings created by mmaps right after they are used, 
                -- they will hold too much files open.            
                loadRsyncRepository appContext worldVersion uri destination objectStore
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
-- | Is not supposed to throw exceptions.
loadRsyncRepository :: Storage s =>                         
                        AppContext s 
                    -> WorldVersion 
                    -> RsyncURL 
                    -> FilePath 
                    -> DB.RpkiObjectStore s 
                    -> ValidatorT IO ()
loadRsyncRepository AppContext{..} worldVersion repositoryUrl rootPath objectStore =    
    void $ bracketChanClosable 
        -- it makes sense to run slightly more tasks because they 
        -- will spend some time waiting for the file IO to finish.
        (2 * cpuParallelism)
        traverseFS
        saveObjects
        (cancelTask . snd)        
  where    
    threads = cpuBottleneck appBottlenecks
    cpuParallelism = config ^. typed @Parallelism . #cpuParallelism

    traverseFS queue = 
        mapException (AppException . RsyncE . FileReadError . U.fmtEx) <$> 
            traverseDirectory queue rootPath

    traverseDirectory queue currentPath = do
        names <- liftIO $ getDirectoryContents currentPath
        let properNames = filter (`notElem` [".", ".."]) names
        forM_ properNames $ \name -> do
            let path = currentPath </> name
            liftIO (doesDirectoryExist path) >>= \case
                True  -> traverseDirectory queue path
                False -> 
                    when (supportedExtension path) $ do         
                        let uri = restoreUriFromPath repositoryUrl rootPath path
                        task <- readAndParseObject path (RsyncU uri) `strictTask` threads                                                                      
                        liftIO $ atomically $ writeCQueue queue (uri, task)
      where                                
        readAndParseObject filePath uri = 
            liftIO (getSizeAndContent (config ^. typed) filePath) >>= \case                    
                Left e        -> pure $! T2 uri (Right $! SError e)
                Right (_, bs) -> 
                    liftIO $ roTx objectStore $ \tx -> do
                        -- Check if the object is already in the storage
                        -- before parsing ASN1 and serialising it.
                        let hash = U.sha256s bs  
                        exists <- DB.hashExists tx objectStore hash
                        pure $! if exists 
                            then T2 uri (Left hash)
                            else 
                                case first ParseE $ readObject uri bs of
                                    Left e -> T2 uri (Right $! SError e)
                                    -- All these bangs here make sense because
                                    -- 
                                    -- 1) "toStorableObject ro" has to happen in this thread, as it is the way to 
                                    -- force computation of the serialised object and gain some parallelism
                                    -- 2) "toStorableObject ro" shouldn't happen inside of "atomically" to prevent
                                    -- a slow CPU-intensive transaction (verify that it's the case)
                                    Right ro -> T2 uri (Right $! SObject $ toStorableObject ro)
    
    saveObjects queue = do            
        mapException (AppException . StorageE . StorageError . U.fmtEx) <$> 
            DB.rwAppTx objectStore go
        where
        go tx = 
            liftIO (atomically (readCQueue queue)) >>= \case 
                Nothing       -> pure ()
                Just (uri, a) -> do 
                    r <- try $ waitTask a
                    process tx uri r
                    go tx

        process tx rsyncURL = \case
            Left (e :: SomeException) -> 
                throwIO $ AppException $ UnspecifiedE (unURI $ getURL rsyncURL) (U.fmtEx e)
            
            Right (T2 rpkiURL (Left hash)) -> do
                DB.linkObjectToUrl tx objectStore rpkiURL hash

            Right (T2 rpkiURL (Right (SError e))) -> do
                logErrorM logger [i|An error parsing or serialising the object #{rpkiURL}: #{e}|]
                appWarn e

            Right (T2 rpkiURL (Right (SObject so@StorableObject {..}))) -> do                        
                alreadyThere <- DB.hashExists tx objectStore (getHash object)
                unless alreadyThere $ do 
                    DB.putObject tx objectStore so worldVersion                                               
                    DB.linkObjectToUrl tx objectStore rpkiURL (getHash object)                                    
                    updateMetric @RsyncMetric @_ (& #processed %~ (+1))
                                
                    

data RsyncMode = RsyncOneFile | RsyncDirectory

rsyncProcess :: ValidationConfig -> RsyncURL -> FilePath -> RsyncMode -> ProcessConfig () () ()
rsyncProcess vc rsyncURL destination rsyncMode = 
    proc "rsync" $ 
        [ "--timeout=300",  "--update",  "--times" ] <> 
        [ "--max-size=" <> show (vc ^. #maxObjectSize) ] <> 
        [ "--min-size=" <> show (vc ^. #minObjectSize) ] <> 
        extraOptions <> 
        [ sourceUrl, destination ]
    where 
        source = Text.unpack (unURI $ getURL rsyncURL)        
        (sourceUrl, extraOptions) = case rsyncMode of 
            RsyncOneFile   -> (source, [])
            RsyncDirectory -> (addTrailingPathSeparator source, [ "--recursive", "--delete", "--copy-links" ])

rsyncDestination :: RsyncMode -> FilePath -> RsyncURL -> IO FilePath
rsyncDestination rsyncMode root (RsyncURL (RsyncHost host) path) = do 
    let fullPath = (U.convert host :: String) :| map (U.convert . unRsyncPathChunk) path
    let mkPath p = foldl (</>) root p
    let pathWithoutLast = NonEmpty.init fullPath
    let target = mkPath $ NonEmpty.toList fullPath
    case rsyncMode of 
        RsyncOneFile -> do             
            createDirectoryIfMissing True (mkPath pathWithoutLast)
            pure target
        RsyncDirectory -> do            
            createDirectoryIfMissing True target
            pure $ addTrailingPathSeparator target
    
                        
getSizeAndContent :: ValidationConfig -> FilePath -> IO (Either AppError (Integer, BS.ByteString))
getSizeAndContent vc path = do 
    r <- first (RsyncE . FileReadError . U.fmtEx) <$> readSizeAndContet
    pure $ r >>= \case 
                (_, Left e)  -> Left e
                (s, Right b) -> Right (s, b)    
  where    
    readSizeAndContet = try $ do
        withFile path ReadMode $ \h -> do
            size <- hFileSize h
            case validateSize vc size of
                Left e  -> pure (size, Left $ ValidationE e)
                Right _ -> do
                    r <- BS.hGetContents h                                
                    pure (size, Right r)

getFileSize :: FilePath -> IO Integer
getFileSize path = withFile path ReadMode hFileSize 

getFileContent :: FilePath -> IO BS.ByteString 
getFileContent = BS.readFile    

restoreUriFromPath :: RsyncURL -> FilePath -> FilePath -> RsyncURL
restoreUriFromPath url@(RsyncURL host rootPath) rsyncRoot filePath = 
    case stripPrefix (splitDirectories rsyncRoot) (splitDirectories filePath) of
        Nothing   -> url
        Just diff -> RsyncURL host (rootPath <> map (RsyncPathChunk . U.convert) diff)

    

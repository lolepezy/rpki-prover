{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Store.AppLmdbStorage where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad (void)
import           Control.Monad.IO.Class

import           Control.Lens                     ((^.))

import           Data.String.Interpolate.IsString
import           Data.Hourglass

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.AppTypes
import           RPKI.Logging
import           RPKI.Worker
import           RPKI.Reporting
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database             as DB
import           RPKI.Store.MakeLmdb
import           RPKI.Store.Base.Storable
import           RPKI.Time
import           RPKI.Metrics.System
import           RPKI.Util

import           System.Directory
import           System.FilePath                  ((</>))
import           System.Posix.Files

import           Lmdb.Connection

data LmdbFlow = UseExisting | Reset


-- | Verify that the cache directory is consistent and use it as LMDB cache.
-- 
-- It case something is wrong with it, wipe the whole cache out and start from scratch,
-- By "consistent" we mean the following:
--  * there's "cache" directory inside of the root directory
--  * there's structure inside of the "cache" directory similar to this exmaple:
-- 
--      ... ${root}/cache/current -> ${root}/cache/lmdb.1605220697
--      ... lmdb.1605220697
-- 
--  In this case `lmdb.1605220697` contains LMDB files.
--
setupLmdbCache :: LmdbFlow -> AppLogger -> FilePath -> Config -> ValidatorT IO LmdbEnv
setupLmdbCache lmdbFlow logger cacheDir config = do    
    -- delete everything in `cache` if `reset` is present
    case lmdbFlow of 
        UseExisting -> pure ()
        Reset -> do 
            logInfo logger [i|The option `reset` is present: cleaning up #{cacheDir}.|] 
            cleanDir cacheDir
    
    currentExists <- liftIO $ doesPathExist currentCache    
    if currentExists 
        then do            
            linkTarget   <- liftIO $ readSymbolicLink currentCache
            targetExists <- liftIO $ doesPathExist $ cacheDir </> linkTarget                        
            if targetExists 
                then do 
                    -- We have what we need, but there could still be other directories 
                    -- left from interrupted de-fragmentations or stuff like that -- delete 
                    -- them all.
                    removePossibleOtherLMDBCaches linkTarget

                    logInfo logger [i|Using #{currentCache} for LMDB cache.|] 
                    createLmdb currentCache
                else do 
                    -- link is broken so clean it up and re-create
                    logWarn logger [i|#{currentCache} doesn't point to an existing directory, resetting LMDB.|] 
                    resetCacheDir  
        else do 
            logWarn logger [i|#{currentCache} doesn't exist, resetting LMDB.|] 
            resetCacheDir
    
    where        
        currentCache = cacheDir </> "current"

        resetCacheDir = do             
            cleanDir cacheDir            
            newLmdbDir <- generateLmdbDir cacheDir
            liftIO $ createDirectory newLmdbDir
            liftIO $ createSymbolicLink newLmdbDir currentCache
            createLmdb currentCache

        createLmdb lmdbDir' =
            fromTry (InitE . InitError . fmtEx) $ mkLmdb lmdbDir' config

        removePossibleOtherLMDBCaches linkTarget = 
            cleanDirFiltered cacheDir $ \f -> 
                f /= "current" && (cacheDir </> f) /= linkTarget    


-- | The same as `setupLmdbCache` but for the worker process.
-- It does not do any cleanups and just expecets the proper layout to be in place.
--
setupWorkerLmdbCache :: AppLogger -> FilePath -> Config -> ValidatorT IO LmdbEnv
setupWorkerLmdbCache logger cacheDir config = do        
    currentExists <- liftIO $ doesPathExist currentCache    
    if currentExists 
        then do    
            linkTarget   <- liftIO $ readSymbolicLink currentCache
            targetExists <- liftIO $ doesPathExist $ cacheDir </> linkTarget
            if targetExists
                then createLmdb currentCache
                else do
                    let message = [i|#{currentCache} doesn't point to an existing directory, broken LMDB.|]
                    logError logger message
                    appError $ InternalE $ InternalError message
        else do
            let message = [i|#{currentCache} doesn't exist, bailing out.|]
            logError logger message
            appError $ InternalE $ InternalError message
    where
        currentCache = cacheDir </> "current"

        createLmdb lmdbDir' =
            fromTry (InitE . InitError . fmtEx) $ mkLmdb lmdbDir' config 
        

-- | De-fragment LMDB cache.
-- 
-- Since we add and delete a lot of data, LMDB files get bigger and bigger over time.
-- It is hard to say why exactly, but the most logical explanation is that it can't
-- reuse pages in the file and the file gets highly fragmented.
--
-- In order to mitigate the problem, we do the following
--  * create a new empty environment
--  * copy every entry of every key-value map into the new environment
--  * start using new environment
--  * delete old environment
-- 
-- On the FS level, it will create another `lmdb.N` directory inside `cache` 
-- and point the `cache/current` symlink to it.
-- 
compactStorageWithTmpDir :: AppContext LmdbStorage -> IO ()
compactStorageWithTmpDir appContext@AppContext {..} = do      
    lmdbEnv <- (\d -> let LmdbStorage {..} = storage d in env) <$> readTVarIO database
    let cacheDir = configValue $ config ^. #cacheDirectory
    let currentCache = cacheDir </> "current"

    logInfo logger [i|Checking if compacting LMDB storage is needed.|]

    currentNativeEnv <- readTVarIO $ nativeEnv lmdbEnv   

    newLmdbDirName <- generateLmdbDir cacheDir        

    let cleanUpAfterException = do 
            -- return Env back to what it was in case of failure
            atomically $ writeTVar (nativeEnv lmdbEnv) currentNativeEnv
            removePathForcibly newLmdbDirName    

    let setEnvToReadOnly = atomically $ do 
            let n = nativeEnv lmdbEnv
            readTVar n >>= \case            
                -- normally we expect it to be in the `RWEnv` state
                RWEnv native -> do 
                    writeTVar n (ROEnv native)                                
                    pure native

                -- this is weird, but lets just wait for it to change 
                -- and don't make things even more weird
                Disabled -> retry                

                -- it shouldn't happen as well, but we can work with it in principle                
                ROEnv nn -> pure nn
                TimedOut -> retry

            
    let copyToNewEnvironmentAndSwap dbStats = do                          
            oldNativeEnv <- setEnvToReadOnly

            -- create new native LMDB environment
            createDirectory newLmdbDirName            

            currentLinkTarget <- liftIO $ readSymbolicLink currentCache
            logDebug logger [i|Created #{newLmdbDirName} for storage copy, current one is #{currentLinkTarget}|]

            runCopyWorker appContext dbStats newLmdbDirName                                    
            logDebug logger [i|Copied all data to #{newLmdbDirName}.|]            

            -- create a new symlink first and only then atomically rename it into "current"
            createSymbolicLink newLmdbDirName $ cacheDir </> "current.new"
            renamePath (cacheDir </> "current.new") currentCache

            updateDatabase appContext newLmdbDirName

            closeEnvironment oldNativeEnv
            removePathForcibly currentLinkTarget

            Size lmdbFileSize <- cacheFsSize appContext 
            let fileSizeMb :: Integer = fromIntegral $ lmdbFileSize `div` (1024 * 1024)
            logInfo logger [i|LMDB file size after compaction is #{fileSizeMb}mb.|]
        
    Size lmdbFileSize <- cacheFsSize appContext 
    
    dbStats <- DB.totalStats <$> lmdbGetStats appContext
    let Size dataSize = dbStats ^. #statKeyBytes + dbStats ^. #statValueBytes    

    let fileSizeMb :: Integer = fromIntegral $ lmdbFileSize `div` (1024 * 1024)
    let dataSizeMb :: Integer = fromIntegral $ dataSize `div` (1024 * 1024)
    if fileSizeMb > (3 * dataSizeMb)    
        then do 
            logInfo logger [i|The total data size is #{dataSizeMb}mb, LMDB file size #{fileSizeMb}mb, will perform compaction.|]
            copyToNewEnvironmentAndSwap dbStats `catch` (
                \(e :: SomeException) -> do
                    logError logger [i|ERROR: #{e}.|]        
                    cleanUpAfterException)
        else 
            logInfo logger [i|The total data size is #{dataSizeMb}mb, LMDB file size #{fileSizeMb}mb, compaction is not needed yet.|]          
        

cacheFsSize :: AppContext s -> IO Size 
cacheFsSize AppContext {..} = do 
    let cacheDir = configValue $ config ^. #cacheDirectory
    let currentCache = cacheDir </> "current"
    currentLinkTarget <- liftIO $ readSymbolicLink currentCache
    fmap (Size . fromIntegral . sum) 
        $ mapM (getFileSize . (currentCache </>)) 
            =<< listDirectory currentLinkTarget    

generateLmdbDir :: MonadIO m => FilePath -> m FilePath
generateLmdbDir cacheDir = do 
    Now now <- thisInstant
    pure $ cacheDir </> ("lmdb." <> show (asSeconds now))        


cleanDirFiltered :: FilePath -> (FilePath -> Bool) -> ValidatorT IO ()
cleanDirFiltered d f = fromTry (InitE . InitError . fmtEx) $ 
                            listDirectory d >>= mapM_ (removePathForcibly . (d </>)) . filter f

cleanDir :: FilePath -> ValidatorT IO ()
cleanDir d = cleanDirFiltered d (const True)


closeLmdbStorage :: AppContext LmdbStorage -> IO ()
closeLmdbStorage AppContext {..} = do 
    closeLmdb . (\d -> let LmdbStorage {..} = storage d in env) =<< readTVarIO database


updateDatabase :: AppContext LmdbStorage -> FilePath -> IO ()
updateDatabase AppContext {..} dbDirectory = do
    reopenedLmdb <- mkLmdb dbDirectory config
    (newDB, _) <- createDatabase reopenedLmdb logger config DontCheckVersion
    atomically $ do
        newNative <- getNativeEnv reopenedLmdb 
        writeTVar (nativeEnv reopenedLmdb) (RWEnv newNative)  
        writeTVar database newDB    


-- Close LMDB environment
-- Delete lock file 
-- Open LMDB environment again
reopenLmdbStorage :: AppContext LmdbStorage -> IO ()
reopenLmdbStorage appContext@AppContext {..} = do 

    closeLmdbStorage appContext
    
    let cacheDir = configValue $ config ^. #cacheDirectory
    let currentCache = cacheDir </> "current"    
    currentLinkTarget <- liftIO $ readSymbolicLink currentCache
    files <- listDirectory currentLinkTarget
    mapM_ removePathForcibly [ currentLinkTarget </> f | f <- files, f == "lock.mdb"]     

    updateDatabase appContext currentLinkTarget


-- This is called from the worker entry point
-- 
copyLmdbEnvironment :: AppContext LmdbStorage -> FilePath -> IO ()
copyLmdbEnvironment AppContext {..} targetLmdbPath = do         
    currentEnv <- (\d -> let LmdbStorage {..} = storage d in env) <$> readTVarIO database
    newLmdb    <- mkLmdb targetLmdbPath config
    currentNativeEnv <- atomically $ getNativeEnv currentEnv
    newNativeEnv     <- atomically $ getNativeEnv newLmdb     
    void $ copyEnv currentNativeEnv newNativeEnv        


-- | The only reason to do LMDB copying as a separate worker process is memory.
-- Even though copying key-value pairs is a perfectly streaming-like activity, 
-- without enough GC pressure it allocates a large heap (because of plenty of 
-- bytestrings apparently), so to avoid it we run a worker process with limited 
-- heap that does the copying.
-- 
runCopyWorker :: AppContext LmdbStorage -> SStats -> FilePath -> IO ()
runCopyWorker appContext@AppContext {..} dbtats targetLmdbPath = do 
    let workerId = WorkerId "lmdb-compaction"
    
    -- Heap size is based on MS = "the biggest KV-pair that we need to copy"
    -- and is equal to max(20*MS, 64mb)
    let maxMemoryMb = let 
            Size maxMemory = (dbtats ^. #statMaxKeyBytes + dbtats ^. #statMaxValueBytes) * 20
            in max (maxMemory `div` 1024 `div` 1024) 64

    let arguments = 
            [ workerIdStr workerId ] <>
            rtsArguments [ rtsN 1, rtsA "20m", rtsAL "64m", rtsMaxMemory (show maxMemoryMb <> "m") ]

    (z, vs) <- runValidatorT 
                (newScopes "lmdb-compaction-worker") $ do 
                     -- 30 minutes should be enough even for a huge cache on a very slow disk    
                    let timeout = Seconds $ 30 * 60
                    workerInput <- makeWorkerInput appContext workerId
                                        (CompactionParams targetLmdbPath)                                        
                                        (Timebox timeout)
                                        Nothing
                    workerInfo <- newWorkerInfo (GenericWorker "lmdb-compaction") timeout (convert $ workerIdStr workerId)
                    runWorker logger workerInput arguments workerInfo
    case z of 
        Left e  -> do 
            let message = [i|Failed to run compaction worker: #{e}, validations: #{vs}.|]
            logError logger message            
            throwIO $ AppException $ InternalE $ InternalError message
        Right wr@WorkerResult { payload = CompactionResult _, .. } -> do
            logWorkerDone logger workerId wr
            pushSystem logger $ cpuMemMetric "compaction" cpuTime clockTime maxMemory                     
            
-- 
cleanupReaders :: AppContext LmdbStorage -> IO Int
cleanupReaders AppContext {..} = do 
    -- eventually call `mdb_reader_check` and try to remove 
    -- potentially dead readers from the reader table
    env <- (\d -> let LmdbStorage {..} = storage d in env) <$> readTVarIO database
    native <- atomically $ getNativeEnv env
    cleanReadersTable native


lmdbGetStats :: AppContext LmdbStorage -> IO StorageStats
lmdbGetStats AppContext {..} = do 
    lmdbEnv   <- (\d -> let LmdbStorage {..} = storage d in env) <$> readTVarIO database
    nativeEnv <- atomically $ getNativeEnv lmdbEnv
    getEnvStats nativeEnv
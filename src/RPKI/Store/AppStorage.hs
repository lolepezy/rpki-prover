{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.Store.AppStorage where

import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.IO.Class

import           Control.Lens                     ((^.))

import           Data.Int                         (Int64)

import           Data.String.Interpolate.IsString

import           RPKI.AppContext
import           RPKI.AppMonad
import           RPKI.Logging
import           RPKI.Reporting
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Storage
import qualified RPKI.Store.Database              as DB
import           RPKI.Store.Util
import           RPKI.Time
import           RPKI.Util

import           Data.Traversable                 (forM)
import           RPKI.Config
import           System.Directory
import           System.FilePath                  ((</>))
import           System.Posix.Files



class MaintainableStorage s where
    runMaintenance :: AppContext s -> IO ()

instance MaintainableStorage LmdbStorage where
    runMaintenance = defragmentStorageWithTmpDir


data LmdbFlow = UseExisting | Reset

maxReadersDefault :: Int
maxReadersDefault = 100


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
setupLmdbCache :: LmdbFlow -> AppLogger -> FilePath -> Size -> ValidatorT IO (LmdbEnv, FilePath)
setupLmdbCache lmdbFlow logger root lmdbSize = do
    
    -- delete everything in `cache` if `reset` is present
    case lmdbFlow of 
        UseExisting -> pure ()
        Reset -> do 
            logInfoM logger [i|The option `reset` is present: cleaning up #{cacheDir}.|] 
            cleanDir cacheDir
    
    currentExists <- liftIO $ doesPathExist currentCache
    lmdbEnv <- 
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

                        logInfoM logger [i|Using #{currentCache} for LMDB cache.|] 
                        createLmdb currentCache
                    else do 
                        -- link is broken so clean it up and re-create
                        logWarnM logger [i|#{currentCache} doesn't point to an existing directory, resetting LMDB.|] 
                        resetCacheDir  
            else do 
                logWarnM logger [i|#{currentCache} doesn't exist, resetting LMDB.|] 
                resetCacheDir

    pure (lmdbEnv, cacheDir)
    where
        cacheDir     = root </> "cache"
        currentCache = cacheDir </> "current"

        resetCacheDir = do             
            cleanDir cacheDir            
            newLmdbDir <- generateLmdbDir cacheDir
            liftIO $ createDirectory newLmdbDir
            liftIO $ createSymbolicLink newLmdbDir currentCache
            createLmdb currentCache

        createLmdb lmdbDir' =
            fromTry (InitE . InitError . fmtEx) $ 
                mkLmdb lmdbDir' lmdbSize maxReadersDefault    

        removePossibleOtherLMDBCaches linkTarget = 
            cleanDirFiltered cacheDir $ \f -> 
                f /= "current" && (cacheDir </> f) /= linkTarget    


-- | De-fragment LMDB cache.
-- 
-- Since we add and delete a lot of data, LMDB files get bigger and bigger over time.
-- It is hard to say why exactly, but the most logical explanation is that it fails
-- to reuse pages in the file and the file gets highly fragmented.
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
defragmentStorageWithTmpDir :: AppContext LmdbStorage -> IO ()
defragmentStorageWithTmpDir AppContext {..} = do      
    lmdbEnv <- getEnv . (\d -> storage d :: LmdbStorage) <$> readTVarIO database
    let cacheDir = config ^. #cacheDirectory
    let currentCache = cacheDir </> "current"

    logInfo_ logger [i|De-fragmenting LMDB storage.|]

    currentNativeEnv <- readTVarIO $ nativeEnv lmdbEnv   

    newLmdbDir <- generateLmdbDir cacheDir
    createDirectory newLmdbDir

    logDebug_ logger [i|Created #{newLmdbDir} for storage copy.|]

    let cleanUpAfterException = do 
            -- return Env back to what it was in case of failure
            atomically $ writeTVar (nativeEnv lmdbEnv) currentNativeEnv
            removePathForcibly newLmdbDir    

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
                -- it shouldn't happes as well, but we can work with it in principle
                ROEnv n  -> pure n

            
    let copyToNewEnvironmentAndSwap = do                        
            oldNativeEnv <- setEnvToReadOnly

            -- create new native LMDB environment in the temporary directory
            newLmdb   <- mkLmdb newLmdbDir (config ^. #lmdbSize) maxReadersDefault
            newNativeEnv <- atomically $ getNativeEnv newLmdb

            copyEnvAsync oldNativeEnv newNativeEnv
            logDebug_ logger [i|Copied data to #{newLmdbDir}.|]

            currentLinkTarget <- liftIO $ readSymbolicLink currentCache

            -- create a new symlink first and only then atomically renamed it into "current"
            createSymbolicLink newLmdbDir $ cacheDir </> "current.new"
            renamePath (cacheDir </> "current.new") currentCache

            newDB <- createDatabase newLmdb
            atomically $ do
                newNative <- getNativeEnv newLmdb
                writeTVar (nativeEnv lmdbEnv) (RWEnv newNative)  
                writeTVar database newDB

            closeNativeLmdb oldNativeEnv
            removePathForcibly currentLinkTarget
    
    currentLinkTarget <- liftIO $ readSymbolicLink currentCache
    
    fileSize <- do 
            lmdbFiles <- listDirectory currentLinkTarget
            sizes <- forM lmdbFiles $ \f -> getFileSize $ currentCache </> f
            pure $! sum sizes
    
    Size dataSize <- fmap DB.totalSpace $ DB.dbStats =<< readTVarIO database

    let fileSizeMb :: Integer = fileSize `div` (1024 * 1024)
    let dataSizeMb :: Integer = fromIntegral $ dataSize `div` (1024 * 1024)
    -- if fileSizeMb > (3 * dataSizeMb)
    if True
        then do 
            logDebug_ logger [i|The total data size is #{dataSizeMb}mb, LMDB file size #{fileSizeMb}mb, will de-fragment.|]
            (_, elapsed) <- timedMS $ copyToNewEnvironmentAndSwap `catch` (\(e :: SomeException) -> do
                                            logError_ logger [i|ERROR: #{e}.|]        
                                            cleanUpAfterException)
            logDebug_ logger [i|Done, took #{elapsed}ms.|]
        else 
            logDebug_ logger [i|The total data size is #{dataSizeMb}, LMDB file size #{fileSizeMb}, de-fragmentation is not needed yet.|]        
        
        
generateLmdbDir :: MonadIO m => FilePath -> m FilePath
generateLmdbDir cacheDir = do 
    Now now <- thisInstant
    pure $ cacheDir </> ("lmdb." <> show (asSeconds now))        


cleanDirFiltered :: FilePath -> (FilePath -> Bool) -> ValidatorT IO ()
cleanDirFiltered d f = fromTry (InitE . InitError . fmtEx) $ 
                            listDirectory d >>= mapM_ (removePathForcibly . (d </>)) . filter f

cleanDir :: FilePath -> ValidatorT IO ()
cleanDir d = cleanDirFiltered d (const True)
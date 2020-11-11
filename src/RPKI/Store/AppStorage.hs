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
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Store.Base.LMDB
import           RPKI.Store.Base.Storage
import           RPKI.Store.Util
import           RPKI.Time
import           RPKI.Util

import           System.Directory
import           System.FilePath                  ((</>))
import           System.IO.Temp
import           System.Posix.Files

class MaintainableStorage s where
    runMaintenance :: AppContext s -> IO ()

instance MaintainableStorage LmdbStorage where
    runMaintenance _ = pure () -- defragmentStorageWithTmpDir


data LmdbFlow = UseExisting | Reset

setupLmdbCache :: LmdbFlow -> AppLogger -> FilePath -> Int64 -> ValidatorT vc IO (LmdbEnv, FilePath)
setupLmdbCache lmdbFlow logger root lmdbSize = do
    
    -- delete everything in `cache` if `reset` is present
    case lmdbFlow of 
        UseExisting -> pure ()
        Reset -> do 
            logInfoM logger [i|The option `reset` is present: cleaning up #{cacheDir}.|] 
            cleanDir cacheDir
    
    currentExists <- liftIO $ doesPathExist currentCache
    lmdbEnv <- if currentExists 
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
                            logErrorM logger [i|#{currentCache} doesn't point to an existing directory, resetting LMDB.|] 
                            resetCacheDir  
                else do 
                    logErrorM logger [i|#{currentCache} doesn't exist, resetting LMDB.|] 
                    resetCacheDir

    pure (lmdbEnv, cacheDir)
    where
        cacheDir     = root </> "cache"
        currentCache = cacheDir </> "current"

        resetCacheDir = do             
            cleanDir cacheDir            
            newLmdbDir <- createLmdbDir cacheDir
            liftIO $ createDirectory newLmdbDir
            liftIO $ createSymbolicLink newLmdbDir currentCache
            createLmdb currentCache

        createLmdb lmdbDir' =
            fromTry (InitE . InitError . fmtEx) $ 
                mkLmdb lmdbDir' lmdbSize 128    

        removePossibleOtherLMDBCaches linkTarget = 
            cleanDirFiltered cacheDir $ \f -> 
                f /= "current" && (cacheDir </> f) /= linkTarget    


-- TODO Move it a more appropriate module 
defragmentStorageWithTmpDir :: AppContext LmdbStorage -> IO ()
defragmentStorageWithTmpDir AppContext {..} = do 
    -- create a temporary LMDB environment     
    let lmdbEnv = getEnv (storage database :: LmdbStorage)    
    let cacheDir = config ^. #cacheDirectory
    let currentCache = cacheDir </> "current"

    currentNativeEnv <- readTVarIO $ nativeEnv lmdbEnv   

    newLmdbDir <- createLmdbDir cacheDir
    createDirectory newLmdbDir

    let cleanUp = do 
            -- return Env back to what it was
            atomically $ writeTVar (nativeEnv lmdbEnv) currentNativeEnv
            removePathForcibly newLmdbDir    

    let copyToNewEnvironmentAndSwap = do            
            -- make current environment read-only
            atomically $ do 
                let n = nativeEnv lmdbEnv
                curentEnv <- readTVar n
                case curentEnv of
                    -- normally we expect it to be in the `RWEnv` state
                    RWEnv native -> writeTVar n (ROEnv native)                                
                    -- this is weird, but lets just wait for it to change 
                    -- and don't make things even more weird
                    Disabled -> retry
                    -- it shouldn't happes as well, but we can work with it in principle
                    ROEnv _  -> pure ()

            -- create new native LMDB environment in the temporary directory
            newLmdb <- mkLmdb newLmdbDir 1000_000 100

            -- copy current environment to the new one
            copyEnv lmdbEnv newLmdb
            
            -- disable current environment, i.e. stop all DB operations
            atomically $ writeTVar (nativeEnv lmdbEnv) Disabled

            currentLinkTarget <- liftIO $ readSymbolicLink currentCache

            -- create a new symlink first and only then atomically renamed it into "current"
            createSymbolicLink newLmdbDir $ cacheDir </> "current.new"
            renamePath (cacheDir </> "current.new") currentCache

            -- set new native LMDB environment as the current DB
            newNative <- getNativeEnv newLmdb
            atomically $ writeTVar (nativeEnv lmdbEnv) (RWEnv newNative)

            -- close old environment
            closeLmdb lmdbEnv

            -- delete old environment files
            removePathForcibly currentLinkTarget
            
    copyToNewEnvironmentAndSwap `onException` cleanUp    
        

        
createLmdbDir :: MonadIO m => FilePath -> m FilePath
createLmdbDir cacheDir = do 
    Now now <- thisInstant
    pure $ cacheDir </> ("lmdb." <> show (asSeconds now))        


cleanDirFiltered :: FilePath -> (FilePath -> Bool) -> ValidatorT env IO ()
cleanDirFiltered d f = fromTry (InitE . InitError . fmtEx) $ 
                            listDirectory d >>= mapM_ (removePathForcibly . (d </>)) . filter f

cleanDir :: FilePath -> ValidatorT env IO ()
cleanDir d = cleanDirFiltered d (const True)
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Workflow where

import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Concurrent.Async.Lifted
import           Control.Monad

import           Control.Lens                     ((^.))
import           Data.Generics.Labels
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed


import           Data.Hourglass                  
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           RPKI.AppMonad
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.TopDown
import           RPKI.Version

import           RPKI.AppContext
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time

import           RPKI.Store.Base.LMDB


type AppEnv = AppContext LmdbStorage

data FlowTask = 
    ValidateTAs WorldVersion | 
    CacheGC WorldVersion |
    CleanOldVersions
  deriving stock (Show, Eq, Ord, Generic)


runWorkflow :: Storage s => 
            AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do
    -- Use a command queue for every to avoid fully concurrent operations, i.e.
    -- cache GC should not run at the same time as the validation (not for consistency reasons,
    -- but we want to avoid locking the DB for long).    
    globalQueue <- newCQueueIO 10

    worldVersion <- getWorldVerion versions
    validateTaTask globalQueue worldVersion

    mapConcurrently_ (\f -> f globalQueue) [ 
            taskExecutor,
            generateNewWorldVersion, 
            cacheGC            
        ]
     
    where  
        config = appContext ^. typed @Config
        cacheCleanupInterval = config ^. #cacheCleanupInterval
        cacheLifeTime        = config ^. #cacheLifeTime
        revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
        objectStore          = appContext ^. #database . #objectStore               

        validateTaTask globalQueue worldVersion = 
            atomically $ writeCQueue globalQueue $ ValidateTAs worldVersion             

        -- periodically update world version and re-validate all TAs
        generateNewWorldVersion globalQueue = periodically revalidationInterval $ do 
            oldWorldVersion <- getWorldVerion versions
            newWorldVersion <- updateWorldVerion versions
            logDebug_ logger [i|Generated new world version, #{oldWorldVersion} ==> #{newWorldVersion}.|]
            validateTaTask globalQueue newWorldVersion

        -- remove old objects from the cache
        cacheGC globalQueue = periodically cacheCleanupInterval $ do
            worldVersion <- getWorldVerion versions
            atomically $ writeCQueue globalQueue $ CacheGC worldVersion

        taskExecutor globalQueue = do
            logDebug_ logger [i|Starting task executor.|]
            forever $ do 
                task <- atomically $ readCQueue globalQueue 
                case task of 
                    Nothing -> pure ()

                    Just CleanOldVersions -> do
                        logWarn_ logger [i|Cleanup for old versions is not implemented yet.|]

                    Just (ValidateTAs worldVersion) -> do
                        logInfo_ logger [i|Validating all TAs, world version #{worldVersion} |]
                        (_, elapsed) <- timedMS $ mapConcurrently_ processTAL tals
                        completeWorldVersion appContext worldVersion
                        logInfo_ logger [i|Validated all TAs, took #{elapsed}ms|]
                        where 
                            processTAL tal = void $ validateTA appContext tal worldVersion

                    Just (CacheGC worldVersion) -> do
                        let now = versionToMoment worldVersion                        
                        (_, elapsed) <- timedMS $ cleanObjectCache objectStore $ \(WorldVersion nanos) -> 
                                let validatedAt = fromNanoseconds nanos
                                in closeEnoughMoments validatedAt now cacheLifeTime
                        logInfo_ logger [i|Done with cache GC, took #{elapsed}ms|]
                            
                                                                

-- Execute certain IO actiion every N secods
-- 
periodically :: Seconds -> IO () -> IO ()
periodically (Seconds interval) action =
    forever $ do 
        Now start <- thisInstant        
        action
        Now end <- thisInstant
        let executionTimeNs = toNanoseconds end - toNanoseconds start
        when (executionTimeNs < nanosPerSecond * interval) $ do        
            let timeToWaitNs = nanosPerSecond * interval - executionTimeNs                        
            threadDelay $ (fromIntegral timeToWaitNs) `div` 1000         
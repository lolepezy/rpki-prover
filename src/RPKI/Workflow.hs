{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE RecordWildCards    #-}

module RPKI.Workflow where

import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.Lifted
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad

import           Control.Lens                     ((^.))
import           Data.Generics.Labels
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.List                        (sortBy)
import           Data.IORef.Lifted

import           Data.Int                         (Int64)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set

import           Data.Hourglass
import           Data.String.Interpolate.IsString

import           GHC.Generics

import           RPKI.Config
import           RPKI.Errors
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Store.Database
import           RPKI.TopDown
import           RPKI.AppState

import           RPKI.AppContext
import           RPKI.Store.Base.Storage
import           RPKI.TAL
import           RPKI.Time
import           RPKI.RTR.RtrContext
import           RPKI.RTR.RtrServer

import           RPKI.Store.Base.LMDB
import           RPKI.Store.Database

import           System.Exit
import qualified Data.Set as Set
import Data.Maybe


type AppEnv = AppContext LmdbStorage

data WorkflowTask = 
    ValidateTAs WorldVersion | 
    CacheGC WorldVersion |
    CleanOldVersions WorldVersion
  deriving stock (Show, Eq, Ord, Generic)


runWorkflow :: Storage s => 
            AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do
    -- Use a command queue to avoid fully concurrent operations, i.e. cache GC 
    -- should not run at the same time as the validation (not for consistency 
    -- reasons, but we want to avoid locking the DB for long time).
    globalQueue <- newCQueueIO 10

    rtrServer <- initRtrIfNeeded

    mapConcurrently_ (\f -> f globalQueue) [ 
            taskExecutor,
            generateNewWorldVersion, 
            cacheGC,
            cleanOldVersions,
            rtrServer   
        ]
    where          
        cacheCleanupInterval = config ^. #cacheCleanupInterval
        oldVersionsLifetime  = config ^. #oldVersionsLifetime
        cacheLifeTime        = config ^. #cacheLifeTime
        revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
        rtrConfig            = config ^. #rtrConfig

        validateTaTask globalQueue worldVersion = 
            atomically $ writeCQueue globalQueue $ ValidateTAs worldVersion             

        -- periodically update world version and re-validate all TAs
        generateNewWorldVersion globalQueue = periodically revalidationInterval $ do 
            oldWorldVersion <- getWorldVerion appState
            newWorldVersion <- updateWorldVerion appState
            logDebug_ logger [i|Generated new world version, #{oldWorldVersion} ==> #{newWorldVersion}.|]
            validateTaTask globalQueue newWorldVersion

        -- remove old objects from the cache
        cacheGC globalQueue = do 
            -- wait a little so that GC doesn't start before the actual validation
            threadDelay 10_000_000
            periodically cacheCleanupInterval $ do
                worldVersion <- getWorldVerion appState
                atomically $ writeCQueue globalQueue $ CacheGC worldVersion

        cleanOldVersions globalQueue = do 
            -- wait a little so that deleting old appState comes last in the queue of actions
            threadDelay 30_000_000
            periodically cacheCleanupInterval $ do
                worldVersion <- getWorldVerion appState
                atomically $ writeCQueue globalQueue $ CleanOldVersions worldVersion        

        taskExecutor globalQueue = do
            logDebug_ logger [i|Starting task executor.|]
            forever $ do 
                task <- atomically $ readCQueue globalQueue 
                case task of 
                    Nothing -> pure ()

                    Just (ValidateTAs worldVersion) -> do
                        logInfo_ logger [i|Validating all TAs, world version #{worldVersion} |]
                        executeOrDie
                            (mconcat <$> mapConcurrently processTAL tals)
                            (\tdResult@TopDownResult{..} elapsed -> do 
                                saveTopDownResult tdResult                                
                                logInfoM logger [i|Validated all TAs, got #{length vrps} VRPs, took #{elapsed}ms|])
                        where 
                            processTAL tal = validateTA appContext tal worldVersion                                

                            saveTopDownResult TopDownResult {..} = rwTx database $ \tx -> do
                                putValidations tx (validationsStore database) worldVersion tdValidations                                 
                                putVrps tx database vrps worldVersion
                                completeWorldVersion tx database worldVersion
                                atomically $ do 
                                    writeTVar (appState ^. #world) worldVersion
                                    writeTVar (appState ^. #currentVrps) vrps

                    Just (CacheGC worldVersion) -> do
                        let now = versionToMoment worldVersion
                        executeOrDie 
                            (cleanObjectCache database $ versionIsOld now cacheLifeTime)
                            (\(deleted, kept) elapsed -> 
                                logInfo_ logger [i|Done with cache GC, deleted #{deleted} objects, kept #{kept}, took #{elapsed}ms|])

                    Just (CleanOldVersions worldVersion) -> do
                        let now = versionToMoment worldVersion
                        executeOrDie 
                            (deleteOldVersions database $ versionIsOld now oldVersionsLifetime)
                            (\deleted elapsed -> 
                                logInfo_ logger [i|Done with deleting older versions, deleted #{deleted} versions, took #{elapsed}ms|])

        versionIsOld now period (WorldVersion nanos) =
            let validatedAt = fromNanoseconds nanos
            in not $ closeEnoughMoments validatedAt now period

        executeOrDie :: IO a -> (a -> Int64 -> IO ()) -> IO ()
        executeOrDie f onRight = 
            exec `catches` [
                    Handler $ \(AppException seriousProblem) ->
                        die [i|Something really bad happened #{seriousProblem}, exiting.|],
                    Handler $ \(_ :: AsyncCancelled) ->
                        die [i|Interrupted with Ctrl-C, exiting.|],                        
                    Handler $ \(weirdShit :: SomeException) ->
                        logError_ logger [i|Something weird happened #{weirdShit}, exiting.|]
                ] 
            where
                exec = do 
                    (r, elapsed) <- timedMS f
                    onRight r elapsed

        initRtrIfNeeded = 
            case rtrConfig of 
                Nothing -> do 
                    pure $ \_ -> pure ()
                Just rtrConfig' -> 
                    pure $ \_ -> runRtrServer appContext rtrConfig' 
                    

-- Execute certain IO actiion every N seconds
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
            when (timeToWaitNs > 0) $ 
                threadDelay $ (fromIntegral timeToWaitNs) `div` 1000         

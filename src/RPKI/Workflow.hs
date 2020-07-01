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

data FlowTask
  = ValidateTA WorldVersion
  | ValidateTACert TAL WorldVersion
  deriving stock (Show, Eq, Ord, Generic)


runWorkflow :: Storage s => 
            AppContext s -> [TAL] -> IO ()
runWorkflow appContext@AppContext {..} tals = do 

    -- Use a command queue for every TA to avoid concurrent operations for one TA
    -- but allow everything running in parallel for different TAs
    talQueues <- forM tals $ \tal -> (tal, ) <$> newCQueueIO 3

    -- schedule cache cleanup
    let config = appContext ^. typed @Config
    let cacheCleanupInterval = config ^. #cacheCleanupInterval
    let cacheLifeTime        = config ^. #cacheLifeTime
    let revalidationInterval = config ^. typed @ValidationConfig . #revalidationInterval
    let objectStore          = appContext ^. #database . #objectStore                

    -- periodically update world version and re-validate all TAs
    let generateNewWorldVersion = Concurrently $ periodically revalidationInterval $ do 
            oldWorldVersion <- getWorldVerion versions
            newWorldVersion <- updateWorldVerion versions
            logDebug_ logger [i|Generated new world version, #{oldWorldVersion} ==> #{newWorldVersion}.|]
            atomically $ forM_ talQueues $ \(_, queue) -> do
                writeCQueue queue $ ValidateTA newWorldVersion

    -- revalidate TA certificates
    let revalidateTACert = Concurrently $ periodically revalidationInterval $ do
            worldVersion <- getWorldVerion versions
            logDebug_ logger [i|Revalidate TA cert: #{worldVersion}.|]
            atomically $ forM_ talQueues $ \(tal, queue) -> do
                writeCQueue queue $ ValidateTACert tal worldVersion

    -- remove old objects from the cache
    let cacheGC = Concurrently $ periodically cacheCleanupInterval $ do            
            Now now <- thisInstant                
            logDebug_ logger [i|Starting cache GC.|]
            cleanObjectCache objectStore $ \(WorldVersion nanos) -> 
                    let validatedAt = fromNanoseconds nanos
                    in closeEnoughMoments validatedAt now cacheLifeTime


    let perTAExecutors = Concurrently $ 
            (flip mapConcurrently) talQueues $ \(tal, queue) -> do
                logDebug_ logger [i|Starting executor for TA #{getTaName tal}|]
                forever $ do 
                    task <- atomically $ readCQueue queue 
                    logDebug_ logger [i|TA: #{getTaName tal}, next task is #{task}|]
                    case task of 
                        Nothing -> pure ()
                        Just t -> do 
                            nextTask <- executeTask appContext t (getTaName tal)
                            case nextTask of 
                                Nothing -> pure ()        
                                Just next -> atomically $ writeCQueue queue next

    void $ runConcurrently $ (,,,)
            <$> generateNewWorldVersion
            <*> revalidateTACert
            <*> cacheGC
            <*> perTAExecutors


executeTask :: Storage s => 
                AppContext s -> FlowTask -> TaName -> IO (Maybe FlowTask)
executeTask appContext@AppContext {..} task taName'@(TaName taNameText) =
    case task of 
        ValidateTACert tal worldVersion -> do
            let context = vContext $ URI taNameText
            (r, validations) <- runValidatorT context $ validateTACertificateFromTAL appContext tal 
            writeVResult appContext validations worldVersion
            case r of
                Left e -> do
                    logError_ logger [i|Error updating TA certificate for #{taNameText}, e = #{e}.|]
                    pure Nothing
                Right (_, _, Updated) ->
                    pure $ Just $ ValidateTA worldVersion
                Right (_, _, Existing) ->
                    -- Nothing to do, TA certificate is the same, 
                    -- so repository re-validation will kick in on it's own time
                    pure Nothing
        ValidateTA worldVersion -> do
            void $ validateTA appContext taName' worldVersion
            pure Nothing
        

-- submitTask :: FlowTask -> TaskProcessor -> IO ()
-- submitTask task (TaskProcessor queue) = 
--     atomically $ writeCQueue queue task


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
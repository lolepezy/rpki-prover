{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Fetch where
    
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Lens                     ((%~), (&), (+=), (^.))
import           Data.Generics.Product.Fields
import           Data.Generics.Product.Typed

import           Data.Bifunctor

import           Control.Exception.Lifted

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.IO.Class

import qualified Data.ByteString                  as BS
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as Map

import GHC.Generics (Generic)

import Time.Types
import System.Timeout

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
import           RPKI.Time
import           RPKI.Util                       
import           RPKI.Validation.ObjectValidation
import           RPKI.AppState
import           RPKI.Rsync
import           RPKI.RRDP.RrdpFetch


data FetchResult = 
    FetchSuccess Repository Instant ValidationState | 
    FetchFailure Repository Instant ValidationState
    deriving stock (Show, Eq, Generic)

data FetchTasks = FetchTasks {
    tasks :: TVar (Map RpkiURL FetchTask),
    cache :: TVar (Map RpkiURL FetchResult)
}

data FetchTask = Stub | Fetching (Async FetchResult)


newFetchTasks :: STM FetchTasks
newFetchTasks = do 
    tasks <- newTVar mempty 
    cache <- newTVar mempty 
    pure FetchTasks {..}
           
newFetchTasksIO :: IO FetchTasks
newFetchTasksIO = atomically newFetchTasks

fetchRepository :: (MonadIO m, Storage s) => 
                    AppContext s 
                -> FetchTasks 
                -> ValidatorPath 
                -> Now 
                -> Repository 
                -> m FetchResult
fetchRepository appContext@AppContext {..} FetchTasks {..} parentContext now repo = liftIO $ do 
    let rpkiUrl = getRpkiURL repo
    join $ atomically $ do 
        c <- readTVar cache
        case Map.lookup rpkiUrl c of 
            Just fr -> pure $ pure fr
            Nothing -> do 
                t <- readTVar tasks
                case Map.lookup rpkiUrl t of 

                    Just Stub -> retry
                    
                    Just (Fetching a) -> 
                        pure $ wait a                    

                    Nothing -> do 
                        modifyTVar' tasks $ Map.insert rpkiUrl Stub
                        
                        pure $ bracketOnError 
                            (async $ do 
                                f <- fetchRepository_ appContext parentContext now repo
                                atomically $ do 
                                    modifyTVar' cache $ Map.insert rpkiUrl f
                                    modifyTVar' tasks $ Map.delete rpkiUrl
                                pure f) 
                            (\a -> do 
                                cancel a
                                atomically $ modifyTVar' tasks $ Map.delete rpkiUrl)
                            (\a -> do 
                                atomically $ modifyTVar' tasks $ Map.insert rpkiUrl (Fetching a)
                                wait a)                                           



fetchRepository_ :: (MonadIO m, Storage s) => 
                AppContext s -> ValidatorPath -> Now -> Repository -> m FetchResult
fetchRepository_ 
    appContext@AppContext {..} 
    parentContext 
    (Now now) 
    repo = liftIO $ do
        let (Seconds maxDduration, timeoutError) = case repoURL of
                RrdpU _  -> 
                    (config ^. typed @RrdpConf . #rrdpTimeout, 
                     RrdpE $ RrdpDownloadTimeout maxDduration)
                RsyncU _ -> 
                    (config ^. typed @RsyncConf . #rsyncTimeout, 
                     RsyncE $ RsyncDownloadTimeout maxDduration)
                
        r <- timeout (1_000_000 * fromIntegral maxDduration) fetchIt
        case r of 
            Nothing -> do 
                logErrorM logger [i|Couldn't fetch repository #{getURL repoURL} after #{maxDduration}s.|]
                pure $ FetchFailure repo now (vState $ mError vContext' timeoutError)
            Just z -> pure z        
    where 
        repoURL      = getRpkiURL repo
        childContext = validatorSubPath (toText repoURL) parentContext
        vContext'    = childContext ^. typed @VPath

        fetchIt = do        
            logDebugM logger [i|Fetching repository #{getURL repoURL}.|]    
            ((r, v), elapsed) <- timedMS $ runValidatorT childContext $                 
                case repo of
                    RsyncR r -> do 
                            RsyncR <$> fromTryM 
                                    (RsyncE . UnknownRsyncProblem . fmtEx) 
                                    (updateObjectForRsyncRepository appContext r)                             
                    RrdpR r -> do                         
                        RrdpR <$> fromTryM 
                                    (RrdpE . UnknownRrdpProblem . fmtEx)
                                    (updateObjectForRrdpRepository appContext r) 
            case r of
                Left e -> do                        
                    logErrorM logger [i|Failed to fetch repository #{getURL repoURL}: #{e} |]
                    pure $ FetchFailure repo now (vState (mError vContext' e) <> v)
                Right resultRepo -> do
                    logDebugM logger [i|Fetched repository #{getURL repoURL}, took #{elapsed}ms.|]
                    pure $ FetchSuccess resultRepo now v          
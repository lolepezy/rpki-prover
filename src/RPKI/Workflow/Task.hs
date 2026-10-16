{-# LANGUAGE StrictData #-}

{- | The periodic tasks of the workflow, and the rules for which of them may
     run at the same time.

     Beyond naming the tasks, nothing here does any validating, fetching or
     storing: it is the scheduling machinery on its own, so that it can be
     reasoned about (and tested) without starting a prover.
-}
module RPKI.Workflow.Task where

import           Control.Concurrent              (forkFinally, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception               (SomeException)
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Hourglass                  (Seconds(..))
import           Data.Int                        (Int64)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import qualified Data.Text                       as Text
import           Data.String.Interpolate.IsString
import           Numeric.Natural
import           GHC.Generics

import           RPKI.AppState
import           RPKI.AppTypes
import           RPKI.Logging
import           RPKI.Time

import           UnliftIO                        (MonadUnliftIO)
import qualified UnliftIO.Exception              as UIO


-- A job run can be the first one or not and 
-- sometimes we need this information.
data JobRun = FirstRun | RanBefore
    deriving stock (Show, Eq, Ord, Generic)  

-- Different types of periodic tasks that may run 
data Task =
    -- delete old objects and old versions
    CacheCleanupTask    

    -- cleanup files in tmp, stale storage-backend state, run-away child processes, etc.
    | LeftoversCleanupTask

    -- async fetches of slow repositories
    | FetchTask

    -- download and validate TA certificates
    | TaCertificateTask

    -- Delete local rsync mirror once in a long while
    | RsyncCleanupTask

    -- fold the WAL back into the database, nobody else does it
    | WalCheckpointTask
    deriving stock (Show, Eq, Ord, Generic)


data Scheduling = Scheduling {        
        task         :: Task,
        initialDelay :: Int,
        interval     :: Seconds,
        -- Whether the completion time is stored in the database, so that
        -- the interval survives restarts instead of starting over.
        persistent   :: Bool,
        action       :: WorldVersion -> JobRun -> IO ()
    }
    deriving stock (Generic)

newtype Tasks = Tasks { 
        running :: TVar (Map Task Natural)
    }
    deriving stock (Generic)

newRunningTasks :: STM Tasks
newRunningTasks = Tasks <$> newTVar mempty

{- | Which tasks must not run at the same time.

    Almost everything may run concurrently with everything else: fetches with
    each other, cleanups with fetches, and the WAL checkpoint with anything
    (SQLite serialises it by itself, and it is the only thing that keeps the
    WAL from growing without bound). The one real rule is that the local rsync
    mirror must not be deleted while anything is fetching into it, and
    downloading a TA certificate is just another (tiny) fetch.
-}
canRunInParallel :: Task -> Task -> Bool
canRunInParallel t1 t2 = not (conflict t1 t2 || conflict t2 t1)
  where
    conflict RsyncCleanupTask FetchTask         = True
    conflict RsyncCleanupTask TaCertificateTask = True
    conflict _                _                 = False


runConcurrentlyIfPossible :: MonadUnliftIO m 
                        => AppLogger -> Task -> Tasks -> m a -> m a
runConcurrentlyIfPossible logger taskType Tasks {..} action = do 
    {- 
        Theoretically, exclusive maintenance tasks can starve indefinitely and 
        never get picked up because of fetchers running all the time.
        But in practice that is very unlikely to happen, so we'll gamble for now.
    -}    
    blockedBy <- liftIO $ atomically $ filter conflicting . Map.toList <$> readTVar running
    unless (null blockedBy) $ 
        logDebug logger [i|Task #{taskType} cannot run concurrently with #{blockedBy} and has to wait.|]        

    UIO.bracket_ (liftIO $ atomically acquire) (liftIO $ atomically release) action
  where
    conflicting = not . canRunInParallel taskType . fst

    acquire = do 
        runningTasks <- readTVar running
        when (any conflicting $ Map.toList runningTasks) retry
        writeTVar running $ Map.insertWith (+) taskType 1 runningTasks

    release = modifyTVar' running $ 
        Map.update (\count -> if count > 1 then Just (count - 1) else Nothing) taskType


versionIsOld :: Instant -> Seconds -> WorldVersion -> Bool
versionIsOld now period version =
    let validatedAt = versionToInstant version
    in not $ closeEnoughMoments (Earlier validatedAt) (Later now) period


-- | Run the action every `interval`, measured from the start of one run to 
-- the start of the next, so a run that takes longer than the interval simply 
-- starts the next one immediately. Only the very first run gets the given 
-- 'JobRun', every one after it 'RanBefore'.
periodically :: Seconds -> JobRun -> (JobRun -> IO ()) -> IO a
periodically interval firstRun action = go firstRun
  where
    go jobRun = do
        Now start <- thisInstant        
        action jobRun
        delayUntilNext start interval
        go RanBefore


leftToWaitMicros :: Earlier -> Later -> Seconds -> Int64
leftToWaitMicros (Earlier earlier) (Later later) (Seconds interval) = 
    timeToWaitNs `div` 1000
  where
    executionTimeNs = toNanoseconds later - toNanoseconds earlier
    timeToWaitNs = nanosPerSecond * interval - executionTimeNs    

-- | Sleep until `interval` has passed since `start`, or return at once if it 
-- already has.
delayUntilNext :: MonadIO m => Instant -> Seconds -> m ()
delayUntilNext start interval = do 
    Now now <- thisInstant
    let pause = leftToWaitMicros (Earlier start) (Later now) interval
    when (pause > 0) $
        liftIO $ threadDelay $ fromIntegral pause

-- | Fork a thread that is not waited for and whose failure is only worth a 
-- log line.
forkLogged :: MonadIO m => AppLogger -> Text.Text -> IO () -> m ()
forkLogged logger message action = 
    liftIO $ void $ forkFinally action (logException logger message)

logException :: MonadIO m => AppLogger -> Text.Text -> Either SomeException a -> m ()
logException logger logText result = 
    case result of
        Left ex -> logDebug logger [i|logException: #{logText}: #{ex}|]
        Right _ -> pure ()

-- | Run an action, swallowing synchronous exceptions. Asynchronous ones
-- still propagate -- that is `UIO.catchAny`'s contract.
ignoreSync :: MonadUnliftIO m => m () -> m ()
ignoreSync f = f `UIO.catchAny` const (pure ())

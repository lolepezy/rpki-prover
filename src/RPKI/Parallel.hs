{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
module RPKI.Parallel where

import Numeric.Natural
import Control.Monad
import Control.Concurrent.STM

import qualified Control.Concurrent.STM.TBQueue as Q

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Lifted

import Control.Monad.Trans.Control
import Control.Concurrent.Async.Lifted

atLeastOne :: Natural -> Natural
atLeastOne n = if n < 2 then 1 else n


-- | Utility function for a specific case of producer-consumer pair 
-- where consumer works within a transaction (represented as withTx function)
--  
txConsumeFold :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
            Natural ->
            t a ->                      -- ^ traversed collection
            (a -> m q) ->               -- ^ producer, called for every item of the traversed argument
            ((tx -> m r) -> m r) ->     -- ^ transaction in which all consumerers are wrapped
            (tx -> q -> r -> m r) ->    -- ^ producer, called for every item of the traversed argument
            r ->                        -- ^ fold initial value
            m r
txConsumeFold poolSize as produce withTx consume accum0 =
    snd <$> bracketChan 
                (atLeastOne poolSize)
                writeAll 
                readAll 
                (const $ pure ())
    where
        writeAll queue = forM_ as $
            liftIO . atomically . Q.writeTBQueue queue <=< produce
        readAll queue = withTx $ \tx -> foldM (f tx) accum0 as 
            where
                f tx accum _ = do
                    a <- liftIO $ atomically $ Q.readTBQueue queue
                    consume tx a accum       

   

-- data NextTx tx a = StartTx | FlushTx | InTx tx a

-- -- | Utility function for a specific case of producer-consumer pair 
-- -- where consumer works within a transaction (represented as withTx function)
-- --  
-- txConsumeFoldChunked :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
--             Natural ->            
--             t a ->                      -- ^ traversed collection
--             (a -> m q) ->               -- ^ producer, called for every item of the traversed argument
--             ((tx -> m r) -> m r) ->     -- ^ transaction in which all consumerers are wrapped
--             Natural -> 
--             (tx -> q -> r -> m r) ->    -- ^ producer, called for every item of the traversed argument
--             r ->                        -- ^ fold initial value
--             m r
-- txConsumeFoldChunked parallelismDegree as produce withTx chunkSize consume accum0 =
--     snd <$> bracketChan 
--                 (atLeastOne parallelismDegree)
--                 writeAll 
--                 (readAll chunkSize)
--                 (const $ pure ())
--     where
--         writeAll queue = forM_ as $
--             liftIO . atomically . Q.writeTBQueue queue <=< produce

--         readAll leftToRead queue = do 
--             n <- next
--             case n of
--                 StartTx -> withTx $ \tx -> foldM (f tx) accum0 as 
--                 InTx tx a  -> consume tx a accum
--             where                    
--                 next = do 
--                     z <- liftIO $ atomically $ Q.tryReadTBQueue queue    
--                     case z of 
--                         Nothing -> pure FlushTx
--                         Just a 
--                             | leftToRead == 0 -> pure FlushTx
--                             | otherwise       -> InTx a

            
--             where
--                 f tx accum _ = do
--                     a <- liftIO $ atomically $ Q.readTBQueue queue
--                     consume tx a accum  

bracketChan :: (MonadBaseControl IO m, MonadIO m) =>
                Natural ->
                (Q.TBQueue t -> m b) ->
                (Q.TBQueue t -> m c) ->
                (t -> m w) ->
                m (b, c)
bracketChan size produce consume kill = do
    queue <- liftIO $ atomically $ Q.newTBQueue size
    concurrently (produce queue) (consume queue)
        `finally`
        killAll queue
    where
        killAll queue = do
            a <- liftIO $ atomically $ Q.tryReadTBQueue queue
            case a of   
                Nothing -> pure ()
                Just as -> kill as >> killAll queue


data QState = QWorks | QClosed

-- Simplest closeable queue  
data ClosableQueue a = ClosableQueue !(TBQueue a) !(TVar QState)    

createClosableQueue :: Natural -> STM (ClosableQueue a)
createClosableQueue n = do 
    q <- newTBQueue n
    s <- newTVar QWorks
    pure $ ClosableQueue q s


writeClosableQueue :: ClosableQueue a -> a -> STM ()
writeClosableQueue (ClosableQueue q s) qe = 
    readTVar s >>= \case 
        QClosed -> pure ()
        QWorks  -> Q.writeTBQueue q qe

-- | Read elements from the queue in chunks and apply the function to 
-- each chunk
readQueueChunked :: ClosableQueue a -> Natural -> ([a] -> IO ()) -> IO ()
readQueueChunked (ClosableQueue q queueState) chunkSize f = go
    where 
        go = do 
            chunk' <- atomically $ do 
                chunk <- readChunk chunkSize q
                case chunk of 
                    [] -> readTVar queueState >>= \case 
                            QClosed -> pure []
                            QWorks  -> retry
                    _ -> pure chunk
            case chunk' of 
                [] -> pure ()
                chu -> f chu >> go              

closeQueue :: ClosableQueue a -> STM ()
closeQueue (ClosableQueue _ s) = writeTVar s QClosed


readChunk :: Natural -> TBQueue a -> STM [a]
readChunk 0 _ = pure []
readChunk leftToRead q = do 
    z <- Q.tryReadTBQueue q
    case z of 
        Just z' -> (z' : ) <$> readChunk (leftToRead - 1) q
        Nothing -> pure []  


-- | Simple straioghtforward implementation of a thread pool for submition of tasks.
-- 
parallelTasks :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
                Threads -> t a -> (a -> m b) -> m (t b)
parallelTasks threads@Threads {..} as f =
    snd <$> bracketChan (atLeastOne maxSize) writeAll readAll cancelTask            
    where
        writeAll queue = forM_ as $ \a -> do
            task <- submit (f a) threads Requestor
            liftIO $ atomically $ Q.writeTBQueue queue task
        readAll queue = forM as $ \_ -> do
            aa <- liftIO . atomically $ Q.readTBQueue queue
            waitTask aa    

-- Thread pool value, current and maximum size
data Threads = Threads {
    maxSize :: Natural,
    currentSize :: TVar Natural
}

makeThreads :: Natural -> STM Threads
makeThreads n = Threads n <$> newTVar 0

makeThreadsIO :: Natural -> IO Threads
makeThreadsIO = atomically . makeThreads

-- Who is going to execute a task when there're no slots in the pool
data OverflownPoolExecutor = Requestor | Submitter

-- A task can be asyncronous, executed by the requester (lazy) 
-- and executed by the submitrter (eager).
data Task m a
    = AsyncTask (Async (StM m a))
    | RequestorTask (m a)
    | SubmitterTask !a


-- | If the pool is overflown, io will be execute by the caller of "waitPool"
-- 
submitLazy :: (MonadBaseControl IO m, MonadIO m) => m a -> Threads -> m (Task m a)                       
submitLazy io pool = submit io pool Requestor

-- | If the pool is overflown, io will be execute by the thread that tries to submit the task.
-- 
submitStrict :: (MonadBaseControl IO m, MonadIO m) => m a -> Threads -> m (Task m a)                       
submitStrict io pool = submit io pool Submitter

-- Common case
submit :: (MonadBaseControl IO m, MonadIO m) => m a -> Threads -> OverflownPoolExecutor -> m (Task m a)
submit io Threads {..} execution = 
    join $ liftIO $ atomically $ do
        size <- readTVar currentSize
        if size >= maxSize             
            then pure $ blockedCase execution
            else do 
                incSize
                pure $! do 
                    a <- async $ io `finally` liftIO (atomically decSize)
                    pure $! AsyncTask a    
        where
            blockedCase Requestor  = pure $ RequestorTask io -- wrap it in RequestorTask
            blockedCase Submitter = SubmitterTask <$> io  -- do it now      

            incSize = modifyTVar' currentSize succ
            decSize = modifyTVar' currentSize pred


waitTask :: (MonadBaseControl IO m, MonadIO m) => Task m a -> m a
waitTask (RequestorTask t) = t
waitTask (SubmitterTask a) = pure a
waitTask (AsyncTask a)     = wait a

cancelTask :: (MonadBaseControl IO m, MonadIO m) => Task m a -> m ()
cancelTask (RequestorTask _) = pure ()
cancelTask (SubmitterTask _) = pure ()
cancelTask (AsyncTask a)     = cancel a

        
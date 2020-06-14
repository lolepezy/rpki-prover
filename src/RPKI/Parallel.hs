{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
module RPKI.Parallel where

import           Control.Concurrent.STM
import           Control.Monad
import           Numeric.Natural

import qualified Control.Concurrent.STM.TBQueue  as Q

import           Control.Exception.Lifted
import           Control.Monad.IO.Class          (MonadIO, liftIO)

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Trans.Control
import           Data.IORef.Lifted

import RPKI.Time

import Debug.Trace

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
    snd <$> bracketChanClosable
                (atLeastOne poolSize)
                writeAll 
                readAll 
                (\_ -> pure ())
    where
        writeAll queue = forM_ as $ \a -> do
            p <- produce a
            liftIO $ atomically $ writeCQueue queue p

        readAll queue = withTx $ \tx -> go tx accum0
            where
                go tx accum = do                
                    a <- liftIO $ atomically $ readCQueue queue
                    case a of
                        Nothing -> pure accum
                        Just a' -> consume tx a' accum >>= go tx

   
-- | Utility function for a specific case of producer-consumer pair 
-- where consumer works within a transaction (represented as withTx function)
--  
txConsumeFoldChunked :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
            Natural ->            
            t a ->                      -- ^ traversed collection
            (a -> m q) ->               -- ^ producer, called for every item of the traversed argument
            ((tx -> m r) -> m r) ->     -- ^ transaction in which all consumerers are wrapped
            Natural -> 
            (tx -> q -> r -> m r) ->    -- ^ producer, called for every item of the traversed argument
            r ->                        -- ^ fold initial value
            m r
txConsumeFoldChunked parallelismDegree as produce withTx chunkSize consume accum0 =
    snd <$> bracketChanClosable 
                (atLeastOne parallelismDegree)
                writeAll 
                readAll 
                (const $ pure ())
    where
        writeAll queue = forM_ as $
            liftIO . atomically . writeCQueue queue <=< produce

        readAll queue = 
            go Nothing chunkSize accum0
            where
                go maybeTx leftToRead accum = do 
                    n <- liftIO $ atomically $ readCQueue queue                            
                    case n of
                        Nothing      -> pure accum
                        Just nextOne ->
                            case maybeTx of
                                Nothing -> do 
                                    accum' <- do 
                                        -- Now n1 <- thisMoment
                                        -- traceM $ show n1 <> ": starting transaction"
                                        withTx $ \tx -> do 
                                            -- Now n2 <- thisMoment
                                            -- traceM $ show n2 <> ": actually started transaction"
                                            work tx nextOne
                                    go Nothing chunkSize accum' 
                                Just tx -> work tx nextOne
                    where                    
                        work tx element = do 
                            accum' <- consume tx element accum
                            case leftToRead of
                                0 -> pure accum'
                                _ -> go (Just tx) (leftToRead - 1) accum'


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


bracketChanClosable :: (MonadBaseControl IO m, MonadIO m) =>
                Natural ->
                (ClosableQueue t -> m b) ->
                (ClosableQueue t -> m c) ->
                (t -> m w) ->
                m (b, c)
bracketChanClosable size produce consume kill = do        
    queue <- liftIO $ atomically $ newCQueue size
    let closeQ = liftIO $ atomically $ closeQueue queue
    concurrently 
            (produce queue `finally` closeQ) 
            (consume queue `finally` closeQ)
        `finally`
            killAll queue
    where
        killAll queue = do
            a <- liftIO $ atomically $ readCQueue queue
            case a of   
                Nothing -> pure ()
                Just as -> kill as >> killAll queue

data QState = QWorks | QClosed

-- Simplest closeable queue  
data ClosableQueue a = ClosableQueue !(TBQueue a) !(TVar QState)    

newCQueue :: Natural -> STM (ClosableQueue a)
newCQueue n = do 
    q <- newTBQueue n
    s <- newTVar QWorks
    pure $ ClosableQueue q s

writeCQueue :: ClosableQueue a -> a -> STM ()
writeCQueue (ClosableQueue q s) qe =
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
readChunk 0 _          = pure []
readChunk leftToRead q = do 
    z <- Q.tryReadTBQueue q
    case z of 
        Just z' -> (z' : ) <$> readChunk (leftToRead - 1) q
        Nothing -> pure []  


readCQueue :: ClosableQueue a -> STM (Maybe a)
readCQueue (ClosableQueue q queueState) = do 
    z <- Q.tryReadTBQueue q
    case z of 
        Just z' -> pure $ Just z'
        Nothing -> 
            readTVar queueState >>= \case 
                QClosed -> pure Nothing
                QWorks  -> retry

readCQueuState :: ClosableQueue a -> STM QState
readCQueuState (ClosableQueue _ queueState) = readTVar queueState

-- | Simple straioghtforward implementation of a thread pool for submition of tasks.
-- 
parallelTasks :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
                Bottleneck -> t a -> (a -> m b) -> m (t b)
parallelTasks bottleneck@Bottleneck {..} as f = do
    tasks <- forM as $ \a -> newTask (f a) bottleneck Submitter
    forM tasks waitTask

parallelTasksN :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
                Natural -> t a -> (a -> m b) -> m (t b)
parallelTasksN n as f = do
    bottleneck <- liftIO $ atomically $ newBottleneck n
    parallelTasks bottleneck as f    

-- Thread pool value, current and maximum size
data Bottleneck = Bottleneck {
    maxSize     :: !Natural,
    currentSize :: TVar Natural
}

newBottleneck :: Natural -> STM Bottleneck
newBottleneck n = Bottleneck n <$> newTVar 0

newBottleneckIO :: Natural -> IO Bottleneck
newBottleneckIO = atomically . newBottleneck

-- Who is going to execute a task when the bottleneck is busy
data BottleneckFullExecutor = Requestor | Submitter

-- A task can be asyncronous, executed by the requestor
-- and executed by the submitrter (eager).
data Task m a
    = AsyncTask !(Async (StM m a))
    | RequestorTask !(m a)
    | SubmitterTask !a


-- | If the bootleneck is full, io will be execute by the caller of "waitTask"
lazyTask :: (MonadBaseControl IO m, MonadIO m) => m a -> Bottleneck -> m (Task m a)                       
lazyTask io bottleneck = newTask io bottleneck Requestor

-- | If the bottleneck is full, io will be execute by the thread that calls newTask.
strictTask :: (MonadBaseControl IO m, MonadIO m) => m a -> Bottleneck -> m (Task m a)                       
strictTask io bottleneck = newTask io bottleneck Submitter

-- Common case
newTask :: (MonadBaseControl IO m, MonadIO m) => 
        m a -> Bottleneck -> BottleneckFullExecutor -> m (Task m a)
newTask io Bottleneck {..} execution = 
    join $ liftIO $ atomically $ do
        size <- readTVar currentSize
        if size >= maxSize             
            then pure $ 
                case execution of
                    Requestor -> pure $ RequestorTask io -- wrap it in RequestorTask                    
                    Submitter -> SubmitterTask <$> io    -- do it now                          
            else do 
                incSize
                pure $! do 
                    a <- async $ io `finally` liftIO (atomically decSize)
                    pure $ AsyncTask a    
        where            
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







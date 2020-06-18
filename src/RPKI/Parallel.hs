{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
module RPKI.Parallel where

import           Control.Concurrent.STM
import           Control.Monad
import           Numeric.Natural

import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NonEmpty

import qualified Control.Concurrent.STM.TBQueue  as Q

import           Control.Exception.Lifted
import           Control.Monad.IO.Class          (MonadIO, liftIO)

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Trans.Control
import           Data.IORef.Lifted

import RPKI.Time

import Debug.Trace

import Streaming
import qualified Streaming.Prelude as S


atLeastOne :: Natural -> Natural
atLeastOne n = if n < 2 then 1 else n

-- 
-- TODO Refactor it so that is shared code with "txFoldPipeline"

-- Consume a stream, map each element and put asyncs in the queue.
-- Read the queue and consume asyncs on the other end.
foldPipeline :: (MonadBaseControl IO m, MonadIO m) =>
            Bottleneck ->
            Stream (Of s) m () ->
            (s -> m p) ->          -- ^ producer
            (p -> r -> m r) ->     -- ^ consumer, called for every item of the traversed argument
            r ->                   -- ^ fold initial value
            m r
foldPipeline bottleneck stream mapStream consume accum0 =
    snd <$> bracketChanClosable
                (chanSize bottleneck)
                writeAll 
                readAll 
                cancel
    where
        chanSize (Bottleneck bottlenecks) = atLeastOne $ minimum $ NonEmpty.map snd bottlenecks

        writeAll queue = S.mapM_ toQueue stream
            where 
                toQueue s = do
                    t <- async $ mapStream s
                    liftIO $ atomically $ writeCQueue queue t

        readAll queue = go accum0
            where
                go accum = do                
                    t <- liftIO $ atomically $ readCQueue queue
                    case t of
                        Nothing -> pure accum
                        Just t' -> do 
                            p <- wait t'
                            consume p accum >>= go

-- | Utility function for a specific case of producer-consumer pair 
-- where consumer works within a transaction (represented as withTx function)
--  
txFoldPipeline :: (MonadBaseControl IO m, MonadIO m) =>
            Natural ->
            Stream (Of q) m () ->
            ((tx -> m r) -> m r) ->     -- ^ transaction in which all consumerers are wrapped
            (tx -> q -> r -> m r) ->    -- ^ consumer, called for every item of the traversed argument
            r ->                        -- ^ fold initial value
            m r
txFoldPipeline poolSize stream withTx consume accum0 =
    snd <$> bracketChanClosable
                (atLeastOne poolSize)
                writeAll 
                readAll 
                (\_ -> pure ())
    where
        writeAll queue = S.mapM_ toQueue stream
            where 
                toQueue = liftIO . atomically . writeCQueue queue

        readAll queue = withTx $ \tx -> go tx accum0
            where
                go tx accum = do                
                    a <- liftIO $ atomically $ readCQueue queue
                    case a of
                        Nothing -> pure accum
                        Just a' -> consume tx a' accum >>= go tx

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
parallelTasks bottleneck as f = do
    tasks <- forM as $ \a -> newTask (f a) bottleneck Submitter
    forM tasks waitTask

-- Thread pool value, current and maximum size
newtype Bottleneck = Bottleneck (NonEmpty (TVar Natural, Natural))
    deriving newtype Semigroup

newBottleneck :: Natural -> STM Bottleneck
newBottleneck n = do 
    currentSize <- newTVar 0
    pure $ Bottleneck $ (currentSize, n) :| []

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

-- | If the bottleneck is full, io will be execute by the thread that calls newTask.
pureTask :: (MonadBaseControl IO m, MonadIO m) => a -> Bottleneck -> m (Task m a)                       
pureTask io = strictTask (pure $! io)

-- General case
newTask :: (MonadBaseControl IO m, MonadIO m) => 
        m a -> Bottleneck -> BottleneckFullExecutor -> m (Task m a)
newTask io (Bottleneck bottlenecks) execution = 
    join $ liftIO $ atomically $ do     
        eachHasSomeSpace <- forM bottlenecks $ \(currentSize, maxSize) -> do 
                                    cs <- readTVar currentSize
                                    pure $ cs < maxSize        
        if and eachHasSomeSpace 
            then do 
                incSizes
                pure $! do 
                    a <- async $ io `finally` liftIO (atomically decSizes)
                    pure $ AsyncTask a    
            else pure $ 
                case execution of
                    Requestor -> pure $ RequestorTask io -- wrap it in RequestorTask                    
                    Submitter -> SubmitterTask <$> io    -- do it now                                      
        where            
            incSizes = forM_ bottlenecks $ \(currentSize, _) -> modifyTVar' currentSize succ
            decSizes = forM_ bottlenecks $ \(currentSize, _) -> modifyTVar' currentSize pred



waitTask :: (MonadBaseControl IO m, MonadIO m) => Task m a -> m a
waitTask (RequestorTask t) = t
waitTask (SubmitterTask a) = pure a
waitTask (AsyncTask a)     = wait a

cancelTask :: (MonadBaseControl IO m, MonadIO m) => Task m a -> m ()
cancelTask (RequestorTask _) = pure ()
cancelTask (SubmitterTask _) = pure ()
cancelTask (AsyncTask a)     = cancel a







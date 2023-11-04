-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}

module RPKI.Parallel where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue  as Q
import           Control.Concurrent.Async.Lifted
import           Control.Exception.Lifted
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans.Control

import           Data.Foldable (for_)

import           Numeric.Natural

import           RPKI.AppMonad
import           Streaming
import qualified Streaming.Prelude               as S



atLeastOne :: Natural -> Natural
atLeastOne n = if n < 2 then 1 else n

-- 
-- TODO Refactor it so that is shared code with "txFoldPipeline"

-- Consume a stream, map each element and put asyncs in the queue.
-- Read the queue and consume asyncs on the other end.
foldPipeline :: (MonadBaseControl IO m, MonadIO m) =>
            Natural ->
            Stream (Of s) (ValidatorTCurried m) () ->
            (s -> ValidatorT m p) ->          -- ^ producer
            (p -> r -> ValidatorT m r) ->     -- ^ consumer, called for every item of the traversed argument
            r ->                              -- ^ fold initial value
            ValidatorT m r
foldPipeline parallelism stream mapStream consume accum0 =
    snd <$> bracketChanClosable
                (atLeastOne parallelism)
                writeAll 
                readAll 
                cancel
  where        
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
            Stream (Of q) (ValidatorTCurried m) () ->
            ((tx -> ValidatorT m ()) -> ValidatorT m ()) -> -- ^ transaction in which all consumerers are wrapped
            (tx -> q -> ValidatorT m ()) ->           -- ^ consumer, called for every item of the traversed argument            
            ValidatorT m ()
txFoldPipeline parallelism stream withTx consume =
    snd <$> bracketChanClosable
                (atLeastOne parallelism)
                writeAll 
                readAll 
                (\_ -> pure ())
  where
    writeAll queue = 
        S.mapM_ 
            (liftIO . atomically . writeCQueue queue) 
            stream
        
    readAll queue = withTx go
      where
        go tx = do                
            a <- liftIO $ atomically $ readCQueue queue
            for_ a $ \a' -> consume tx a' >> go tx


-- 
-- | Create two threads and queue between then. Calls
-- 'produce' in one thread and 'consume' in the other thread,
-- 'kill' is used to kill an item in the queue in case
-- the whole thing is interrupted with an exception.
--    
bracketChanClosable :: (MonadBaseControl IO m, MonadIO m) =>
                Natural ->
                (ClosableQueue t -> m b) ->
                (ClosableQueue t -> m c) ->
                (t -> m w) ->
                m (b, c)
bracketChanClosable size produce consume kill = do     
    queue <- liftIO $ atomically $ newCQueue size
    let closeQ = liftIO $ atomically $ closeCQueue queue
    concurrently
            (produce queue `finally` closeQ) 
            (consume queue `finally` closeQ)
        `finally`
            killAll queue kill    


data QState = QOperational | QClosed
    deriving (Show, Eq)

-- Simplest closeable queue  
data ClosableQueue a = ClosableQueue (TBQueue a) (TVar QState)    

newCQueueIO :: Natural -> IO (ClosableQueue a)
newCQueueIO = atomically . newCQueue

newCQueue :: Natural -> STM (ClosableQueue a)
newCQueue n = ClosableQueue <$> newTBQueue n <*> newTVar QOperational

writeCQueue :: ClosableQueue a -> a -> STM ()
writeCQueue (ClosableQueue q s) qe =
    readTVar s >>= \case         
        QOperational -> Q.writeTBQueue q qe
        QClosed      -> pure ()

ifFullCQueue :: ClosableQueue a -> STM Bool
ifFullCQueue (ClosableQueue q _) = isFullTBQueue q

        
closeCQueue :: ClosableQueue a -> STM ()
closeCQueue (ClosableQueue _ s) = writeTVar s QClosed

isClosedCQueue :: ClosableQueue a -> STM Bool
isClosedCQueue (ClosableQueue _ s) = (QClosed ==) <$> readTVar s 

readCQueue :: ClosableQueue a -> STM (Maybe a)
readCQueue (ClosableQueue q queueState) =
    Q.tryReadTBQueue q >>= \case    
        Just z' -> pure $ Just z'
        Nothing -> 
            readTVar queueState >>= \case 
                QClosed -> pure Nothing
                QOperational  -> retry

readChunk :: Natural -> ClosableQueue a -> STM [a]
readChunk 0 _ = pure []
readChunk leftToRead cq@(ClosableQueue q queueState) =
    Q.tryReadTBQueue q >>= \case     
        Just z -> (z : ) <$> readChunk (leftToRead - 1) cq
        Nothing -> 
            readTVar queueState >>= \case 
                QClosed      -> pure []
                QOperational -> retry

-- | Read elements from the queue in chunks and apply the function to 
-- each chunk
readQueueChunked :: ClosableQueue a -> Natural -> ([a] -> IO ()) -> IO ()
readQueueChunked cq chunkSize f = go
  where     
    go = atomically (readChunk chunkSize cq) >>= \case             
            []    -> pure ()
            chunk -> f chunk >> go  

killAll :: MonadIO m => ClosableQueue t -> (t -> m a) -> m ()
killAll queue kill = do
    a <- liftIO $ atomically $ readCQueue queue    
    for_ a $ \as -> kill as >> killAll queue kill

-- Auxialliary stuff for limiting the amount of parallel reading LMDB transactions    
data Semaphore = Semaphore Int (TVar Int)
    deriving (Eq)

createSemaphoreIO :: Int -> IO Semaphore
createSemaphoreIO = atomically . createSemaphore

createSemaphore :: Int -> STM Semaphore
createSemaphore n = Semaphore n <$> newTVar 0

-- Execute using a semaphore as a barrier
withSemaphore :: Semaphore -> IO a -> IO a
withSemaphore (Semaphore maxCounter current) f = 
    bracket incr decr (const f)
    where 
        incr = atomically $ do 
            c <- readTVar current
            if c >= maxCounter 
                then retry
                else writeTVar current (c + 1)
        decr _ = atomically $ modifyTVar' current $ \c -> c - 1

-- Execute using a semaphore as a barrier, but if the sempahore 
-- is not allowing execution, execute after a timeout anyway
withSemaphoreAndTimeout :: Semaphore -> Int -> IO a -> IO a
withSemaphoreAndTimeout (Semaphore maxCounter current) intervalMicroSeconds f =     
    bracket aquireSlot releaseSlot (const f)
  where  
    thereIsSpaceToRun = do 
        c <- readTVar current
        if c >= maxCounter 
            then retry
            else writeTVar current (c + 1)                                        

    aquireSlot = 
        either (const True) (const False) <$> 
            race 
                (atomically thereIsSpaceToRun)
                (threadDelay intervalMicroSeconds)
        
    releaseSlot increasedCounter = 
        when increasedCounter $ 
            atomically $ modifyTVar' current $ \c -> c - 1  
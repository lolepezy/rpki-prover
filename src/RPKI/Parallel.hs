{-# LANGUAGE StrictData          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RPKI.Parallel where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.MVar
import           Control.Concurrent.QSem
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue  as Q
import qualified Control.Concurrent.Async        as IOAsync
import qualified Control.Exception               as IOExc
import           UnliftIO                        (MonadUnliftIO)
import qualified UnliftIO.Exception              as UIO
import           Control.Monad

import           Effectful
import           Effectful.Concurrent.Async
import           Effectful.Exception             (finally, throwIO)

import           Data.IORef
import qualified Data.Vector                     as V
import           GHC.Conc                        (getNumCapabilities)

import           Data.Hourglass
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
foldPipeline :: (ValidatorIO es, Concurrent :> es) =>
            Natural ->
            Stream (Of s) (Eff es) () ->
            (s -> Eff es p) ->          -- ^ producer
            (p -> r -> Eff es r) ->     -- ^ consumer, called for every item of the traversed argument
            r ->                              -- ^ fold initial value
            Eff es r
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
txFoldPipeline :: (ValidatorIO es, Concurrent :> es) =>
            Natural ->
            Stream (Of q) (Eff es) () ->
            ((tx -> Eff es ()) -> Eff es ()) -> -- ^ transaction in which all consumerers are wrapped
            (tx -> q -> Eff es ()) ->           -- ^ consumer, called for every item of the traversed argument            
            Eff es ()
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

-- | Process every item of a list with a pool of worker threads and feed the
-- results to a single consumer running inside one transaction. The consumer 
-- is called exactly once for every item, either in the order of the items 
-- (an RRDP delta, where a later item may depend on an earlier one) or in the 
-- order they are ready (a snapshot). Keeping the order costs a snapshot ~6%:
-- the consumer is nearly saturated, and it can't make up for the time it 
-- spends waiting for the oldest item.
--
-- Made for saving an RRDP snapshot, where the workers do CPU-heavy parsing and
-- the consumer is the SQLite writer, which is one thread by necessity. What
-- limited `txFoldPipeline` there to ~6 busy cores of 16 was not SQLite:
--
--   * Every SQLite call is a safe foreign call, which gives the capability
--     away and has to win it back on return. With parsing threads on every
--     capability that wait, not the SQL, was most of the writer's time, ~6
--     calls per object. So the consumer gets capability 0 to itself and the
--     workers are pinned to the others.
--
--   * A producer forking one thread per item is queued behind those same
--     threads on its own capability and starves the pipeline. The workers here
--     just take the next item from a shared list.
--
--   * Results are handed over through MVars, not a `TBQueue`. A consumer 
--     blocking in STM once per item leaked TVar watch queue entries
--     and transaction records that the GC keeps on the mutable list and
--     rescans on every minor GC; with one worker (-N2) that more than doubled
--     the run time, and it gets worse the longer the run.
--
txPoolPipeline :: (ValidatorIO es, Concurrent :> es) =>
            ResultOrder ->
            [s] ->
            (s -> Eff es p) ->                  -- ^ worker, an exception here fails the whole pipeline
            ((tx -> Eff es ()) -> Eff es ()) -> -- ^ transaction in which the consumer runs
            (tx -> p -> Eff es ()) ->           -- ^ consumer
            Eff es ()
txPoolPipeline order items process withTx consume = do
    caps <- liftIO getNumCapabilities
    let !itemCount  = length items
        consumerCap = 0
        workerCaps  = if caps > 1 then [1 .. caps - 1] else [0]
        window      = 16 * length workerCaps

    -- Items are numbered as they are taken and in item order, each result 
    -- goes to the slot for its number, which the consumer empties in order. 
    -- A worker holds one of `window` permits from taking an item until the 
    -- consumer takes its result, so no more than `window` items are ever in 
    -- flight and the slot of each of them has already been emptied. In 
    -- completion order all results go through the same slot.
    remaining <- liftIO $ newIORef (0 :: Int, items)
    slots     <- liftIO $ V.replicateM window newEmptyMVar
    permits   <- liftIO $ newQSem window
    let slot i = case order of
            ItemOrder       -> slots V.! (i `mod` window)
            CompletionOrder -> slots V.! 0

    let worker = do
            liftIO $ waitQSem permits
            next <- liftIO $ atomicModifyIORef' remaining $ \case
                        (i, s : ss) -> ((i + 1, ss), Just (i, s))
                        done        -> (done, Nothing)
            case next of
                Nothing     -> liftIO $ signalQSem permits
                Just (i, s) -> do
                    p <- process s
                    liftIO $ putMVar (slot i) p
                    worker

    let consumer = withTx $ \tx ->
            for_ [0 .. itemCount - 1] $ \i -> do
                p <- liftIO $ takeMVar (slot i) <* signalQSem permits
                consume tx p

    withAsyncOn consumerCap consumer $ \c ->
        withAsyncsOn workerCaps worker $ \ws ->
            waitAllOrThrow (c : ws)
  where
    withAsyncsOn [] _ k = k []
    withAsyncsOn (cap : caps) f k =
        withAsyncOn cap f $ \a -> withAsyncsOn caps f $ \as -> k (a : as)

    -- Any failure is rethrown right away, which cancels everything
    -- else on the way out of the `withAsyncOn` brackets.
    waitAllOrThrow [] = pure ()
    waitAllOrThrow as = do
        (done, r) <- waitAnyCatch as
        either throwIO (const $ waitAllOrThrow $ filter (/= done) as) r

data ResultOrder = ItemOrder | CompletionOrder
    deriving stock (Eq, Show)

-- 
-- | Create two threads and queue between then. Calls
-- 'produce' in one thread and 'consume' in the other thread,
-- 'kill' is used to kill an item in the queue in case
-- the whole thing is interrupted with an exception.
--    
bracketChanClosable :: (Concurrent :> es, IOE :> es) =>
                        Natural ->
                        (ClosableQueue t -> Eff es b) ->
                        (ClosableQueue t -> Eff es c) ->
                        (t -> Eff es w) ->
                        Eff es (b, c)
bracketChanClosable size produce consume kill = do     
    queue <- liftIO $ atomically $ newCQueue size
    let closeQ = liftIO $ atomically $ closeCQueue queue
    concurrently
            (produce queue `finally` closeQ)
            (consume queue `finally` closeQ)
        `finally`
            killAll queue
  where
    killAll queue = do
        a <- liftIO $ atomically $ readCQueue queue    
        for_ a $ \as -> kill as >> killAll queue


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
        
closeCQueue :: ClosableQueue a -> STM ()
closeCQueue (ClosableQueue _ s) = writeTVar s QClosed

isEmptyCQueue :: ClosableQueue a -> STM Bool
isEmptyCQueue (ClosableQueue q _) = isEmptyTBQueue q 

readCQueue :: ClosableQueue a -> STM (Maybe a)
readCQueue (ClosableQueue q queueState) =
    Q.tryReadTBQueue q >>= \case    
        Just z  -> pure $ Just z
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

-- Auxialliary stuff for limiting the amount of parallel reading DB transactions
data Semaphore = Semaphore { 
        capacity :: Int,
        current  :: TVar Int
    }
    deriving (Eq)

newSemaphoreIO :: MonadIO m => Int -> m Semaphore
newSemaphoreIO = liftIO . atomically . newSemaphore

newSemaphore :: Int -> STM Semaphore
newSemaphore n = Semaphore n <$> newTVar 0

-- Execute using a semaphore as a barrier
withSemaphore :: MonadUnliftIO m => Semaphore -> m a -> m a
withSemaphore Semaphore {..} f = 
    UIO.bracket (liftIO incr) (liftIO . decr) (const f)
  where 
    incr = atomically $ do 
        c <- readTVar current
        if c >= capacity 
            then retry
            else writeTVar current (c + 1)

    decr _ = atomically $ modifyTVar' current $ \c -> c - 1


-- Execute using a semaphore as a barrier, but if the sempahore 
-- is not allowing execution, execute after a timeout anyway
withSemaphoreOrTimeout :: Semaphore -> Seconds -> IO a -> IO a
withSemaphoreOrTimeout Semaphore {..} timeout f =     
    IOExc.bracket aquireSlot releaseSlot (const f)
  where  
    aquireSlot = 
        either (const True) (const False) <$> 
            IOAsync.race 
                (atomically thereIsSpaceToRun)
                (threadDelay $ let Seconds s = timeout in fromIntegral $ s * 1000_000)

    thereIsSpaceToRun = do 
        c <- readTVar current
        if c >= capacity 
            then retry
            else writeTVar current (c + 1)                                        
        
    releaseSlot increasedCounter = 
        when increasedCounter $ 
            atomically $ modifyTVar' current $ \c -> c - 1  
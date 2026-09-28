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
import qualified UnliftIO.Exception              as UIO
import           Control.Monad

import           Effectful
import           Effectful.Concurrent.Async
import           Effectful.Exception             (SomeException, finally, throwIO)

import           Data.IORef
import qualified Data.List                       as List
import qualified Data.List.Split                 as Split
import qualified Data.Sequence                   as Seq
import           Data.Sequence                   (Seq)
import qualified Data.Vector                     as V
import           Data.Void                       (Void, absurd)
import           GHC.Conc                        (getNumCapabilities)

import           Data.Hourglass
import           Data.Foldable (for_)

import           Numeric.Natural

import           RPKI.AppMonad
import           Streaming
import qualified Streaming.Prelude               as S


atLeastOne :: Natural -> Natural
atLeastOne n = if n < 2 then 1 else n

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


-- | Process every item of a stream with a pool of worker threads and feed the
-- results to a single consumer running inside one transaction. The consumer 
-- is called exactly once for every item, either in the order of the items 
-- (an RRDP delta, where a later item may depend on an earlier one) or in the 
-- order they are ready (a snapshot). Keeping the order costs a snapshot ~6%:
-- the consumer is nearly saturated, and it can't make up for the time it 
-- spends waiting for the oldest item. The workers take items from the stream
-- one at a time, so it can be a walk through a directory tree of any size.
--
-- Made for saving an RRDP snapshot, where the workers do CPU-heavy parsing and
-- the consumer is the SQLite writer, which is one thread by necessity. What
-- limited the pipeline before it, an `async` per item and a `TBQueue` of them,
-- to ~6 busy cores of 16 was not SQLite:
--
--   * Every SQLite call is a safe foreign call, which gives the capability
--     away and has to win it back on return. With parsing threads on every
--     capability that wait, not the SQL, was most of the writer's time, ~6
--     calls per object. So the consumer gets capability 0 to itself and the
--     workers are pinned to the others.
--
--   * A producer forking one thread per item is queued behind those same
--     threads on its own capability and starves the pipeline. The workers here
--     just take the next item from the stream.
--
--   * Results are handed over through MVars, not a `TBQueue`. A consumer 
--     blocking in STM once per item leaked TVar watch queue entries
--     and transaction records that the GC keeps on the mutable list and
--     rescans on every minor GC; with one worker (-N2) that more than doubled
--     the run time, and it gets worse the longer the run.
--
txPoolPipeline :: (ValidatorIO es, Concurrent :> es) =>
            ResultOrder ->
            Stream (Of s) IO () ->
            (s -> Eff es p) ->                  -- ^ worker, an exception here fails the whole pipeline
            ((tx -> Eff es ()) -> Eff es ()) -> -- ^ transaction in which the consumer runs
            (tx -> p -> Eff es ()) ->           -- ^ consumer
            Eff es ()
txPoolPipeline order items process withTx consume = do
    caps <- liftIO getNumCapabilities
    let consumerCap = 0
        workerCaps  = if caps > 1 then [1 .. caps - 1] else [0]
        window      = 16 * length workerCaps

    -- Items are numbered as they are taken and in item order, each result 
    -- goes to the slot for its number, which the consumer empties in order. 
    -- A worker holds one of `window` permits from taking an item until the 
    -- consumer takes its result, so no more than `window` items are ever in 
    -- flight and the slot of each of them has already been emptied. In 
    -- completion order all results go through the same slot.
    --
    -- How many items there are is only known at the end of the stream: the 
    -- worker that gets there first puts the count in the next slot, as if it
    -- were one more item.
    remaining <- liftIO $ newMVar (0 :: Int, Just items)
    slots     <- liftIO $ V.replicateM window newEmptyMVar
    permits   <- liftIO $ newQSem window
    let slot i = case order of
            ItemOrder       -> slots V.! (i `mod` window)
            CompletionOrder -> slots V.! 0

    let takeItem = modifyMVar remaining $ \case
            (i, Just stream) -> 
                S.next stream >>= \case
                    Right (s, rest) -> pure ((i + 1, Just rest), Item i s)
                    Left ()         -> pure ((i, Nothing), LastItem i)
            ended -> 
                pure (ended, NoItems)

    let worker = do
            liftIO $ waitQSem permits
            liftIO takeItem >>= \case
                NoItems    -> liftIO $ signalQSem permits
                LastItem n -> liftIO $ putMVar (slot n) (Left n)
                Item i s   -> do
                    p <- process s
                    liftIO $ putMVar (slot i) (Right p)
                    worker

    let consumer = withTx $ \tx -> 
            let go taken itemCount
                    | Just taken == itemCount = pure ()
                    | otherwise = 
                        liftIO (takeMVar (slot taken) <* signalQSem permits) >>= \case
                            Left n  -> go taken (Just n)
                            Right p -> consume tx p >> go (taken + 1) itemCount
            in go 0 Nothing

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

-- | What a worker of `txPoolPipeline` gets from the stream: an item, the 
-- news that there were `n` of them, or nothing, when someone else got that.
data PipelineItem s = Item Int s | LastItem Int | NoItems


-- | A fixed set of workers taking tasks from one queue, for work that is
-- recursive and uneven, like validating a tree of CAs. A task may submit more
-- tasks and wait for them. Whoever waits for a task runs it itself if no worker
-- has taken it yet, and runs other tasks while it's running elsewhere, so a
-- parent waiting for its children doesn't take a worker out of the pool.
--
-- Deciding on parallelism locally, e.g. per manifest from the number of its
-- entries, left ARIN (1 CA of 20 CAs of 500 CAs each) validated on one thread.
-- With one pool shared by everything, the workers are busy as long as there's
-- work left anywhere.
--
-- A synchronous exception in a task goes to whoever waits for the task.
-- An asynchronous one kills the worker, and with it the whole pool.
data WorkPool = WorkPool {
        queue     :: IORef (Seq (IO ())),
        -- | Signalled for every task put into the queue. Waiters take tasks
        -- without waiting for it, so a worker may find the queue empty.
        available :: QSem
    }

data PoolTask a = PoolTask {
        -- | Run the task on this thread, unless someone has taken it already
        runIfNotTaken :: IO (),
        result        :: MVar (Either SomeException a)
    }

newWorkPool :: IO WorkPool
newWorkPool = WorkPool <$> newIORef Seq.empty <*> newQSem 0

-- | Run the pool's workers on the given capabilities, one each, for as
-- long as the action runs.
withWorkers :: WorkPool -> [Int] -> IO a -> IO a
withWorkers pool caps f =
    withWorkersOn caps $ \workers ->
        -- A worker only ever stops with an exception
        either absurd id <$> IOAsync.race (snd <$> IOAsync.waitAny workers) f
  where
    withWorkersOn [] k = k []
    withWorkersOn (cap : caps') k =
        IOAsync.withAsyncOn cap worker $ \w -> withWorkersOn caps' $ \ws -> k (w : ws)

    worker :: IO Void
    worker = forever $ do
        waitQSem $ available pool
        takeQueued pool >>= sequence_

submitTask :: WorkPool -> IO a -> IO (PoolTask a)
submitTask WorkPool {..} action = do
    result <- newEmptyMVar
    -- Emptied by whoever takes the task. A task stays in the queue until
    -- a worker gets to it, even when it's been run by whoever waited for it,
    -- and the queue shouldn't keep the action and its result all that time.
    toRun  <- newIORef $ Just $ UIO.tryAny action >>= putMVar result
    let runIfNotTaken = atomicModifyIORef' toRun (Nothing, ) >>= sequence_
    atomicModifyIORef' queue $ \q -> (q Seq.|> runIfNotTaken, ())
    signalQSem available
    pure PoolTask {..}

-- | Wait for a task from a worker or a task of the pool: run it here if
-- nobody has taken it yet, and run other tasks while it's running elsewhere.
awaitTask :: WorkPool -> PoolTask a -> IO (Either SomeException a)
awaitTask pool PoolTask {..} = runIfNotTaken >> go
  where
    go = tryReadMVar result >>= \case
        Just r  -> pure r
        Nothing -> takeQueued pool >>= \case
            Just other -> other >> go
            Nothing    -> readMVar result

-- | Wait for a task from outside the pool.
waitTask :: PoolTask a -> IO (Either SomeException a)
waitTask PoolTask {..} = readMVar result

pollTask :: PoolTask a -> IO (Maybe (Either SomeException a))
pollTask PoolTask {..} = tryReadMVar result

-- | The newest task. Taking the oldest, CAs higher up in the tree, goes 
-- through the tree breadth first, with more of it in progress at once: ~20%
-- more live data in a first validation of all five RIRs, and no faster.
takeQueued :: WorkPool -> IO (Maybe (IO ()))
takeQueued WorkPool {..} =
    atomicModifyIORef' queue $ \q ->
        case Seq.viewr q of
            Seq.EmptyR    -> (q, Nothing)
            rest Seq.:> t -> (rest, Just t)

-- | `forM` for a task of the pool. The items that `isHeavy` picks are tasks of
-- their own, the others go in tasks of `chunkSize` items. With just one task
-- to make, it doesn't use the pool. Results are in the order of the items.
--
-- Every task is waited for before this returns or rethrows the first
-- exception, the tasks would run in an environment that is gone otherwise.
forInPool :: IOE :> es =>
            WorkPool
            -> Int
            -> (a -> Bool)
            -> [a]
            -> (a -> Eff es b)
            -> Eff es [b]
forInPool pool chunkSize isHeavy items f =
    case units of
        _ : _ : _ ->
            withEffToIO (ConcUnlift Ephemeral Unlimited) $ \unlift -> do
                tasks <- forM units $ \unit ->
                    submitTask pool $ forM unit $ \(i, x) -> (i, ) <$> unlift (f x)

                -- Newest first, the way workers take them too, so that child
                -- CAs, submitted first, are started last.
                results <- reverse <$> mapM (awaitTask pool) (reverse tasks)
                case sequence results of
                    Left e   -> IOExc.throwIO e
                    Right rs -> pure $! map snd $ List.sortOn fst $ concat rs
        _ ->
            forM items f
  where
    (heavy, light) = List.partition (isHeavy . snd) $ zip [0 :: Int ..] items
    units = map pure heavy <> Split.chunksOf chunkSize light

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
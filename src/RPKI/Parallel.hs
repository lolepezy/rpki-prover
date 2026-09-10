{-# LANGUAGE StrictData          #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RPKI.Parallel where

import           Control.Concurrent              (threadDelay)
import           Control.Concurrent.STM
import qualified Control.Concurrent.STM.TBQueue  as Q
import qualified Control.Concurrent.Async        as IOAsync
import qualified Control.Exception               as IOExc
import           Control.Monad

import           Effectful
import           Effectful.Concurrent.Async
import           Effectful.Exception             (finally)

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
foldPipeline :: ValidatorIO es =>
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
txFoldPipeline :: ValidatorIO es =>
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
        current  :: TVar Int, 
        highest  :: TVar Int
    }
    deriving (Eq)


newSemaphore :: Int -> STM Semaphore
newSemaphore n = Semaphore n <$> newTVar 0 <*> newTVar 0

-- Execute using a semaphore as a barrier
withSemaphore :: Semaphore -> IO a -> IO a
withSemaphore Semaphore {..} f = 
    IOExc.bracket incr decr (const f)
  where 
    incr = atomically $ do 
        c <- readTVar current
        if c >= capacity 
            then retry
            else do 
                let c' = c + 1
                writeTVar current c'
                h <- readTVar highest 
                when (c' > h) $ writeTVar highest c'

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
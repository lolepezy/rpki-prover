module RPKI.Parallel where

import Numeric.Natural
import Control.Monad
import Control.Concurrent.STM

import qualified Control.Concurrent.STM.TBQueue as Q

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Lifted

import Control.Monad.Trans.Control
import Control.Concurrent.Async.Lifted as AsyncL


-- FIXME Do something about 'wait' throwing an exception
parallel :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
            Natural -> t a -> (a -> m b) -> m (t b)
parallel poolSize as f =
    snd <$> bracketChan (max 1 (poolSize - 1)) writeAll readAll AsyncL.cancel
    where
        writeAll queue = forM_ as $ \a -> do
            aa <- AsyncL.async $ f a
            liftIO $ atomically $ Q.writeTBQueue queue aa
        readAll queue = forM as $ \_ ->         
            AsyncL.wait =<< (liftIO . atomically $ Q.readTBQueue queue)    

txFunnel :: (Traversable t, Monad m, MonadBaseControl IO m, MonadIO m) =>
            Natural ->
            t a ->
            (a -> m b) -> 
            ((tx -> m (t c)) -> m (t c)) ->
            (tx -> b -> m c) -> m (t c)
txFunnel poolSize as produce withTx consume =
    snd <$> bracketChan (max 1 (poolSize - 1)) writeAll readAll (const $ pure ())
    where
        writeAll queue = forM_ as $
            liftIO . atomically . Q.writeTBQueue queue <=< produce
        readAll queue = withTx $ \tx ->
            forM as $ \_ -> 
                consume tx =<< (liftIO . atomically $ Q.readTBQueue queue)


txConsumeFold :: (Traversable t, Monad m, MonadBaseControl IO m, MonadIO m) =>
            Natural ->
            t a ->
            (a -> m q) -> 
            ((tx -> m r) -> m r) ->
            (tx -> q -> r -> m r) -> 
            r -> 
            m r
txConsumeFold poolSize as produce withTx consume accum0 =
    snd <$> bracketChan 
                (max 1 (poolSize - 1)) 
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


bracketChan :: (MonadBaseControl IO m, MonadIO m) =>
                Natural ->
                (Q.TBQueue t -> m b) ->
                (Q.TBQueue t -> m c) ->
                (t -> m w) ->
                m (b, c)
bracketChan size produce consume kill = do
    queue <- liftIO $ atomically $ Q.newTBQueue size
    (AsyncL.concurrently (produce queue) (consume queue))
        `finally`
        (killAll queue)
    where
        killAll queue = do
            a <- liftIO $ atomically $ Q.tryReadTBQueue queue
            case a of   
                Nothing -> pure ()
                Just as -> kill as >> killAll queue



{- It's really bad

parallelPooled :: (Traversable t, MonadUnliftIO m) =>
            Natural ->
            (a -> m b) -> t a -> m (t (Either SomeException b))
parallelPooled poolSize f as = do
    pool <- liftIO $ newPool poolSize
    tasks <- forM as (\a -> asyncOrNow pool (f a))
    mapM (waitTask pool) tasks    


-- Simple pool of asyncs
data Task m a = AsyncTask !(Unlift.Async a) | Immediate !(m (Either SomeException a))

data AsyncPool = AsyncPool {
    maxSize     :: !Natural,
    currentSize :: !(TVar Natural)
}

newPool :: Natural -> IO AsyncPool
newPool n = atomically $ AsyncPool n <$> newTVar 0

asyncOrNow :: MonadUnliftIO m => AsyncPool -> m a -> m (Task m a)
asyncOrNow (AsyncPool {..}) f = 
    join $ atomically $ do 
        cs <- readTVar currentSize
        if cs <= maxSize
            then do 
                writeTVar currentSize (cs + 1)
                pure $! AsyncTask <$> Unlift.async f
            else pure $! pure $! 
                -- This one forces actual execution strictly
                -- TODO Handle exceptions
                Immediate $! try f

waitTask :: MonadUnliftIO m => AsyncPool -> Task m a -> m (Either SomeException a)
waitTask pool task = 
    case task of
        Immediate io -> io
        AsyncTask a -> try (Unlift.wait a) `finally` decrement pool 
         

cancelTask :: MonadUnliftIO m => AsyncPool -> Task m a -> m ()
cancelTask pool task =
    case task of
        Immediate _ -> pure ()
        AsyncTask a -> Unlift.cancel a `finally` decrement pool 

decrement :: MonadUnliftIO m => AsyncPool -> m ()
decrement (AsyncPool {..}) = 
    liftIO $ atomically $ modifyTVar' currentSize $ \n -> n - 1

-}
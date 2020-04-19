{-# LANGUAGE BangPatterns #-}
module RPKI.Parallel where

import Numeric.Natural
import Control.Monad

import qualified Control.Concurrent.STM.TBQueue as Q

import UnliftIO
import qualified UnliftIO.Async as Unlift


-- FIXME Do something about 'wait' throwing an exception
parallel :: (Traversable t, MonadUnliftIO m) =>
            Natural ->
            (a -> m b) -> t a -> m (t b)
parallel poolSize f as =
    snd <$> bracketChan (max 1 (poolSize - 1)) writeAll readAll Unlift.cancel
    where
        writeAll queue = forM_ as $ \a ->
            Unlift.async (f a) >>= liftIO . atomically . Q.writeTBQueue queue        
        readAll queue = forM as $ \_ ->         
            Unlift.wait =<< liftIO (atomically $ Q.readTBQueue queue)

txFunnel :: (Traversable t, Monad m, MonadUnliftIO m) =>
            Natural ->
            t a ->
            (a -> m b) -> 
            ((tx -> m (t c)) -> m (t c)) ->
            (tx -> b -> m c) -> m (t c)
txFunnel poolSize as produce withTx consume = do
    snd <$> bracketChan (max 1 (poolSize - 1)) writeAll readAll (const $ pure ())
    where
        writeAll queue = forM_ as $
            liftIO . atomically . Q.writeTBQueue queue <=< produce
        readAll queue = withTx $ \tx ->
            forM as $ \_ -> 
                consume tx =<< liftIO (atomically $ Q.readTBQueue queue)


txConsumeFold :: (Traversable t, Monad m, MonadUnliftIO m) =>
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


bracketChan :: MonadUnliftIO m =>
                Natural ->
                (Q.TBQueue t -> m b) ->
                (Q.TBQueue t -> m c) ->
                (t -> m w) ->
                m (b, c)
bracketChan size produce consume kill = do
    queue <- liftIO $ atomically $ Q.newTBQueue size
    (Unlift.concurrently (produce queue) (consume queue))
        `finally`
        (killAll queue)
    where
        killAll queue = do
            a <- liftIO $ atomically $ Q.tryReadTBQueue queue
            case a of   
                Nothing -> pure ()
                Just as -> kill as >> killAll queue
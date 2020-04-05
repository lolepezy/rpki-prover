module RPKI.Parallel where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import           Control.Monad

import UnliftIO
import qualified UnliftIO.Async as Unlift

-- FIXME Do something about 'wait' throwing an exception
parallel :: (Traversable t, MonadUnliftIO m) => 
            Int -> 
            (a -> m b) -> t a -> m (t b)
parallel poolSize f as = do
  (chanIn, chanOut) <- liftIO $ Chan.newChan $ max 1 (poolSize - 1)
  snd <$> Unlift.concurrently (writeAll chanIn) (readAll chanOut)
  where
    writeAll chanIn  = forM_ as $ \a ->
      Unlift.async (f a) >>= liftIO . Chan.writeChan chanIn
    readAll chanOut = forM as $ \_ -> 
      Unlift.wait =<< liftIO (Chan.readChan chanOut)

boundedFunnel :: (Traversable t, MonadUnliftIO m) => 
            Int ->
            t a -> 
            (a -> m b) -> 
            (b -> m c) -> m (t c)
boundedFunnel poolSize as f g = do
  (chanIn, chanOut) <- liftIO $ Chan.newChan $ max 1 (poolSize - 1)
  snd <$> Unlift.concurrently (writeAll chanIn) (readAll chanOut)
  where
    writeAll chanIn  = forM_ as $
      liftIO . Chan.writeChan chanIn <=< f
    readAll chanOut = forM as $ \_ -> 
      g =<< liftIO (Chan.readChan chanOut)


txFunnel :: (Traversable t, MonadUnliftIO m) => 
            Int ->
            t a -> 
            (a -> m b) -> 
            ((tx -> m (t c)) -> m (t c)) ->
            (tx -> b -> m c) -> m (t c)
txFunnel poolSize as produce withTx consume = do
    (chanIn, chanOut) <- liftIO $ Chan.newChan $ max 1 (poolSize - 1)
    snd <$> Unlift.concurrently (writeAll chanIn) (readAll chanOut)
    where
        writeAll chanIn = forM_ as $
            liftIO . Chan.writeChan chanIn <=< produce
        readAll chanOut = withTx $ \tx -> 
            forM as $ \_ -> 
                consume tx =<< liftIO (Chan.readChan chanOut)

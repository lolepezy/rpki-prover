module RPKI.Parallel where

import Numeric.Natural
import Control.Monad
import Control.Concurrent.STM

import Data.Maybe

import qualified Control.Concurrent.STM.TBQueue as Q

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Lifted

import Control.Monad.Trans.Control
import Control.Concurrent.Async.Lifted as AsyncL


data Parallelism = Dynamic !(TVar Natural) Natural | Fixed Natural

dynamicPara :: Natural -> IO Parallelism
dynamicPara n = atomically $ do 
    c <- newTVar 0
    pure $ Dynamic c n

fixedPara :: Natural -> Parallelism
fixedPara = Fixed

parallel :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
            Parallelism -> t a -> (a -> m b) -> m (t b)
parallel parallelism as f =
    case parallelism of 
        Fixed n                     -> doFixed n
        Dynamic currentPara maxPara -> doDynamic currentPara maxPara
    where 
        doFixed para =
            snd <$> bracketChan (max 1 (para - 1)) writeAll readAll AsyncL.cancel
            where
                writeAll queue = forM_ as $ \a -> do
                    aa <- AsyncL.async $ f a
                    liftIO $ atomically $ Q.writeTBQueue queue aa
                readAll queue = forM as $ \_ ->         
                    AsyncL.wait =<< (liftIO . atomically $ Q.readTBQueue queue)    

        doDynamic currentPara maxPara =
            snd <$> bracketChan (max 1 (maxPara - 1)) writeAll readAll cancelIt
            where
                cancelIt aa = do 
                    liftIO $ atomically $ modifyTVar' currentPara $ \c -> c - 1
                    AsyncL.cancel aa

                writeAll queue = forM_ as $ \a -> 
                    join $ liftIO $ atomically $ do 
                        c <- readTVar currentPara
                        if c >= maxPara
                            then retry
                            else pure $ do 
                                aa <- AsyncL.async $ f a
                                liftIO $ atomically $ do 
                                    Q.writeTBQueue queue aa
                                    writeTVar currentPara (c + 1)

                readAll queue = forM as $ \_ -> do
                    aa <- liftIO $ atomically $ do 
                        modifyTVar' currentPara $ \c -> c - 1
                        Q.readTBQueue queue
                    AsyncL.wait aa

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

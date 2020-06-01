module RPKI.Parallel where

import Numeric.Natural
import Control.Monad
import Control.Concurrent.STM

import qualified Control.Concurrent.STM.TBQueue as Q

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Lifted

import Control.Monad.Trans.Control
import Control.Concurrent.Async.Lifted as AsyncL

data Parallelism = Dynamic !(TVar Natural) Natural | Fixed Natural

dynamicPara :: Natural -> STM Parallelism
dynamicPara n = do 
    c <- newTVar 0
    pure $ Dynamic c n

dynamicParaIO :: Natural -> IO Parallelism
dynamicParaIO = atomically . dynamicPara    

fixedPara :: Natural -> Parallelism
fixedPara = Fixed

atLeastOne :: Natural -> Natural
atLeastOne n = if n < 2 then 1 else n

parallel :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
            Parallelism -> t a -> (a -> m b) -> m (t b)
parallel parallelism as f =
    case parallelism of 
        Fixed n                     -> doFixed n
        Dynamic currentPara maxPara -> doDynamic1 currentPara maxPara
    where 
        doFixed para =
            snd <$> bracketChan (atLeastOne para) writeAll readAll AsyncL.cancel
            where
                writeAll queue = forM_ as $ \a -> do
                    aa <- AsyncL.async $ f a
                    liftIO $ atomically $ Q.writeTBQueue queue aa
                readAll queue = forM as $ \_ ->         
                    AsyncL.wait =<< (liftIO . atomically $ Q.readTBQueue queue)    

        -- doDynamic currentPara maxPara = do
        --     current <- liftIO $ readTVarIO currentPara
        --     snd <$> bracketChan (atLeastOne $ min current maxPara) writeAll readAll cancelIt
        --     where
        --         cancelIt aa = do 
        --             liftIO $ atomically $ modifyTVar' currentPara decN
        --             AsyncL.cancel aa

        --         writeAll queue = forM_ as $ \a -> 
        --             join $ liftIO $ atomically $ do 
        --                 c <- readTVar currentPara
        --                 if c >= maxPara
        --                     then retry
        --                     else 
        --                         -- TODO lock?
        --                         pure $ do 
        --                         aa <- AsyncL.async $ f a
        --                         liftIO $ atomically $ do 
        --                             -- TODO unlock?
        --                             Q.writeTBQueue queue aa
        --                             modifyTVar' currentPara incN

        --         readAll queue = forM as $ \_ -> do
        --             aa <- liftIO $ atomically $ do                         
        --                 modifyTVar' currentPara decN
        --                 Q.readTBQueue queue
        --             AsyncL.wait aa

        doDynamic1 currentPara maxPara = do
            current <- liftIO $ readTVarIO currentPara
            -- snd <$> bracketChan (atLeastOne $ min current maxPara) writeAll readAll AsyncL.cancel
            snd <$> bracketChan maxPara writeAll readAll AsyncL.cancel
            where
                writeAll queue = forM_ as $ \a -> do
                    aa <- AsyncL.async $ f a
                    liftIO $ atomically $ Q.writeTBQueue queue aa
                readAll queue = forM as $ \_ ->         
                    AsyncL.wait =<< (liftIO . atomically $ Q.readTBQueue queue)    

        -- incN, decN :: Natural -> Natural
        -- incN c = c + 1
        -- decN c = if c <= 1 then 1 else c - 1


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
                (atLeastOne poolSize)
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



-- data NextTx tx a = StartTx | FlushTx | InTx tx a

-- -- | Utility function for a specific case of producer-consumer pair 
-- -- where consumer works within a transaction (represented as withTx function)
-- --  
-- txConsumeFoldChunked :: (Traversable t, MonadBaseControl IO m, MonadIO m) =>
--             Natural ->            
--             t a ->                      -- ^ traversed collection
--             (a -> m q) ->               -- ^ producer, called for every item of the traversed argument
--             ((tx -> m r) -> m r) ->     -- ^ transaction in which all consumerers are wrapped
--             Natural -> 
--             (tx -> q -> r -> m r) ->    -- ^ producer, called for every item of the traversed argument
--             r ->                        -- ^ fold initial value
--             m r
-- txConsumeFoldChunked parallelismDegree as produce withTx chunkSize consume accum0 =
--     snd <$> bracketChan 
--                 (atLeastOne parallelismDegree)
--                 writeAll 
--                 (readAll chunkSize)
--                 (const $ pure ())
--     where
--         writeAll queue = forM_ as $
--             liftIO . atomically . Q.writeTBQueue queue <=< produce

--         readAll leftToRead queue = do 
--             n <- next
--             case n of
--                 StartTx -> withTx $ \tx -> foldM (f tx) accum0 as 
--                 InTx tx a  -> consume tx a accum
--             where                    
--                 next = do 
--                     z <- liftIO $ atomically $ Q.tryReadTBQueue queue    
--                     case z of 
--                         Nothing -> pure FlushTx
--                         Just a 
--                             | leftToRead == 0 -> pure FlushTx
--                             | otherwise       -> InTx a

            
--             where
--                 f tx accum _ = do
--                     a <- liftIO $ atomically $ Q.readTBQueue queue
--                     consume tx a accum  

bracketChan :: (MonadBaseControl IO m, MonadIO m) =>
                Natural ->
                (Q.TBQueue t -> m b) ->
                (Q.TBQueue t -> m c) ->
                (t -> m w) ->
                m (b, c)
bracketChan size produce consume kill = do
    queue <- liftIO $ atomically $ Q.newTBQueue size
    AsyncL.concurrently (produce queue) (consume queue)
        `finally`
        killAll queue
    where
        killAll queue = do
            a <- liftIO $ atomically $ Q.tryReadTBQueue queue
            case a of   
                Nothing -> pure ()
                Just as -> kill as >> killAll queue

-- General closeable queue
data QState = QWorks | QClosed

data Quu a = Quu !(TBQueue a) !(TVar QState)    

createQuu :: Natural -> STM (Quu a)
createQuu n = do 
    q <- newTBQueue n
    s <- newTVar QWorks
    pure $ Quu q s


writeQuu :: Quu a -> a -> STM ()
writeQuu (Quu q s) qe = 
    readTVar s >>= \case 
        QClosed -> pure ()
        QWorks  -> Q.writeTBQueue q qe

readQueueChunked :: Quu a -> Natural -> ([a] -> IO ()) -> IO ()
readQueueChunked (Quu q queueState) chunkSize f = go
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

closeQueue :: Quu a -> STM ()
closeQueue (Quu _ s) = writeTVar s QClosed


readChunk :: Natural -> TBQueue a -> STM [a]
readChunk 0 _ = pure []
readChunk leftToRead q = do 
    z <- Q.tryReadTBQueue q
    case z of 
        Just z' -> (z' : ) <$> readChunk (leftToRead - 1) q
        Nothing -> pure []  
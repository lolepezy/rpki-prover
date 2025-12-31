{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.Store.Cache where

import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Hashable

import qualified StmContainers.Map as StmMap

newtype Cache k v = Cache (StmMap.Map k (CacheEntry v))

data CacheEntry v = WillGet | Getting (Async v) | Got v

new :: IO (Cache k v)
new = Cache <$> StmMap.newIO

get :: Hashable k => Cache k v -> k -> IO v -> IO (Maybe v)
get (Cache m) k f = join $ atomically $ do 
    StmMap.lookup k m >>= \case
        Just WillGet -> retry 

        Just (Getting a) -> 
            pure $ Just <$> wait a

        Just (Got v) -> pure $ pure $ Just v

        Nothing -> do 
            StmMap.insert WillGet k m
            pure $ do 
                a <- async $ fetch `catch` cleanup
                register a 
                Just <$> wait a
              where 
                fetch = do 
                    z <- f                    
                    atomically $ StmMap.insert (Got z) k m                        
                    pure z

                cleanup (e :: SomeException) = do 
                    atomically $ StmMap.delete k m
                    throwIO e

                register a = 
                    atomically $ do                     
                        StmMap.lookup k m >>= \case                        
                            Just WillGet -> StmMap.insert (Getting a) k m
                            _            -> pure ()                        


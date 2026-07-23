{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.AppMonadUtil where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           UnliftIO (pooledForConcurrentlyN)
import           Control.Monad.Reader

import           Data.Either                 (partitionEithers)
import           Data.Hourglass

import           System.Timeout

import           RPKI.AppMonad
import           RPKI.Parallel
import           RPKI.Reporting

recover :: Monad m => ValidatorT m a -> ValidatorT m () -> ValidatorT m a
recover tryF finallyF = 
    tryIt `catchError` catchIt
  where
    tryIt = do  
        z <- tryF 
        finallyF
        pure z
    catchIt e = do
        finallyF
        throwError e            


timeoutVT :: Seconds -> ValidatorT IO a -> ValidatorT IO a -> ValidatorT IO a
timeoutVT s toDo timedOut = do 
    let Seconds t = s
    scopes <- askScopes 
    z <- liftIO $ timeout (1_000_000 * fromIntegral t) (runValidatorT scopes toDo)
    maybe timedOut (embedValidatorT . pure) z    


andThen :: ValidatorT IO a -> ValidatorT IO () -> ValidatorT IO a
andThen f action = do
    !z <- f
    action
    pure $! z

bracketVT :: IO a 
        -> (a -> ValidatorT IO r) 
        -> (a -> ValidatorT IO b) 
        -> ValidatorT IO b
bracketVT acquire release f = do 
    scopes <- askScopes    
    z <- liftIO $ bracket acquire (runValidatorT scopes . release) (runValidatorT scopes . f)  
    embedValidatorT $ pure z

concurrentlyVTLenientN :: Int
                        -> [a] 
                        -> (a -> ValidatorT IO r) 
                        -> ValidatorT IO [r]
concurrentlyVTLenientN n as f = do
    scopes <- askScopes    
    (rs, vss) <- liftIO $ unzip <$> pooledForConcurrentlyN n as (runValidatorT scopes . f)
    embedState (mconcat vss)   
    let (failures, successes) = partitionEithers rs
    case successes of 
        [] -> appError $ ComposeE failures
        _  -> pure $! successes    

concurrentlyVTStrictN :: Int
                    -> [a] 
                    -> (a -> ValidatorT IO r) 
                    -> ValidatorT IO [r]
concurrentlyVTStrictN n as f = do
    scopes <- askScopes    
    (rs, vss) <- liftIO $ unzip <$> pooledForConcurrentlyN n as (runValidatorT scopes . f)
    embedState (mconcat vss)   
    let (failures, successes) = partitionEithers rs
    case failures of 
        [] -> pure $! successes
        _  -> appError $ ComposeE failures


withSemaphoreVT :: Semaphore -> ValidatorT IO a -> ValidatorT IO a
withSemaphoreVT s v = do
    scopes <- askScopes
    embedValidatorT $! liftIO $! withSemaphore s $! runValidatorT scopes v
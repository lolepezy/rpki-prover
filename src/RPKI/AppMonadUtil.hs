{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ValidatorT helpers that need 'RPKI.Parallel'. They cannot live in
-- 'RPKI.AppMonad' itself, since Parallel imports AppMonad.
module RPKI.AppMonadUtil where

import           Control.Exception.Lifted
import           Control.Monad.Except
import           UnliftIO (pooledForConcurrentlyN)
import           Control.Monad.Reader

import           Data.Either                 (partitionEithers)

import           RPKI.AppMonad
import           RPKI.Parallel
import           RPKI.Reporting

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
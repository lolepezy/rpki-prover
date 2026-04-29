{-# LANGUAGE OverloadedStrings #-}

module RPKI.Store.CacheSpec where

import           Control.Exception
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Data.IORef
import           Data.Maybe

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU

import qualified RPKI.Store.Cache as Cache
import qualified StmContainers.Map as StmMap


cacheGroup :: TestTree
cacheGroup = testGroup "Unit tests for cache" [
    shouldCacheBasics,
    shouldDoIOJustOnce,    
    shouldCrashWithoutIssues    
  ]


shouldCacheBasics :: TestTree
shouldCacheBasics = HU.testCase "Cache should cache" $ do        
    cache :: Cache.Cache Int Int <- Cache.new 
    v1 <- Cache.get cache (1 :: Int) (pure 42)
    v2 <- Cache.get cache (2 :: Int) (pure 43)
    v3 <- Cache.get cache (3 :: Int) (pure 44)    
    HU.assertEqual "First get should return value" (Just 42) v1    
    HU.assertEqual "First get should return value" (Just 43) v2    
    HU.assertEqual "First get should return value" (Just 44) v3    


shouldDoIOJustOnce :: TestTree
shouldDoIOJustOnce = HU.testCase "Cache should cache" $ do        
    cache :: Cache.Cache Int Int <- Cache.new 

    counter <- newIORef (0 :: Int)
    v1 <- Cache.get cache (2 :: Int) (makeValue counter)
    counterValue <- readIORef counter

    HU.assertEqual "First get should return value" (Just 42) v1
    HU.assertEqual "First get should return value" 1 counterValue
     
    v2 <- Cache.get cache (2 :: Int) (makeValue counter)
    counterValue1 <- readIORef counter
    HU.assertEqual "Second get should the same value from cache" (Just 42) v2   
    HU.assertEqual "First get should return value" 1 counterValue1

    forConcurrently_ [1..1000] $ \(_ :: Int) -> do
        v <- Cache.get cache (2 :: Int) (makeValue counter)
        HU.assertEqual "Second get should the same value from cache" (Just 42) v

    counterValue2 <- readIORef counter
    HU.assertEqual "First get should return value" 1 counterValue2        

    v_ <- Cache.get cache (3 :: Int) (modifyIORef' counter (+1) >> pure 43)
    counterValue_ <- readIORef counter
    HU.assertEqual "Second get should the same value from cache" (Just 43) v_
    HU.assertEqual "First get should return value" 2 counterValue_

shouldCrashWithoutIssues :: TestTree
shouldCrashWithoutIssues = HU.testCase "Cache should cache" $ do        
    cache :: Cache.Cache Int Int <- Cache.new 
    let i = 3
    t <- try $ Cache.get cache i (throwIO Overflow)

    let Cache.Cache m = cache
    z <- atomically $ StmMap.lookup i m
    HU.assertBool "Should be empty" (isNothing z)

    case t of 
        Left (_ :: SomeException) -> pure ()
        Right _ -> HU.assertBool "Shouldn't be here" False    

    
    
makeValue :: (Num a, Num b) => IORef a -> IO b
makeValue counter = do 
    modifyIORef' counter (+1)
    pure 42


{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}


module RPKI.RRDP.MetaSpec where

import           Control.Monad

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Test.Tasty
import qualified Test.Tasty.HUnit                  as HU
import           RPKI.Time


intervalMetaGroup :: TestTree
intervalMetaGroup = testGroup "Interval prediction"
    [
        emulateTicksGroup        
    ]

emulateTicksGroup :: TestTree
emulateTicksGroup = testGroup "Object storage test"
    [                    
        HU.testCase "Should insert and get back" emulateTicks               
    ]        

emulateTicks :: HU.Assertion
emulateTicks = do     
    updatedValue <- newTVarIO (0 :: Integer)
    currentInterval <- newTVarIO 60_000
    tickerTime <- newTVarIO (0 :: Integer)

    let eventTick = replicateM 1000 $ do                         
            atomically $ do 
                modifyTVar' updatedValue (+1)
                modifyTVar' tickerTime (+100_000)
            
            ct <- readTVarIO tickerTime
            Now t <- thisInstant
            putStrLn $ "Ticker " <> show t <> ", " <> show ct

            threadDelay 100_000

    let emulateDuration = do 
            localCounter <- newTVarIO 0
            pollerTime <- newTVarIO 0
            replicateM 1500 $ do 
                interval <- 
                    atomically $ do 
                        counter <- readTVar updatedValue 
                        local   <- readTVar localCounter
                        if counter > local then 
                            if (counter - local > 1) then do 
                                modifyTVar' currentInterval decreaseInterval
                            else do  
                                -- ct <- readTVar pollerTime
                                -- when (ct `mod` 7 == 0) $                                
                                --     modifyTVar' currentInterval decreaseIntervalABit
                                pure ()
                        else 
                            modifyTVar' currentInterval increaseInterval

                        writeTVar localCounter counter
                        readTVar currentInterval
                                
                ct <- readTVarIO pollerTime
                Now t <- thisInstant
                putStrLn $ "Poller " <> show t <> ", " <> show ct <> ", interval = " <> show interval

                threadDelay interval
                atomically $ modifyTVar' pollerTime (+interval)

    void $ concurrently eventTick emulateDuration
  
  where 
    increaseInterval s = s + 1 + s `div` 10        
    decreaseInterval s = s - s `div` 3 - 1
    decreaseIntervalABit s = s - s `div` 7 - 1






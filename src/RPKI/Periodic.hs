module RPKI.Periodic where

import           Control.Concurrent
import           Control.Monad
import           Data.Int  (Int64)
import           Data.Hourglass
import           Data.Foldable (for_)

import           RPKI.AppTypes
import           RPKI.Time


data NextStep = Repeat | Done

-- 
versionIsOld :: Instant -> Seconds -> WorldVersion -> Bool
versionIsOld now period (WorldVersion nanos) =
    let validatedAt = fromNanoseconds nanos
    in not $ closeEnoughMoments validatedAt now period


-- | Execute an IO action every N seconds
periodically :: Seconds -> IO NextStep -> IO ()
periodically interval action = go
  where
    go = do
        Now start <- thisInstant
        nextStep <- action
        Now end <- thisInstant
        let pause = leftToWait start end interval
        when (pause > 0) $
            threadDelay $ fromIntegral pause
        case nextStep of
            Repeat -> go
            Done   -> pure ()

-- | Execute an IO action every N seconds
periodicallyWithDelay :: Seconds -> IO (Maybe Seconds) -> IO ()
periodicallyWithDelay initialDelay action = go initialDelay
  where
    go delay = do
        Now start <- thisInstant
        nextDelay <- action
        Now end <- thisInstant
        let pause = leftToWait start end delay
        when (pause > 0) $
            threadDelay $ fromIntegral pause
        for_ nextDelay go
        

leftToWait :: Instant -> Instant -> Seconds -> Int64
leftToWait start end (Seconds interval) = let
    executionTimeNs = toNanoseconds end - toNanoseconds start
    timeToWaitNs = nanosPerSecond * interval - executionTimeNs
    in timeToWaitNs `div` 1000               




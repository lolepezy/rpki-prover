{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Time where

import Data.Int
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Hourglass         
import           System.Hourglass       (dateCurrent)

import           RPKI.Orphans.Serialise

import GHC.Generics (Generic)
import Codec.Serialise (Serialise)


newtype Instant = Instant DateTime
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise

instance Show Instant where
    show (Instant d) = timePrint ISO8601_DateAndTime d

-- | Current time that is to be passed into the environment of validating functions
newtype Now = Now { unNow :: Instant }
    deriving stock (Show, Eq, Ord)

thisInstant :: MonadIO m => m Now
thisInstant = Now . Instant <$> liftIO dateCurrent


timed :: MonadIO m => m a -> m (a, Int64)
timed action = do 
    Now (Instant begin) <- thisInstant
    !z <- action
    Now (Instant end) <- thisInstant
    let (Seconds s, NanoSeconds ns) = timeDiffP end begin
    pure (z, s * nanosPerSecond + ns)

timedMS :: MonadIO m => m a -> m (a, Int64)
timedMS action = do 
    (!z, ns) <- timed action   
    pure (z, fromIntegral $! ns `div` microsecondsPerSecond)

nanosPerSecond :: Num p => p
nanosPerSecond = 1000_000_000
{-# INLINE nanosPerSecond #-}

microsecondsPerSecond :: Num p => p
microsecondsPerSecond = 1000_000
{-# INLINE microsecondsPerSecond #-}

toNanoseconds :: Instant -> Int64
toNanoseconds (Instant instant) = 
    nanosPerSecond * seconds + nanos
    where 
        ElapsedP (Elapsed (Seconds seconds)) (NanoSeconds nanos) = timeGetElapsedP instant

asSeconds :: Instant -> Int64
asSeconds (Instant instant) = seconds
    where 
        ElapsedP (Elapsed (Seconds seconds)) _ = timeGetElapsedP instant

fromNanoseconds :: Int64 -> Instant
fromNanoseconds totalNanos =    
    Instant $ timeConvert elapsed
    where 
        elapsed = ElapsedP (Elapsed (Seconds seconds)) (NanoSeconds nanos)
        (seconds, nanos) = totalNanos `divMod` nanosPerSecond     

closeEnoughMoments :: Instant -> Instant -> Seconds -> Bool
closeEnoughMoments (Instant firstMoment) (Instant secondMoment) intervalSeconds = 
    timeDiff secondMoment firstMoment < intervalSeconds


uiDateFormat :: Instant -> String
uiDateFormat (Instant d) = timePrint format d
  where 
    format = TimeFormatString [
            Format_Year, dash, Format_Month2, dash, Format_Day2,
            Format_Text ' ',
            Format_Hour, colon, Format_Minute, colon, Format_Second,
            Format_TimezoneName
        ]
    dash = Format_Text '-'
    colon = Format_Text ':'   

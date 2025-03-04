{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RPKI.Time where

import           Control.DeepSeq
import           Data.Int
import           Data.Semigroup
import           Control.Monad.IO.Class (MonadIO, liftIO)

import           GHC.Generics

import           Data.Hourglass         
import           System.Hourglass       (dateCurrent)
import           System.CPUTime

import           RPKI.Store.Base.Serialisation
import           RPKI.Orphans.Store


newtype Instant = Instant DateTime
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)

data LogDateFormat = LogDateFormat
    deriving stock (Eq, Ord, Generic)

instance TimeFormat LogDateFormat where
    toFormat _ = TimeFormatString [
            Format_Year, dash, Format_Month2, dash, Format_Day2, 
            Format_Text ' ',
            Format_Hour, colon, Format_Minute, colon, Format_Second, Format_Text '.', Format_MilliSecond, 
            Format_TzHM_Colon_Z
        ]
      where 
        dash = Format_Text '-'
        colon = Format_Text ':'

instance Show Instant where
    show (Instant d) = timePrint LogDateFormat d

-- | Current time that is to be passed into the environment of validating functions
newtype Now = Now { unNow :: Instant }
    deriving stock (Show, Eq, Ord)

newtype TimeMs = TimeMs { unTimeMs :: Int64 }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving newtype (Num)
    deriving Semigroup via Sum TimeMs
    deriving Monoid via Sum TimeMs

newtype CPUTime = CPUTime { unCPUTime :: Integer }
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)
    deriving newtype (Num)

instance Show TimeMs where 
    show (TimeMs ms) = show ms

instance Show CPUTime where 
    show (CPUTime ms) = show ms

thisInstant :: MonadIO m => m Now
thisInstant = Now . Instant <$> liftIO dateCurrent

getCpuTime :: MonadIO m => m CPUTime
getCpuTime = do 
    picos <- liftIO getCPUTime
    pure $! CPUTime $ picos `div` 1000_000_000

durationMs :: Instant -> Instant -> TimeMs
durationMs (Instant begin) (Instant end) = let 
    (Seconds s, NanoSeconds ns) = timeDiffP end begin    
    totalNanos = s * nanosPerSecond + ns
    in fromIntegral $! totalNanos `div` nanosPerMicrosecond 
    
timed :: MonadIO m => m a -> m (a, Int64)
timed action = do 
    Now (Instant begin) <- thisInstant
    !z <- action
    Now (Instant end) <- thisInstant
    let (Seconds s, NanoSeconds ns) = timeDiffP end begin
    pure $! (z, s * nanosPerSecond + ns)

timedMS :: MonadIO m => m a -> m (a, TimeMs)
timedMS action = do 
    (!z, ns) <- timed action   
    pure $! (z, fromIntegral $! ns `div` nanosPerMicrosecond)

nanosPerSecond :: Num p => p
nanosPerSecond = 1000_000_000
{-# INLINE nanosPerSecond #-}

nanosPerMicrosecond :: Num p => p
nanosPerMicrosecond = 1000_000
{-# INLINE nanosPerMicrosecond #-}

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
closeEnoughMoments firstMoment secondMoment intervalSeconds = 
    instantDiff secondMoment firstMoment < intervalSeconds

instantDiff :: Instant -> Instant -> Seconds
instantDiff (Instant firstMoment) (Instant secondMoment) = 
    timeDiff firstMoment secondMoment 

momentAfter :: Instant -> Seconds -> Instant
momentAfter (Instant moment) seconds = Instant $ timeAdd moment seconds

instantDateFormat :: Instant -> String
instantDateFormat (Instant d) = timePrint format d
  where 
    format = TimeFormatString [
            Format_Year, dash, Format_Month2, dash, Format_Day2,
            Format_Text ' ',
            Format_Hour, colon, Format_Minute, colon, Format_Second,
            Format_TimezoneName
        ]
    dash = Format_Text '-'
    colon = Format_Text ':'   

secondsToInt :: Seconds -> Int
secondsToInt (Seconds s) = fromIntegral s

toMicroseconds :: Seconds -> Int
toMicroseconds (Seconds s) = fromIntegral $ 1000_000 * s

cpuTimePerSecond :: CPUTime -> Instant -> Instant -> Double
cpuTimePerSecond (CPUTime t) from to = let
    Seconds duration = instantDiff to from
    in (fromInteger t :: Double) / (fromIntegral duration :: Double)

asCpuTime :: Seconds -> CPUTime 
asCpuTime (Seconds s) = CPUTime $ fromIntegral $ s * 1000

isoFormat :: Instant -> String
isoFormat (Instant t) = timePrint ISO8601_DateAndTime t

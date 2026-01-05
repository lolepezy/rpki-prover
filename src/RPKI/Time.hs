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


newtype Instant = Instant Int64
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

logPrint :: DateTime -> String
logPrint = timePrint LogDateFormat

instance Show Instant where
    show (Instant d) = logPrint (fromNanoseconds d)

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

newtype Timebox = Timebox { unTimebox :: Seconds }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

instance Show TimeMs where 
    show (TimeMs ms) = show ms

instance Show CPUTime where 
    show (CPUTime ms) = show ms

newInstant :: DateTime -> Instant
newInstant = Instant . toNanos

thisInstant :: MonadIO m => m Now
thisInstant = Now . Instant . toNanos <$> liftIO dateCurrent

getCpuTime :: MonadIO m => m CPUTime
getCpuTime = do 
    picos <- liftIO getCPUTime
    pure $! CPUTime $ picos `div` 1000_000_000

durationMs :: Instant -> Instant -> TimeMs
durationMs (Instant begin) (Instant end) = 
    fromIntegral $ (end - begin) `div` nanosPerMicrosecond 
    
timed :: MonadIO m => m a -> m (a, Int64)
timed action = do 
    Now (Instant begin) <- thisInstant
    !z <- action
    Now (Instant end) <- thisInstant    
    pure (z, end - begin)

timedMS :: MonadIO m => m a -> m (a, TimeMs)
timedMS action = do 
    (!z, ns) <- timed action   
    pure (z, fromIntegral $ ns `div` nanosPerMicrosecond)

nanosPerSecond :: Num p => p
nanosPerSecond = 1000_000_000
{-# INLINE nanosPerSecond #-}

nanosPerMicrosecond :: Num p => p
nanosPerMicrosecond = 1000_000
{-# INLINE nanosPerMicrosecond #-}

toNanoseconds :: Instant -> Int64
toNanoseconds (Instant instant) = instant

toNanos :: DateTime -> Int64
toNanos d = nanosPerSecond * seconds + nanos
  where 
    ElapsedP (Elapsed (Seconds seconds)) (NanoSeconds nanos) = timeGetElapsedP d

asSeconds :: Instant -> Int64
asSeconds (Instant instant) = fromIntegral $ instant `div` nanosPerSecond

fromNanoseconds :: Int64 -> DateTime
fromNanoseconds totalNanos =    
    timeConvert elapsed
  where 
    elapsed = ElapsedP (Elapsed (Seconds seconds)) (NanoSeconds nanos)
    (seconds, nanos) = totalNanos `divMod` nanosPerSecond     

closeEnoughMoments :: Earlier -> Later -> Seconds -> Bool
closeEnoughMoments earlierInstant laterInstant intervalSeconds = 
    instantDiff earlierInstant laterInstant < intervalSeconds

newtype Earlier = Earlier Instant
newtype Later = Later Instant

instantDiff :: Earlier -> Later -> Seconds
instantDiff (Earlier (Instant earlierInstant)) (Later (Instant laterInstant)) = 
    Seconds $ (laterInstant - earlierInstant) `div` nanosPerSecond

momentAfter :: Instant -> Seconds -> Instant
momentAfter (Instant moment) (Seconds seconds) = 
    Instant $ moment + seconds * nanosPerSecond

instantDateFormat :: Instant -> String
instantDateFormat (Instant d) = timePrint format (fromNanoseconds d)
  where 
    format = TimeFormatString [
            Format_Year, dash, Format_Month2, dash, Format_Day2,
            Format_Text ' ',
            Format_Hour, colon, Format_Minute, colon, Format_Second,
            Format_TimezoneName
        ]
    dash = Format_Text '-'
    colon = Format_Text ':'   

instantTimeFormat :: Instant -> String
instantTimeFormat (Instant d) = timePrint format (fromNanoseconds d)
  where 
    format = TimeFormatString [            
            Format_Hour, colon, Format_Minute, colon, Format_Second,
            Format_TimezoneName
        ]
    colon = Format_Text ':'   

toMicroseconds :: Seconds -> Int
toMicroseconds (Seconds s) = fromIntegral $ 1_000_000 * s

cpuTimePerSecond :: CPUTime -> Earlier -> Later -> Double
cpuTimePerSecond (CPUTime t) earlier later = let
    Seconds duration = instantDiff earlier later
    in (fromInteger t :: Double) / (fromIntegral duration :: Double)

asCpuTime :: Seconds -> CPUTime 
asCpuTime (Seconds s) = CPUTime $ fromIntegral $ s * 1000

isoFormat :: Instant -> String
isoFormat (Instant t) = timePrint ISO8601_DateAndTime (fromNanoseconds t)

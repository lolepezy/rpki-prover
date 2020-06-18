{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Time where

import Data.Int
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Hourglass         (DateTime, ISO8601_DateAndTime (..), NanoSeconds (..), Seconds (..), timeDiff,
                                         timeDiffP, timePrint)
import           System.Hourglass       (dateCurrent)

import           RPKI.Serialise.Orphans

import GHC.Generics (Generic)
import Codec.Serialise (Serialise)


newtype Instant = Instant DateTime
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Serialise

instance Show Instant where
    show (Instant d) = timePrint ISO8601_DateAndTime d

-- | Current time that is to be passed into the environment of validating functions
newtype Now = Now Instant
    deriving stock (Show, Eq, Ord)

thisMoment :: MonadIO m => m Now
thisMoment = Now . Instant <$> liftIO dateCurrent


timed :: MonadIO m => m a -> m (a, Int64)
timed action = do 
    Now (Instant begin) <- thisMoment
    z <- action
    Now (Instant end) <- thisMoment
    let (Seconds s, NanoSeconds ns) = timeDiffP end begin
    pure (z, s * 1000_000_000 + ns)

timedMS :: MonadIO m => m a -> m (a, Int64)
timedMS action = do 
    (z, ns) <- timed action   
    pure (z, fromIntegral (ns `div` 1000_000))

closeEnoughMoments :: Instant -> Instant -> Seconds -> Bool
closeEnoughMoments (Instant firstMoment) (Instant secondMoment) interval = 
    timeDiff secondMoment firstMoment < interval
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Time where

import Data.Int

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Hourglass         (NanoSeconds(..), Seconds(..), DateTime, timeDiffP)
import           System.Hourglass       (dateCurrent)


-- | Current time that is to be passed into the environment of validating functions
newtype Now = Now DateTime
    deriving stock (Show, Eq, Ord)

thisMoment :: MonadIO m => m Now
thisMoment = Now <$> liftIO dateCurrent


timed :: MonadIO m => m a -> m (a, Int64)
timed action = do 
    Now begin <- thisMoment
    z <- action
    Now end <- thisMoment
    let (Seconds s, NanoSeconds ns) = timeDiffP end begin
    pure (z, s * 1000_000_000 + ns)

timedMS :: MonadIO m => m a -> m (a, Int)
timedMS action = do 
    (z, ns) <- timed action   
    pure (z, fromIntegral (ns `div` 1000_000))
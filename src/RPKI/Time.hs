{-# LANGUAGE DerivingStrategies #-}

module RPKI.Time where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Hourglass         (NanoSeconds(..), DateTime, timeDiffP)
import           System.Hourglass       (dateCurrent)


-- | Current time that is to be passed into the environment of validating functions
newtype Now = Now DateTime
    deriving stock (Show, Eq, Ord)

thisMoment :: MonadIO m => m Now
thisMoment = Now <$> liftIO dateCurrent

newtype Elapsed = Elapsed Int

timed :: MonadIO m => m a -> m (a, Int)
timed action = do 
    Now begin <- thisMoment
    z <- action
    Now end <- thisMoment
    let (_, NanoSeconds ns) = timeDiffP end begin
    pure (z, fromIntegral (ns `div` 1000))
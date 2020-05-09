{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module RPKI.Logging where

import Colog

import Control.Monad.IO.Class
import Control.Concurrent.Lifted
import Control.Concurrent.Async.Lifted

import GHC.Stack (callStack, withFrozenCallStack)

import Data.Text

class Logger logger where
    logError_ :: logger -> Text -> IO ()
    logWarn_  :: logger -> Text -> IO ()
    logInfo_  :: logger -> Text -> IO ()
    logDebug_ :: logger -> Text -> IO ()


data AppLogger = AppLogger !(LogAction IO Message) !(MVar Bool)

instance Logger AppLogger where
    logError_ (AppLogger la lock) = logWhat E la lock
    logWarn_  (AppLogger la lock) = logWhat W la lock
    logInfo_  (AppLogger la lock) = logWhat I la lock
    logDebug_ (AppLogger la lock) = logWhat D la lock

logWhat :: Severity -> LogAction IO Message -> MVar Bool -> Text -> IO ()
logWhat sev la lock textMessage = 
    withMVar lock $ \_ -> withFrozenCallStack $ la <& Msg sev callStack textMessage


logErrorM, logWarnM, logInfoM, logDebugM :: (Logger logger, MonadIO m) => 
                                            logger -> Text -> m ()
logErrorM logger t = liftIO $ logError_ logger t
logWarnM logger t  = liftIO $ logWarn_ logger t
logInfoM logger t  = liftIO $ logInfo_ logger t
logDebugM logger t = liftIO $ logDebug_ logger t

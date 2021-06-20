{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings         #-}

module RPKI.Logging where

import Colog

import Data.Text (Text)

import Control.Monad.IO.Class

import GHC.Stack (callStack)
import System.IO (BufferMode (..), hSetBuffering, stdout)

class Logger logger where
    logError_ :: logger -> Text -> IO ()
    logWarn_  :: logger -> Text -> IO ()
    logInfo_  :: logger -> Text -> IO ()
    logDebug_ :: logger -> Text -> IO ()


newtype AppLogger = AppLogger (LogAction IO Message)

instance Logger AppLogger where
    logError_ (AppLogger la) = logWhat E la
    logWarn_  (AppLogger la) = logWhat W la
    logInfo_  (AppLogger la) = logWhat I la
    logDebug_ (AppLogger la) = logWhat D la

logWhat :: Severity -> LogAction IO Message -> Text -> IO ()
logWhat sev la textMessage = la <& Msg sev callStack textMessage    

logErrorM, logWarnM, logInfoM, logDebugM :: (Logger logger, MonadIO m) => 
                                            logger -> Text -> m ()
logErrorM logger t = liftIO $ logError_ logger t
logWarnM logger t  = liftIO $ logWarn_ logger t
logInfoM logger t  = liftIO $ logInfo_ logger t
logDebugM logger t = liftIO $ logDebug_ logger t


withAppLogger :: (AppLogger -> LoggerT Text IO a) -> IO a
withAppLogger f = do     
    hSetBuffering stdout LineBuffering
    withBackgroundLogger
        defCapacity
        logTextStdout
        (\logg -> usingLoggerT logg $ f $ AppLogger $ fullMessageAction logg)
  where
    fullMessageAction logg = upgradeMessageAction defaultFieldMap $ 
        cmapM (\msg -> fmtRichMessageCustomDefault msg formatRichMessage) logg    
        
    formatRichMessage _ (maybe "" showTime -> time) Msg{..} =
        showSeverity msgSeverity
        <> time            
        <> msgText        
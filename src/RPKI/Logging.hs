{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings         #-}

module RPKI.Logging where

import Colog

import Data.Text (Text)

import Control.Monad (when)
import Control.Monad.IO.Class

import GHC.Stack (callStack)
import System.IO (BufferMode (..), hSetBuffering, stdout)


class Logger logger where
    logError_ :: logger -> Text -> IO ()
    logWarn_  :: logger -> Text -> IO ()
    logInfo_  :: logger -> Text -> IO ()
    logDebug_ :: logger -> Text -> IO ()

data LogLevel = ErrorL | WarnL | InfoL | DebugL
    deriving (Show, Eq, Ord)

data AppLogger = AppLogger {
        logLevel :: LogLevel,
        logAction :: LogAction IO Message
    }


instance Logger AppLogger where
    logError_ AppLogger {..} = logWhat E logAction

    logWarn_  AppLogger {..} s = 
        when (logLevel >= WarnL) $ logWhat W logAction s        

    logInfo_  AppLogger {..} s = 
        when (logLevel >= InfoL) $ logWhat I logAction s

    logDebug_ AppLogger {..} s = 
        when (logLevel >= DebugL) $ logWhat D logAction s        

logWhat :: Severity -> LogAction IO Message -> Text -> IO ()
logWhat sev la textMessage = la <& Msg sev callStack textMessage    

logErrorM, logWarnM, logInfoM, logDebugM :: (Logger logger, MonadIO m) => 
                                            logger -> Text -> m ()
logErrorM logger t = liftIO $ logError_ logger t
logWarnM logger t  = liftIO $ logWarn_ logger t
logInfoM logger t  = liftIO $ logInfo_ logger t
logDebugM logger t = liftIO $ logDebug_ logger t


withAppLogger :: LogLevel -> (AppLogger -> LoggerT Text IO a) -> IO a
withAppLogger logLevel f = do     
    hSetBuffering stdout LineBuffering
    withBackgroundLogger
        defCapacity
        logTextStdout
        (\logg -> usingLoggerT logg $ f $ AppLogger logLevel (fullMessageAction logg))
  where
    fullMessageAction logg = upgradeMessageAction defaultFieldMap $ 
        cmapM (\msg -> fmtRichMessageCustomDefault msg formatRichMessage) logg    
        
    formatRichMessage _ (maybe "" showTime -> time) Msg{..} =
        showSeverity msgSeverity
        <> time            
        <> msgText        
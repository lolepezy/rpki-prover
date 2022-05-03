{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Logging where

import Codec.Serialise
import Colog

import Data.Text (Text)

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Control.Concurrent.STM.TBQueue  as Q

import GHC.Generics (Generic)

import GHC.Stack (callStack)
import System.IO (BufferMode (..), Handle, hSetBuffering, stdout, stderr)


class Logger logger where
    logError_ :: logger -> Text -> IO ()
    logWarn_  :: logger -> Text -> IO ()
    logInfo_  :: logger -> Text -> IO ()
    logDebug_ :: logger -> Text -> IO ()

data LogLevel = ErrorL | WarnL | InfoL | DebugL
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

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

defaultsLogLevel :: LogLevel
defaultsLogLevel = InfoL

logWhat :: Severity -> LogAction IO Message -> Text -> IO ()
logWhat sev la textMessage = la <& Msg sev callStack textMessage    

logErrorM, logWarnM, logInfoM, logDebugM :: (Logger logger, MonadIO m) => 
                                            logger -> Text -> m ()
logErrorM logger t = liftIO $ logError_ logger t
logWarnM logger t  = liftIO $ logWarn_ logger t
logInfoM logger t  = liftIO $ logInfo_ logger t
logDebugM logger t = liftIO $ logDebug_ logger t


withMainAppLogger :: LogLevel -> (AppLogger -> LoggerT Text IO a) -> IO a
withMainAppLogger logLevel = withLogger logLevel (stdout, logTextStdout)

withWorkerLogger :: LogLevel -> (AppLogger -> LoggerT Text IO a) -> IO a
withWorkerLogger logLevel = withLogger logLevel (stderr, logTextStderr)

withLogger :: LogLevel -> (Handle, LogAction IO Text) -> (AppLogger -> LoggerT Text IO a) -> IO a
withLogger logLevel (stream, streamLogger) f = do     
    hSetBuffering stream LineBuffering
    withBackgroundLogger
        defCapacity
        streamLogger
        (\logg -> usingLoggerT logg $ f $ AppLogger logLevel (fullMessageAction logg))
  where
    fullMessageAction logg = upgradeMessageAction defaultFieldMap $ 
        cmapM (`fmtRichMessageCustomDefault` formatRichMessage) logg    
        
    formatRichMessage _ (maybe "" showTime -> time) Msg{..} =
        showSeverity msgSeverity
        <> time            
        <> msgText           


data LogMessage1 = LogMessage1 LogLevel Text

newtype Logger1 = Logger1 (TBQueue LogMessage1)

withLogger_ f = do 
    q <- Q.newTBQueueIO 1000
    withAsync (logForever q) $ \_ -> f (Logger1 q)          
    where
        logForever q = forever $ do
            z <- atomically $ Q.readTBQueue q

            pure ()

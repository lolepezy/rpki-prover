{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.Logging where

import Codec.Serialise
import Colog

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8

import Data.Bifunctor
import Data.Traversable
import Data.Text (Text)

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Control.Concurrent.STM.TBQueue  as Q

import GHC.Generics (Generic)

import GHC.Stack (callStack)
import System.IO (BufferMode (..), Handle, hSetBuffering, stdout, stderr)

import RPKI.Domain
import RPKI.Util

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
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

newtype Logger1 = Logger1 (TBQueue LogMessage1)

withLogger_ f = do 
    q <- Q.newTBQueueIO 1000
    withAsync (logForever q) $ \_ -> f (Logger1 q)          
    where
        logForever q = forever $ do
            z <- atomically $ Q.readTBQueue q
            -- write z to the actual file/stdout
            pure ()



{-

main process


logWithLevel level t = do 
    putStrLn $ logFormat level t    

worker 
 - readChildLog = forever $ readBS >>= logRaw

 - logWithLevel level t = do 
     logRaw $ toBase64 $ serialise $ Msg level t     
     logRaw EOL

-}

eol :: Char
eol = '\n' 

readChildLog readBS = go mempty
  where
    go accum = do 
        bs <- readBS
        let (complete, leftover) = gatherMsgs accum bs
        -- for_ complete $ logMessage . deserialise                
        go leftover        

gatherMsgs :: BB.Builder -> BS.ByteString -> ([BS.ByteString], BB.Builder)
gatherMsgs accum bs = 
    case C8.split eol bs of
        []      -> ([], accum)
        [chunk] -> ([], accum <> BB.byteString chunk)
        chunk : chunks -> let
                z = LBS.toStrict $ BB.toLazyByteString $ accum <> BB.byteString chunk
                (inits, last) = splitLast chunks
            in (z : inits, BB.byteString last)
  where
    splitLast [] = error "Will never happen"
    splitLast [a] = ([], a)
    splitLast (a: as) = let 
        (inits, last) = splitLast as
        in (a : inits, last)


msgToBs :: LogMessage1 -> BS.ByteString
msgToBs msg = let 
    s = serialise msg
    EncodedBase64 bs = encodeBase64 (DecodedBase64 $ LBS.toStrict s)
    in bs

bsToMsg :: BS.ByteString -> Either Text LogMessage1
bsToMsg bs = 
    case decodeBase64 (EncodedBase64 bs) "Broken base64 input" of 
        Left e -> Left $ fmtGen e
        Right (DecodedBase64 decoded) -> 
            first fmtGen $ deserialiseOrFail $ LBS.fromStrict decoded    
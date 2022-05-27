{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}

module RPKI.Logging where

import Codec.Serialise
-- import Colog

import           Conduit

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8

import Data.Bifunctor
import Data.Foldable
import Data.Traversable
import Data.Text (Text)

import Data.String.Interpolate.IsString

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
import RPKI.Time


data LogLevel = ErrorL | WarnL | InfoL | DebugL
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

-- class Logger logger where
--     logError_ :: logger -> Text -> IO ()
--     logWarn_  :: logger -> Text -> IO ()
--     logInfo_  :: logger -> Text -> IO ()
--     logDebug_ :: logger -> Text -> IO ()


-- data AppLogger = AppLogger {
--         logLevel :: LogLevel,
--         logAction :: LogAction IO Message
--     }


-- instance Logger AppLogger where
--     logError_ AppLogger {..} = logWhat E logAction

--     logWarn_  AppLogger {..} s = 
--         when (logLevel >= WarnL) $ logWhat W logAction s        

--     logInfo_  AppLogger {..} s = 
--         when (logLevel >= InfoL) $ logWhat I logAction s

--     logDebug_ AppLogger {..} s = 
--         when (logLevel >= DebugL) $ logWhat D logAction s        

defaultsLogLevel :: LogLevel
defaultsLogLevel = InfoL

-- logWhat :: Severity -> LogAction IO Message -> Text -> IO ()
-- logWhat sev la textMessage = la <& Msg sev callStack textMessage    

-- logErrorM, logWarnM, logInfoM, logDebugM :: (Logger logger, MonadIO m) => 
--                                             logger -> Text -> m ()
-- logErrorM logger t = liftIO $ logError_ logger t
-- logWarnM logger t  = liftIO $ logWarn_ logger t
-- logInfoM logger t  = liftIO $ logInfo_ logger t
-- logDebugM logger t = liftIO $ logDebug_ logger t



-- withMainAppLogger :: LogLevel -> (AppLogger -> LoggerT Text IO a) -> IO a
-- withMainAppLogger logLevel = withLogger logLevel (stdout, logTextStdout)

-- withWorkerLogger :: LogLevel -> (AppLogger -> LoggerT Text IO a) -> IO a
-- withWorkerLogger logLevel = withLogger logLevel (stderr, logTextStderr)

-- withLogger :: LogLevel -> (Handle, LogAction IO Text) -> (AppLogger -> LoggerT Text IO a) -> IO a
-- withLogger logLevel (stream, streamLogger) f = do     
--     hSetBuffering stream LineBuffering
--     withBackgroundLogger
--         defCapacity
--         streamLogger
--         (\logg -> usingLoggerT logg $ f $ AppLogger logLevel (fullMessageAction logg))
--   where
--     fullMessageAction logg = upgradeMessageAction defaultFieldMap $ 
--         cmapM (`fmtRichMessageCustomDefault` formatRichMessage) logg    
        
--     formatRichMessage _ (maybe "" showTime -> time) Msg{..} =
--         showSeverity msgSeverity
--         <> time            
--         <> msgText           


data AppLogger = AppLogger {
        logLevel :: LogLevel,
        actualLogger :: ProcessLogger
    }

logError_, logWarn_, logInfo_, logDebug_ :: AppLogger -> Text -> IO ()

logError_ AppLogger {..} s = logWLevel actualLogger $ LogMessage ErrorL s

logWarn_ = logIfAboveLevel WarnL
logInfo_ = logIfAboveLevel InfoL
logDebug_ = logIfAboveLevel DebugL

logIfAboveLevel :: LogLevel -> AppLogger -> Text -> IO ()
logIfAboveLevel level AppLogger {..} s = 
    when (logLevel >= level) $ 
        logWLevel actualLogger $ LogMessage level s


logErrorM, logWarnM, logInfoM, logDebugM :: MonadIO m => AppLogger -> Text -> m ()
logErrorM logger t = liftIO $ logError_ logger t
logWarnM logger t  = liftIO $ logWarn_ logger t
logInfoM logger t  = liftIO $ logInfo_ logger t
logDebugM logger t = liftIO $ logDebug_ logger t

data LogMessage = LogMessage LogLevel Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data QElem = BinQE BS.ByteString | MsgQE LogMessage
    deriving stock (Eq, Ord, Generic)

data ProcessLogger = MainLogger (TBQueue QElem) 
                   | WorkerLogger (TBQueue QElem) 


logWLevel :: ProcessLogger -> LogMessage -> IO ()
logWLevel logger msg = 
    atomically $ Q.writeTBQueue (getQueue logger) $ MsgQE msg  

logBytes :: ProcessLogger -> BS.ByteString -> IO ()
logBytes logger bytes = 
    atomically $ Q.writeTBQueue (getQueue logger) $ BinQE bytes             


forQueue :: ProcessLogger -> (TBQueue QElem -> p) -> (TBQueue QElem -> p) -> p
forQueue logger f g = 
    case logger of 
        MainLogger q -> f q
        WorkerLogger q -> g q

getQueue :: ProcessLogger -> TBQueue QElem
getQueue logger = forQueue logger id id

withLogger :: (TBQueue a -> ProcessLogger) -> LogLevel -> (AppLogger -> IO b) -> IO b
withLogger mkLogger logLevel f = do 
    q <- Q.newTBQueueIO 1000
    let logger = mkLogger q    
    let appLogger = AppLogger logLevel logger

    -- Main process writes to stdout, workers -- to stderr
    -- TODO Think about it more, maybe there's more consistent option
    let logStream = forQueue logger (const stdout) (const stderr)
    hSetBuffering logStream LineBuffering    
    
    runWithLog (BS.hPut logStream) logger (\_ -> f appLogger)

  where
    loop g q = forever $ g =<< atomically (Q.readTBQueue q)

    runWithLog logRaw logger f =         
        withAsync (forQueue logger loopMain loopWorker) f
      where                
        loopMain = loop $ \case 
            BinQE b -> do 
                case bsToMsg b of 
                    Left e ->                                     
                        mainLogWithLevel ErrorL [i|Problem deserialising binary log message: #{b}.|]
                    Right (LogMessage level text) -> 
                        mainLogWithLevel level text
            MsgQE (LogMessage level text) -> 
                mainLogWithLevel level text

        loopWorker = loop $ \qe -> do 
            logRaw $ case qe of 
                        BinQE b   -> b
                        MsgQE msg -> msgToBs msg
            logRaw $ C8.singleton eol                      
        

        mainLogWithLevel level message = do 
            t <- thisInstant
            logRaw $ logFormat t level message

    -- TODO Make it appropriate
    logFormat t level message = [i|#{level}\t#{t}\t#{message}|]

eol :: Char
eol = '\n' 
     

sinkLog :: MonadIO m => AppLogger -> ConduitT C8.ByteString o m ()
sinkLog AppLogger {..} = go mempty        
  where
    go accum = do 
        z <- await
        for_ z $ \bs -> do 
            let (complete, leftover) = gatherMsgs accum bs            
            for_ complete $ \b -> 
                liftIO $ logBytes actualLogger b
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


msgToBs :: LogMessage -> BS.ByteString
msgToBs msg = let 
    s = serialise msg
    EncodedBase64 bs = encodeBase64 $ DecodedBase64 $ LBS.toStrict s
    in bs

bsToMsg :: BS.ByteString -> Either Text LogMessage
bsToMsg bs = 
    case decodeBase64 (EncodedBase64 bs) "Broken base64 input" of 
        Left e -> Left $ fmtGen e
        Right (DecodedBase64 decoded) -> 
            first fmtGen $ deserialiseOrFail $ LBS.fromStrict decoded    


{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StrictData         #-}

module RPKI.Logging where

import           Conduit
import           Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8

import Data.Bifunctor
import Data.Foldable
import Data.Text (Text, justifyLeft)

import Data.Hourglass
import Data.String.Interpolate.IsString

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM

import GHC.Generics (Generic)

import System.Posix.Types
import System.Posix.Process
import System.IO

import RPKI.AppTypes
import RPKI.Domain
import RPKI.Util
import RPKI.Time
import RPKI.Parallel
import RPKI.Metrics.System

import RPKI.Store.Base.Serialisation

{- 
Every process, the main one or a worker, has it's own queue of messages.

These messages are 
 - sent to the parent process if current process is a worker 
 - interpreted, e.g. printed to the stdout/file/whatnot if current process is the main one

At the moment there are 3 kinds of messages, logging messages, RTR logging messages 
and system metrics upadates.

In order to sent a message to the parent process the following happens
 - messsage gets serialised to a bytestring
 - this bytestring get converted to base64
 - it is printed to stderr (at the moment) with '\n' after it.
 That's the way the parent knows where one message ends and the other one begins.
 It looks cumbersome but it is actually quiet simple and reliable.

If the parent accepting a message is not the main one, it just passes the bytes
without any interpretation further to it's parent until the main one is reached.
-}

data LogLevel = ErrorL | WarnL | InfoL | DebugL
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary)
   
instance Show LogLevel where
    show = \case 
        ErrorL -> "Error"
        WarnL  -> "Warn"
        InfoL  -> "Info"
        DebugL -> "Debug"

data AppLogger = AppLogger {
        commonLogger :: CommonLogger,
        rtrLogger    :: RtrLogger        
    }


data WorkerInfo = WorkerInfo {
        workerPid  :: CPid,
        endOfLife  :: Instant,
        cli        :: Text,
        workerKind :: WorkerKind
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data WorkerKind = RsyncWorker
                | GenericWorker Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data WorkerMessage = AddWorker WorkerInfo
                   | RemoveWorker CPid
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)                    

newtype SystemStatusMessage = SystemStatusMessage SystemState
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)                    

-- Messages in the queue 
data BusMessage = LogM LogMessage 
                | RtrLogM LogMessage 
                | SystemMetricsM SystemMetrics
                | WorkerM WorkerMessage
                | SystemStatusM SystemStatusMessage
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data LogMessage = LogMessage { 
        logLevel  :: LogLevel,
        message   :: Text,
        processId :: ProcessID,
        timestamp :: Instant
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data QElem = BinQE BS.ByteString | MsgQE BusMessage
    deriving stock (Show, Eq, Ord, Generic)

class Logger logger where  
    logMessage_ :: MonadIO m => logger -> LogMessage -> m ()
    logLevel_   :: logger -> LogLevel

newtype CommonLogger = CommonLogger ALogger
newtype RtrLogger    = RtrLogger ALogger

data ALogger = ALogger {
        queue    :: ClosableQueue QElem,
        logLevel :: LogLevel
    }

instance Logger CommonLogger where 
    logMessage_ (CommonLogger ALogger {..}) message =
        liftIO $ atomically $ writeCQueue queue $ MsgQE $ LogM message  

    logLevel_ (CommonLogger ALogger {..}) = logLevel

instance Logger RtrLogger where 
    logMessage_ (RtrLogger ALogger {..}) message = 
        liftIO $ atomically $ writeCQueue queue $ MsgQE $ RtrLogM message  

    logLevel_ (RtrLogger ALogger {..}) = logLevel

instance Logger AppLogger where 
    logMessage_ AppLogger {..} = logMessage_ commonLogger
    logLevel_ AppLogger {..}   = logLevel_ commonLogger

data LogConfig = LogConfig {
        logLevel            :: LogLevel,
        logType             :: LogType,
        metricsHandler      :: SystemMetrics -> IO (), -- ^ what to do with incoming system metrics messages
        workerHandler       :: WorkerMessage -> IO (), -- ^ what to do with incoming worker messages
        systemStatusHadnler :: SystemStatusMessage -> IO () -- ^ what to do with incoming system status messages
    }
    deriving stock (Generic)

data LogType = WorkerLog | MainLog | MainLogWithRtr String
    deriving stock (Eq, Ord, Show, Generic)

newLogConfig :: LogLevel -> LogType -> LogConfig
newLogConfig logLevel logType = let 
    metricsHandler = const $ pure ()
    workerHandler = const $ pure ()
    systemStatusHadnler = const $ pure ()
    in LogConfig {..}

newWorkerInfo :: MonadIO m => WorkerKind -> Seconds -> Text -> m WorkerInfo
newWorkerInfo workerKind timeout cli = do 
    Now now <- thisInstant
    let endOfLife = momentAfter now timeout
    pure $ WorkerInfo { workerPid = 0, .. }

logError, logWarn, logInfo, logDebug :: (Logger log, MonadIO m) => log -> Text -> m ()
logError logger t = liftIO $ logMessage_ logger =<< createLogMessage ErrorL t
logWarn  = logIfAboveLevel WarnL
logInfo  = logIfAboveLevel InfoL
logDebug = logIfAboveLevel DebugL


logIfAboveLevel :: (Logger log, MonadIO m) => 
                    LogLevel -> log -> Text -> m ()
logIfAboveLevel messageLogLevel logger t = liftIO $ 
    when (logLevel_ logger >= messageLogLevel) $ 
        logMessage_ logger =<< createLogMessage messageLogLevel t


createLogMessage :: LogLevel -> Text -> IO LogMessage
createLogMessage logLevel message = do 
    Now timestamp <- thisInstant
    processId <- getProcessID
    pure LogMessage {..}

pushSystem :: MonadIO m => AppLogger -> SystemMetrics -> m ()
pushSystem logger sm = 
    liftIO $ atomically $ writeCQueue (getQueue logger) $ MsgQE $ SystemMetricsM sm  

registerWorker :: MonadIO m => AppLogger -> WorkerInfo -> m ()
registerWorker logger wi = 
    liftIO $ atomically $ writeCQueue (getQueue logger) $ MsgQE $ WorkerM $ AddWorker wi

deregisterhWorker :: MonadIO m => AppLogger -> CPid -> m ()
deregisterhWorker logger pid = 
    liftIO $ atomically $ writeCQueue (getQueue logger) $ MsgQE $ WorkerM $ RemoveWorker pid

logBytes :: AppLogger -> BS.ByteString -> IO ()
logBytes logger bytes = 
    atomically $ writeCQueue (getQueue logger) $ BinQE bytes             

getQueue :: AppLogger -> ClosableQueue QElem
getQueue AppLogger { commonLogger = CommonLogger ALogger {..} } = queue


-- The main entry-point, done in CPS style.
withLogger :: LogConfig 
            -> (AppLogger -> IO b)      -- ^ what to do with the configured and initialised logger
            -> IO b
withLogger LogConfig {..} f = do 
    -- this one is the process' log message queue
    messageQueue <- newCQueueIO 1000    

    -- Main process writes to stdout, workers -- to stderr
    -- TODO Think about it more, maybe there's more consistent option
    (commonLogStream, rtrLogStream) <-
            case logType of
                WorkerLog -> pure (stderr, stderr)
                MainLog   -> pure (stdout, stdout)
                MainLogWithRtr rtrLog -> 
                    (stdout, ) <$> openFile rtrLog WriteMode    
     
    -- TODO Figure out why removing it leads to the whole process getting stuck
    hSetBuffering commonLogStream LineBuffering    
    hSetBuffering rtrLogStream LineBuffering    
    
    let logToStream stream t = 
            mapM_ (BS.hPut stream) [t, C8.singleton eol]

    let logRaw = logToStream commonLogStream
    let logRtr = logToStream rtrLogStream    

    -- Process queue messages in the main process, i.e. 
    -- output them to stdout or a separate RTR log
    let processMessageInMainProcess = \case
            LogM logMessage          -> logRaw $ messageToText logMessage
            RtrLogM logMessage       -> logRtr $ messageToText logMessage
            WorkerM workerInfo       -> workerHandler workerInfo
            SystemMetricsM sysMetric -> metricsHandler sysMetric
            SystemStatusM sysStatus  -> systemStatusHadnler sysStatus
            
    
    let loopMain = loopReadQueue messageQueue $ \case 
            BinQE b -> 
                case bsToMsg b of 
                    Left e ->                                     
                        logRaw . messageToText =<< 
                            createLogMessage ErrorL [i|Problem deserialising binary log message: [#{b}], error: #{e}.|]
                    Right z -> 
                        processMessageInMainProcess z                        
                                                    
            MsgQE z -> processMessageInMainProcess z

    -- Worker simply resends all the binary messages 
    -- (received from children processes) to its parent. 
    -- Messages from this process are serialised and then sent
    let loopWorker = loopReadQueue messageQueue $ logRaw . \case 
                                BinQE b   -> b
                                MsgQE msg -> msgToBs msg        
    let actualLoop = 
            case logType of
                WorkerLog -> loopWorker
                _         -> loopMain
                
    let appLogger = AppLogger {
            commonLogger = CommonLogger $ ALogger messageQueue logLevel,
            rtrLogger    = RtrLogger $ ALogger messageQueue logLevel
        }

    snd <$> concurrently 
                (finallyCloseQ messageQueue actualLoop)                     
                (finallyCloseQ messageQueue $ f appLogger)

  where
    loopReadQueue queue g = do 
        z <- atomically $ readCQueue queue        
        for_ z $ \message -> g message >> loopReadQueue queue g   

    finallyCloseQ queue g = 
        g `finally` atomically (closeCQueue queue)

    messageToText LogMessage { logLevel = logLevel', .. } = let
            level = justifyLeft 6 ' ' [i|#{logLevel'}|]
            pid   = justifyLeft 16 ' ' [i|[pid #{processId}]|]     
        in [i|#{level}  #{pid}  #{timestamp}  #{message}|] 


drainLog :: MonadIO m => AppLogger -> m ()
drainLog (getQueue -> queue) =     
    liftIO $ atomically $ do 
        empty <- isEmptyCQueue queue
        unless empty retry    

eol :: Char
eol = '\n' 

-- Read input stream and extract serialised log messages from it.
-- Messages are separated by the EOL character.
sinkLog :: MonadIO m => AppLogger -> ConduitT C8.ByteString o m ()
sinkLog logger = go mempty        
  where
    go accum = do 
        z <- await
        for_ z $ \bs -> do 
            let (complete, leftover') = gatherMsgs accum bs            
            for_ complete $ liftIO . logBytes logger
            go leftover'

gatherMsgs :: BB.Builder -> BS.ByteString -> ([BS.ByteString], BB.Builder)
gatherMsgs accum bs = 
    (reverse $ map LBS.toStrict $ filter (not . LBS.null) fullChunks, accum')
  where  
    (accum', fullChunks) = C8.foldl' splitByEol (accum, []) bs      
    splitByEol (acc, chunks) c 
        | c == eol  = (mempty, BB.toLazyByteString acc : chunks)
        | otherwise = (acc <> BB.char8 c, chunks)

msgToBs :: BusMessage -> BS.ByteString
msgToBs msg = let     
    EncodedBase64 bs = encodeBase64 $ DecodedBase64 $ serialise_ msg
    in bs

bsToMsg :: BS.ByteString -> Either Text BusMessage
bsToMsg bs = 
    case decodeBase64 (EncodedBase64 bs) ("Broken base64 input" :: Text) of 
        Left e -> Left $ fmtGen e
        Right (DecodedBase64 decoded) -> 
            first fmtGen $ deserialiseOrFail_ decoded    


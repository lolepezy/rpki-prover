{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}

module RPKI.Logging where

import Codec.Serialise

import           Conduit
import           Control.Exception

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8

import Data.Bifunctor
import Data.Foldable
import Data.Text (Text, justifyLeft)

import Data.String.Interpolate.IsString

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM

import GHC.Generics (Generic)

import System.Posix.Types
import System.Posix.Process
import System.IO (BufferMode (..), hSetBuffering, stdout, stderr)

import RPKI.Domain
import RPKI.Util
import RPKI.Time
import RPKI.Parallel


data LogLevel = ErrorL | WarnL | InfoL | DebugL
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (Serialise)
   
instance Show LogLevel where
    show = \case 
        ErrorL -> "Error"
        WarnL  -> "Warn"
        InfoL  -> "Info"
        DebugL -> "Debug"

defaultsLogLevel :: LogLevel
defaultsLogLevel = InfoL

data AppLogger = AppLogger {
        logLevel :: LogLevel,
        actualLogger :: ProcessLogger
    }

logError_, logWarn_, logInfo_, logDebug_ :: AppLogger -> Text -> IO ()

logError_ AppLogger {..} s = logWLevel actualLogger =<< mkLogMessage ErrorL s
logWarn_ = logIfAboveLevel WarnL
logInfo_ = logIfAboveLevel InfoL
logDebug_ = logIfAboveLevel DebugL

logIfAboveLevel :: LogLevel -> AppLogger -> Text -> IO ()
logIfAboveLevel level AppLogger {..} s = 
    when (logLevel >= level) $ 
        logWLevel actualLogger =<< mkLogMessage level s        


logErrorM, logWarnM, logInfoM, logDebugM :: MonadIO m => AppLogger -> Text -> m ()
logErrorM logger t = liftIO $ logError_ logger t
logWarnM logger t  = liftIO $ logWarn_ logger t
logInfoM logger t  = liftIO $ logInfo_ logger t
logDebugM logger t = liftIO $ logDebug_ logger t

mkLogMessage :: LogLevel -> Text -> IO LogMessage
mkLogMessage logLevel message = do 
    Now timestamp <- thisInstant
    processId <- getProcessID
    pure LogMessage {..}

data LogMessage = LogMessage { 
        logLevel  :: LogLevel,
        message   :: Text,
        processId :: ProcessID,
        timestamp :: Instant
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data QElem = BinQE BS.ByteString | MsgQE LogMessage
    deriving stock (Show, Eq, Ord, Generic)

data ProcessLogger = MainLogger (ClosableQueue QElem) 
                   | WorkerLogger (ClosableQueue QElem) 


logWLevel :: ProcessLogger -> LogMessage -> IO ()
logWLevel logger msg = 
    atomically $ writeCQueue (getQueue logger) $ MsgQE msg  

logBytes :: ProcessLogger -> BS.ByteString -> IO ()
logBytes logger bytes = 
    atomically $ writeCQueue (getQueue logger) $ BinQE bytes             


forLogger :: ProcessLogger -> (ClosableQueue QElem -> p) -> (ClosableQueue QElem -> p) -> p
forLogger logger f g = 
    case logger of 
        MainLogger q -> f q
        WorkerLogger q -> g q

forLogger1 :: ProcessLogger -> (ClosableQueue QElem -> p) -> p
forLogger1 logger f = forLogger logger f f 

getQueue :: ProcessLogger -> ClosableQueue QElem
getQueue logger = forLogger1 logger id

withLogger :: (ClosableQueue a -> ProcessLogger) -> LogLevel -> (AppLogger -> IO b) -> IO b
withLogger mkLogger maxLogLevel f = do 
    q <- newCQueueIO 1000
    let logger = mkLogger q    
    let appLogger = AppLogger maxLogLevel logger

    -- Main process writes to stdout, workers -- to stderr
    -- TODO Think about it more, maybe there's more consistent option
    let logStream = forLogger logger (const stdout) (const stderr)

    -- TODO Figure out why removing it leads to the whole process getting stuck
    hSetBuffering logStream LineBuffering    
    
    let logRaw s = do 
            BS.hPut logStream s
            BS.hPut logStream $ C8.singleton eol        

    runWithLog logRaw logger (f appLogger)

  where

    finallyCloseQ logger ff = 
            ff `finally` forLogger1 logger (atomically . closeCQueue)

    runWithLog logRaw logger g = do        
        snd <$> concurrently 
                    (finallyCloseQ logger $ forLogger logger loopMain loopWorker)                     
                    (finallyCloseQ logger g)
      where                

        loopMain = loop $ \case 
            BinQE b -> do 
                case bsToMsg b of 
                    Left e ->                                     
                        mainLogWithLevel =<< mkLogMessage ErrorL [i|Problem deserialising binary log message: [#{b}], error: #{e}.|]
                    Right logMessage -> 
                        mainLogWithLevel logMessage
            MsgQE logMessage -> 
                mainLogWithLevel logMessage

        loopWorker = loop $ logRaw . \case 
                                BinQE b   -> b
                                MsgQE msg -> msgToBs msg

        loop g queue = do 
                z <- atomically $ readCQueue queue        
                for_ z $ \m -> g m >> loop g queue            

        mainLogWithLevel LogMessage {..} = do
            let level = justifyLeft 6 ' ' [i|#{logLevel}|]
            let pid   = justifyLeft 16 ' ' [i|[pid #{processId}]|]     
            logRaw [i|#{level}  #{pid}  #{timestamp}  #{message}|]      
    
eol :: Char
eol = '\n' 

sinkLog :: MonadIO m => AppLogger -> ConduitT C8.ByteString o m ()
sinkLog AppLogger {..} = go mempty        
  where
    go accum = do 
        z <- await
        for_ z $ \bs -> do 
            let (complete, leftover') = gatherMsgs accum bs            
            for_ complete $ liftIO . logBytes actualLogger
            go leftover'

gatherMsgs :: BB.Builder -> BS.ByteString -> ([BS.ByteString], BB.Builder)
gatherMsgs accum bs = 
    (reverse $ map LBS.toStrict $ filter (not . LBS.null) fullChunks, accum')
  where  
    (accum', fullChunks) = C8.foldl' splitByEol (accum, []) bs      
    splitByEol (acc, chunks) c 
        | c == eol  = (mempty, BB.toLazyByteString acc : chunks)
        | otherwise = (acc <> BB.char8 c, chunks)


msgToBs :: LogMessage -> BS.ByteString
msgToBs msg = let 
    s = serialise msg
    EncodedBase64 bs = encodeBase64 $ DecodedBase64 $ LBS.toStrict s
    in bs

bsToMsg :: BS.ByteString -> Either Text LogMessage
bsToMsg bs = 
    case decodeBase64 (EncodedBase64 bs) ("Broken base64 input" :: Text) of 
        Left e -> Left $ fmtGen e
        Right (DecodedBase64 decoded) -> 
            first fmtGen $ deserialiseOrFail $ LBS.fromStrict decoded    


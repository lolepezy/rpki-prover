{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE ImpredicativeTypes #-}

module RPKI.Logging2 where

import           Conduit
import           Control.Exception
import           Control.Lens

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
import System.Process.Typed
import System.IO

import RPKI.Domain
import RPKI.Util
import RPKI.Time
import RPKI.Parallel
import RPKI.Metrics.System

import RPKI.Store.Base.Serialisation
import RPKI.Logging.Types
import RPKI.IPC.Types
import RPKI.IPC.Ipc
import Control.Lens (At(at))

{- 
Every process, the main one or a worker, has it's own queue of messages.

These messages are 
 - sent to the main process if current process is a worker 
 - interpreted, e.g. printed to the stdout/file/whatnot if current process is the main one
-}



createLogMessage :: LogLevel -> Text -> IO LogMessage
createLogMessage logLevel message = do 
    Now timestamp <- thisInstant
    processId <- getProcessID
    pure LogMessage {..}

pushSystem :: MonadIO m => AppLogger -> SystemMetrics -> m ()
pushSystem logger sm = 
    liftIO $ atomically $ writeCQueue (getQueue logger) $ MsgQE $ SystemM sm  

registerhWorker :: MonadIO m => AppLogger -> WorkerInfo -> m ()
registerhWorker logger wi = 
    liftIO $ atomically $ writeCQueue (getQueue logger) $ MsgQE $ WorkerM $ AddWorker wi

deregisterhWorker :: MonadIO m => AppLogger -> Pid -> m ()
deregisterhWorker logger pid = 
    liftIO $ atomically $ writeCQueue (getQueue logger) $ MsgQE $ WorkerM $ RemoveWorker pid

logBytes :: AppLogger -> BS.ByteString -> IO ()
logBytes logger bytes = 
    atomically $ writeCQueue (getQueue logger) $ BinQE bytes             

getQueue :: AppLogger -> ClosableQueue QElem
getQueue AppLogger { defaultLogger = CommonLogger ALogger {..} } = queue


data ALogConsumer where 
    ALogConsumer :: forall a . LogConsumer a => a -> ALogConsumer

newtype CommonLogger2 = CommonLogger2 Logger2
newtype RtrLogger2    = RtrLogger2 Logger2

data Logger2 = Logger2 {
        queue    :: ClosableQueue QElem,
        logLevel :: LogLevel,
        consumer :: TVar (Maybe ALogConsumer)
    }
    deriving stock (Generic)

data AppLogger2 = AppLogger2 {
        defaultLogger :: CommonLogger2,
        rtrLogger     :: RtrLogger2        
    }

class LogConsumer consumer where 
    consumeMessage :: MonadIO m => consumer -> LogMessage -> m ()

data StdOutConsumer = StdOutConsumer 
data SocketConsumer = SocketConsumer 
newtype FileConsumer = FileConsumer Handle

instance LogConsumer StdOutConsumer where
    consumeMessage _ _ = pure ()
 
instance LogConsumer FileConsumer where
    consumeMessage _ _ = pure ()
 
instance LogConsumer SocketConsumer where
    consumeMessage _ _ = pure ()


{- 

    Worker logger:
        - one logger, all messages get into the same queue
        - the queue is consumed by the socket, using RPC server
        - the 
            atomically $ writeTVar (logger ^. #consumer) $ Just $ ALogConsumer IpcQueueConsumer
            i.e. 
            logMessage <- readCQueue (getQueue logger)
            writeToSocket $ LogM logMessage

    Main logger
        - one logger, all messages get into the same queue
        - the queue is consumed by StdOutConsumer
        - IPC server reads messages from the socket and pushes log messages 
          into the queue of the logger

    Main + RTR logger
        - two loggers, each with it's own queue
            - default queue is consumed by StdOutConsumer
            - rtr queue is consumed by FileConsumer            
        - IPC server reads messages from the socket and pushes log messages 
          into the queue of the default logger

-}


withLogger2 :: LogConfig 
        -> (AppLogger2 -> IO b)      -- ^ what to do with the configured and initialised logger
        -> IO b
withLogger2 logConfig@LogConfig {..} f = do
        
    (defaultLogger, rtrLogger) <- 
        case logType of
            WorkerLog -> do 
                logger <- makeLogger2 logConfig
                pure (logger, logger)

            MainLog -> do 
                logger <- makeLogger2 logConfig
                atomically $ writeTVar (logger ^. #consumer) $ Just $ ALogConsumer StdOutConsumer
                pure (logger, logger)

            MainLogWithRtr rtrLog -> do 
                logger <- makeLogger2 logConfig
                rtrLogger <- makeLogger2 logConfig
                file <- openFile rtrLog WriteMode   
                atomically $ do 
                    writeTVar (logger ^. #consumer) $ Just $ ALogConsumer StdOutConsumer                
                    writeTVar (rtrLogger ^. #consumer) $ Just $ ALogConsumer $ FileConsumer file
                pure (logger, rtrLogger)

    let appLogger = AppLogger2 { 
            defaultLogger = CommonLogger2 defaultLogger, 
            rtrLogger = RtrLogger2 rtrLogger 
        }

    f appLogger
    -- fst <$> concurrently
    --             (withQ messageQueue (f logger))    
    --             (withQ messageQueue actualLoop)                     
  where
    withQ queue = 
        (`finally` atomically (closeCQueue queue))
    

makeLogger2 :: LogConfig -> IO Logger2
makeLogger2 LogConfig {..} = do
    queue <- newCQueueIO 1000
    consumer <- newTVarIO Nothing
    pure Logger2 {..}
    

drainLog :: MonadIO m => AppLogger -> m ()
drainLog (getQueue -> queue) =     
    liftIO $ atomically $ do 
        empty <- isEmptyCQueue queue
        unless empty retry        
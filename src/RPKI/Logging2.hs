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

consumeLogger Logger2 {..} = do     
    -- block until the consumer is initialised
    ALogConsumer consume <- atomically $ maybe retry pure =<< readTVar consumer
    go consume
  where
    go consume = do 
        z <- atomically $ readCQueue queue        
        for_ z $ \message -> consume message >> go consume


withLogger2 :: LogConfig 
        -> (AppLogger2 -> IO b)      -- ^ what to do with the configured and initialised logger
        -> IO b
withLogger2 logConfig@LogConfig {..} f = do
        
    let newAppLogger logger1 logger2 = 
            AppLogger2 (CommonLogger2 logger1) (RtrLogger2 logger2)

    let oneLoggerCase logger = 
            fst <$> concurrently 
                (withQ (logger ^. #queue) $ f $ newAppLogger logger logger)
                (withQ (logger ^. #queue) $ consumeLogger logger)        

    case logType of

        WorkerLog -> oneLoggerCase =<< newLogger2 Nothing            
        MainLog ->   oneLoggerCase =<< newLogger2 (Just $ ALogConsumer stdoutConsumer)

        MainLogWithRtr rtrLog -> do 
            file <- openFile rtrLog WriteMode   

            logger    <- newLogger2 $ Just $ ALogConsumer stdoutConsumer            
            rtrLogger <- newLogger2 $ Just $ ALogConsumer $ fileConsumer file            

            fst <$> concurrently 
                (withQ (logger ^. #queue) $ f $ newAppLogger logger rtrLogger)
                (concurrently 
                    (withQ (logger ^. #queue) $ consumeLogger logger)
                    (withQ (logger ^. #queue) $ consumeLogger rtrLogger))                  
  where  
    newLogger2 :: Maybe ALogConsumer -> IO Logger2
    newLogger2 c = do
        -- Create a long queue, we don't want to 
        -- potetially deadlock anything through logging        
        queue <- newCQueueIO 100_000
        consumer <- newTVarIO c
        pure Logger2 {..}     

    withQ queue = 
        (`finally` atomically (closeCQueue queue))
        

drainLog :: MonadIO m => AppLogger -> m ()
drainLog (getQueue -> queue) =     
    liftIO $ atomically $ do 
        empty <- isEmptyCQueue queue
        unless empty retry        
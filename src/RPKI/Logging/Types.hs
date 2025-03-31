
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE InstanceSigs #-}

module RPKI.Logging.Types where

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
import System.Process.Typed
import System.IO

import RPKI.Domain
import RPKI.Util
import RPKI.Time
import RPKI.Parallel
import RPKI.Metrics.System

import RPKI.Store.Base.Serialisation


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
        defaultLogger :: CommonLogger,
        rtrLogger     :: RtrLogger        
    }

data WorkerInfo = WorkerInfo {
        workerPid :: Pid,
        endOfLife :: Instant,
        cli       :: Text
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data WorkerMessage = AddWorker WorkerInfo
                   | RemoveWorker Pid
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)                    

-- Messages in the queue 
data BusMessage = LogM LogMessage 
                | RtrLogM LogMessage 
                | SystemM SystemMetrics
                | WorkerM WorkerMessage
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

data SomeLogger where 
    SomeLogger :: forall a . Logger a => a -> SomeLogger     

instance Logger CommonLogger where 
    logMessage_ (CommonLogger ALogger {..}) message =
        liftIO $ atomically $ writeCQueue queue $ MsgQE $ LogM message  

    logLevel_ (CommonLogger ALogger {..}) = logLevel

instance Logger RtrLogger where 
    logMessage_ (RtrLogger ALogger {..}) message = 
        liftIO $ atomically $ writeCQueue queue $ MsgQE $ RtrLogM message  

    logLevel_ (RtrLogger ALogger {..}) = logLevel

instance Logger AppLogger where 
    logMessage_ AppLogger {..} = logMessage_ defaultLogger
    logLevel_ AppLogger {..}   = logLevel_ defaultLogger

instance Logger SomeLogger where 
    logMessage_ (SomeLogger a) = logMessage_ a
    logLevel_ (SomeLogger a)   = logLevel_ a

-- data LogConfig = LogConfig {
--         logLevel       :: LogLevel,
--         logType        :: LogType,
--         metricsHandler :: SystemMetrics -> IO (), -- ^ what to do with incoming system metrics messages
--         workerHandler  :: WorkerMessage -> IO () -- ^ what to do with incoming worker messages
--     }
--     deriving stock (Generic)

data LogConfig = LogConfig {
        logLevel       :: LogLevel,
        logType        :: LogType
    }
    deriving stock (Generic)

data LogType = WorkerLog | MainLog | MainLogWithRtr String
    deriving stock (Eq, Ord, Show, Generic)


newtype ALogConsumer = ALogConsumer { 
        consumeMessage :: forall m . MonadIO m => LogMessage -> m ()
    }
    
newtype CommonLogger2 = CommonLogger2 Logger2    
newtype RtrLogger2    = RtrLogger2 Logger2

deriving instance Logger CommonLogger2 
deriving instance Logger RtrLogger2 

data Logger2 = Logger2 {
        queue    :: ClosableQueue LogMessage,
        logLevel :: LogLevel,
        consumer :: TVar (Maybe ALogConsumer)
    }
    deriving stock (Generic)

data AppLogger2 = AppLogger2 {
        defaultLogger :: CommonLogger2,
        rtrLogger     :: RtrLogger2        
    }
    deriving stock (Generic)

fileConsumer :: (MonadIO m, TheBinary a) => Handle -> a -> m ()
fileConsumer handle message = 
    liftIO $ BS.hPut handle $ serialise_ message

stdoutConsumer :: (MonadIO m, TheBinary a) => a -> m ()
stdoutConsumer message = 
    liftIO $ BS.hPut stdout $ serialise_ message
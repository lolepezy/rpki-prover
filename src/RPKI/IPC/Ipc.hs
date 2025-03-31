
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.IPC.Ipc where

import Control.Lens
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception (bracket, try, finally, SomeException)
import Control.Monad (unless, forever, void, when)

import Data.Text

import GHC.Generics (Generic)

import qualified Data.ByteString.Char8 as BS
import Data.String.Interpolate.IsString

import System.Posix.Types

import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix.Files (removeLink, setFileMode)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import RPKI.AppTypes
import RPKI.AppContext
import RPKI.Config
import RPKI.Logging
import RPKI.Metrics.System
import RPKI.Parallel
import RPKI.Store.Base.Serialisation
import RPKI.Logging.Types
import RPKI.IPC.Types
import Data.Foldable (for_)


makeIpcMessageHandler :: AppLogger2
                    -> (CommandSpec -> IO CommandResultSpec) 
                    -> (IpcMessage -> (CommandResult -> IO ()) -> IO ())
makeIpcMessageHandler AppLogger2 {..} commandHandler message resultHandler =
    case message of
        LogIpc logMessage -> do
            logMessage_ defaultLogger logMessage                            
        RtrLogIpc logMessage -> 
            logMessage_ rtrLogger logMessage                    
        SystemIpc metrics -> do
            -- TODO Handle system metrics
            pure ()
        CommandIpc Command {..} -> do
            result <- commandHandler commandSpec
            resultHandler $ CommandResult {..}
            

runIpcServer :: Logger logger => 
                logger 
            -> (IpcMessage -> (CommandResult -> IO ()) -> IO ()) 
            -> IO ()
runIpcServer logger handleMessage = do
    socketPath <- unixSocketPath
    logDebug logger [i|Starting server on Unix socket at #{socketPath}...|]    
    socketExists <- doesFileExist socketPath
    when socketExists $ do
        logInfo logger [i|"Socket file #{socketPath} already exists, removing it.|]
        removeLink socketPath
    
    setFileMode socketPath 0o600

    bracket (openSocket socketPath) close $ \sock -> do
        logDebug logger "Server started. Waiting for connections..."
        
        forever $ do
            (clientSock, _) <- accept sock
            logDebug logger "Client connected"            
            forkIO $ handleClient clientSock `finally` close clientSock
  where
    openSocket path = do
        sock <- socket AF_UNIX Stream defaultProtocol
        bind sock (SockAddrUnix path)
        listen sock 1024
        pure sock
        
    handleClient clientSock = do        
        resultQueue <- atomically $ newCQueue 1024
        let withQueue f = f `finally` atomically (closeCQueue resultQueue)
        void $ concurrently 
            (withQueue $ processMessages clientSock resultQueue)
            (withQueue $ processResults clientSock resultQueue)
      where 
        processResults clientSock queue = do
            z <- atomically $ readCQueue queue
            for_ z $ \message -> do 
                sendAll clientSock $ serialise_ message
                processResults clientSock queue

        processMessages clientSock queue = do 
            message <- recv clientSock 4096
            if BS.null message
                then 
                    logDebug logger "Client disconnected"
                else do
                    case deserialiseOrFail_ message of
                        Left err -> logError logger [i|Error deserialising message #{message}: #{err}|]
                        Right r  -> handleMessage r (atomically . writeCQueue queue)                


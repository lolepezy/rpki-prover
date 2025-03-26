
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}

module RPKI.IPC.Ipc where

import Control.Concurrent
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
import RPKI.Logging
import RPKI.Logging.Types
import RPKI.Metrics.System
import RPKI.Store.Base.Serialisation
import RPKI.Time
import RPKI.IPC.Types


runIpc :: AppContext s -> IO ()
runIpc _ = do 
    pure ()


handleIpcMessage :: AppContext s -> IpcMessage -> IO ()
handleIpcMessage appContext@AppContext {..} = \case
    LogIpc logMessage -> 
        logMessage_ (getRtrLogger appContext) logMessage        
    RtrLogIpc logMessage -> do
        logMessage_ logger logMessage                
    SystemIpc metrics -> do
        -- TODO Handle system metrics
        pure ()


runServer :: AppContext s -> FilePath -> (IpcMessage -> IO ()) -> IO ()
runServer AppContext {..} socketPath handleMessage = do
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
        message <- recv clientSock 4096
        if BS.null message
            then 
                logDebug logger "Client disconnected"
            else do
                case deserialiseOrFail_ message of
                    Left err -> logError logger [i|Error deserialising message #{message}: #{err}|]
                    Right r -> handleMessage r                

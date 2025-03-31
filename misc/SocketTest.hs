{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Lens ((^.), (&))
import Control.Lens.Setter
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket, try, finally, SomeException)
import Control.Monad (unless, forever, void, when)

import Control.Monad.IO.Class

import Data.Bifunctor
import Data.String.Interpolate.IsString
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix.Files (removeLink)

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Options.Generic

import RPKI.AppContext
import RPKI.AppMonad
import RPKI.AppState
import RPKI.Config
import RPKI.Domain
import RPKI.Reporting
import RPKI.Logging
import RPKI.Store.AppStorage
import RPKI.Store.AppLmdbStorage
import qualified RPKI.Store.MakeLmdb as Lmdb

import RPKI.RRDP.RrdpFetch

import RPKI.Time
import RPKI.Util               
import RPKI.UniqueId
import RPKI.RRDP.Types
import RPKI.RRDP.Http

-- Default socket path
defaultSocketPath :: FilePath
defaultSocketPath = "/tmp/rpki-prover.sock" 

-- | Client implementation
runClient :: FilePath -> IO ()
runClient socketPath = do
    putStrLn [i|Connecting to Unix socket at #{socketPath}...|]
    
    bracket (openSocket socketPath) close $ \sock -> do
        putStrLn "Connected! Type messages to send (empty line to quit):"
        
        let loop = do
                putStr "> "
                hFlush stdout
                line <- BS.getLine
                
                unless (BS.null line) $ do
                    result <- try (sendAndReceive sock line) :: IO (Either SomeException BS.ByteString)
                    case result of
                        Left err -> putStrLn [i|Error: #{show err}|]
                        Right _  -> pure ()
                    loop
        
        loop
  where
    openSocket path = do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock (SockAddrUnix path)
        putStrLn [i|Connected to socket at #{path}|]
        return sock

    sendAndReceive sock message = do
        sendAll sock message
        response <- recv sock 4096
        putStrLn [i|Received: #{response}|]
        return response

-- | Server implementation
runServer :: FilePath -> IO ()
runServer socketPath = do
    putStrLn [i|Starting server on Unix socket at #{socketPath}...|]
    
    -- Remove socket file if it already exists
    socketExists <- doesFileExist socketPath
    when socketExists $ do
        putStrLn "Socket file already exists, removing it..."
        removeLink socketPath
    
    bracket (openSocket socketPath) close $ \sock -> do
        putStrLn "Server started. Waiting for connections..."
        
        forever $ do
            (clientSock, _) <- accept sock
            putStrLn "Client connected"
            
            forkIO $ handleClient clientSock `finally` close clientSock
  where
    openSocket path = do
        sock <- socket AF_UNIX Stream defaultProtocol
        bind sock (SockAddrUnix path)
        listen sock 5  -- Allow up to 5 queued connections
        return sock
        
    handleClient clientSock = do
        let processMessages = do
                message <- recv clientSock 4096
                if BS.null message
                    then putStrLn "Client disconnected"
                    else do
                        putStrLn [i|Received: #{message}|]
                        -- Echo the message back with a prefix
                        let response = BS.concat ["Server received: ", message]
                        sendAll clientSock response
                        processMessages
                        
        processMessages

data Options = Options
  { mode :: String
  , path :: Maybe FilePath
  } deriving (Generic, Show)

instance ParseRecord Options

-- | Parse command line arguments
parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    case args of
        ("--as":mode:rest) -> do
            let path = case rest of
                        (p:_) -> Just p
                        [] -> Nothing
            return Options { mode = mode, path = path }
        _ -> return Options { mode = "client", path = Nothing }  -- Default to client mode

main :: IO ()
main = do
    Options{..} <- parseOptions
    
    let socketPath = maybe defaultSocketPath id path
    
    case mode of
        "client" -> runClient socketPath
        "server" -> runServer socketPath
        _ -> putStrLn [i|Unknown mode: #{mode}. Use '--as client' or '--as server'.|]
{-# LANGUAGE FlexibleInstances #-}
module RPKI.Logging where

import Colog
import GHC.Stack (callStack)

import Data.Text

class Logger logger where
  logError_ :: logger -> Text -> IO ()
  logWarn_  :: logger -> Text -> IO ()
  logInfo_  :: logger -> Text -> IO ()
  logDebug_ :: logger -> Text -> IO ()


newtype AppLogger = AppLogger (LogAction IO Text)

instance Logger AppLogger where
  logError_ (AppLogger la) = logWhat E la 
  logWarn_  (AppLogger la) = logWhat W la 
  logInfo_  (AppLogger la) = logWhat I la 
  logDebug_ (AppLogger la) = logWhat D la 


logWhat :: Severity -> LogAction IO Text -> Text -> IO ()
logWhat sev la e = do 
  let logAction = cmap fmtMessage la
  logAction <& Msg sev callStack e


{-# LANGUAGE OverloadedStrings #-}

module RPKI.Metrics.Memory (getProcessPeakRss) where

import           Control.Exception             (try, SomeException)
import           Control.Monad.IO.Class

import qualified Data.ByteString               as BS
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.Read                as Text

import           RPKI.AppTypes


-- | Peak RSS the process has ever reached (VmHWM).
-- Works only on Linux with procfs, returns 0 otherwise
getProcessPeakRss :: MonadIO m => m Size
getProcessPeakRss = liftIO $ do
    procStatus <- readProcStatus
    pure $ Size $ 1024 * Map.findWithDefault 0 "VmHWM" procStatus
  where        
    readProcStatus = do
        content <- try @SomeException $ BS.readFile "/proc/self/status"
        pure $ case content of
            Left _      -> Map.empty
            Right bytes -> Map.fromList $ concatMap parseLine $ Text.lines $ Text.decodeUtf8 bytes
      where
        parseLine line =
            case Text.breakOn ":" line of
                (key, rest)
                    | Text.null rest -> []
                    | otherwise ->
                        case Text.decimal $ Text.stripStart $ Text.drop 1 rest of
                            Right (value, _) -> [(key, value)]
                            Left _           -> []

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.LoggingSpec where

import Codec.Serialise

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as C8

import           Test.Tasty
import           Test.Tasty.QuickCheck   as QC

import qualified Test.Tasty.HUnit        as HU

import           RPKI.AppMonad
import           RPKI.Logging
import           RPKI.Orphans

import RPKI.Util (toNatural)
import Data.Maybe (fromMaybe)


splitToMessagesCorrectly :: QC.Property
splitToMessagesCorrectly = QC.forAll arbitrary $ \(msgs :: [LogMessage1]) -> 
    let
        messages = map msgToBs msgs
        all = mconcat $ map (<> C8.singleton eol) messages
        chunks = aplitRandomly all 
        (_, msgs' :: [LogMessage1]) = 
                foldr (\chunk (builder, msgs) -> let 
                        (complete, leftover) = gatherMsgs builder chunk
                        mm = map (\c -> let Right d = bsToMsg c in d) complete
                        in (leftover, msgs <> mm)
                     ) (mempty, []) chunks
        in msgs' == msgs
    where 
        aplitRandomly bs = 
            [bs]


loggingSpec :: TestTree
loggingSpec = testGroup "Logging" [
        QC.testProperty "Splitted log messages are restored" splitToMessagesCorrectly
    ]

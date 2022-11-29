{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.LoggingSpec where

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


loggingSpec :: TestTree
loggingSpec = testGroup "Logging" [
        loggingUnitSpec
    ]

loggingUnitSpec :: TestTree
loggingUnitSpec = testGroup "Logging split stream" [
        HU.testCase "Should gather messages" $ do         
            check mempty "" (("" ==) . BB.toLazyByteString) null
            check mempty "123" (("123" ==) . BB.toLazyByteString) null
            check mempty "123\n" ((mempty ==) . BB.toLazyByteString) (==["123"])
            check mempty "123\n678" (("678" ==) . BB.toLazyByteString) (==["123"])
            check mempty "123\n678\n" ((mempty ==) . BB.toLazyByteString) (==["123", "678"])           
            check mempty "123\n678\n\n" ((mempty ==) . BB.toLazyByteString) (==["123", "678"])           
    ]
  where 
    check acc bs f g = do
        let (chunks, acc') = gatherMsgs acc bs
        HU.assertBool "Wrong acc" $ f acc'
        HU.assertBool ("Wrong chunks: |" <> show chunks <> "|") $ g chunks
{-# LANGUAGE OverloadedStrings #-}

module RPKI.LoggingSpec where

import qualified Data.ByteString.Builder as BB

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU

import           RPKI.Logging


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
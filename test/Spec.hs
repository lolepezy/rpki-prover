{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import Test.QuickCheck.Monadic

import RPKI.RRDP.ParseSpec
import RPKI.Parse.ObjectParseSpec

main :: IO ()
main = defaultMain $ testGroup "All tests" [  

  ]  

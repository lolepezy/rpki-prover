{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RrdpSpec where

import qualified Data.ByteString           as B

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef

import           Control.Lens              ((^.))

import           Control.Monad.Primitive   (PrimMonad (..), ioToPrim, stToPrim)
import           Control.Monad.Identity
import           Control.Monad.Trans.Class


import           RPKI.Domain
import           RPKI.RRDP.Parse

import           System.Directory
import           System.Environment
import           System.FilePath.Find

testParseSnapshot = do
  snapshot <- B.readFile "./snapshot.xml"  
  runIdentityT $ parseXml snapshot
      (\x -> lift $ ioToPrim $ print ("e = " ++ show x))
      (\t -> lift $ ioToPrim $ print ("t = " ++ show t))


{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RrdpSpec where

import qualified Data.ByteString.Lazy           as B

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
  let x = show $ parseSnapshot snapshot
  print $ length x
  -- runIdentityT $ parseXml (B.toStrict snapshot)
  --     (\x -> lift $ ioToPrim $ print ("e = " ++ show x))
  --     (\t -> lift $ ioToPrim $ print ("t = " ++ show t))




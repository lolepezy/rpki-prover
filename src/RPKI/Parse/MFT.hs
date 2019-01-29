{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}

module RPKI.Parse.MFT where
  
import Data.Data (Typeable)

import Data.ASN1.Types
import Data.ASN1.Parse

import RPKI.Domain  

newtype Mft = Mft RealMft
  deriving (Show, Eq, Ord, Typeable)

instance ASN1Object Mft where
  toASN1 _   = (\s -> [] ++ s)
  fromASN1 s = runParseASN1State parseMft s

parseMft :: ParseASN1 Mft
parseMft = throwParseError "Not implemented"

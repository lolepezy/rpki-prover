{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}

module RPKI.Parse.MFT where

import qualified Data.ByteString as B

import Data.ASN1.Types
import Data.ASN1.Parse

import RPKI.Domain  

parseMft :: B.ByteString -> ParseASN1 MFT
parseMft bs = throwParseError "Not implemented"

{-# LANGUAGE LambdaCase #-}
module RPKI.Util where
  
import Crypto.Hash.SHA256

import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as S256

import RPKI.Domain

sha256 :: B.ByteString -> Hash
sha256 bs = Hash SHA256 (S256.finalize (S256.update S256.init bs))
{-# LANGUAGE LambdaCase #-}
module RPKI.Util where
  
import Crypto.Hash.SHA256

import qualified Data.ByteString as B
import qualified Crypto.Hash.SHA256 as SHA256

sha256 :: B.ByteString -> B.ByteString
sha256 bs = SHA256.finalize (SHA256.update SHA256.init bs)
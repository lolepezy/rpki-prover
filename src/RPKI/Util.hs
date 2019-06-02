{-# LANGUAGE LambdaCase #-}
module RPKI.Util where

import qualified Crypto.Hash.SHA256      as S256
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.String.Conversions as SC

import           RPKI.Domain

sha256 :: BL.ByteString -> Hash
sha256 = Hash SHA256 . S256.hashlazy

sha256s :: B.ByteString -> Hash
sha256s = Hash SHA256 . S256.hash

convert :: SC.ConvertibleStrings s1 s2 => s1 -> s2
convert = SC.cs
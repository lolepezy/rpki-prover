
module RPKI.Store.Base.Serialisation where

import Data.Bifunctor
import Data.Store

import qualified  Data.Text as Text

import qualified Data.ByteString as BS

type TheBinary = Store

serialise_ :: TheBinary a => a -> BS.ByteString
serialise_ = encode

deserialise_ :: TheBinary a => BS.ByteString -> a
deserialise_ = decodeEx

deserialiseOrFail_ :: TheBinary a => BS.ByteString -> Either Text.Text a
deserialiseOrFail_ bs = first (Text.pack . show) $ decode bs

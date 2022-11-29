
module RPKI.Store.Base.Serialisation where

import Data.Store

import Data.Text (Text)
import qualified  Data.Text as Text

import Data.Bifunctor

import qualified Data.ByteString as BS

type TheBinary = Store

serialise_ :: forall a. TheBinary a => a -> BS.ByteString
serialise_ = encode

deserialise_ :: forall a. TheBinary a => BS.ByteString -> a
deserialise_ = decodeEx

deserialiseOrFail_ :: forall a. TheBinary a => BS.ByteString -> Either Text a
deserialiseOrFail_ bs = first (Text.pack . show) $ decode bs

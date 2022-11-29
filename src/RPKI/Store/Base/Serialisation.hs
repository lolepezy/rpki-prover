
module RPKI.Store.Base.Serialisation where

import Codec.Serialise

import Data.Text (Text)
import qualified  Data.Text as Text

import Data.Bifunctor

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


type TheBinary = Serialise

serialise_ :: forall a. TheBinary a => a -> BS.ByteString
serialise_ = LBS.toStrict . serialise

deserialise_ :: forall a. TheBinary a => BS.ByteString -> a
deserialise_ = deserialise . LBS.fromStrict

deserialiseOrFail_ :: forall a. TheBinary a => BS.ByteString -> Either Text a
deserialiseOrFail_ bs = first (Text.pack . show) <$> deserialiseOrFail $ LBS.fromStrict bs
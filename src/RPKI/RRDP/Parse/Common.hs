{-# LANGUAGE OverloadedStrings #-}

module RPKI.RRDP.Parse.Common where

import           Control.Monad.ST
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Data.Bifunctor
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Base64           as B64
import qualified Data.List                        as List
import qualified Data.Text                        as Text

import           Data.STRef

import           Text.Read                        (readMaybe)

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.RRDP.Types
import           RPKI.Util


type Element = (BS.ByteString, [(BS.ByteString, BS.ByteString)])

-- Parsing HEX stuff
newtype HexString = HexString BS.ByteString 
    deriving (Show, Eq, Ord)

hexString :: BS.ByteString -> Maybe HexString
hexString bs = HexString <$> unhex bs

toBytes :: HexString -> BS.ByteString
toBytes (HexString bs) = bs

forAttribute :: Monad m =>
                [(BS.ByteString, BS.ByteString)] ->
                BS.ByteString ->
                RrdpError ->
                (BS.ByteString -> ExceptT RrdpError m v) ->
                ExceptT RrdpError m v
forAttribute as name err f = case List.lookup name as of
    Nothing -> throwE err
    Just v  -> f v    


valuesOrError :: STRef s (Maybe v) -> RrdpError -> ExceptT RrdpError (ST s) v
valuesOrError v e =
    (lift . readSTRef) v >>= \case
        Nothing -> throwE e
        Just s  -> (lift . pure) s
     
parseInteger :: BS.ByteString -> Maybe Integer
parseInteger bs = readMaybe $ convert bs

parseSerial :: Monad m =>
            BS.ByteString ->
            (RrdpSerial -> ExceptT RrdpError m v) ->
            ExceptT RrdpError m v
parseSerial v f =  case parseInteger v of
    Nothing -> throwE $ BrokenSerial $ convert v
    Just s  -> f $ RrdpSerial s

makeHash :: BS.ByteString -> Maybe Hash
makeHash bs = mkHash . toBytes <$> hexString bs

decodeBase64 :: Show c => EncodedBase64 -> c -> Either RrdpError DecodedBase64
decodeBase64 (EncodedBase64 bs) context = 
    bimap 
        (\e -> BadBase64 (e <> " for " <> Text.pack (show context)) $ convert bs)
        DecodedBase64
        $ B64.decodeBase64 bs

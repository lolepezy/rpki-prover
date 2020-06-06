{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RPKI.Util where

import           Control.Exception
import           Numeric.Natural

import qualified Crypto.Hash.SHA256      as S256
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Short   as BSS
import           Data.Char
import qualified Data.String.Conversions as SC
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Word
import           RPKI.Domain


sha256 :: LBS.ByteString -> Hash
sha256 = Hash . BSS.toShort . S256.hashlazy

sha256s :: BS.ByteString -> Hash
sha256s = Hash . BSS.toShort . S256.hash

mkHash :: BS.ByteString -> Hash
mkHash = Hash . BSS.toShort

class ConvertibleAsSomethigString s1 s2 where
    convert :: s1 -> s2

instance SC.ConvertibleStrings s1 s2 => ConvertibleAsSomethigString s1 s2 where
    convert = SC.cs

instance {-# OVERLAPPING #-} ConvertibleAsSomethigString Text s => ConvertibleAsSomethigString URI s  where
    convert (URI u) = convert u

normalizeUri :: Text.Text -> Text.Text
normalizeUri = Text.map (\c -> if isOkForAFile c then c else '_')
    where
        isOkForAFile c = isAlpha c || isDigit c || c == '.'

trim :: BS.ByteString -> BS.ByteString
trim = C.dropWhile isSpace . fst . C.breakEnd (not . isSpace)

isSpace_ :: Word8 -> Bool
isSpace_ = isSpace . chr . fromEnum

fmtEx :: SomeException -> Text.Text
fmtEx = Text.pack . show

toNatural :: Int -> Maybe Natural 
toNatural i | i > 0     = Just (fromIntegral i :: Natural)
            | otherwise = Nothing


-- Some URL utilities 
isRsyncURI, isRrdpURI, isHttpsURI, isHttpURI :: URI -> Bool
isRsyncURI (URI u) = "rsync://" `Text.isPrefixOf` u
isHttpsURI (URI u) = "https://" `Text.isPrefixOf` u 
isHttpURI (URI u) = "http://" `Text.isPrefixOf` u
isRrdpURI u = isHttpURI u || isHttpsURI u

isParentOf :: URI -> URI -> Bool
isParentOf (URI p) (URI c) = p `Text.isPrefixOf` c

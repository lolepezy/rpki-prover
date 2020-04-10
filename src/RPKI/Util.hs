module RPKI.Util where

import           Control.Exception
import           Numeric.Natural

import qualified Crypto.Hash.SHA256      as S256
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as C
import qualified Data.ByteString.Lazy    as BL
import           Data.Char
import qualified Data.String.Conversions as SC
import qualified Data.Text               as T
import           Data.Word

import           Data.Char               (isAlpha)

import           RPKI.Domain

sha256 :: BL.ByteString -> Hash
sha256 = Hash . S256.hashlazy

sha256s :: B.ByteString -> Hash
sha256s = Hash . S256.hash

convert :: SC.ConvertibleStrings s1 s2 => s1 -> s2
convert = SC.cs

normalizeUri :: T.Text -> T.Text
normalizeUri = T.map (\c -> if isOkForAFile c then c else '_')
  where
    isOkForAFile c = isAlpha c || isDigit c || c == '.'

trim :: B.ByteString -> B.ByteString
trim = C.dropWhile isSpace . fst . C.breakEnd (not . isSpace)

isSpace_ :: Word8 -> Bool
isSpace_ = isSpace . chr . fromEnum

fmtEx :: SomeException -> T.Text
fmtEx = T.pack . show

toNatural :: Int -> Maybe Natural 
toNatural i | i > 0     = Just (fromIntegral i :: Natural)
            | otherwise = Nothing
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RPKI.Util where

import           Control.Exception
import           Control.Lens
import           Numeric.Natural
import           Data.Bifunctor

import qualified Crypto.Hash.SHA256          as S256
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexLazy
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Short       as BSS
import qualified Data.ByteString.Base64      as B64
import           Data.Char
import qualified Data.List                   as List
import           Data.Foldable (toList)
import qualified Data.String.Conversions     as SC
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Word
import           RPKI.Domain
import           RPKI.Reporting

import           Control.Monad.IO.Class
import           Data.IORef.Lifted

import qualified Text.URI as MURI
import           Text.URI.Lens


sha256 :: LBS.ByteString -> Hash
sha256 = mkHash . S256.hashlazy

sha256s :: BS.ByteString -> Hash
sha256s = mkHash . S256.hash

mkHash :: BS.ByteString -> Hash
mkHash = Hash . BSS.toShort

unhex :: BS.ByteString -> Maybe BS.ByteString
unhex hexed = either (const Nothing) Just $ Hex.decode hexed    

hex :: BS.ByteString -> BS.ByteString
hex = Hex.encode    

hexL :: LBS.ByteString -> LBS.ByteString
hexL = HexLazy.encode    

class ConvertibleAsSomethigString s1 s2 where
    convert :: s1 -> s2

instance SC.ConvertibleStrings s1 s2 => ConvertibleAsSomethigString s1 s2 where
    convert = SC.cs

instance {-# OVERLAPPING #-} ConvertibleAsSomethigString Text s => ConvertibleAsSomethigString URI s where
    convert (URI u) = convert u

instance {-# OVERLAPPING #-} ConvertibleAsSomethigString Text s => ConvertibleAsSomethigString RsyncURL s where
    convert = convert . getURL

instance {-# OVERLAPPING #-} ConvertibleAsSomethigString Text s => ConvertibleAsSomethigString RrdpURL s where
    convert (RrdpURL u) = convert u

instance {-# OVERLAPPING #-} ConvertibleAsSomethigString Text s => ConvertibleAsSomethigString RpkiURL s where
    convert (RsyncU u) = convert u
    convert (RrdpU u) = convert u

normalizeUri :: Text.Text -> Text.Text
normalizeUri = Text.map (\c -> if isOkForAFile c then c else '_')
  where
    isOkForAFile c = isAlpha c || isDigit c || c `elem` ("-._" :: String)

trim :: BS.ByteString -> BS.ByteString
trim = C.dropWhile isSpace . fst . C.breakEnd (not . isSpace)

trimmed :: Show a => a -> Text
trimmed = Text.strip . Text.pack . show

removeSpaces :: BS.ByteString -> BS.ByteString
removeSpaces = C.filter (not . isSpace)

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

isParentOf :: WithURL u => u -> u -> Bool
isParentOf p c = pt `Text.isPrefixOf` ct
    where
        URI pt = getURL p
        URI ct = getURL c

parseRpkiURL :: Text -> Either Text RpkiURL
parseRpkiURL t
    | isRrdpURI u  = Right $ RrdpU $ RrdpURL u
    | isRsyncURI u = RsyncU <$> parseRsyncURL t            
    | otherwise = Left $ "Unknown URL type: " <> t
    where
        u = URI t

parseRsyncURL :: Text -> Either Text RsyncURL
parseRsyncURL t = 
    case MURI.mkURI t of 
        Nothing -> Left $ "Unparseable rsync URL: " <> t 
        Just mu -> 
            case mu ^. uriAuthority of 
                Left _  -> Left "" 
                Right a -> let                                         
                    host = RsyncHost $ a ^. authHost . unRText
                    path = map (RsyncPathChunk . (^. unRText)) $ mu ^. uriPath
                    in Right $ RsyncURL host path

increment :: (MonadIO m, Num a) => IORef a -> m ()
increment counter = liftIO $ atomicModifyIORef' counter $ \c -> (c + 1, ())        

decrement :: (MonadIO m, Num a) => IORef a -> m ()
decrement counter = liftIO $ atomicModifyIORef' counter $ \c -> (c - 1, ())        

ifJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
ifJustM a f = maybe (pure ()) f =<< a

decodeBase64 :: Show c => EncodedBase64 -> c -> Either RrdpError DecodedBase64
decodeBase64 (EncodedBase64 bs) context = 
    bimap 
        (\e -> BadBase64 (e <> " for " <> Text.pack (show context)) $ convert bs)
        DecodedBase64
        $ B64.decodeBase64 bs 

encodeBase64 :: DecodedBase64 -> EncodedBase64
encodeBase64 (DecodedBase64 bs) = EncodedBase64 $ B64.encodeBase64' bs
    
textual :: LBS.ByteString -> Text
textual = decodeUtf8 . LBS.toStrict

fmtLocations :: Locations -> Text
fmtLocations = mconcat . 
               List.intersperse "," . 
               map (Text.pack . show) . 
               toList . 
               unLocations
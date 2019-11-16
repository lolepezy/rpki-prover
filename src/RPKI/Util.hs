module RPKI.Util where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import           Control.Monad
import           Control.Exception

import qualified Crypto.Hash.SHA256      as S256
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as C
import qualified Data.ByteString.Lazy    as BL
import qualified Data.String.Conversions as SC
import qualified Data.Text               as T
import           Data.Char
import           Data.Word

import Data.Char (isAlpha)

import UnliftIO
import qualified UnliftIO.Async as Unlift

import           RPKI.Domain

sha256 :: BL.ByteString -> Hash
sha256 = Hash . S256.hashlazy

sha256s :: B.ByteString -> Hash
sha256s = Hash . S256.hash

convert :: SC.ConvertibleStrings s1 s2 => s1 -> s2
convert = SC.cs

normalizeUri :: T.Text -> T.Text
normalizeUri = T.map (\c -> if isAlpha c then c else '_') 

trim :: B.ByteString -> B.ByteString
trim = C.dropWhile isSpace . fst . C.breakEnd (not . isSpace)    

isSpace_ :: Word8 -> Bool
isSpace_ = isSpace . chr . fromEnum

fmtEx :: SomeException -> T.Text
fmtEx = T.pack . show 


-- FIXME Do something about 'wait' throwing an exception
parallel :: (Traversable t, MonadUnliftIO m) => 
            Int -> 
            (a -> m b) -> t a -> m (t b)
parallel poolSize f as = do
  (chanIn, chanOut) <- liftIO $ Chan.newChan $ max 1 (poolSize - 1)
  snd <$> Unlift.concurrently (writeAll chanIn) (readAll chanOut)
  where
    writeAll chanIn  = forM_ as $ \a ->
      Unlift.async (f a) >>= liftIO . Chan.writeChan chanIn
    readAll chanOut = forM as $ \_ -> 
      Unlift.wait =<< liftIO (Chan.readChan chanOut)

boundedFunnel :: (Traversable t, MonadUnliftIO m) => 
            Int ->
            t a -> 
            (a -> m b) -> 
            (b -> m c) -> m (t c)
boundedFunnel poolSize as f g = do
  (chanIn, chanOut) <- liftIO $ Chan.newChan $ max 1 (poolSize - 1)
  snd <$> Unlift.concurrently (writeAll chanIn) (readAll chanOut)
  where
    writeAll chanIn  = forM_ as $
      liftIO . Chan.writeChan chanIn <=< f
    readAll chanOut = forM as $ \_ -> 
      g =<< liftIO (Chan.readChan chanOut)


txFunnel :: (Traversable t, MonadUnliftIO m) => 
            Int ->
            t a -> 
            (a -> m b) -> 
            ((tx -> m (t c)) -> m (t c)) ->
            (tx -> b -> m c) -> m (t c)
txFunnel poolSize as f withTx g = do
  (chanIn, chanOut) <- liftIO $ Chan.newChan $ max 1 (poolSize - 1)
  snd <$> Unlift.concurrently (writeAll chanIn) (readAll chanOut)
  where
    writeAll chanIn = forM_ as $
      liftIO . Chan.writeChan chanIn <=< f
    readAll chanOut = withTx $ \tx -> 
      forM as $ \_ -> 
        g tx =<< liftIO (Chan.readChan chanOut)

-- pipeToTx :: (Traversable t, MonadUnliftIO m) => 
--         Int ->
--         t a -> 
--         (a (Chan.Chan )-> m b) -> 
--         ((tx -> m (t c)) -> m (t c)) ->
--         (tx -> b -> m c) -> m (t c)
-- pipeToTx poolSize as f withTx g = do
--   (chanIn, chanOut) <- liftIO $ Chan.newChan $ max 1 (poolSize - 1)
--   snd <$> Unlift.concurrently (f chanIn) (readAll chanOut)
--   where
--     writeAll chanIn = forM_ as $
--       liftIO . Chan.writeChan chanIn <=< f
--     readAll chanOut = withTx go 
--       where 
--         go tx =
--           liftIO (Chan.readChan chanOut) >>= \case
--             Nothing -> pure ()
--             Just a -> g tx a    -- 

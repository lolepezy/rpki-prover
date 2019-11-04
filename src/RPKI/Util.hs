{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module RPKI.Util where

import qualified Control.Concurrent.Chan.Unagi.Bounded as Chan
import           Control.Monad

import qualified Crypto.Hash.SHA256      as S256
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.String.Conversions as SC

import Conduit

import qualified UnliftIO.Async as Unlift
import Control.Monad.IO.Unlift

import           RPKI.Domain

sha256 :: BL.ByteString -> Hash
sha256 = Hash . S256.hashlazy

sha256s :: B.ByteString -> Hash
sha256s = Hash . S256.hash

convert :: SC.ConvertibleStrings s1 s2 => s1 -> s2
convert = SC.cs

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

-- Conduit, calculating SHA256 hash 
sinkHash :: Monad m => forall o. ConduitT B.ByteString o m B.ByteString
sinkHash = loop S256.init 
  where
    loop ctx = await >>= \case
      Nothing -> return $! S256.finalize ctx
      Just bs -> let !ctx' = S256.update ctx bs
                  in loop ctx'
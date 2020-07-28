{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}

module RPKI.RRDP.HttpContext where

import           Control.Monad.Except
import           Data.ByteString.Streaming.HTTP

newtype HttpContext = HttpContext Manager

withHttp :: MonadIO m => (HttpContext -> m a) -> m a
withHttp f = do
  tlsManager <- liftIO $ newManager tlsManagerSettings
  f $ HttpContext tlsManager

newHttpContext :: IO HttpContext
newHttpContext = HttpContext <$> newManager tlsManagerSettings

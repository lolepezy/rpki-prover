{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.AppMonad where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception

import Data.Bifunctor

import RPKI.Domain    


-- Application monad stack
type ValidatorT conf m r = ReaderT conf (ExceptT SomeError (StateT ValidationWarning m)) r

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
        m a -> t1 (t2 m) a
lift2 = lift . lift

lift3 :: (MonadTrans t1, MonadTrans t2, MonadTrans t3, Monad m,
                Monad (t2 (t3 m)), Monad (t3 m)) =>
        m a -> t1 (t2 (t3 m)) a
lift3 = lift . lift . lift

fromIOEither :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
                m (Either e a) -> t1 (ExceptT e (t2 m)) a
fromIOEither = lift . ExceptT . lift 

fromTry :: (MonadTrans t1, MonadTrans t2, Monad (t2 IO), Exception exc) =>
            (exc -> e) -> IO a2 -> t1 (ExceptT e (t2 IO)) a2
fromTry mapErr t = fromIOEither $ first mapErr <$> try t

fromEither :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
            Either e a -> t1 (ExceptT e (t2 m)) a
fromEither = fromIOEither . pure

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ (`runReaderT` env) f

runValidatorT :: Monoid s =>
                r -> ReaderT r (ExceptT e (StateT s m)) a -> m (Either e a, s)
runValidatorT conf w = (runStateT $ runExceptT $ w `runReaderT` conf) mempty
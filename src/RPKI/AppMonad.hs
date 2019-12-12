{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.AppMonad where

import Control.Monad.Except
import Control.Monad.Trans.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception
import Control.Monad.Morph

import Data.Has

import Data.Bifunctor
import RPKI.Errors


-- Application monad stack
type ValidatorT env m r = Has ValidationContext env => 
        ReaderT env (ExceptT SomeError (StateT [ValidationWarning] m)) r

type PureValidator env r = Has ValidationContext env =>
        ReaderT env (ExceptT SomeError (State [ValidationWarning])) r

pureToValidatorT :: (Monad m, Has ValidationContext env) => 
                    PureValidator env r -> ValidatorT env m r
pureToValidatorT = hoist $ hoist $ hoist generalize

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
        m a -> t1 (t2 m) a
lift2 = lift . lift

lift3 :: (MonadTrans t1, MonadTrans t2, MonadTrans t3, Monad m,
                Monad (t2 (t3 m)), Monad (t3 m)) =>
        m a -> t1 (t2 (t3 m)) a
lift3 = lift . lift . lift

fromIOEither :: Monad m => m (Either SomeError r) -> ValidatorT env m r
fromIOEither = lift . ExceptT . lift 

-- TODO Make it not so ugly
validatorT :: Monad m => m (Either SomeError r, [ValidationWarning]) -> ValidatorT env m r
validatorT s = lift $ ExceptT $ do
                        (v, w) <- lift s
                        put w
                        pure v

fromTry :: Exception exc => (exc -> SomeError) -> IO r -> ValidatorT env IO r
fromTry mapErr t = fromIOEither $ first mapErr <$> try t

fromEither :: Monad m => Either SomeError r -> ValidatorT env m r
fromEither = fromIOEither . pure

fromTryEither :: Exception exc =>
                 (exc -> SomeError) -> IO (Either SomeError r) -> ValidatorT env IO r
fromTryEither mapErr t = fromEither =<< fromTry mapErr t

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ runReaderT f env

runPureValidator :: Has ValidationContext env =>
                    env -> PureValidator env r -> (Either SomeError r, [ValidationWarning])
runPureValidator env w = (runState $ runExceptT $ runReaderT w env) mempty

runValidatorT :: Has ValidationContext env =>
                 env -> ValidatorT env m r -> m (Either SomeError r, [ValidationWarning])
runValidatorT env w = (runStateT $ runExceptT $ runReaderT w env) mempty

-- TODO Introduce some sort of error/warning context, an 
-- URI of the object the error is related to
validatorWarning :: Monad m => ValidationWarning -> ValidatorT env m ()
validatorWarning = pureToValidatorT . pureWarning

validatorError :: Monad m => ValidationError -> ValidatorT env m r
validatorError = pureToValidatorT . pureError

pureWarning :: ValidationWarning -> PureValidator env ()
pureWarning w = lift $ modify' (w:)

pureError :: ValidationError -> PureValidator env r
pureError e = lift $ throwE $ ValidationE e

pureErrorIfNot :: Bool -> ValidationError -> PureValidator env ()
pureErrorIfNot b e = if b then pure () else lift $ throwE $ ValidationE e
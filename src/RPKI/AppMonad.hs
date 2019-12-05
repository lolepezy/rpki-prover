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

import Data.Bifunctor
import RPKI.Errors


-- Application monad stack
type ValidatorT conf m r = ReaderT conf (ExceptT SomeError (StateT [ValidationWarning] m)) r

type PureValidator r = ExceptT SomeError (State [ValidationWarning]) r

pureToValidatorT :: Monad m => PureValidator r -> ValidatorT conf m r
pureToValidatorT p = lift $ hoist (hoist generalize) p

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
        m a -> t1 (t2 m) a
lift2 = lift . lift

lift3 :: (MonadTrans t1, MonadTrans t2, MonadTrans t3, Monad m,
                Monad (t2 (t3 m)), Monad (t3 m)) =>
        m a -> t1 (t2 (t3 m)) a
lift3 = lift . lift . lift

fromIOEither :: Monad m => m (Either SomeError r) -> ValidatorT conf m r
fromIOEither = lift . ExceptT . lift 

-- TODO Make it not so ugly
fromIOEitherSt :: Monad m => m (Either SomeError r, [ValidationWarning]) -> ValidatorT conf m r
fromIOEitherSt s = lift $ ExceptT $ do
                        (v, w) <- lift s
                        put w
                        pure v

fromTry :: Exception exc => (exc -> SomeError) -> IO r -> ValidatorT conf IO r
fromTry mapErr t = fromIOEither $ first mapErr <$> try t

fromEither :: Monad m => Either SomeError r -> ValidatorT conf m r
fromEither = fromIOEither . pure

fromTryEither :: Exception exc =>
                 (exc -> SomeError) -> IO (Either SomeError r) -> ValidatorT conf IO r
fromTryEither mapErr t = fromEither =<< fromTry mapErr t

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ runReaderT f env

runPureValidator :: PureValidator r -> (Either SomeError r, [ValidationWarning])
runPureValidator p = (runState $ runExceptT p) mempty

runValidatorT  :: conf -> ValidatorT conf m r -> m (Either SomeError r, [ValidationWarning])
runValidatorT conf w = (runStateT $ runExceptT $ runReaderT w conf) mempty

-- TODO Introduce some sort of error/warning context, an 
-- URI of the object the error is related to
validatorWarning :: Monad m => ValidationWarning -> ValidatorT conf m ()
validatorWarning = pureToValidatorT . pureWarning

validatorError :: Monad m => ValidationError -> ValidatorT conf m r
validatorError = pureToValidatorT . pureError

pureWarning :: ValidationWarning -> PureValidator ()
pureWarning w = lift $ modify' (w:)

pureError :: ValidationError -> PureValidator r
pureError e = throwE $ ValidationE e

pureErrorIfNot :: Bool -> ValidationError -> PureValidator ()
pureErrorIfNot b e = if b then pure () else throwE $ ValidationE e
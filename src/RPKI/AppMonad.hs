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
import Data.Has
import RPKI.Errors

-- Application monad stack
type ValidatorT env m r = Has VContext env => 
        ReaderT env (ExceptT AppError (StateT [VWarning] m)) r

type PureValidator env r = Has VContext env =>
        ReaderT env (ExceptT AppError (State [VWarning])) r

pureToValidatorT :: Monad m => PureValidator env r -> ValidatorT env m r
pureToValidatorT = hoist $ hoist $ hoist generalize

lift2 :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m)) =>
        m a -> t1 (t2 m) a
lift2 = lift . lift

lift3 :: (MonadTrans t1, MonadTrans t2, MonadTrans t3, Monad m,
                Monad (t2 (t3 m)), Monad (t3 m)) =>
        m a -> t1 (t2 (t3 m)) a
lift3 = lift . lift . lift

fromIOEither :: Monad m => m (Either AppError r) -> ValidatorT env m r
fromIOEither = lift . ExceptT . lift 

-- TODO Make it not so ugly
validatorT :: Monad m => m (Either AppError r, [VWarning]) -> ValidatorT env m r
validatorT s = lift $ ExceptT $ do
                (v, w) <- lift s
                put w
                pure v

fromTry :: Exception exc => (exc -> AppError) -> IO r -> ValidatorT env IO r
fromTry mapErr t = fromIOEither $ first mapErr <$> try t

fromEither :: Monad m => Either AppError r -> ValidatorT env m r
fromEither = fromIOEither . pure

fromTryEither :: Exception exc =>
                (exc -> AppError) -> IO (Either AppError r) -> ValidatorT env IO r
fromTryEither mapErr t = fromEither =<< fromTry mapErr t

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ runReaderT f env

runPureValidator :: Has VContext env =>
                env -> PureValidator env r -> (Either AppError r, [VWarning])
runPureValidator env v = (runState $ runExceptT $ runReaderT v env) mempty

runValidatorT :: Has VContext env =>
                env -> ValidatorT env m r -> m (Either AppError r, [VWarning])
runValidatorT env v = (runStateT $ runExceptT $ runReaderT v env) mempty

validatorWarning :: Monad m => VWarning -> ValidatorT env m ()
validatorWarning = pureToValidatorT . pureWarning

vError :: Monad m => ValidationError -> ValidatorT env m r
vError = pureToValidatorT . pureError

pureWarning :: VWarning -> PureValidator env ()
pureWarning w = lift $ modify' (w:)

pureError :: ValidationError -> PureValidator env r
pureError e = lift $ throwE $ ValidationE e

pureErrorIfNot :: Bool -> ValidationError -> PureValidator env ()
pureErrorIfNot b e = if b then pure () else lift $ throwE $ ValidationE e

pureFromEither :: Either ValidationError r -> PureValidator env r
pureFromEither (Left e) = pureError e
pureFromEither (Right r) = pure r

valid :: Applicative m =>
        m (Either AppError (), [VWarning])
valid = pure (Right (), [])

vWarn :: (Monad m, Has VContext env) =>
        ValidationError -> ValidatorT env m ()
vWarn = validatorWarning . VWarning . ValidationE

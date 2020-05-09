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
        ReaderT env (ExceptT AppError (StateT Validations m)) r

type PureValidator env r = Has VContext env =>
        ReaderT env (ExceptT AppError (State Validations)) r

vHoist :: Monad m => PureValidator env r -> ValidatorT env m r
vHoist = hoist $ hoist $ hoist generalize

fromEitherM :: Monad m => m (Either AppError r) -> ValidatorT env m r
fromEitherM = lift . ExceptT . lift 

-- TODO Make it not so ugly
validatorT :: Monad m => m (Either AppError r, Validations) -> ValidatorT env m r
validatorT s = lift $ ExceptT $ do
                (v, w) <- lift s
                put w
                pure v

fromTry :: Exception exc => (exc -> AppError) -> IO r -> ValidatorT env IO r
fromTry mapErr t = fromEitherM $ first mapErr <$> try t

fromTryEither :: Exception exc =>
                (exc -> AppError) -> IO (Either AppError r) -> ValidatorT env IO r
fromTryEither mapErr t = fromEitherM . pure =<< fromTry mapErr t

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ runReaderT f env

runPureValidator :: Has VContext env =>
                env -> PureValidator env r -> (Either AppError r, Validations)
runPureValidator env v = (runState $ runExceptT $ runReaderT v env) mempty

runValidatorT :: Has VContext env =>
                env -> ValidatorT env m r -> m (Either AppError r, Validations)
runValidatorT env v = (runStateT $ runExceptT $ runReaderT v env) mempty

validatorWarning :: Monad m => VWarning -> ValidatorT env m ()
validatorWarning = vHoist . pureWarning

vError :: Monad m => ValidationError -> ValidatorT env m r
vError = vHoist . vPureError

appError :: Monad m => AppError -> ValidatorT env m r
appError = vHoist . pureError

pureWarning :: Has VContext env => VWarning -> PureValidator env ()
pureWarning w = do 
    vc <- asks getter 
    lift $ modify' (mWarning vc w <>)

vPureError :: ValidationError -> PureValidator env r
vPureError = pureError . ValidationE

pureError :: AppError -> PureValidator env r
pureError = lift . throwE

pureErrorIfNot :: Bool -> ValidationError -> PureValidator env ()
pureErrorIfNot b e = if b then pure () else lift $ throwE $ ValidationE e

fromEither :: Either AppError r -> PureValidator env r
fromEither (Left e) = pureError e
fromEither (Right r) = pure r

vFromEither :: Either ValidationError r -> PureValidator env r
vFromEither e = fromEither $ first ValidationE e

valid :: Applicative m =>
        m (Either AppError (), Validations)
valid = pure (Right (), mempty)

vWarn :: (Monad m, Has VContext env) =>
            ValidationError -> ValidatorT env m ()
vWarn = validatorWarning . VWarning . ValidationE

appWarn :: (Monad m, Has VContext env) =>
            AppError -> ValidatorT env m ()
appWarn = validatorWarning . VWarning
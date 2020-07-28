{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.AppMonad where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except

import           Data.Bifunctor
import           Data.Text                  (Text)
import           RPKI.Errors


-- Application monad stack
type ValidatorT env m r = WithVContext env => 
        ReaderT env (ExceptT AppError (StateT Validations m)) r

type PureValidatorT env r = WithVContext env =>
        ReaderT env (ExceptT AppError (State Validations)) r

vHoist :: Monad m => PureValidatorT env r -> ValidatorT env m r
vHoist = hoist $ hoist $ hoist generalize

fromEitherM :: Monad m => m (Either AppError r) -> ValidatorT env m r
fromEitherM = lift . ExceptT . lift 

-- TODO Make it not so ugly
validatorT :: Monad m => m (Either AppError r, Validations) -> ValidatorT env m r
validatorT s = lift $ ExceptT $ do
                (v, w) <- lift s
                put w
                pure v

-- This one is slightly heuristical: never catch AsyncExceptions.
fromTry :: Exception exc => 
            (exc -> AppError) -> 
            IO r -> 
            ValidatorT env IO r
fromTry mapErr t =
    fromEitherM $ (Right <$> t) `catch` recoverOrRethrow        
    where
        recoverOrRethrow e = 
            case fromException (toException e) of
                Just (SomeAsyncException _) -> throwIO e
                Nothing                     -> pure $ Left $ mapErr e

fromTryEither :: Exception exc =>
                (exc -> AppError) -> IO (Either AppError r) -> ValidatorT env IO r
fromTryEither mapErr t = do 
    z <- fromTry mapErr t
    fromEitherM $ pure z

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ runReaderT f env

runPureValidator :: WithVContext env =>
                env -> PureValidatorT env r -> (Either AppError r, Validations)
runPureValidator env v = (runState $ runExceptT $ runReaderT v env) mempty

runValidatorT :: WithVContext env =>
                env -> ValidatorT env m r -> m (Either AppError r, Validations)
runValidatorT env v = (runStateT $ runExceptT $ runReaderT v env) mempty

validatorWarning :: Monad m => VWarning -> ValidatorT env m ()
validatorWarning = vHoist . pureWarning

vError :: Monad m => ValidationError -> ValidatorT env m r
vError = vHoist . vPureError

appError :: Monad m => AppError -> ValidatorT env m r
appError = vHoist . pureError

pureWarning :: WithVContext env => VWarning -> PureValidatorT env ()
pureWarning w = do 
    vc <- asks getVC 
    lift $ modify' (mWarning vc w <>)

vPureError :: ValidationError -> PureValidatorT env r
vPureError e = pureError $ ValidationE e    

pureError :: AppError -> PureValidatorT env r
pureError e = do
    vc <- asks getVC     
    lift $ do 
        modify' (mError vc e <>)
        throwE e

pureErrorIfNot :: Bool -> ValidationError -> PureValidatorT env ()
pureErrorIfNot b e = if b then pure () else vPureError e

fromEither :: Either AppError r -> PureValidatorT env r
fromEither (Left e) = pureError e
fromEither (Right r) = pure r

vFromEither :: Either ValidationError r -> PureValidatorT env r
vFromEither e = fromEither $ first ValidationE e

valid :: Applicative m =>
        m (Either AppError (), Validations)
valid = pure (Right (), mempty)

vWarn :: (Monad m, WithVContext env) =>
            ValidationError -> ValidatorT env m ()
vWarn = validatorWarning . VWarning . ValidationE

appWarn :: (Monad m, WithVContext env) =>
            AppError -> ValidatorT env m ()
appWarn = validatorWarning . VWarning


forChild :: (Monad m, WithVContext env) => 
            Text -> ValidatorT env m r -> ValidatorT env m r
forChild = local . childVC
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}

module RPKI.AppMonad where

import           Control.Lens                ((.~), (&), (%~))
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import           GHC.Generics (Generic)

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Except (throwE)

import           Data.Bifunctor             (Bifunctor (first))

import           Data.Text                   (Text)
import           RPKI.Reporting


-- Application monad stack
type ValidatorT m r = 
        ReaderT ValidatorContext (ExceptT AppError (StateT ValidationState m)) r

type PureValidatorT r = 
        ReaderT ValidatorContext (ExceptT AppError (State ValidationState)) r

vHoist :: Monad m => PureValidatorT r -> ValidatorT m r
vHoist = hoist $ hoist $ hoist generalize

fromEitherM :: Monad m => m (Either AppError r) -> ValidatorT m r
fromEitherM = lift . ExceptT . lift 

-- TODO Make it not so ugly
validatorT :: Monad m => m (Either AppError r, ValidationState) -> ValidatorT m r
validatorT s = lift $ ExceptT $ do
                (v, w) <- lift s
                put w
                pure v

-- This one is slightly heuristical: never catch AsyncExceptions.
fromTry :: Exception exc => 
            (exc -> AppError) -> 
            IO r -> 
            ValidatorT IO r
fromTry mapErr t =
    fromEitherM $ (Right <$> t) `catch` recoverOrRethrow        
    where
        recoverOrRethrow e = 
            case fromException (toException e) of
                Just (SomeAsyncException _) -> throwIO e
                Nothing                     -> pure $ Left $ mapErr e


fromTryM :: Exception exc =>              
            (exc -> AppError) -> 
            ValidatorT IO r -> 
            ValidatorT IO r
fromTryM mapErr t =
    t `catch` recoverOrRethrow        
    where
        recoverOrRethrow e = 
            case fromException (toException e) of
                Just (SomeAsyncException _) -> throwIO e
                Nothing                     -> appError $ mapErr e

fromTryEither :: Exception exc =>
                (exc -> AppError) -> 
                IO (Either AppError r) -> ValidatorT IO r
fromTryEither mapErr t = do 
    z <- fromTry mapErr t
    fromEitherM $ pure z

toEither :: r -> ReaderT r (ExceptT e m) a -> m (Either e a)
toEither env f = runExceptT $ runReaderT f env

runPureValidator :: ValidatorContext -> PureValidatorT r -> (Either AppError r, ValidationState)
runPureValidator vc v = (runState $ runExceptT $ runReaderT v vc) mempty

runValidatorT :: ValidatorContext -> ValidatorT m r -> m (Either AppError r, ValidationState)
runValidatorT vc v = (runStateT $ runExceptT $ runReaderT v vc) mempty

validatorWarning :: Monad m => VWarning -> ValidatorT m ()
validatorWarning = vHoist . pureWarning

vError :: Monad m => ValidationError -> ValidatorT m r
vError = vHoist . vPureError

appError :: Monad m => AppError -> ValidatorT m r
appError = vHoist . pureError

pureWarning :: VWarning -> PureValidatorT ()
pureWarning w = do 
    vc :: VTrail <- asks getContext 
    lift $ modify' (typed %~ (mWarning vc w <>))

vPureError :: ValidationError -> PureValidatorT r
vPureError e = pureError $ ValidationE e    

pureError :: AppError -> PureValidatorT r
pureError e = do
    vc <- asks getContext
    lift $ do 
        modify' (typed %~ (mError vc e <>))
        throwE e

pureErrorIfNot :: Bool -> ValidationError -> PureValidatorT ()
pureErrorIfNot b e = if b then pure () else vPureError e

fromEither :: Either AppError r -> PureValidatorT r
fromEither (Left e) = pureError e
fromEither (Right r) = pure r

vFromEither :: Either ValidationError r -> PureValidatorT r
vFromEither e = fromEither $ first ValidationE e

valid :: Applicative m =>
        m (Either AppError (), Validations)
valid = pure (Right (), mempty)

vWarn :: Monad m =>
            ValidationError -> ValidatorT m ()
vWarn = validatorWarning . VWarning . ValidationE

appWarn :: Monad m =>
            AppError -> ValidatorT m ()
appWarn = validatorWarning . VWarning


inSubVContext :: Monad m => 
                Text -> ValidatorT m r -> ValidatorT m r
inSubVContext t = local (& typed @VTrail %~ (trail t <>))

inSubMetricContext :: Monad m => 
                    Text -> ValidatorT m r -> ValidatorT m r
inSubMetricContext t = local (& typed @MetricTrail %~ (trail t <>))

inSubContext :: Monad m => 
                Text -> ValidatorT m r -> ValidatorT m r
inSubContext t va = inSubVContext t $ inSubMetricContext t va    


-- appMetric :: (Monad m, WithContext Metric env) => AppMetric -> ValidatorT m ()
-- appMetric = vHoist . pureMetric

-- pureMetric :: WithContext Metric env => AppMetric -> PureValidatorT env ()
-- pureMetric z = do 
--     mc <- asks getContext
--     lift $ modify' (typed %~ (validationMetric metric <>))

-- modifyMetric :: Monad m => MetricTrail -> (AMetric -> Maybe AMetric) -> ValidatorT m ()
-- modifyMetric key f = vHoist $ modifyPureMetric key f 

-- modifyPureMetric :: MetricKey -> (AMetric -> Maybe AMetric) -> PureValidatorT env ()
-- modifyPureMetric key f = 
--     lift $ modify' $ typed %~ (\am -> modifyAMetric am key f)

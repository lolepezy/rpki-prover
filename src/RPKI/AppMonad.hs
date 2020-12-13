{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.AppMonad where

import           Control.Lens
import           Data.Generics.Labels
import           Data.Generics.Product.Typed

import           GHC.Generics (Generic)

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.Bifunctor             (Bifunctor (first))

import           Data.Text                   (Text)
import           RPKI.Reporting
import           RPKI.Time
import Data.Proxy


-- Application monad stack
type ValidatorT m r = ValidatorTCurried m r

type ValidatorTCurried m = 
        ReaderT ValidatorPath (ExceptT AppError (StateT ValidationState m))

type PureValidatorT r = 
        ReaderT ValidatorPath (ExceptT AppError (State ValidationState)) r

vHoist :: Monad m => PureValidatorT r -> ValidatorT m r
vHoist = hoist $ hoist $ hoist generalize

fromEitherM :: Monad m => m (Either AppError r) -> ValidatorT m r
fromEitherM = lift . ExceptT . lift 

appLift :: Monad m => m r -> ValidatorT m r
appLift = lift . lift . lift 

-- TODO Make it not so ugly
validatorT :: Monad m => m (Either AppError r, ValidationState) -> ValidatorT m r
validatorT s = 
    lift $ ExceptT $ do
        (v, w) <- lift s
        put w
        pure v

embedValidatorT :: Monad m => m (Either AppError r, ValidationState) -> ValidatorT m r
embedValidatorT s = 
    lift $ ExceptT $ do
        (v, w) <- lift s
        modify' (<> w)
        pure v

embedState :: Monad m => ValidationState -> ValidatorT m ()
embedState w = lift $ lift $ modify' (<> w)    

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

runPureValidator :: ValidatorPath -> PureValidatorT r -> (Either AppError r, ValidationState)
runPureValidator vc v = (runState $ runExceptT $ runReaderT v vc) mempty

runValidatorT :: ValidatorPath -> ValidatorT m r -> m (Either AppError r, ValidationState)
runValidatorT vc v = (runStateT $ runExceptT $ runReaderT v vc) mempty

runValidatorStateT :: ValidatorPath -> ValidationState -> ValidatorT m r -> m (Either AppError r, ValidationState)
runValidatorStateT vc state v = (runStateT $ runExceptT $ runReaderT v vc) state

-- | Shorthand version for cases when we need to actually 
-- run IO or something similar like that
voidRun :: Functor m => Text -> ValidatorT m r -> m ()
voidRun t = void <$> runValidatorT (newValidatorPath t)

validatorWarning :: Monad m => VWarning -> ValidatorT m ()
validatorWarning = vHoist . pureWarning

vError :: Monad m => ValidationError -> ValidatorT m r
vError = vHoist . vPureError

appError :: Monad m => AppError -> ValidatorT m r
appError = vHoist . pureError

pureWarning :: VWarning -> PureValidatorT ()
pureWarning warning = do 
    validationPath <- asks (^. typed)
    modify' (typed %~ (mWarning validationPath warning <>))

vPureError :: ValidationError -> PureValidatorT r
vPureError e = pureError $ ValidationE e    

pureError :: AppError -> PureValidatorT r
pureError e = do
    validationPath <- asks (^. typed)
    modify' $ typed %~ (mError validationPath e <>)
    throwError e

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

askEnv :: MonadReader r m => m r
askEnv = ask

subVPath :: Monad m => 
            Text -> ValidatorT m r -> ValidatorT m r
subVPath t = local (& typed @VPath %~ (newPath t <>))

subMetricPath :: Monad m => 
                Text -> ValidatorT m r -> ValidatorT m r
subMetricPath text = local (& typed @MetricPath %~ (newPath text <>))

inSubContext :: Monad m => 
                Text -> ValidatorT m r -> ValidatorT m r
inSubContext text va = subVPath text $ subMetricPath text va    

updateMetric :: forall metric m . 
                (Monad m, MetricC metric) => 
                (metric -> metric) -> ValidatorT m ()
updateMetric = vHoist . updatePureMetric

updatePureMetric :: forall metric . MetricC metric => 
                    (metric -> metric) -> PureValidatorT ()
updatePureMetric f = do 
    mp <- asks (^. typed)
    modify' (& typed . metricLens %~ updateMetricInMap mp f)    


timedMetric :: forall m metric r . 
                (MonadIO m, MetricC metric, HasType TimeTakenMs metric) =>                 
                Proxy metric -> ValidatorT m r -> ValidatorT m r
timedMetric _ v = do         
    (r, elapsed) <- timedMS v          
    updateMetric ((& typed .~ TimeTakenMs elapsed) :: metric -> metric)
    pure r        


getMetric :: forall metric m . 
            (Monad m, MetricC metric) => 
            ValidatorT m (Maybe metric)
getMetric = vHoist getPureMetric

getPureMetric :: forall metric . MetricC metric => 
                 PureValidatorT (Maybe metric)
getPureMetric = do 
    metricPath <- asks (^. typed)
    metricMap  <- gets (^. typed . metricLens)
    pure $ lookupMetric metricPath metricMap
            
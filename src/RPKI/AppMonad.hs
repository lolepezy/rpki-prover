{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module RPKI.AppMonad where

import           Control.Lens

import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Morph
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.Bifunctor             (Bifunctor (first))
import           Data.Generics.Product       (HasField)
import           Data.Generics.Product.Typed
import           Data.Proxy
import           Data.Text                   (Text)

import           RPKI.Reporting
import           RPKI.Time
import           System.Timeout



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

catchAndEraseError :: Monad m => 
                        ValidatorT m r 
                    -> (AppError -> Bool) 
                    -> ValidatorT m r 
                    -> ValidatorT m r
catchAndEraseError f predicate errorHandler = do
    catchError f $ \e -> 
        if predicate e 
            then do 
                validationPath <- asks (^. typed)
                modify' $ typed %~ removeValidation validationPath predicate
                errorHandler 
            else throwError e


pureErrorIfNot :: Bool -> ValidationError -> PureValidatorT ()
pureErrorIfNot b e = if b then pure () else vPureError e

fromEither :: Either AppError r -> PureValidatorT r
fromEither = lift . ExceptT . pure

vFromEither :: Either ValidationError r -> PureValidatorT r
vFromEither = fromEither . first ValidationE

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

inSubVPath :: Monad m => 
            Text -> ValidatorT m r -> ValidatorT m r
inSubVPath t = local (& typed @VPath %~ (newPath t <>))

inSubMetricPath :: Monad m => 
                Text -> ValidatorT m r -> ValidatorT m r
inSubMetricPath text = local (& typed @MetricPath %~ (newPath text <>))

inSubPath :: Monad m => 
                Text -> ValidatorT m r -> ValidatorT m r
inSubPath text va = inSubVPath text $ inSubMetricPath text va    

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
                (MonadIO m, 
                 MetricC metric, 
                 HasField "totalTimeMs" metric metric TimeMs TimeMs) =>                 
                Proxy metric -> ValidatorT m r -> ValidatorT m r
timedMetric _ v = do
    (r, elapsed) <- timedMS v          
    updateMetric ((& #totalTimeMs .~ TimeMs elapsed) :: metric -> metric)
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
            

finallyError :: Monad m => ValidatorT m a -> ValidatorT m () -> ValidatorT m a
finallyError tryF finallyF = 
    tryIt `catchError` catchIt
  where
    tryIt = do  
        z <- tryF 
        finallyF
        pure z
    catchIt e = do
        finallyF
        throwError e            


timeoutVT :: Int -> ValidatorT IO a -> ValidatorT IO a -> ValidatorT IO a
timeoutVT t toDo timedOut = do 
    vp <- ask 
    z <- liftIO $ timeout t (runValidatorT vp toDo)
    case z of 
        Nothing -> timedOut
        Just q  -> embedValidatorT $ pure q


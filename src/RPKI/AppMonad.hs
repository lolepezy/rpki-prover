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

import           Data.Bifunctor              (Bifunctor (first))
import           Data.Generics.Product       (HasField)
import           Data.Generics.Product.Typed
import           Data.Hourglass
import           Data.Int                    (Int64)
import           Data.Proxy
import           Data.Text                   (Text)

import           System.Timeout

import           RPKI.Reporting
import           RPKI.Time


-- Application monad stack
type ValidatorT m r = ValidatorTCurried m r

type ValidatorTCurried m = 
        ReaderT Scopes (ExceptT AppError (StateT ValidationState m))

type PureValidatorT r = 
        ReaderT Scopes (ExceptT AppError (State ValidationState)) r

vHoist :: Monad m => PureValidatorT r -> ValidatorT m r
vHoist = hoist $ hoist $ hoist generalize

fromEither :: Either AppError r -> PureValidatorT r
fromEither z =
    case z of 
        Left e -> do 
            validationScope <- asks (^. typed)
            modify' $ typed %~ (mError validationScope e <>)
            lift $ ExceptT $ pure z
        Right _ -> 
            lift $ ExceptT $ pure z

fromEitherM :: Monad m => m (Either AppError r) -> ValidatorT m r
fromEitherM s = embedValidatorT $ (, mempty) <$> s

vFromEither :: Either ValidationError r -> PureValidatorT r
vFromEither = fromEither . first ValidationE

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
    liftIO t `catch` recoverOrRethrow        
    where
        recoverOrRethrow e = 
            case fromException (toException e) of
                Just (SomeAsyncException _) -> throwIO e
                Nothing                     -> appError $ mapErr e


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

runPureValidator :: Scopes -> PureValidatorT r -> (Either AppError r, ValidationState)
runPureValidator vc v = (runState $ runExceptT $ runReaderT v vc) mempty

runValidatorT :: Scopes -> ValidatorT m r -> m (Either AppError r, ValidationState)
runValidatorT vc v = (runStateT $ runExceptT $ runReaderT v vc) mempty

-- | Shorthand version for cases when we need to actually 
-- run IO or something similar like that
voidRun :: Functor m => Text -> ValidatorT m r -> m ()
voidRun t = void <$> runValidatorT (newScopes t)

validatorWarning :: Monad m => VWarning -> ValidatorT m ()
validatorWarning = vHoist . pureWarning

vError :: Monad m => ValidationError -> ValidatorT m r
vError = vHoist . vPureError

appError :: Monad m => AppError -> ValidatorT m r
appError = vHoist . pureError

pureWarning :: VWarning -> PureValidatorT ()
pureWarning warning = do 
    validationScope <- asks (^. typed)
    modify' (typed %~ (mWarning validationScope warning <>))

vPureError :: ValidationError -> PureValidatorT r
vPureError = pureError . ValidationE

vPureWarning :: ValidationError -> PureValidatorT r
vPureWarning e = pureError $ ValidationE e    

pureError :: AppError -> PureValidatorT r
pureError e = do
    validationScope <- asks (^. typed)
    modify' $ typed %~ (mError validationScope e <>)
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
                validationScope <- asks (^. typed)
                modify' $ typed %~ removeValidation validationScope predicate
                errorHandler 
            else throwError e


vWarn :: Monad m =>
        ValidationError -> ValidatorT m ()
vWarn = validatorWarning . VWarning . ValidationE

appWarn :: Monad m =>
            AppError -> ValidatorT m ()
appWarn = validatorWarning . VWarning

askScopes :: MonadReader r m => m r
askScopes = ask

inSubVScope :: Monad m => 
              Text -> ValidatorT m r -> ValidatorT m r
inSubVScope = inSubVScope' TextFocus

inSubObjectVScope :: Monad m => 
                    Text -> ValidatorT m r -> ValidatorT m r
inSubObjectVScope = inSubVScope' ObjectFocus

inSubVScope' :: Monad m => 
                (a -> Focus) -> a -> ValidatorT m r -> ValidatorT m r
inSubVScope' c t = local (& typed @VScope %~ subScope' c t)

inSubMetricScope' :: Monad m => 
                (a -> Focus) -> a -> ValidatorT m r -> ValidatorT m r
inSubMetricScope' c t = local (& typed @MetricScope %~ subScope' c t)

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
timedMetric p = timedMetric' p (\elapsed -> (& #totalTimeMs .~ TimeMs elapsed))

timedMetric' :: forall m metric r . 
                (MonadIO m, 
                 MetricC metric, 
                 HasField "totalTimeMs" metric metric TimeMs TimeMs) =>                 
                Proxy metric 
            -> (Int64 -> metric -> metric)                 
            -> ValidatorT m r 
            -> ValidatorT m r
timedMetric' _ f v = do
    vp <- askScopes
    ((r, vs), elapsed) <- appLift $ timedMS $ runValidatorT vp v          
    embedState vs
    updateMetric (f elapsed)
    either appError pure r 


getMetric :: forall metric m . 
            (Monad m, MetricC metric) => 
            ValidatorT m (Maybe metric)
getMetric = vHoist getPureMetric

getPureMetric :: forall metric . MetricC metric => 
                 PureValidatorT (Maybe metric)
getPureMetric = do 
    metricScope <- asks (^. typed)
    metricMap   <- gets (^. typed . metricLens)
    pure $ lookupMetric metricScope metricMap
            

recover :: Monad m => ValidatorT m a -> ValidatorT m () -> ValidatorT m a
recover tryF finallyF = 
    tryIt `catchError` catchIt
  where
    tryIt = do  
        z <- tryF 
        finallyF
        pure z
    catchIt e = do
        finallyF
        throwError e            


timeoutVT :: Seconds -> ValidatorT IO a -> ValidatorT IO a -> ValidatorT IO a
timeoutVT s toDo timedOut = do 
    let Seconds t = s
    vp <- ask 
    z <- liftIO $ timeout (1_000_000 * fromIntegral t) (runValidatorT vp toDo)
    maybe timedOut (embedValidatorT . pure) z    


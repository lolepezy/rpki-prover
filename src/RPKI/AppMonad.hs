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
import           Data.Proxy
import           Data.Text                   (Text)

import           System.Timeout

import           RPKI.Domain
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

fromValue :: Either AppError r -> PureValidatorT r
fromValue r = lift $ ExceptT $ pure r

fromEither :: Either AppError r -> PureValidatorT r
fromEither z =
    case z of 
        Left e -> do 
            validationScope <- asks (^. typed)
            modify' $ typed %~ (mError validationScope e <>)
            fromValue z
        Right _ -> 
            fromValue z

fromEitherM :: Monad m => m (Either AppError r) -> ValidatorT m r
fromEitherM s = 
    appLift s >>= \case 
        Left e  -> appError e
        Right r -> pure r    

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
fromTry mapErr t = fromTryM mapErr (liftIO t)

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

runPureValidator :: Scopes -> PureValidatorT r -> (Either AppError r, ValidationState)
runPureValidator vc v = (runState $ runExceptT $ runReaderT v vc) mempty

runValidatorT :: Scopes -> ValidatorT m r -> m (Either AppError r, ValidationState)
runValidatorT vc v = (runStateT $ runExceptT $ runReaderT v vc) mempty

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

vPureWarning :: ValidationError -> PureValidatorT ()
vPureWarning e = pureWarning $ VWarning $ ValidationE e    

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


withCurrentScope :: (Scopes -> ValidationState -> a) -> PureValidatorT a
withCurrentScope f = do 
    scopes <- askScopes
    f scopes <$> get    

vWarn :: Monad m => ValidationError -> ValidatorT m ()
vWarn = validatorWarning . VWarning . ValidationE

appWarn :: Monad m => AppError -> ValidatorT m ()
appWarn = validatorWarning . VWarning

trace :: Monad m => Trace -> ValidatorT m ()
trace t = modify' $ typed %~ (mTrace t <>)    

askScopes :: MonadReader r m => m r
askScopes = ask

inSubVScope :: Monad m => Text -> ValidatorT m r -> ValidatorT m r
inSubVScope = vFocusOn TextFocus

inSubLocationScope :: Monad m => URI -> ValidatorT m r -> ValidatorT m r
inSubLocationScope = vFocusOn LocationFocus

vFocusOn :: Monad m => (a -> Focus) -> a -> ValidatorT m r -> ValidatorT m r
vFocusOn c f = local (typed @VScope %~ subScope c f)

metricFocusOn :: Monad m => (a -> Focus) -> a -> ValidatorT m r -> ValidatorT m r
metricFocusOn c t = local (typed @MetricScope %~ subScope c t)

updateMetric :: forall metric m . 
                (Monad m, MetricC metric) => 
                (metric -> metric) -> ValidatorT m ()
updateMetric f = vHoist $ do 
    mp <- asks (^. typed)    
    modify' (typed . metricLens %~ updateMetricInMap mp f)    

timedMetric :: forall m metric r . 
                (MonadIO m, 
                 MetricC metric, 
                 HasField "totalTimeMs" metric metric TimeMs TimeMs) =>                 
                Proxy metric -> ValidatorT m r -> ValidatorT m r
timedMetric p = timedMetric' p (\elapsed -> #totalTimeMs .~ elapsed)

timedMetric' :: forall m metric r . 
                (MonadIO m, 
                 MetricC metric, 
                 HasField "totalTimeMs" metric metric TimeMs TimeMs) =>                 
                Proxy metric 
            -> (TimeMs -> metric -> metric)                 
            -> ValidatorT m r 
            -> ValidatorT m r
timedMetric' _ f v = do
    vp <- askScopes
    ((r, vs), elapsed) <- appLift $ timedMS $ runValidatorT vp v          
    embedState vs
    updateMetric (f elapsed)
    vHoist $ fromValue r
            

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
    scopes <- askScopes 
    z <- liftIO $ timeout (1_000_000 * fromIntegral t) (runValidatorT scopes toDo)
    maybe timedOut (embedValidatorT . pure) z    


andThen :: ValidatorT IO a -> ValidatorT IO () -> ValidatorT IO a
andThen f action = do
    !z <- f
    action
    pure $! z
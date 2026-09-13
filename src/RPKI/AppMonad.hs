{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module RPKI.AppMonad where

import           Control.Lens

import           Data.Bifunctor              (Bifunctor (first))
import           Data.Generics.Product       (HasField)
import           Data.Generics.Product.Typed
import           Data.Hourglass
import           Data.Proxy
import           Data.Text                   (Text)

import           Effectful
import           Effectful.Concurrent          (Concurrent, runConcurrent)
import           Effectful.Error.Static
import           Effectful.Exception           (SomeException, catchSync)
import           Effectful.Reader.Static
import           Effectful.State.Static.Shared
import           Effectful.Timeout             (Timeout, runTimeout, timeout)

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Time


-- | The validator effect set.
type Validator es =
    ( Reader Scopes         :> es
    , Error AppError        :> es
    , State ValidationState :> es
    )

{- | Validator effects plus IO.

   Deliberately does *not* include `Concurrent` or `Timeout`: those are added
   per function, so a signature says whether it can fork or time out.
   `AppEffects` still provides both and `runValidatorIO` discharges them.
-}
type ValidatorIO es = (Validator es, IOE :> es)

-- | Concrete stack discharged by 'runValidatorIO'.
type AppEffects =
    '[Reader Scopes, Error AppError, State ValidationState, Concurrent, Timeout, IOE]

-- | Concrete stack discharged by 'runValidatorPure'.
type PureEffects = '[Reader Scopes, Error AppError, State ValidationState]


{- | Run a validator inside an enclosing effect stack.

   `State` is discharged last, i.e. it is the outermost handler, mirroring
   `StateT` sitting under `ExceptT` in the old stack: validations recorded
   before an error survive the error.

   It is fine (and used a lot) for `es` to already contain any of these three
   effects -- the newly pushed handlers shadow the outer ones, and `runError`
   tags its exception with a fresh `Unique` so an inner `throwError` can never
   be caught by an outer `runError`.
-}
runValidator :: Scopes
             -> Eff (Reader Scopes : Error AppError : State ValidationState : es) a
             -> Eff es (Either AppError a, ValidationState)
runValidator scopes = runState mempty . runErrorNoCallStack . runReader scopes

-- | Run a validator all the way down to IO
runValidatorIO :: Scopes -> Eff AppEffects a -> IO (Either AppError a, ValidationState)
runValidatorIO scopes = runEff . runTimeout . runConcurrent . runValidator scopes

runValidatorPure :: Scopes -> Eff PureEffects a -> (Either AppError a, ValidationState)
runValidatorPure scopes = runPureEff . runValidator scopes

-- | Monomorphic wrappers around the `Reader`/`State` operations.
askScopes :: Reader Scopes :> es => Eff es Scopes
askScopes = ask

localScopes :: Reader Scopes :> es => (Scopes -> Scopes) -> Eff es a -> Eff es a
localScopes = local

getVState :: State ValidationState :> es => Eff es ValidationState
getVState = get

putVState :: State ValidationState :> es => ValidationState -> Eff es ()
putVState = put

modifyVState :: State ValidationState :> es => (ValidationState -> ValidationState) -> Eff es ()
modifyVState = modify


-- Lifting Either into the validator ------------------------------------------

-- | Short-circuit on `Left` without recording the error in `Validations`.
fromValue :: forall r es . Error AppError :> es => Either AppError r -> Eff es r
fromValue = either throwError pure

-- | Short-circuit on `Left`, recording the error in `Validations` first.
fromEither :: forall r es . Validator es => Either AppError r -> Eff es r
fromEither = either appError pure

fromEitherM :: forall r es . Validator es => Eff es (Either AppError r) -> Eff es r
fromEitherM s = s >>= either appError pure

vFromEither :: forall r es . Validator es => Either ValidationError r -> Eff es r
vFromEither = fromEither . first ValidationE


-- State plumbing --------------------------------------------------------------

embedValidatorT :: Validator es => Eff es (Either AppError r, ValidationState) -> Eff es r
embedValidatorT s = do
    (v, w) <- s
    modifyVState (<> w)
    fromValue v

embedState :: State ValidationState :> es => ValidationState -> Eff es ()
embedState w = modifyVState (<> w)


-- Exceptions ------------------------------------------------------------------

{- NOTE: `Effectful.Exception.catchSync` is re-exported instead of the
   hand-rolled `catchSync` this module used to define; it has the same
   semantics (catch synchronous exceptions, rethrow asynchronous ones).
-}
fromTry :: (Validator es, IOE :> es) => (SomeException -> AppError) -> IO r -> Eff es r
fromTry mapErr t = fromTryM mapErr (liftIO t)

fromTryM :: Validator es => (SomeException -> AppError) -> Eff es r -> Eff es r
fromTryM mapErr t = t `catchSync` (appError . mapErr)

fromTryEither :: (Validator es, IOE :> es)
              => (SomeException -> AppError) -> IO (Either AppError r) -> Eff es r
fromTryEither mapErr t = fromTry mapErr t >>= either appError pure


-- Errors and warnings ----------------------------------------------------------

appError :: Validator es => AppError -> Eff es r
appError e = do
    scopes <- askScopes
    modifyVState $ typed %~ (mError (scopes ^. typed) e <>)
    throwError e

vError :: Validator es => ValidationError -> Eff es r
vError = appError . ValidationE

-- | Record a warning against the current validation scope and carry on.
validatorWarning :: Validator es => VWarning -> Eff es ()
validatorWarning warning = do
    scopes <- askScopes
    modifyVState (typed %~ (mWarning (scopes ^. typed) warning <>))

appWarn :: Validator es => AppError -> Eff es ()
appWarn = validatorWarning . VWarning

vWarn :: Validator es => ValidationError -> Eff es ()
vWarn = appWarn . ValidationE

trace :: State ValidationState :> es => Trace -> Eff es ()
trace t = modifyVState $ typed %~ (mTrace t <>)

-- Error recovery -----------------------------------------------------------------

catchAndEraseError :: Validator es =>
                        Eff es r
                    -> (AppError -> Bool)
                    -> Eff es r
                    -> Eff es r
catchAndEraseError f predicate errorHandler =
    f `catchError` \cs e ->
        if predicate e
            then do
                scopes <- askScopes
                modifyVState $ typed %~ removeValidation (scopes ^. typed) predicate
                errorHandler
            else rethrowError cs e

recover :: Validator es => Eff es a -> Eff es () -> Eff es a
recover tryF finallyF =
    catchError @AppError tryIt catchIt
  where
    tryIt = do
        z <- tryF
        finallyF
        pure z
    catchIt cs e = do
        finallyF
        rethrowError cs e


-- Scopes ---------------------------------------------------------------------------

withCurrentScope :: Validator es => (Scopes -> ValidationState -> a) -> Eff es a
withCurrentScope f = f <$> askScopes <*> getVState

vFocusOn :: Reader Scopes :> es => (a -> Focus) -> a -> Eff es r -> Eff es r
vFocusOn c f = localScopes (typed @VScope %~ subScope c f)

metricFocusOn :: Reader Scopes :> es => (a -> Focus) -> a -> Eff es r -> Eff es r
metricFocusOn c t = localScopes (typed @MetricScope %~ subScope c t)

inSubVScope :: Reader Scopes :> es => Text -> Eff es r -> Eff es r
inSubVScope = vFocusOn TextFocus

inSubLocationScope :: Reader Scopes :> es => URI -> Eff es r -> Eff es r
inSubLocationScope = vFocusOn LocationFocus

vFocusOnLocated :: (Reader Scopes :> es, WithHash a) => Located a -> Eff es r -> Eff es r
vFocusOnLocated (Located locations x) =
    case locations of
        Just ls -> vFocusOn LocationFocus (getURL $ pickLocation ls)
        Nothing -> vFocusOn HashFocus (getHash x)


-- Metrics ---------------------------------------------------------------------------

-- NOTE: keep the `forall metric es` order, call sites use `updateMetric @RrdpMetric @_`.
updateMetric :: forall metric es .
                (Validator es, MetricC metric) =>
                (metric -> metric) -> Eff es ()
updateMetric f = do
    scopes <- askScopes
    modifyVState (typed . metricLens %~ updateMetricInMap (scopes ^. typed) f)

timedMetric :: forall metric es r .
                (ValidatorIO es,
                 MetricC metric,
                 HasField "totalTimeMs" metric metric TimeMs TimeMs) =>
                Proxy metric -> Eff es r -> Eff es r
timedMetric p = timedMetric' p (\elapsed -> #totalTimeMs .~ elapsed)

{- | Time an action and record the elapsed time in the metric of the current
   metric scope, whether or not the action failed.
-}
timedMetric' :: forall metric es r .
                (ValidatorIO es,
                 MetricC metric,
                 HasField "totalTimeMs" metric metric TimeMs TimeMs) =>
                Proxy metric
            -> (TimeMs -> metric -> metric)
            -> Eff es r
            -> Eff es r
timedMetric' _ f v = do
    saved <- getVState
    putVState mempty
    (r, elapsed) <- timedMS $ tryError @AppError v
    inner <- getVState
    putVState (saved <> inner)
    updateMetric (f elapsed)
    either (uncurry rethrowError) pure r


-- Misc ---------------------------------------------------------------------------

timeoutVT :: (ValidatorIO es, Timeout :> es) => Seconds -> Eff es a -> Eff es a -> Eff es a
timeoutVT s toDo timedOut = do
    let Seconds t = s
    timeout (1_000_000 * fromIntegral t) toDo >>= maybe timedOut pure


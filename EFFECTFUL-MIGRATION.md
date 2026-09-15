# Migrating `ValidatorT` from MTL to `effectful`

Status: **implemented** on branch `effectful`. Written against `b480c81d`
(branch `ui-redesign-0.11`), GHC 9.10.3, cabal 3.16.1.0, effectful 2.7.1.0 /
effectful-core 2.7.1.1.

The plan below is kept as written, with a **§11 "What the implementation actually
hit"** appended: the traps that only showed up once the compiler got involved.
Read §11 alongside §7 — three of its items were not predicted here at all.

---

## 1. Goal and scope

Replace the transformer stack

```haskell
type ValidatorTCurried m = ReaderT Scopes (ExceptT AppError (StateT ValidationState m))
type ValidatorT m r      = ValidatorTCurried m r
type PureValidatorT r    = ReaderT Scopes (ExceptT AppError (State ValidationState)) r
```

with `effectful`'s `Eff` and three static effects (`Reader Scopes`, `Error AppError`,
`State ValidationState`), plus `Concurrent`, `Timeout` and `IOE` for the IO-capable half.

**In scope**: `src/RPKI/AppMonad.hs` and the 36 modules that mention `ValidatorT` /
`PureValidatorT`, plus `src/RPKI/Parallel.hs`.

**Out of scope** (explicitly *not* part of this migration, possible follow-ups in §10):
* Turning `AppContext s`, `AppLogger` or `DB` into effects — they stay explicit arguments.
* Moving `Workflow.hs`, `Main.hs`, `HttpServer.hs`, `RtrServer.hs` into `Eff`. They stay
  in `IO` and call across the boundary through `runValidatorIO` (§4.3).
* Any change to `Reporting.hs` types (`Scopes`, `ValidationState`, `AppError`, …).

### Why this is worth doing

* `PureValidatorT` and `ValidatorT m` collapse into one type. **`vHoist` disappears
  entirely — 67 call sites of pure noise.**
* `ValidatorTCurried`, the `mmorph`/`generalize` hack, and the `lift . lift . lift` in
  `appLift` all disappear.
* The `MonadBaseControl`/`StM` machinery in `Parallel.hs` goes away, and with it a
  currently-silent bug: `bracketChanClosable`'s producer `ValidationState` is dropped on
  the floor by `restoreM` (§7.4).
* Effect order stops mattering: `runError`/`runState` commute in `effectful` (§5.1).

---

## 2. Current-state inventory (measured)

| Metric | Count |
|---|---|
| `.hs` files in `src app test bench` | 99 |
| Files mentioning `ValidatorT` | 36 |
| Files mentioning `PureValidatorT` | 17 |
| Lines mentioning `ValidatorT` | 261 (178 of them type signatures) |
| `ValidatorT IO` occurrences | 74 |
| `ValidatorT <var>` occurrences | 86 |
| Modules importing `Control.Monad.{Except,Reader,State,Morph}` | 7 |

The public API surface of `AppMonad.hs` is small and stable — 35 combinators. Usage
outside `AppMonad.hs`:

```
vError 80   vHoist 67   appError 56   vPureError 52   runValidatorT 49   vFocusOn 33
appWarn 26  fromEither 24   inSubLocationScope 22   throwError 20*  fromTry 18
updateMetric 17   pureError 16   vWarn 12   runPureValidator 12   embedState 11
timedMetric 10   askScopes 10   catchError 6   timeoutVT 5   embedValidatorT 4
validatorT 3   inSubVScope 3   catchSync 3   withCurrentScope 2   recover 2
fromEitherM 2   vFromEither 1   validatorWarning 1   pureWarning 1   metricFocusOn 1
catchAndEraseError 1   andThen 1   fromValue 0   appLift 0
```

`*` — all 20 `throwError` hits outside `AppMonad.hs` are in `Http/HttpServer.hs` and
belong to **Servant's `Handler`**, not `ValidatorT`. That module is untouched by this
migration; no name clash, because it never imports `AppMonad`.

`fromValue` and `appLift` have zero external users — they can be dropped or kept private.

**Because ~90 % of call sites go through these combinators rather than raw `mtl`
operations, the migration is mostly a type-signature rewrite.** If the combinator names
and argument orders are preserved (they can be), the bodies of most modules do not change.

---

## 3. Dependency and build changes

`package.yaml` is **stale and untracked** (commit `9d70bd14` "Don't track package.yaml,
it's generated"; it still says version 0.10.1 and `asn1-encoding`). **Edit
`rpki-prover.cabal` directly.**

In the `common shared` stanza's `build-depends`, add:

```
    , effectful
    , effectful-core
```

Pin in `cabal.project` (`effectful-core` 2.7.1.1 / `effectful` 2.7.1.0 are current and
`tested-with: GHC == 9.10.3`):

```
constraints:
    ...
  , effectful >= 2.7 && < 2.8
  , effectful-core >= 2.7 && < 2.8
```

Then `cabal freeze` to refresh `cabal.project.freeze`.

**No new default extensions are needed.** `default-language: GHC2024` already implies
`DataKinds`, `TypeOperators`, `ConstraintKinds`, `RankNTypes`, `ScopedTypeVariables`,
`FlexibleContexts`, `TypeApplications` and `MonoLocalBinds` — everything the effect
encoding requires. (`MonoLocalBinds` is also the source of trap §7.1, read it.)

Dependencies that become removable **only after the final cleanup pass** (§9), and only
if nothing else uses them — verify with `grep` before deleting:

* `mmorph` — after `vHoist` is deleted, `Control.Monad.Morph` has no other user.
* `lifted-base`, `lifted-async`, `monad-control` — after every `Eff`-typed module moves to
  `Effectful.Exception` / `Effectful.Concurrent.Async`. Note `RtrServer.hs`,
  `RRDP/Parse/Xeno.hs` and `Main.hs` use `Control.Exception.Lifted` at `m ~ IO`, which is
  fine and can stay; check each before removing the dependency.
* `mtl` / `transformers` — almost certainly still needed transitively (Servant, conduit,
  `effectful-core` itself depends on both). **Do not remove.**

---

## 4. Target design

### 4.1 Effect aliases (new top of `AppMonad.hs`)

```haskell
import           Effectful
import           Effectful.Concurrent          (Concurrent, runConcurrent)
import           Effectful.Error.Static
import           Effectful.Reader.Static
import           Effectful.State.Static.Local  -- see §4.4 for Local vs Shared
import           Effectful.Timeout             (Timeout, runTimeout, timeout)

-- | Effects every validator computation needs. Replaces both
--   @Monad m => ValidatorT m@ and @PureValidatorT@.
type Validator es =
    ( Reader Scopes        :> es
    , Error AppError       :> es
    , State ValidationState :> es
    )

-- | Validator effects plus IO, forking and timeouts. Replaces @ValidatorT IO@.
type ValidatorIO es = (Validator es, Concurrent :> es, Timeout :> es, IOE :> es)

-- | Concrete stack discharged by 'runValidatorIO'.
type AppEffects =
    '[Reader Scopes, Error AppError, State ValidationState, Concurrent, Timeout, IOE]

-- | Concrete stack discharged by 'runPureValidator'.
type PureEffects = '[Reader Scopes, Error AppError, State ValidationState]
```

Signature translation table — this is the whole mechanical part of the job:

| Today | Tomorrow |
|---|---|
| `f :: ValidatorT IO r` | `f :: ValidatorIO es => Eff es r` |
| `f :: Monad m => ValidatorT m r` | `f :: Validator es => Eff es r` |
| `f :: MonadIO m => ValidatorT m r` | `f :: ValidatorIO es => Eff es r` |
| `f :: PureValidatorT r` | `f :: Validator es => Eff es r` |
| `Stream (Of s) (ValidatorTCurried m) ()` | `Stream (Of s) (Eff es) ()` |
| `vHoist act` | `act` |
| `appLift io` | `liftIO io` |
| `modify' f` | `modify f` (effectful's `modify` is already WHNF-strict; there is no `modify'`) |
| `x \`catchError\` \e -> h e` | ``x `catchError` \_cs e -> h e`` (handler gains a leading `CallStack`) |

### 4.2 Runner inside an existing stack

```haskell
runValidatorT
    :: Scopes
    -> Eff (Reader Scopes : Error AppError : State ValidationState : es) a
    -> Eff es (Either AppError a, ValidationState)
runValidatorT scopes = runState mempty . runErrorNoCallStack . runReader scopes
```

Effects are discharged head-first, so `State` being last in the list makes it outermost —
mirroring `StateT` sitting under `ExceptT` today, and giving the identical
"validations survive an error" behaviour (§5.1).

Nesting is safe: `es` may already contain `Reader Scopes` / `Error AppError` /
`State ValidationState`. `effectful`'s `(:>)` has
`instance {-# OVERLAPPING #-} e :> (e : es)` before `instance e :> es => e :> (x : es)`,
so **the newly pushed handler shadows the outer one**, and `runError` tags its
`ErrorWrapper` with a fresh `Unique` so an inner `throwError` cannot be caught by an outer
`runError`. This is exactly what `TopDown.hs` relies on at lines 1009, 1024, 1039, 1196
and 1401.

### 4.3 Runner at the IO boundary

```haskell
runValidatorIO :: Scopes -> Eff AppEffects a -> IO (Either AppError a, ValidationState)
runValidatorIO scopes = runEff . runTimeout . runConcurrent . runValidatorT scopes

runPureValidator :: Scopes -> Eff PureEffects a -> (Either AppError a, ValidationState)
runPureValidator scopes = runPureEff . runValidatorT scopes
```

`runPureEff` works with `Error` because `Error`'s `DispatchOf` is
`Static NoSideEffects` — no `IOE` needed.

**Naming decision.** Keep the name `runValidatorT` for the `Eff`-nested version (§4.2),
because that is what the hot loops in `TopDown.hs` use with an inline `do` block. Add
`runValidatorIO` for the ~25 call sites that sit in plain `IO`. Sites to switch to
`runValidatorIO`:

* `app/Main.hs`: 128, 171, 235, 593
* `src/RPKI/Workflow.hs`: 605, 628, 695, 904, 961
* `test/src/RPKI/TestCommons.hs`: 52
* `test/src/RPKI/Validation/TopDownSpec.hs`: 60, 61, 65, 82, 83, 94
* `test/src/RPKI/Store/DatabaseSpec.hs`: 356, 357, 1100, 1104, 1108, 1130
* `test/src/RPKI/SLURM/SlurmSpec.hs`: 91, 126, 160
* `test/src/RPKI/AppMonadSpec.hs`: all `runValidatorT` uses
* `bench/Main.hs`: 175

Everything else (`TopDown.hs`, `Database.hs`'s `appTx`, `Rsync.hs`, `AppMonad.hs`'s own
combinators) stays on `runValidatorT`.

### 4.4 Decision: `State.Static.Local` vs `State.Static.Shared`

`Effectful.State.Static.Local` and `Effectful.State.Static.Shared` export **identical
names** (`runState`, `evalState`, `execState`, `get`, `gets`, `put`, `state`, `modify`,
`stateM`, `modifyM`), so this is a one-line import swap in `AppMonad.hs` and nothing else.

They differ only under `Effectful.Concurrent.Async`, every function of which **clones the
effect environment** for the child computation:

* **Local** — the child gets a private copy. Its `ValidationState` updates are **lost** to
  the parent. Matches `StateT`-under-`lifted-async` closely.
* **Shared** — the rep is an `MVar` and cloning copies the reference, so child updates are
  visible to the parent.

**Recommendation: use `Shared`.** Rationale:

1. With `Local`, `Parallel.hs` cannot be ported without giving `foldPipeline`,
   `txFoldPipeline` and `bracketChanClosable` rank-2 (`forall es'.`) parameters so each
   branch can be re-run under a fresh `runValidatorT` and merged by hand. For
   `txFoldPipeline` this infects the `withTx` parameter too and gets genuinely unpleasant.
   With `Shared`, `Parallel.hs` is an import swap plus a constraint change.
2. `Shared` *removes* an existing silent bug: today `bracketChanClosable`'s producer state
   is discarded and the consumer's *replaces* rather than merges the outer state (§7.4).
3. Cost is one `MVar` op per `ValidationState` read/write. These are not in the per-object
   hot path — `vWarn`/`vError` fire only on problems, `updateMetric` fires per repository
   or per TA, `get` is read in exactly two places (`withCurrentScope`). And `TopDown.hs`
   already wraps per-child work in `runValidatorT`, which allocates a *fresh* `MVar` per
   frame, so cross-thread contention on any single `MVar` stays low.

If profiling later shows `MVar` contention, switching back to `Local` is one import line
plus the rank-2 work in `Parallel.hs`. Note the choice in a comment in `AppMonad.hs`.

### 4.5 Full target `src/RPKI/AppMonad.hs`

Names, argument order and `forall` order are preserved throughout so that call sites do
not change. **Keep the `forall metric es` order in `updateMetric`** — call sites use
`updateMetric @RrdpMetric @_ (...)` (e.g. `RrdpFetch.hs:192`) and rely on it.

```haskell
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
import           Effectful.State.Static.Shared -- see §4.4; swap for .Local if needed
import           Effectful.Timeout             (Timeout, runTimeout, timeout)

import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Time

-- ---------------------------------------------------------------- effect aliases
-- (as in §4.1)

-- ---------------------------------------------------------------- runners
-- runValidatorT / runValidatorIO / runPureValidator as in §4.2, §4.3

-- ---------------------------------------------------------------- transitional shim
-- PureValidatorT and ValidatorT are the same thing now. Kept as `id` for the
-- mechanical phase so that 67 call sites keep their indentation; deleted in §9.
vHoist :: Eff es a -> Eff es a
vHoist = id
{-# INLINE vHoist #-}
-- Do NOT add a DEPRECATED pragma: the library builds with -Wall and it would
-- produce 67 warnings for the whole transition.

-- ---------------------------------------------------------------- lifting Either
fromValue :: Error AppError :> es => Either AppError a -> Eff es a
fromValue = either throwError pure

fromEither :: Validator es => Either AppError a -> Eff es a
fromEither = either pureError pure

fromEitherM :: Validator es => Eff es (Either AppError a) -> Eff es a
fromEitherM s = s >>= either appError pure

vFromEither :: Validator es => Either ValidationError a -> Eff es a
vFromEither = fromEither . first ValidationE

-- ---------------------------------------------------------------- state plumbing
validatorT :: Validator es => Eff es (Either AppError a, ValidationState) -> Eff es a
validatorT s = do
    (v, w) <- s
    put w
    fromValue v

embedValidatorT :: Validator es => Eff es (Either AppError a, ValidationState) -> Eff es a
embedValidatorT s = do
    (v, w) <- s
    modify (<> w)
    fromValue v

embedState :: State ValidationState :> es => ValidationState -> Eff es ()
embedState w = modify (<> w)

-- ---------------------------------------------------------------- exceptions
-- RPKI.AppMonad.catchSync is DELETED; Effectful.Exception.catchSync has the same
-- behaviour (catchIf isSyncException) and is re-exported here so that the three
-- external call sites keep compiling.

fromTry :: (Validator es, IOE :> es) => (SomeException -> AppError) -> IO a -> Eff es a
fromTry mapErr t = fromTryM mapErr (liftIO t)

fromTryM :: Validator es => (SomeException -> AppError) -> Eff es a -> Eff es a
fromTryM mapErr t = t `catchSync` (appError . mapErr)

fromTryEither :: (Validator es, IOE :> es)
              => (SomeException -> AppError) -> IO (Either AppError a) -> Eff es a
fromTryEither mapErr t = fromTry mapErr t >>= either appError pure

-- ---------------------------------------------------------------- errors & warnings
-- After the migration vError == vPureError, appError == pureError and
-- validatorWarning == pureWarning. All six names are kept so call sites don't move;
-- collapsing them is an optional follow-up (§10).

pureError :: Validator es => AppError -> Eff es a
pureError e = do
    validationScope <- asks (^. typed)
    modify $ typed %~ (mError validationScope e <>)
    throwError e

pureWarning :: Validator es => VWarning -> Eff es ()
pureWarning warning = do
    validationScope <- asks (^. typed)
    modify (typed %~ (mWarning validationScope warning <>))

vPureError :: Validator es => ValidationError -> Eff es a
vPureError = pureError . ValidationE

vPureWarning :: Validator es => ValidationError -> Eff es ()
vPureWarning = pureWarning . VWarning . ValidationE

appError, vError, validatorWarning, vWarn, appWarn -- same bodies as today, minus vHoist

trace :: State ValidationState :> es => Trace -> Eff es ()
trace t = modify $ typed %~ (mTrace t <>)

-- ---------------------------------------------------------------- error recovery
catchAndEraseError :: Validator es
                   => Eff es a -> (AppError -> Bool) -> Eff es a -> Eff es a
catchAndEraseError f predicate errorHandler =
    f `catchError` \cs e ->
        if predicate e
            then do
                validationScope <- asks (^. typed)
                modify $ typed %~ removeValidation validationScope predicate
                errorHandler
            else rethrowError cs e   -- rethrowError keeps the original CallStack

recover :: Validator es => Eff es a -> Eff es () -> Eff es a
recover tryF finallyF = tryIt `catchError` catchIt
  where
    tryIt      = do { z <- tryF; finallyF; pure z }
    catchIt cs e = do { finallyF; rethrowError cs e }

-- ---------------------------------------------------------------- scopes
askScopes :: Reader Scopes :> es => Eff es Scopes
askScopes = ask

withCurrentScope :: Validator es => (Scopes -> ValidationState -> a) -> Eff es a
withCurrentScope f = f <$> askScopes <*> get

vFocusOn :: Reader Scopes :> es => (a -> Focus) -> a -> Eff es r -> Eff es r
vFocusOn c f = local (typed @VScope %~ subScope c f)

metricFocusOn :: Reader Scopes :> es => (a -> Focus) -> a -> Eff es r -> Eff es r
metricFocusOn c t = local (typed @MetricScope %~ subScope c t)

inSubVScope :: Reader Scopes :> es => Text -> Eff es r -> Eff es r
inSubVScope = vFocusOn TextFocus

inSubLocationScope :: Reader Scopes :> es => URI -> Eff es r -> Eff es r
inSubLocationScope = vFocusOn LocationFocus

-- ---------------------------------------------------------------- metrics
updateMetric :: forall metric es . (Validator es, MetricC metric)
             => (metric -> metric) -> Eff es ()
updateMetric f = do
    mp <- asks (^. typed)
    modify (typed . metricLens %~ updateMetricInMap mp f)

timedMetric :: forall metric es r .
               (ValidatorIO es, MetricC metric,
                HasField "totalTimeMs" metric metric TimeMs TimeMs)
            => Proxy metric -> Eff es r -> Eff es r
timedMetric p = timedMetric' p (\elapsed -> #totalTimeMs .~ elapsed)

-- See §7.2: cannot be a nested runValidatorT any more, so the fresh-state
-- isolation is emulated explicitly to keep behaviour identical.
timedMetric' :: forall metric es r .
                (ValidatorIO es, MetricC metric,
                 HasField "totalTimeMs" metric metric TimeMs TimeMs)
             => Proxy metric -> (TimeMs -> metric -> metric) -> Eff es r -> Eff es r
timedMetric' _ f v = do
    saved <- get @ValidationState
    put (mempty :: ValidationState)
    (r, elapsed) <- timedMS $ tryError @AppError v
    inner <- get @ValidationState
    put (saved <> inner)
    updateMetric (f elapsed)
    either (uncurry rethrowError) pure r

-- ---------------------------------------------------------------- misc
timeoutVT :: ValidatorIO es => Seconds -> Eff es a -> Eff es a -> Eff es a
timeoutVT (Seconds t) toDo timedOut =
    timeout (1_000_000 * fromIntegral t) toDo >>= maybe timedOut pure

andThen :: Eff es a -> Eff es () -> Eff es a
andThen f action = do { !z <- f; action; pure $! z }
```

Deleted outright: `ValidatorTCurried`, `ValidatorT`, `PureValidatorT`, `appLift`,
`RPKI.AppMonad.catchSync` (replaced by the `Effectful.Exception` one). `vHoist` survives
as `id` until §9.

---

## 5. Semantics: what stays identical, what changes

### 5.1 Identical — state survives errors

Today `StateT` sits *under* `ExceptT`, so `ValidationState` written before a `throwError`
survives. `effectful` documents the same behaviour explicitly, and makes it independent of
handler order:

```
runEff . runState "Hi" . runError @String $ m2   ==>  (Right (),"Hi there!")
runEff . runError @String . runState "Hi" $ m2   ==>  Right ((),"Hi there!")
```

So `pureError` (write the error into `Validations`, then short-circuit),
`catchAndEraseError` and the `AppMonadSpec` nesting tests all behave the same.

### 5.2 Identical — `throwError` will not be swallowed by catch-alls

`effectful` implements `Error` with a real exception, `ErrorWrapper`, but deliberately
classifies it as **asynchronous**:

```haskell
instance Exception ErrorWrapper where
  toException   = asyncExceptionToException
  fromException = asyncExceptionFromException
```

Consequences, all verified against the existing catch-alls in this tree:

* `AppMonad.fromTryM`'s `catchSync` will **not** intercept a `throwError` — `catchSync` is
  `catchIf isSyncException`, and `ErrorWrapper` is async. Behaviour preserved.
* `Worker.hs:310-313` — `catches [Handler $ \e@(SomeAsyncException _) -> throwIO e,
  Handler $ \e@(SomeException _) -> complain …]`. The async handler comes **first** and
  rethrows, so an `ErrorWrapper` passing through is rethrown, not turned into "died in a
  strange way". Preserved — **but this ordering is now load-bearing; add a comment.**
* `Workflow.hs:1305 ignoreSync` — same shape, rethrows async. Preserved.
* `Store/SQLite.hs:135`'s `onException` rollback is never reached by an `ErrorWrapper`,
  because `appTx` wraps the body in `runValidatorT`, which catches the error inside the
  transaction and converts it to an explicit `TxRollbackException` (§6.5). Preserved.

**Still audit for any *new* `catch`/`handle` with a bare `SomeException` handler added in
`Eff`-typed code** — that would swallow `throwError`, unlike `ExceptT`. Use
`Effectful.Exception.catchSync` / `trySync` instead.

### 5.3 Changed — concurrent branches (see §4.4)

With `State.Static.Shared`, `ValidationState` written inside `async` / `concurrently` /
`pooledForConcurrentlyN` is now visible to the parent, where today it is lost or
overwritten. This affects `Parallel.hs` only (§7.4) and is a strict improvement, but it
means **more warnings may be reported than before** in RRDP delta and rsync load paths.
Expect and verify this in the tests of §8.

### 5.4 Changed — `timeoutVT` keeps partial state

Today `timeoutVT` runs `timeout t (runValidatorT scopes toDo)`; on timeout the whole
`(result, ValidationState)` pair is discarded, so warnings recorded before the timeout are
lost. The new one-liner (§4.5) lets whatever `toDo` already wrote stay. Five call sites:
`Fetch.hs:146`, `Fetch.hs:162`, `Fetch.hs:188`, `Rsync.hs:178`, `TopDown.hs:325`. Arguably
a fix; flag it in the commit message. If exact preservation is wanted instead, wrap the same
save/restore dance used in `timedMetric'`.

### 5.5 Changed — `catchError` handler arity

`Effectful.Error.Static.catchError`'s handler is `CallStack -> e -> Eff es a`. Six external
sites need `\_cs e ->` (or `\cs e ->` if the stack is then passed to `rethrowError`):
`Fetch.hs:185`, `RrdpFetch.hs:183`, `TopDown.hs:425`, `:440`, `:699`, `:751`.

### 5.6 Unchanged but worth knowing — `HasCallStack`

Every `effectful` operation carries `HasCallStack`. It costs a little and shows up in
profiles as extra `CallStack` allocation. There is no build flag to switch it off in
2.7.x; accept it. In exchange, an escaped-error `ErrorWrapper` prints the throw site.

---

## 6. Module-by-module plan

Order matters: the dependency chain is
`AppMonad → Parse/* → Validation/* → Store/Database → Rsync/RrdpFetch/Fetch → TopDown → Workflow/Main → tests`.
**The switch cannot be done incrementally per module** — `AppMonad.hs` changing type means
everything that imports it must compile against the new types in the same commit. Plan for
one large "make it compile" commit, then follow-up cleanup commits. Use
`cabal build lib:rpki-prover -j` in a loop and work the error list top-down.

### 6.1 `src/RPKI/AppMonad.hs` — rewrite

Per §4.5. Drop imports of `Control.Monad.Except/Morph/Reader/State.Strict`,
`Control.Monad.Trans.Control`, `Control.Exception.Lifted`, `System.Timeout`.

### 6.2 Leaf pure modules — signature-only

Purely mechanical `PureValidatorT r` → `Validator es => … -> Eff es r`. No body changes.

* `Parse/Internal/Common.hs` (115, 190, 420, 447), `Parse/Internal/Cert.hs` (6 sigs),
  `Parse/Internal/{Aspa,CRL,GBR,MFT,ROA,RSC,SPL,SignedObject}.hs` (1 each),
  `Parse/Parse.hs` (2)
* `Validation/ResourceValidation.hs` (24, 40, 84)
* `Validation/ObjectValidation.hs` — the largest (49 `PureValidatorT` sigs) but also the
  most mechanical; there is no IO, no concurrency, no `catchError` in it.
* `Validation/Common.hs` (40, 65, 81, 91) — `Monad m => ValidatorT m ()` → `Validator es => Eff es ()`
* `SLURM/SlurmProcessing.hs` (114 → `ValidatorIO es`, 132/158/169 → `Validator es`)

**Give every `where`-bound helper an explicit signature** — see trap §7.1.

### 6.3 `src/RPKI/Parallel.hs`

* `ValidatorTCurried m` vanishes: `Stream (Of s) (ValidatorTCurried m) ()` becomes
  `Stream (Of s) (Eff es) ()`. `streaming` needs only `Monad`/`MonadIO` from the base, both
  of which `Eff es` provides (`MonadIO` given `IOE :> es`).
* Swap `Control.Concurrent.Async.Lifted` → `Effectful.Concurrent.Async` and
  `Control.Exception.Lifted` → `Effectful.Exception`. Drop `Control.Monad.Trans.Control`.
* Constraints: `(MonadBaseControl IO m, MonadIO m)` → `(Concurrent :> es, IOE :> es)` for
  `bracketChanClosable`, and `ValidatorIO es` for `foldPipeline` / `txFoldPipeline`.
* **`bracketChanClosable` must stay validator-agnostic** — `TopDown.hs:290` calls it at
  plain `IO`, where there is no `Scopes`/`ValidationState` at all.
* **Do not leave `Control.Concurrent.Async.Lifted` in place "because `Eff` has a
  `MonadBaseControl` instance".** It does (`StM (Eff es) a = a`, `restoreM = pure`), so it
  *type-checks* — but `liftBaseWith` uses the `IOE` unlift strategy, which defaults to
  `SeqUnlift`, which **throws at runtime** if the unlifting function is used from another
  thread. `concurrently` does exactly that. This is trap §7.3.
* The bodies are otherwise unchanged; with `State.Static.Shared` the state merging is
  automatic (§4.4).

### 6.4 `src/RPKI/Rsync.hs`, `src/RPKI/RRDP/RrdpFetch.hs`, `src/RPKI/RRDP/Http.hs`, `src/RPKI/Fetch.hs`

* Signatures per §4.1. `Control.Exception.Lifted` → `Effectful.Exception`.
* `Rsync.hs:266-270` — `let task = runValidatorT s (readAndParseObject path …)` followed by
  `a <- liftIO $ async $ evaluate =<< task`. `runValidatorT` no longer returns `IO`. Use
  `Effectful.Concurrent.Async.async` and inline the run:
  ```haskell
  a <- async $ runValidatorT s (readAndParseObject path (RsyncU uri))
  ```
  `readAndParseObject` is an unsignatured `where` binding — **it needs an explicit
  `forall es'. ValidatorIO es' => …` signature** or this will not typecheck (§7.1).
* `RrdpFetch.hs:183` and `Fetch.hs:185` — `catchError` handler arity (§5.5).
* `RrdpFetch.hs:332,345` and `Rsync.hs:371` — `local` is `Effectful.Reader.Static.local`,
  same shape, no change beyond the import.

### 6.5 `src/RPKI/Store/Database.hs`

Only `appTx` / `appTxEx` (lines 1302–1360) are interesting. The transaction runners
(`withReadTx`, `withWriteTx`) take an **`IO` callback**, so an unlift is needed:

```haskell
appTx db f txF = do
    scopes <- askScopes
    r <- withSeqEffToIO $ \unlift ->
            txF db (\tx -> do
                z@(res, vs) <- unlift $ runValidatorT scopes (f tx)
                case res of
                    Left e  -> throwIO (TxRollbackException e vs)
                    Right _ -> pure z)
            `catch` \(TxRollbackException e vs) -> pure (Left e, vs)
    embedValidatorT (pure r)
```

`withSeqEffToIO` is correct here: SQLite runs the callback on the calling thread, and
`SeqUnlift` permits repeated *sequential* calls (so nested `roAppTx` still works). The
`throwIO`/`catch` in that snippet are `Control.Exception`'s, operating in plain `IO`
inside the callback — keep them as they are.

Note the `runValidatorT` inside the callback is what stops an `ErrorWrapper` from reaching
SQLite's `onException` rollback path (§5.2).

`DatabaseSpec.hs:1100-1112` covers exactly this — rollback on error, commit on success,
`TxRollbackException` visibility. Run it early.

### 6.6 `src/RPKI/Validation/TopDown.hs`

The largest consumer (34 `ValidatorT` lines) but structurally the easiest, because it
*already* does everything explicitly.

* Drop `liftIO` in front of the `forChildren` / `forAllChildren` blocks (1006, 1021, 1401)
  and in front of `runValidatorT` at 1196 — `runValidatorT` now returns `Eff es`, and
  `pooledForConcurrentlyN` comes from `Effectful.Concurrent.Async` instead of
  `UnliftIO.Async`. Everything else in those blocks (`askScopes`, `embedState`,
  `mconcat . map snd`) is unchanged.
* `Control.Concurrent.Async (forConcurrently)` at line 14 → `Effectful.Concurrent.Async`;
  `UnliftIO.Async (pooledForConcurrentlyN)` at line 47 → same module.
* `Control.Exception.Lifted` → `Effectful.Exception`.
* Four `catchError` handlers gain `\_cs` (425, 440, 699, 751).
* `thisScopeIssues` (1324) and `vUniqueFocusOn` (1707) read the state via
  `withCurrentScope`. **They depend on `runValidatorT` giving a fresh `mempty` state** —
  which it still does, because they sit inside inline `runValidatorT scopes $ do …` blocks
  at 1009/1024/1039. This is also why `timedMetric'` must keep its fresh-state isolation
  (§7.2): `TopDown.hs:366` wraps a whole TA in `timedMetric`, and `thisScopeIssues` runs
  inside it.
* All `where`-bound helpers with `ValidatorT IO` signatures (384, 486, 508, 566, 608, 787,
  921, 965, 1184, 1334-1337, 1502, 1582, 1603) → `forall es. ValidatorIO es => … Eff es …`
  with **their own explicit `forall`**, so they can be used both at the outer stack and
  inside a nested `runValidatorT` (§7.1).

### 6.7 `src/RPKI/Worker.hs`

`runWorker :: ValidatorT IO r` → `ValidatorIO es => Eff es r`. Swap
`Control.Exception.Lifted` → `Effectful.Exception` for the `catches`/`bracket` at 310/320.
Keep the `SomeAsyncException` handler first and add a comment explaining why (§5.2).

### 6.8 `src/RPKI/Workflow.hs` and `app/Main.hs` — the IO boundary

These stay in `IO`. Changes:

* `runValidatorT` → `runValidatorIO` at the sites listed in §4.3.
* `Workflow.hs:904` currently reads
  `runValidatorT (newScopes' RepositoryFocus url) $ runConcurrentlyIfPossible … $ fetchRepository …`.
  `runConcurrentlyIfPossible` is `MonadBaseControl IO m => … -> m a -> m a` and is being
  used at `m ~ ValidatorT IO`. It does not fork — it is STM plus a `bracket`-style
  `finally` — so retype it as `(Concurrent :> es, IOE :> es) => … -> Eff es a -> Eff es a`
  and use `Effectful.Exception.finally`. Same for `withWorkflowShared` (130) and
  `ignoreSync` (1305); check each caller's `m` and pick `Eff es` or plain `IO`.
* `AppState.hs:49` — **`readSlurm :: Maybe (ValidatorT IO Slurm)` is the one genuinely
  awkward spot.** A constrained type cannot be stored in a field without impredicativity.
  Options, in order of preference:
  1. Newtype with an explicit rank-2 field (`RankNTypes` is already on via GHC2021):
     ```haskell
     newtype SlurmReader = SlurmReader (forall es . ValidatorIO es => Eff es Slurm)
     -- field becomes:  readSlurm :: Maybe SlurmReader
     ```
     Three touch points: `AppState.hs:49`, `Main.hs:566-570`, `Workflow.hs:691-695`.
  2. Store the concrete stack: `Maybe (Eff AppEffects Slurm)`. Simpler, but then
     `Workflow.hs:695` must run it exactly with `runValidatorIO` — which it already does.
     **This is fine today and is the lower-friction choice**; switch to (1) only if a
     second consumer at a different stack appears.
  3. Pre-run it: `Maybe (IO (Either AppError Slurm, ValidationState))`. Loses the ability
     to pick scopes at the call site; not recommended.
* `Main.hs`'s `catch`/`finally` at 143, 200, 204, 257 are at `m ~ IO`;
  `Control.Exception.Lifted` keeps working there. Leave them.

### 6.9 Tests and benchmarks

* `test/src/RPKI/AppMonadSpec.hs` — `runValidatorT` → `runValidatorIO` throughout. This
  file is the **primary semantic oracle** for the migration:
  `runValidatorT . validatorT == id`, `forMShouldSavesState`, and above all
  `scopesShouldBeProperlyNested`, which exercises nested `timedMetric` +
  `vFocusOn` + `appWarn` + `appError` and asserts the exact scope→issue map. If §4.5's
  `timedMetric'` is right, this passes unchanged.
* `test/src/RPKI/TestCommons.hs:22` imports `runValidatorT` — re-point to `runValidatorIO`.
* `TopDownSpec.hs`, `DatabaseSpec.hs`, `SlurmSpec.hs`, `bench/Main.hs` — mechanical, plus
  dropping `vHoist` (`TopDownSpec.hs:61,83`, `bench/Main.hs:20,175`).

---

## 7. Traps — read before writing any code

### 7.1 `MonoLocalBinds` will bite, and it is the #1 source of confusing errors

GHC2024 implies `MonoLocalBinds`, so **`where`/`let` bindings without a type signature are
not generalised**. Any such helper used inside a nested `runValidatorT scopes $ …` (which
pushes three *new* effects onto `es`) will be inferred at the outer `es` and fail to
typecheck at the inner one.

> **Rule: every `where`-bound validator helper gets an explicit type signature with its
> own `forall es.`** — not the parent's scoped `es`.

Concretely: `Rsync.hs:271 readAndParseObject`, and every signatured `where` helper in
`TopDown.hs` (§6.6). The symptom is a "rigid type variable" error mentioning
`Reader Scopes : Error AppError : State ValidationState : es0`.

Related, and the reason the rule exists: **you cannot re-run an already-monomorphised
`Eff es a` under fresh handlers.** Neither `raise` nor `inject` helps — they re-index the
frames but the `(:>)` witnesses baked into the value still point at the *outer* handlers.
Inline `do` blocks are fine (GHC infers them at the extended stack); named values are not.

### 7.2 `timedMetric'` cannot be a nested `runValidatorT`

It takes its action as a *parameter* (`Eff es r`), so §7.1 applies. Emulate the fresh state
by hand — `get`/`put mempty`/run/`get`/`put (saved <> inner)` — as in §4.5. Do **not**
"simplify" it to a bare `tryError` + `updateMetric`: `TopDown.hs:366` wraps a whole TA in
`timedMetric`, and `thisScopeIssues` (1324) reads the state inside it to decide whether to
build manifest shortcuts. Leaking outer state into that read would silently suppress
shortcut creation.

### 7.3 `MonadBaseControl IO (Eff es)` exists and is a trap

`effectful-core` provides it for compatibility (`StM (Eff es) a = a`, `restoreM = pure`),
so old `lifted-base` / `lifted-async` code keeps *compiling* against `Eff`. But
`liftBaseWith` uses the `IOE` unlift strategy, which `runEff` sets to `SeqUnlift`, which
**throws at runtime** when the unlift function is called from a second thread.

* Anything that forks (`concurrently`, `async`, `race`, `mapConcurrently`) →
  `Effectful.Concurrent.Async`, and add `Concurrent :> es`.
* Anything that only catches/brackets in the same thread → `Effectful.Exception` (same
  names: `catch`, `try`, `throwIO`, `bracket`, `finally`, `handle`, `mask`, `catches`,
  `Handler`).

Never leave `Control.Concurrent.Async.Lifted` pointed at an `Eff`-typed action.
`Parallel.hs:103` (`bracketChanClosable`'s `concurrently`) is the one that would blow up.

### 7.4 `bracketChanClosable`'s state handling is currently wrong

Today, at `m ~ ValidatorT IO`, lifted-async's `concurrently` restores both branches with
`restoreM sa >> restoreM sb`. For `StateT`, `restoreM` *replaces* the state — so the
producer's `ValidationState` is discarded and the consumer's replaces (rather than merges
with) whatever the caller had. Likewise `foldPipeline`'s `wait` currently restores the
async's state into the consumer.

After the migration with `State.Static.Shared`, all of it merges properly. **Expect the
RRDP-delta and rsync-load paths to report validations they previously dropped.** This is a
fix, but it will change test expectations and possibly the UI's validation counts — verify
with §8.3 before assuming a regression.

### 7.5 Small things

* `modify'` does not exist in `effectful`; `modify` is already WHNF-strict. Plain rename.
* `throwError` requires `Show e`. `AppError` derives `Show`. Fine. (`throwError_` exists
  for types that do not.)
* `askScopes` is currently `MonadReader r m => m r` — fully general. Specialising it to
  `Reader Scopes :> es => Eff es Scopes` is what we want, but check the 10 call sites for
  any that relied on the generality. (Spot-check says none do.)
* Duplicated effects in the stack are legal and shadow correctly, but GHC's error messages
  when they *don't* unify are long. When stuck, add an explicit `@AppError` /
  `@ValidationState` type application to `tryError` / `get` / `put`.
* `Effectful.Timeout` is a separate effect from `Concurrent`; both must be in
  `AppEffects` and both discharged in `runValidatorIO`.

---

## 8. Verification

### 8.1 Build gates, in this order
1. `cabal build lib:rpki-prover -j` — expect a long error list; work top-down by module in
   the §6 order.
2. `cabal build --enable-tests --enable-benchmarks all`.
3. `cabal build -O2` (the library uses `-O2 -Wall`) — watch for newly-unused imports.

### 8.2 Test gates
1. `cabal test` — the full `rpki-prover-test` suite. `run-tests.sh` exists; check what it
   wraps.
2. `AppMonadSpec` first and on its own: it is the semantic oracle (§6.9).
3. `DatabaseSpec`'s tx-rollback trio (1100/1104/1108) second — it pins §6.5.
4. `TopDownSpec` third — it pins the shortcut/prevalidation paths that §7.2 protects.

### 8.3 Behavioural diff against `master`
Because §5.3, §5.4 and §7.4 change *how much* is reported (never *what* is valid), run a
real validation on both builds against the same cache and diff:
* VRP set — must be **byte-identical**. Any difference is a bug, not an improvement.
* `Validations` / metrics JSON from the HTTP API — differences are expected, and every one
  should map to §5.3, §5.4 or §7.4. Anything else needs explaining.
* Wall-clock and peak RSS for a full run — guards the `State.Static.Shared` choice (§4.4).

### 8.4 Runtime smoke checks for §7.3
Grep the final tree for `Control.Concurrent.Async.Lifted` and `Control.Exception.Lifted`
and confirm every remaining occurrence is at `m ~ IO`. A `SeqUnlift` violation is a
*runtime* error, so the type checker will not find these for you.

---

## 9. Cleanup pass — DONE

1. **`vHoist` deleted**, all 66 call sites removed. 61 were a plain `vHoist $ expr`
   (scriptable); 3 were multi-line blocks needing hand treatment:
   * `TopDown:441` — the parens were load-bearing for an infix `catchError`, so only
     the call was dropped, leaving `(do …)`.
   * `TopDown:950` — the block was spliced into the enclosing `do` (same monad now).
   * `TopDown:1210` — became a plain `<- do`.
   One composition site, `hoistHere = vHoist . fromEither . first RrdpE`, was mangled
   by the naive substitution into `= . fromEither …`; a scan for `(=|$|<-|\() *\.`
   caught it. **Check compositions when scripting this kind of removal.**
2. **`pure*` aliases collapsed.** They were exact synonyms once the pure/IO split
   disappeared. 73 call sites renamed:
   * `vPureError` (52) → `vError`
   * `pureError` (16) → `appError`
   * `vPureWarning` (4) → `vWarn`
   * `pureWarning` (1) → `validatorWarning`

   The error/warning surface is now five names: `appError`, `vError`,
   `validatorWarning`, `appWarn`, `vWarn`.
3. `appLift` is gone (it was `lift . lift . lift`; `liftIO` replaces it). `fromValue`
   is kept — it is the only way to short-circuit *without* recording into
   `Validations`, and `validatorT`/`embedValidatorT` rely on that.
4. **Dependencies**: `mmorph` and `lifted-async` removed — GHC's
   `-Wunused-packages` confirms neither is referenced any more.
   `mtl` **must stay**: `Http/HttpServer.hs` uses `Control.Monad.Error.Class` for
   Servant's `Handler` (see §10). `lifted-base`, `monad-control`, `transformers`
   and `unliftio` all still have live users.

   For the record, `-Wunused-packages` also flags these as unused *by the library*:
   QuickCheck, cborg, crypton-connection, generic-arbitrary, http-client-tls,
   list-t, unordered-containers, pipes, optparse-generic, optparse-applicative,
   quickcheck-instances, shower, stm-containers, tasty*, warp. Most are needed by
   the exe/test stanzas, which share one `common shared` block — untangling that is
   a separate job and was left alone.
5. **Unused imports**: 11 removed (`Control.Monad.IO.Class` ×8, made redundant by
   `import Effectful`; a duplicate `Control.Monad` the migration script inserted in
   `RrdpFetch.hs`; two now-dead `Control.Monad`s). Zero remain in any file this
   migration touched. Twelve remain elsewhere and were **deliberately left**: they
   pre-date this work, and most (`RPKI.Orphans.*`, `Data.Generics.Labels`,
   `RPKI.Store.Types`) are orphan-instance imports — `-Wunused-imports` does not
   account for instances, so removing them can silently break instance resolution
   in downstream modules.

Re-verified after cleanup — build clean, `cabal test` **168/168**, and the cleaned
binary re-run live against the pre-migration instance:

| | baseline (mtl) | effectful (cleaned) |
|---|---|---|
| unique VRPs | 1 003 335 | 1 003 334 (99.9997 % identical) |
| repos with identical metric counters | — | **118 / 119** |
| `validCertNumber` / `validRoaNumber` | 54 458 / 387 175 | 54 458 / 387 175 (exact) |
| `validMftNumber` / `validCrlNumber` / `mftShortcutNumber` | 54 317 | 54 317 (exact) |
| `validAspaNumber` | 3 003 | 3 003 (exact) |
| validation issue sets | — | 219 / 221 identical |

The three differing VRPs are, again, a *different* set from the previous sample, and
one of them is decisive on its own: `36.255.97.0/24` appears as `AS205759` in one
instance and `AS207184` in the other — the same prefix with a different origin AS,
i.e. an ROA caught mid-reissue. Code cannot produce that; only wall-clock can.

## 10. Optional follow-ups (not part of this migration)

* Make `AppLogger` an effect (`Effectful.Dispatch.Dynamic`) — logging is threaded
  through almost every signature as an explicit argument today.
* Make `AppContext s` a `Reader` effect, removing another ubiquitous parameter. Do
  this *after* the logger, since `AppContext` contains the logger.
* Move `Workflow.hs` into `Eff`, shrinking the `IO` boundary to `Main.hs`.
* Reconsider `bracketChanClosable`/`foldPipeline`/`txFoldPipeline` now that `Eff`
  makes the unlifting explicit — the `TODO Refactor it so that is shared code with
  "txFoldPipeline"` at `Parallel.hs:29` becomes much easier to act on.

### 10.1 `servant-effectful` — evaluated, not adopted

`servant-effectful-1.0.0` would move `Http/HttpServer.hs` off Servant's `Handler`
and onto `type Handler es = Eff (Error ServerError : es)`. Assessment:

**Fits, technically.** It needs `servant >=0.20 && <0.21`; the freeze pins
servant/servant-server at 0.20.3.0. It is a thin adapter — 354 lines — that unlifts
per request and wraps back into `Servant.Handler`.

**Reasons not to, now:**

* *No payoff yet.* `HttpServer.hs` (647 lines, 20 `throwError err4xx` sites) is the
  one part of the app with **zero** contact with the validator stack — it reads
  TVars and the DB. `throwError err404` in `Handler` is idiomatic Servant, not a
  wart left by the migration. The only concrete win is dropping the last direct
  `mtl` import.
* *Dependency weight and youth.* It pulls in `wai-effectful` (and `warp-effectful`
  if the server itself moves), all first released 2026-07-28 with a single release
  each, from a small maintainer. The tight `<0.21` servant bound means a servant
  bump would block on an upstream release.
* *Licence.* `servant-effectful` is EUPL-1.2 (copyleft); this project is BSD-3.
  Fine for an unmodified dependency, but worth a deliberate decision rather than an
  accident.
* *One implementation detail to check if adopted.* `serveWithContextT` uses
  `withEffToIO SeqUnlift`, created per request. That is correct for a handler that
  stays on its own thread, but a handler that forks would hit the "unlifting
  function used in multiple threads" error (§7.3), and the effect environment is
  shared across concurrent requests — so the server's `es` must not carry
  thread-unsafe state (`State.Static.Local`).

**When it would be worth revisiting:** once `AppContext` becomes a `Reader` effect.
At that point the handlers stop taking it as an explicit parameter and running them
in `Eff` buys something real. Until then it is churn.

---

## 11. What the implementation actually hit

Everything in §4–§6 held up. These are the corrections and the four traps §7 missed.

### 11.1 NEW TRAP — `ask`/`get`/`modify`/`local` are ambiguous, and it is pervasive

The single biggest source of errors, and it hits on the very first module.

`ReaderT Scopes` fixes the environment type; `Reader Scopes :> es` does **not**.
`(:>)` is a plain constraint, not a functional dependency, so `ask`'s `r` and
`modify`'s `s` are not determined by the constraint being in scope:

```haskell
-- ambiguous: nothing says which Reader / which State
validationScope <- asks (^. typed)
modify $ typed %~ (mError validationScope e <>)
```

Fix: monomorphic wrappers in `AppMonad.hs`, used everywhere instead of the raw ops.

```haskell
askScopes    :: Reader Scopes :> es => Eff es Scopes
localScopes  :: Reader Scopes :> es => (Scopes -> Scopes) -> Eff es a -> Eff es a
getVState    :: State ValidationState :> es => Eff es ValidationState
putVState    :: State ValidationState :> es => ValidationState -> Eff es ()
modifyVState :: State ValidationState :> es => (ValidationState -> ValidationState) -> Eff es ()
```

Type applications (`modify @ValidationState`) also work but read badly at every site.

### 11.2 NEW TRAP — type-application order silently changes

`fromEither @()` meant `@r` under `fromEither :: Either AppError r -> PureValidatorT r`.
After the rewrite `es` comes first (it appears in the constraint), so `@()` bound
`es` and produced a kind error. Fix: pin the order with an explicit `forall` rather
than editing call sites —

```haskell
fromEither :: forall r es . Validator es => Either AppError r -> Eff es r
```

Same treatment for `fromValue`, `fromEitherM`, `vFromEither`. `updateMetric`'s
`forall metric es` was already correct in the plan (§4.5) and needed no change.

### 11.3 NEW TRAP — `Eff` has no `MonadFail`, and no `MonadReader` for `Stream`

* `app/Main.hs` bound a failable pattern (`[cached, rsyncd, tald, tmpd] <- forM …`).
  `Eff` needs the `Fail` effect for that. Rather than widen `AppEffects`, the four
  directories are now bound one at a time.
* `Rsync.traverseDirectory` runs in `Stream (Of …) (Eff es)` and called `askScopes`
  directly — which worked because `streaming` has a `MonadReader` instance for
  `Stream`. There is no such instance for the effect constraint, so it needs an
  explicit `lift askScopes`.
* `AppMonadSpec` ran `runValidatorT` inside `PropertyM IO`, relying on the old
  runner being polymorphic in the base monad. `runValidatorIO` returns `IO`, so it
  now needs QuickCheck's `run`.

### 11.4 §7.1 confirmed, and worse than described — `TopDown.hs` needs `NoMonoLocalBinds`

`validateCaNoFetch` has seven signatured local helpers plus unsignatured
`getManifestEntry` / `validateMftChild` / `shortcutIfNoIssues`, and the latter are
used **both** at the enclosing stack and under a nested `runValidatorT`. Two
conflicting requirements appear at once:

* helpers used under the nested runner must be polymorphic in `es`;
* helpers that close over the enclosing scope want the parent's `es`.

Writing out the full signatures by hand is possible but long. What is actually in
the tree: every local signature in `TopDown.hs` was rewritten to quantify over its
own `es'`, and the module got

```haskell
{-# LANGUAGE NoMonoLocalBinds #-}
```

so the unsignatured helpers generalise too. This is confined to that one module and
is documented in a comment at the top of it.

### 11.5 §6.5 simplified — `appTx` needs no nested `runValidatorT` at all

The plan's version re-ran the transaction body under `runValidatorT` to capture its
`ValidationState`. That is impossible (§7.1: `f tx :: Eff es a` is monomorphic) and
also unnecessary with shared state: the body writes into the enclosing
`ValidationState` directly and those writes survive the error. What is needed is
only to stop the validator error from reaching SQLite's rollback path:

```haskell
r <- withSeqEffToIO $ \unlift ->
        txF db (\tx ->
            unlift (tryError @AppError (f tx)) >>= \case
                Left (_, e) -> throwIO (TxRollbackException e mempty)
                Right a     -> pure (Right a, mempty))
        `catch` (\(TxRollbackException e vs) -> pure (Left e, vs))
embedValidatorT (pure r)
```

`DatabaseSpec`'s rollback/commit/exception trio passes unchanged.

### 11.6 §6.8 resolved — `readSlurm` took option 2

`AppState.readSlurm :: Maybe (Eff AppEffects Slurm)`, the concrete stack. Its only
consumer runs it with `runValidatorIO`, which expects exactly that stack, so the
rank-2 newtype bought nothing.

### 11.7 §3 correction — the freeze file blocks effectful

`cabal.project.freeze` pinned `index-state` to 2026-08-06, before effectful 2.7
existed, so the solver only offered 2.6.1.0. Bumped to `2026-08-27T11:08:33Z`. Safe:
every other package in that file is `==`-pinned, so nothing else can move.

### 11.8 §7.3 audited — the remaining `lifted-base` uses are safe, and were left alone

`Workflow.{withWorkflowShared, runConcurrentlyIfPossible, ignoreSync}` and
`Worker.runWorker` still use `Control.Exception.Lifted` at `m ~ Eff es`. That is
correct: none of them forks (`Worker.runIt`'s `concurrently` and `bracket` are
inside `liftIO`, i.e. at `IO`; `runConcurrentlyIfPossible` is STM plus `finally`),
and `SeqUnlift` permits repeated *sequential* unlifts on the same thread.
`runConcurrentlyIfPossible` is additionally used at `m ~ IO` (Workflow:427), so it
has to stay `MonadBaseControl`-polymorphic anyway. Converting them is cosmetic —
but it is the thing to check first if a `"unlifting function used in multiple
threads"` error ever appears.

### 11.9 `bench/` was already broken before this migration

`bench/Main.hs` references `RpkiObject` (renamed to `RpkiObject_`) and `DB.noTx`
(no longer exported) at `b480c81d`. Unrelated to effectful; the benchmarks are
excluded from the build gate until someone fixes them.

### 11.10 Results

Built and tested; then run side by side against a live RPKI cache with the
pre-migration build (`ui-redesign-0.11` on :50299, effectful on :50399), both
`--cpu-count 8 --allow-overclaiming`.

**Build/test**

* `cabal build --enable-tests lib exe test` clean (`bench` excluded, §11.9).
* `cabal test`: **168/168**, including the semantic oracles —
  `AppMonadSpec.scopesShouldBeProperlyNested`, `forM saves state`, and
  `DatabaseSpec`'s "Should rollback App transactions properly" / "Should preserve
  state from StateT in transactions".

**Validation outcomes** (`/api/vrps`, `/api/validations`, `/api/metrics`)

| | baseline (mtl) | effectful | |
|---|---|---|---|
| unique VRPs | 1 003 327 | 1 003 326 | 99.9999 % identical |
| `vrpCounter` total | 1 012 399 | 1 012 398 | −1 |
| `validCertNumber` | 54 456 | 54 456 | exact |
| `validRoaNumber` | 387 167 | 387 166 | −1 |
| `validMftNumber` / `validCrlNumber` | 54 315 | 54 315 | exact |
| `validAspaNumber` | 3 002 | 3 002 | exact |
| repos with identical counters | — | — | 117 / 119 |
| validation issues per URL | — | — | 218 / 220 identical |

The handful of differences are **live RPKI churn, not logic**. The decisive evidence:
*the differing VRPs are not the same between samples* — one sample differed on two
ARIN `2602:f83d::/32` entries, a sample minutes later differed on one RIPE
`AS213035 91.198.123.0/24` instead. A logic difference would differ identically
every time. Likewise both differing validation issues are stale-repository errors
carrying *different `Next update time` values*, i.e. the two instances hold
different versions of the same notification file.

Per-URL issue text is otherwise byte-identical once the wall-clock embedded in the
message ("current time is …") is normalised. Entries keyed by `ObjectKey` (bare
integers) are not comparable across instances — those keys are per-database.

**Runtime behaviour**

* No `SeqUnlift` violations ("unlifting function used in multiple threads") and no
  escaped-error `ErrorWrapper`s in any run — the §11.8 audit holds in practice.
* Concurrent-fetch behaviour identical: 0 repositories ever had >1 worker in flight
  in either build.

**Resource usage** — no regression.

| | baseline (mtl) | effectful |
|---|---|---|
| process RSS | 326 MB @ 1 h 27 m uptime | 325 MB @ 12 m uptime |
| `validation` avg heap | 433 MB | 381 MB |
| `validation` max heap | 683 MB | 483 MB |
| RIPE revalidation, median | 778 ms (n=43) | 763 ms (n=5) |
| full revalidation cycle, median | 2348 ms (n=86) | 1765 ms (n=9) |

Read the heap and cycle-time columns as "no regression", **not** as an improvement:
`maxRtsHeap` is a high-water mark and the baseline had 7× the uptime and ~10× the
cycles to reach one, and incremental revalidation cost depends on what changed
upstream, so the two are not doing identical work.

The specific worry from §4.4 — that `State.Static.Shared`'s MVar would cost in the
hot path — did not materialise: RIPE's 275 MB RRDP snapshot was downloaded, parsed
and stored in 19 s cold, APNIC's 115 MB in 8.8 s, and steady-state RIPE
revalidation is unchanged at ~0.77 s.

**Operational note, unrelated to the migration**: ARIN's RRDP snapshot is >640 MB
and its download regularly outruns the default `--rrdp-timeout 660`; on a
contended link it needed `3600` here. Nothing to do with `effectful` — the
pre-migration build has the same problem, and the fallback to rsync works.

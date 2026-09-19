{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Validator helpers that need 'RPKI.Parallel'. They cannot live in
-- 'RPKI.AppMonad' itself, since Parallel imports AppMonad.
module RPKI.AppMonadUtil where

import           Effectful
import           Effectful.Concurrent.Async  (Concurrent, pooledForConcurrentlyN)
import           Effectful.Error.Static      (tryError)
import           Effectful.Exception         (bracket)

import           Data.Bifunctor              (first)
import           Data.Either                 (partitionEithers)

import           RPKI.AppMonad
import           RPKI.Parallel
import           RPKI.Reporting

{- NOTE: these used to re-run their argument under a nested `runValidator` to
   isolate its `ValidationState`. That is not possible with `effectful` -- an
   action of type @Eff es r@ has its effect indices already resolved -- and it
   is also unnecessary: `ValidationState` is a *shared* effect, so writes from
   the forked jobs are merged back into the caller automatically. All that is
   left to do is turn a per-item error into an `Either`, which `tryError` does.
-}

bracketVT :: (ValidatorIO es, IOE :> es)
          => IO a
          -> (a -> Eff es r)
          -> (a -> Eff es b)
          -> Eff es b
bracketVT acquire release f = bracket (liftIO acquire) release f

concurrentlyVTLenientN :: (ValidatorIO es, Concurrent :> es)
                       => Int
                       -> [a]
                       -> (a -> Eff es r)
                       -> Eff es [r]
concurrentlyVTLenientN n as f = do
    rs <- runEachSeparately n as f
    let (failures, successes) = partitionEithers rs
    case successes of
        [] -> appError $ ComposeE failures
        _  -> pure $! successes

concurrentlyVTStrictN :: (ValidatorIO es, Concurrent :> es)
                      => Int
                      -> [a]
                      -> (a -> Eff es r)
                      -> Eff es [r]
concurrentlyVTStrictN n as f = do
    rs <- runEachSeparately n as f
    let (failures, successes) = partitionEithers rs
    case failures of
        [] -> pure $! successes
        _  -> appError $ ComposeE failures

-- | Run every job concurrently, keeping each job's failure instead of letting
-- it abort the others.
runEachSeparately :: (ValidatorIO es, Concurrent :> es)
                  => Int -> [a] -> (a -> Eff es r) -> Eff es [Either AppError r]
runEachSeparately n as f =
    pooledForConcurrentlyN n as $ \a -> first snd <$> tryError @AppError (f a)

withSemaphoreVT :: ValidatorIO es => Semaphore -> Eff es a -> Eff es a
withSemaphoreVT = withSemaphore

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

{- | A pool of Erik relays providing load balancing, fall-back and parallelism
     limits for relay queries.

     Objects on a relay are content-addressed (@/.well-known/ni/sha-256/<hash>@)
     and the caller verifies them against the expected hash, so any relay may
     serve any object. That is what makes both spreading and retrying safe.

     Tracked per relay and for the pool as a whole:

       * queries in flight, which drive the load-balancing choice,
       * a parallelism cap, enforced per relay /and/ globally,
       * served/failed counters, for logging and diagnostics.
-}
module RPKI.Fetch.RelayPool where

import           Control.Concurrent.STM
import           Control.Monad

import           Effectful
import           Effectful.Error.Static            (catchError, rethrowError, tryError)
import           Effectful.Exception               (bracket_)

import           Data.List                        (sortOn)
import           Data.String.Interpolate.IsString

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Parallel
import           RPKI.Reporting


data Relay = Relay {
        relayUri :: URI,
        -- | Queries running against this relay right now; drives selection.
        inFlight :: TVar Int,
        served   :: TVar Int,
        failed   :: TVar Int,
        -- | Failures since this relay last answered successfully. A relay that
        --   keeps failing sinks to the back of the preference order, so the
        --   cost of a dead relay is paid a few times, not on every query.
        consecutiveFailures :: TVar Int,
        -- | Per-relay parallelism cap.
        relaySem :: Semaphore
    }
    deriving stock (Eq)

data RelayPool = RelayPool {
        relays         :: [Relay],
        -- | Cap on queries in flight across all relays together.
        globalSem      :: Semaphore,
        globalInFlight :: TVar Int
    }
    deriving stock (Eq)

data RelayStat = RelayStat {
        statRelay    :: URI,
        statInFlight :: Int,
        statServed   :: Int,
        statFailed   :: Int
    }
    deriving stock (Eq, Show)


-- | Build a pool. @perRelay@ caps queries against any single relay,
--   @global@ caps them across the pool as a whole.
newRelayPool :: MonadIO m => Int -> Int -> [URI] -> m RelayPool
newRelayPool global perRelay uris = liftIO $ atomically $ do
    rs <- forM uris $ \uri ->
            Relay uri <$> newTVar 0 <*> newTVar 0 <*> newTVar 0 <*> newTVar 0
                      <*> newSemaphore perRelay
    RelayPool rs <$> newSemaphore global <*> newTVar 0


{- | Run a query against the pool.

     The relay with the fewest queries in flight goes first, so load spreads
     and a slow relay sheds traffic by itself. If the query fails, the failure
     is recorded as a warning (the way repository fall-back does it) and the
     next relay is tried. If every relay fails the whole thing is an error.

     Both caps are held for the duration of an attempt, global acquired first
     so the ordering is the same everywhere and cannot deadlock.
-}
withRelay :: ValidatorIO es => AppLogger -> RelayPool -> (URI -> Eff es a) -> Eff es a
withRelay logger pool@RelayPool {..} f
    | null relays = appError $ ErikE $ UnknownErikProblem "No Erik relays configured."
    | otherwise   = go =<< leastLoadedFirst
  where
    go [] = appError $ ErikE $ UnknownErikProblem
                "All Erik relays failed to answer the query."
    go (r : rest) =
        runOn r `catchError` \_cs e ->
            case rest of
                [] -> appError e
                _  -> do
                    let Relay { relayUri = thisUri } = r
                    logWarn logger
                        [i|Erik relay #{thisUri} failed with #{e}, trying the next relay.|]
                    validatorWarning $ VWarning e
                    go rest

    runOn relay@Relay {..} =
        withSemaphore globalSem $
            withSemaphore relaySem $
                counted relay (f relayUri)

    -- `bracket_` so the in-flight counters unwind on any exception, not just
    -- on a validator error.
    counted Relay {..} action = do
        r <- bracket_ (adjust 1) (adjust (-1)) (tryError @AppError action)
        liftIO $ atomically $
            case r of
                Left _  -> do
                    modifyTVar' failed (+ 1)
                    modifyTVar' consecutiveFailures (+ 1)
                Right _ -> do
                    modifyTVar' served (+ 1)
                    writeTVar consecutiveFailures 0
        either (uncurry rethrowError) pure r
      where
        adjust d = liftIO $ atomically $ do
            modifyTVar' inFlight (+ d)
            modifyTVar' globalInFlight (+ d)

    -- Prefer a relay that is answering over one that is not, and among
    -- equals the least busy one. A relay that starts failing therefore drifts
    -- to the back by itself and drifts forward again once it recovers.
    leastLoadedFirst = liftIO $ atomically $ do
        loaded <- forM relays $ \r@Relay { inFlight = n, consecutiveFailures = cf } ->
                    (,r) <$> ((,) <$> readTVar cf <*> readTVar n)
        pure $! map snd $ sortOn fst loaded


relayStats :: MonadIO m => RelayPool -> m [RelayStat]
relayStats RelayPool {..} = liftIO $ atomically $
    forM relays $ \Relay {..} ->
        RelayStat relayUri <$> readTVar inFlight <*> readTVar served <*> readTVar failed

globalInFlightCount :: MonadIO m => RelayPool -> m Int
globalInFlightCount RelayPool {..} = liftIO $ readTVarIO globalInFlight

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

{- | A work queue drained by worker threads bound to Erik relays.

     Objects on a relay are content-addressed (@/.well-known/ni/sha-256/<hash>@)
     and the caller verifies them against the expected hash, so any relay may
     serve any object. That is what makes both spreading and retrying safe.

     The shape is a thread pool rather than a scheduler:

       * every relay gets a fixed number of worker threads, so the per-relay
         parallelism cap /is/ the thread count -- there is no semaphore to hold
         and no in-flight counter to sort on,
       * load balances itself: a fast relay's threads come back for more work
         sooner, so they drain a larger share of the queue,
       * a failed item goes back on the queue with the relay recorded, and the
         next relay to reach for work picks it up. Fall-back therefore never
         blocks the thread that hit the failure.

     Work is keyed by 'Hash' and deduplicated against everything ever queued,
     so an object referenced from several places is fetched once.
-}
module RPKI.Fetch.RelayPool where

import           Control.Concurrent.STM
import           Control.Monad

import           Effectful
import           Effectful.Concurrent.Async       (Concurrent, pooledForConcurrentlyN)
import           Effectful.Error.Static           (tryError)

import qualified Data.List                        as List
import qualified Data.Set                         as Set
import           Data.Set                         (Set)
import           Data.String.Interpolate.IsString

import           RPKI.AppMonad
import           RPKI.Domain
import           RPKI.Logging
import           RPKI.Reporting


{- | Failures against one relay, within a single worker, before it is reported
     to the root process.

     This is 1 on purpose. Deciding whether a relay is really dead is the root
     process's job: it is the only place that sees reports from every worker,
     so that is where they are counted (see 'RPKI.AppState.relayDeadAfterReports').
     Reporting early is what lets the other workers in the same round hear about
     a dead relay before they waste a timeout on it.
-}
deadRelayThreshold :: Int
deadRelayThreshold = 1

{- | Consecutive failures before this worker stops handing the relay any more
     work of its own accord.

     Derived from the thread count rather than fixed, because all of a relay's
     threads are typically in flight against it at once: a single round of
     timeouts against an unreachable host produces exactly @perRelayThreads@
     consecutive failures. A fixed threshold above that number would need a
     *second* full round of timeouts before the relay was benched, and with a
     connect timeout measured in minutes that is the difference between a fetch
     that takes seconds and one that takes five minutes.

     Still higher than 'deadRelayThreshold': telling the root process about a
     suspicious relay is cheap and reversible, while benching one here removes
     capacity from a fetch already in progress. A relay that answers clears the
     count, so only a whole round of failures with nothing served gets it
     benched.
-}
benchThreshold :: Relays -> Int
benchThreshold Relays { perRelayThreads } = max 2 perRelayThreads

data Relay = Relay {
        relayUri            :: URI,
        served              :: TVar Int,
        failed              :: TVar Int,
        -- | Failures since this relay last answered successfully.
        consecutiveFailures :: TVar Int,
        -- | False once the relay is benched: its workers stop taking work.
        --   The last live relay is never benched, so the queue always drains.
        alive               :: TVar Bool
    }
    deriving stock (Eq)

data Relays = Relays {
        relayList       :: [Relay],
        -- | Worker threads per relay, i.e. the per-relay parallelism cap.
        perRelayThreads :: Int
    }
    deriving stock (Eq)

data RelayStat = RelayStat {
        statRelay  :: URI,
        statServed :: Int,
        statFailed :: Int
    }
    deriving stock (Eq, Show)

-- | One queued item: what to do, plus which relays have already refused it.
data Task t = Task {
        payload  :: t,
        taskHash :: Hash,
        tried    :: Set URI,
        attempts :: Int
    }
    deriving stock (Eq, Show)

data WorkPool t = WorkPool {
        -- | Never attempted; any worker may take these. A stack rather than a
        --   queue, so the walk goes depth-first and reaches the leaves (which
        --   are the bulk of the work) without draining a level first.
        fresh     :: TVar [Task t],
        -- | Attempted and failed at least once. Stays short, so the linear
        --   scan for an eligible task costs nothing.
        retries   :: TVar [Task t],
        -- | Every hash ever queued, successful or not. The deduplication key.
        queued    :: TVar (Set Hash),
        -- | Workers currently holding a task. A task in flight may still
        --   enqueue more work, so "queue empty" alone does not mean "done".
        active    :: TVar Int,
        failures  :: TVar [(Hash, AppError)],
        succeeded :: TVar Int
    }
    deriving stock (Eq)


newRelays :: MonadIO m => Int -> [URI] -> m Relays
newRelays perRelay uris = liftIO $ atomically $ do
    rs <- forM uris $ \uri ->
            Relay uri <$> newTVar 0 <*> newTVar 0 <*> newTVar 0 <*> newTVar True
    pure $ Relays rs (max 1 perRelay)

newWorkPool :: MonadIO m => m (WorkPool t)
newWorkPool = liftIO $ atomically $
    WorkPool <$> newTVar [] <*> newTVar [] <*> newTVar mempty
             <*> newTVar 0  <*> newTVar [] <*> newTVar 0

-- | Add work, skipping anything whose hash has been queued before.
enqueue :: MonadIO m => WorkPool t -> [(Hash, t)] -> m ()
enqueue pool items = liftIO $ atomically $ enqueueSTM pool items

enqueueSTM :: WorkPool t -> [(Hash, t)] -> STM ()
enqueueSTM WorkPool {..} items = do
    q <- readTVar queued
    -- Deduplicate against what is already queued *and* within `items` itself.
    let (q', new) = foldl' add (q, []) items
        add (seen, acc) (h, t)
            | h `Set.member` seen = (seen, acc)
            | otherwise           = (Set.insert h seen, Task t h mempty 0 : acc)
    unless (null new) $ do
        writeTVar queued $! q'
        modifyTVar' fresh (new <>)


{- | Drain the pool.

     @process@ is handed the relay to talk to and one item, and returns whatever
     new work that item revealed. Throwing an 'AppError' from it means "this
     relay could not serve this item": the item is re-queued against another
     relay, and only when no relay is left to try does it count as a failure.

     Returns once every queued item has either been processed or failed on every
     relay. An exception (as opposed to an 'AppError') propagates and cancels
     the other workers, which is what makes the enclosing timeout work.
-}
runRelayWorkers :: (ValidatorIO es, Concurrent :> es)
                => AppLogger
                -> Relays
                -> WorkPool t
                -> (URI -> t -> Eff es [(Hash, t)])
                -> Eff es ()
runRelayWorkers logger relays@Relays {..} pool process
    | null relayList = appError $ ErikE $ UnknownErikProblem "No Erik relays configured."
    | otherwise      = do
        let slots = [ r | r <- relayList, _ <- [1 .. perRelayThreads] ]
        void $ pooledForConcurrentlyN (length slots) slots worker
        -- Every relay was benched before the queue ran dry. Nothing is going to
        -- pick these up, so they are failures like any other.
        leftovers <- liftIO $ atomically $ do
            f <- readTVar pool.fresh
            r <- readTVar pool.retries
            writeTVar pool.fresh []
            writeTVar pool.retries []
            pure (f <> r)
        forM_ leftovers $ \t ->
            liftIO $ atomically $ modifyTVar' pool.failures
                ((t.taskHash, ErikE $ UnknownErikProblem
                    "No usable Erik relay left to fetch the object from.") :)
  where
    worker relay = go
      where
        go = takeTask relays pool relay >>= \case
            Nothing   -> pure ()
            Just task -> do
                r <- tryError @AppError $ process relay.relayUri task.payload
                case r of
                    Right newWork -> do
                        liftIO $ atomically $ do
                            modifyTVar' pool.active (subtract 1)
                            modifyTVar' pool.succeeded (+ 1)
                            enqueueSTM pool newWork
                            modifyTVar' relay.served (+ 1)
                            writeTVar relay.consecutiveFailures 0
                        go
                    Left (_, e) -> do
                        onFailure logger relays pool relay task e
                        go


{- | Take the next item this relay is allowed to work on, blocking while the
     queue is empty but other workers are still busy, and returning 'Nothing'
     once the whole pool is drained (or this relay has been benched).

     The eligibility rule for a re-queued item is "prefer a relay that has not
     tried it, but take it anyway if no other live relay is left to try". The
     second half is what makes this deadlock-free: it is never possible for the
     queue to hold an item that no live worker will accept.
-}
takeTask :: MonadIO m => Relays -> WorkPool t -> Relay -> m (Maybe (Task t))
takeTask Relays {..} WorkPool {..} Relay { relayUri = thisUri, alive = thisAlive } =
    liftIO $ atomically $ do
        readTVar thisAlive >>= \case
            False -> pure Nothing
            True  -> readTVar fresh >>= \case
                (t : ts) -> do
                    writeTVar fresh $! ts
                    took t
                [] -> do
                    rs <- readTVar retries
                    -- Reading every relay's liveness inside the transaction is
                    -- what wakes a blocked worker when another relay is benched
                    -- and its share of the queue becomes this one's problem.
                    liveUris <- Set.fromList . map (\r -> r.relayUri)
                                    <$> filterM (\r -> readTVar r.alive) relayList
                    let eligible t =
                            not (thisUri `Set.member` t.tried)
                                || Set.null (liveUris `Set.difference` t.tried)
                    case List.break eligible rs of
                        (before, t : after) -> do
                            writeTVar retries $! before <> after
                            took t
                        _ -> do
                            a <- readTVar active
                            if a == 0 && null rs
                                then pure Nothing
                                else retry
  where
    took t = do
        modifyTVar' active (+ 1)
        pure (Just t)


{- | Charge one failure to a relay, and say whether this is the call that
     benched it.

     Shared by the work pool and the index fetch so a relay's health is one
     account: an index that could not be served is evidence about the relay
     just as much as an object that could not be, and the work pool needs to
     hear about it before it starts handing that relay threads.

     Returns the relays that were still alive /before/ this one was benched,
     since callers need that to decide whether anything is left to try.
-}
chargeFailureSTM :: AppLogger -> Relays -> Relay -> Bool -> STM (Bool, [Relay])
chargeFailureSTM logger relays@Relays { relayList } relay benchNow = do
    let thisUri = relay.relayUri
    modifyTVar' relay.failed (+ 1)
    modifyTVar' relay.consecutiveFailures (+ 1)
    cf <- readTVar relay.consecutiveFailures

    -- The verdict and the report it triggers happen in one transaction, so the
    -- root process hears about a suspect relay the moment this fetch concludes
    -- it is one -- not at the end, by which time every other worker in the
    -- round has already started and the news is too late to spare them the
    -- same timeout.
    when (cf == deadRelayThreshold) $
        pushErikRelayReportSTM logger [ ErikRelayReport thisUri 0 1 ]

    -- Never bench the last relay standing: a fetch with no live relay cannot
    -- make progress, and failing each item with its real error is more useful
    -- than failing all of them with "no relays left".
    othersAlive <- filterM (\r -> readTVar r.alive)
                        [ r | r <- relayList, r.relayUri /= thisUri ]
    -- `wasAlive` makes this a transition rather than a level: without it every
    -- thread that fails after the threshold benches the relay again and logs
    -- another warning.
    wasAlive <- readTVar relay.alive
    let bench = wasAlive
                    && (benchNow || cf >= benchThreshold relays)
                    && not (null othersAlive)
    when bench $ writeTVar relay.alive False
    pure (bench, othersAlive)


-- | Account for a failed attempt: charge the relay, then either re-queue the
-- item for another relay or give up on it.
onFailure :: ValidatorIO es
          => AppLogger -> Relays -> WorkPool t -> Relay -> Task t -> AppError -> Eff es ()
onFailure logger relays pool relay task e = do
    let thisUri = relay.relayUri
    (gaveUp, benched) <- liftIO $ atomically $ do
        (bench, othersAlive) <- chargeFailureSTM logger relays relay False

        let task' = task { tried    = Set.insert thisUri task.tried
                         , attempts = task.attempts + 1 }
        -- `othersAlive` is computed before benching this relay, so an item that
        -- has now been refused by every live relay is not re-queued forever.
        let noRelayLeft = null [ r | r <- othersAlive
                                   , r.relayUri `Set.notMember` task'.tried ]
            giveUp      = task'.attempts >= length relays.relayList + 1 || noRelayLeft

        modifyTVar' pool.active (subtract 1)
        if giveUp
            then modifyTVar' pool.failures ((task'.taskHash, e) :)
            else modifyTVar' pool.retries (task' :)

        pure (giveUp, bench)

    when benched $ do
        let n = benchThreshold relays
        logWarn logger
            [i|Erik relay #{thisUri} failed #{n} times in a row, not using it for the rest of this fetch.|]
    if gaveUp
        then do
            -- Only the final verdict becomes a validation warning. A re-queued
            -- item is not a problem with the fetch -- another relay is about to
            -- serve it -- so it stays at debug level.
            let h = task.taskHash
            logWarn logger
                [i|No Erik relay could serve #{h}, last error from #{thisUri}: #{e}.|]
            validatorWarning $ VWarning e
        else
            logDebug logger
                [i|Erik relay #{thisUri} failed with #{e}, re-queueing for another relay.|]


{- | Run a one-off query that is not part of the work queue, trying relays in
     turn until one answers.

     This is for the index: unlike every other Erik download it is relay state
     rather than a content-addressed object, so it is fetched once, up front,
     before there is a queue to put it on.

     A relay that fails here is benched immediately rather than merely charged a
     failure. The index is the smallest, most certainly present thing a relay
     serves, and it is fetched once per FQDN -- a relay that cannot produce it
     is not going to produce the objects underneath it either. Benching it now
     is what keeps the work pool from starting that relay's threads and paying
     a second full connect timeout to learn the same thing.
-}
withAnyRelay :: ValidatorIO es => AppLogger -> Relays -> (URI -> Eff es a) -> Eff es a
withAnyRelay logger relays f = do
    live <- liftIO $ atomically $ filterM (\r -> readTVar r.alive) relays.relayList
    go live
  where
    go [] = appError $ ErikE $ UnknownErikProblem
                "All Erik relays failed to answer the query."
    go (relay : rest) = do
        let uri = relay.relayUri
        tryError @AppError (f uri) >>= \case
            -- Credit the success as well as charging the failures. Without this
            -- a relay that serves an index which turns out to be unchanged --
            -- the steady state, and much the commonest outcome -- is never
            -- credited with anything, so a healthy relay looks exactly like one
            -- that has never been tried.
            Right r -> do
                liftIO $ atomically $ do
                    modifyTVar' relay.served (+ 1)
                    writeTVar relay.consecutiveFailures 0
                pure r

            Left (_, e) -> do
                benched <- liftIO $ atomically $
                                fst <$> chargeFailureSTM logger relays relay True
                when benched $
                    logDebug logger
                        [i|Erik relay #{uri} could not serve the index, not using it for the rest of this fetch.|]
                case rest of
                    [] -> appError e
                    _  -> do
                        logWarn logger
                            [i|Erik relay #{uri} failed with #{e}, trying the next relay.|]
                        validatorWarning $ VWarning e
                        go rest


relayStats :: MonadIO m => Relays -> m [RelayStat]
relayStats Relays { relayList } = liftIO $ atomically $
    forM relayList $ \r ->
        RelayStat r.relayUri <$> readTVar r.served <*> readTVar r.failed

poolFailures :: MonadIO m => WorkPool t -> m [(Hash, AppError)]
poolFailures pool = liftIO $ readTVarIO pool.failures

poolSucceeded :: MonadIO m => WorkPool t -> m Int
poolSucceeded pool = liftIO $ readTVarIO pool.succeeded

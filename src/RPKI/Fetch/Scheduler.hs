{-# LANGUAGE StrictData #-}

{- | Which repository fetch runs when.

    Fetches of the repositories of the TA operators, i.e. of the root groups
    (see 'FetchGroup'), run right away. There are only a few of them and
    nothing else matters if they don't run.

    Every other fetch waits for a slot. Not more than so many of them run at
    once, separately for the repositories that were fetched successfully the
    last time and for all the others, so that new or broken repositories can't
    take the slots of the ones that work. Besides, every group has a window,
    the number of fetches it can have running at once. The window gets bigger
    by one after every fetch that succeeds and halves after every one that
    fails, the way the congestion window of TCP does. A fetch needs room in
    the windows of all the groups it is inside of, so a delegated CA can't get
    more slots by making groups inside its own.

    When a slot is free, it goes to the fetch whose groups have spent the least
    time fetching lately, comparing the outermost groups first. A group that
    keeps its slots busy with slow fetches is served after everybody else, and
    a group that was just discovered is served first.
-}
module RPKI.Fetch.Scheduler where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens

import           Data.Foldable           (for_)
import           Data.Hourglass          (Seconds (..))
import           Data.List               (sortOn)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromMaybe, listToMaybe)

import           GHC.Generics

import           RPKI.Repository
import           RPKI.Time


-- | Which slots a fetch takes
data FetchPool = TrustedPool | UntrustedPool
    deriving stock (Show, Eq, Ord, Generic)

data FetchOutcome = GoodFetch | BadFetch
    deriving stock (Show, Eq, Ord, Generic)

data SchedulerConfig = SchedulerConfig {
        -- | How many fetches of all groups but the root ones can run at once, per pool
        poolCapacity  :: Int,
        -- | The window of a group nothing is known about yet
        initialWindow :: Int,
        -- | How fast the time spent fetching is forgotten
        costHalfLife  :: Seconds
    }
    deriving stock (Show, Eq, Generic)

data GroupState = GroupState {
        window   :: Int,
        inFlight :: Int,
        -- | Seconds spent fetching, as of `costAt`. It halves every `costHalfLife`.
        cost     :: Double,
        costAt   :: Instant
    }
    deriving stock (Show, Eq, Generic)

data Ticket = Ticket {
        ticketId :: Int,
        group    :: FetchGroup,
        pool     :: FetchPool,
        granted  :: TVar Bool
    }
    deriving stock (Generic)

data FetchScheduler = FetchScheduler {
        config  :: SchedulerConfig,
        running :: TVar (Map FetchPool Int),
        groups  :: TVar (Map FetchGroup GroupState),
        waiting :: TVar (Map Int Ticket),
        nextId  :: TVar Int
    }
    deriving stock (Generic)


newFetchScheduler :: SchedulerConfig -> STM FetchScheduler
newFetchScheduler config = do
    running <- newTVar mempty
    groups  <- newTVar mempty
    waiting <- newTVar mempty
    nextId  <- newTVar 0
    pure FetchScheduler {..}


-- | Run the fetch once there is a slot for it. `outcomeOf` tells whether the
-- fetch succeeded, an exception is a failure, unless it's an asynchronous one:
-- a fetch that is cancelled tells nothing about the repository.
withFetchSlot :: FetchScheduler -> FetchGroup -> FetchPool -> (a -> FetchOutcome) -> IO a -> IO a
withFetchSlot scheduler group pool outcomeOf f
    | isRootGroup group = f
    | otherwise = mask $ \restore -> do
        Now now <- thisInstant
        ticket  <- atomically $ enqueue now scheduler group pool
        restore (atomically $ readTVar (ticket ^. #granted) >>= check)
            `onException` cancelTicket scheduler ticket

        Now start <- thisInstant
        r <- try $ restore f
        Now end <- thisInstant
        let spent = durationMs start end
        case r of
            Right a -> do
                finish end ticket (Just $ outcomeOf a) spent
                pure a
            Left (e :: SomeException) -> do
                let outcome = case fromException e of
                        Just (_ :: SomeAsyncException) -> Nothing
                        Nothing                        -> Just BadFetch
                finish end ticket outcome spent
                throwIO e
  where
    finish now ticket outcome spent =
        atomically $ releaseSlot now scheduler ticket outcome spent


-- | Put the fetch in the queue and give out the slots that are free.
enqueue :: Instant -> FetchScheduler -> FetchGroup -> FetchPool -> STM Ticket
enqueue now scheduler@FetchScheduler {..} group pool = do
    ticketId <- stateTVar nextId (\n -> (n, n + 1))
    granted  <- newTVar False
    let ticket = Ticket {..}
    modifyTVar' waiting $ Map.insert ticketId ticket
    dispatch now scheduler
    pure ticket


-- | The ticket is not needed anymore, whether it got its slot or not.
cancelTicket :: FetchScheduler -> Ticket -> IO ()
cancelTicket scheduler@FetchScheduler {..} ticket = do
    Now now <- thisInstant
    atomically $ do
        readTVar (ticket ^. #granted) >>= \case
            True  -> releaseSlot now scheduler ticket Nothing (TimeMs 0)
            False -> do
                modifyTVar' waiting $ Map.delete (ticket ^. #ticketId)
                dispatch now scheduler


-- | Give the slot back. A fetch that has an outcome changes the windows of its
-- groups, the time it took is added to their cost anyway.
releaseSlot :: Instant -> FetchScheduler -> Ticket -> Maybe FetchOutcome -> TimeMs -> STM ()
releaseSlot now scheduler@FetchScheduler {..} Ticket {..} outcome (TimeMs spentMs) = do
    modifyTVar' running $ Map.adjust (\n -> max 0 (n - 1)) pool
    modifyTVar' groups $ \gs -> foldr (Map.adjust release_) gs (groupPath group)
    dispatch now scheduler
  where
    spent = fromIntegral spentMs / 1000
    release_ gs@GroupState {..} = gs {
            inFlight = max 0 (inFlight - 1),
            window   = case outcome of
                            Nothing             -> window
                            Just GoodFetch -> min (config ^. #poolCapacity) (window + 1)
                            Just BadFetch    -> max 1 (window `div` 2),
            cost     = decayedCost config now gs + spent,
            costAt   = now
        }


-- | Give the free slots to the tickets that can have them, the ones of the
-- groups that spent the least time fetching first.
dispatch :: Instant -> FetchScheduler -> STM ()
dispatch now scheduler@FetchScheduler {..} = do
    ws <- readTVar waiting
    gs <- readTVar groups
    rs <- readTVar running
    let stateOf g = fromMaybe (newGroupState config now) $ Map.lookup g gs
    let hasRoom g = let GroupState {..} = stateOf g in inFlight < window
    let poolHasRoom p = Map.findWithDefault 0 p rs < config ^. #poolCapacity
    let eligible t = poolHasRoom (t ^. #pool) && all hasRoom (groupPath $ t ^. #group)
    let priority t = (groupCosts config now gs (t ^. #group), t ^. #ticketId)

    for_ (listToMaybe $ sortOn priority $ filter eligible $ Map.elems ws) $ \ticket -> do
        writeTVar (ticket ^. #granted) True
        modifyTVar' waiting $ Map.delete (ticket ^. #ticketId)
        modifyTVar' running $ Map.insertWith (+) (ticket ^. #pool) 1
        writeTVar groups $ foldr
            (\g -> Map.insert g ((stateOf g) & #inFlight %~ (+ 1)))
            gs (groupPath $ ticket ^. #group)
        dispatch now scheduler


-- | What a group spent fetching lately, the outermost group first. The less, 
-- the sooner its fetches get a slot.
groupCosts :: SchedulerConfig -> Instant -> Map FetchGroup GroupState -> FetchGroup -> [Double]
groupCosts config now gs group = 
    [ maybe 0 (decayedCost config now) (Map.lookup g gs) | g <- groupPath group ]


-- | Which of the groups that need a repository it is fetched for: the TA 
-- operator if it needs it, otherwise the group that spent the least time 
-- fetching lately. A repository shared by many delegated CAs, e.g. of a 
-- hosting provider, is then not at the mercy of the most expensive of them: 
-- a group can make itself more expensive, but not cheaper than the others.
cheapestGroup :: FetchScheduler -> [FetchGroup] -> IO (Maybe FetchGroup)
cheapestGroup FetchScheduler {..} candidates = do 
    Now now <- thisInstant
    gs <- readTVarIO groups
    pure $ pickCheapest config now gs candidates

pickCheapest :: SchedulerConfig -> Instant -> Map FetchGroup GroupState -> [FetchGroup] -> Maybe FetchGroup
pickCheapest config now gs candidates = 
    case filter isRootGroup candidates of 
        root : _ -> Just root
        []       -> listToMaybe $ sortOn (\g -> (groupCosts config now gs g, ownerGroup [g], g)) candidates


newGroupState :: SchedulerConfig -> Instant -> GroupState
newGroupState config now = GroupState {
        window   = config ^. #initialWindow,
        inFlight = 0,
        cost     = 0,
        costAt   = now
    }

decayedCost :: SchedulerConfig -> Instant -> GroupState -> Double
decayedCost config now GroupState {..} =
    cost * 0.5 ** (elapsed / halfLife)
  where
    Seconds halfLifeSeconds = config ^. #costHalfLife
    halfLife = max 1 (fromIntegral halfLifeSeconds)
    elapsed  = max 0 (fromIntegral (toNanoseconds now - toNanoseconds costAt) / 1e9)


-- | The state of every group the scheduler knows about, with its cost as of now.
schedulerGroups :: FetchScheduler -> IO (Map FetchGroup GroupState)
schedulerGroups FetchScheduler {..} = do
    Now now <- thisInstant
    atomically $ Map.map (\gs -> gs { cost = decayedCost config now gs, costAt = now })
                    <$> readTVar groups

-- | How many fetches wait for their slot, per group
waitingPerGroup :: FetchScheduler -> STM (Map FetchGroup Int)
waitingPerGroup FetchScheduler {..} =
    Map.fromListWith (+) . map (\t -> (t ^. #group, 1)) . Map.elems <$> readTVar waiting

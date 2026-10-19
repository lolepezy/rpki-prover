module RPKI.Fetch.SchedulerSpec where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Control.Monad

import qualified Data.ByteString                 as BS
import           Data.Hourglass                  (Seconds (..))
import qualified Data.Map.Strict                 as Map

import           Test.Tasty
import qualified Test.Tasty.HUnit                as HU

import           RPKI.Domain
import           RPKI.Fetch.Scheduler
import           RPKI.Repository
import           RPKI.Time


schedulerSpec :: TestTree
schedulerSpec = testGroup "Fetch scheduler" [
        HU.testCase "A group runs as many fetches as its window" shouldLimitFetchesByWindow,
        HU.testCase "The window grows after a good fetch and halves after a bad one" shouldAdjustWindow,
        HU.testCase "A fetch needs room in the windows of all its groups" shouldUseWindowsOfOuterGroups,
        HU.testCase "Every pool has its own slots" shouldHaveSlotsPerPool,
        HU.testCase "A free slot goes to the group that spent less time fetching" shouldPreferCheaperGroups,
        HU.testCase "Inner groups don't make the outer one cheaper" shouldCompareOuterGroupsFirst,
        HU.testCase "Fetches of the root group don't wait" shouldNotLimitRootGroup,
        HU.testCase "A repository of many groups is fetched for the cheapest one" shouldPickCheapestGroup,
        HU.testCase "A cancelled ticket leaves the queue or gives its slot back" shouldCancelTickets,
        HU.testCase "An exception is a bad fetch, a cancellation is neither" shouldTellExceptionsFromCancellation
    ]


shouldLimitFetchesByWindow :: HU.Assertion
shouldLimitFetchesByWindow = do
    s <- newScheduler 10
    [t1, t2, t3] <- replicateM 3 $ atomically $ enqueue now s (groupOf [1]) UntrustedPool
    grantedOf [t1, t2, t3] >>= HU.assertEqual "Two of them, the initial window" [True, True, False]

    atomically $ releaseSlot now s t1 (Just GoodFetch) (TimeMs 100)
    grantedOf [t3] >>= HU.assertEqual "The window is bigger and the slot is free" [True]
    windowOf s (groupOf [1]) >>= HU.assertEqual "Grown by one" 3


shouldAdjustWindow :: HU.Assertion
shouldAdjustWindow = do
    s <- newScheduler 10
    let fetchWith outcome = do
            t <- atomically $ enqueue now s (groupOf [1]) UntrustedPool
            atomically $ releaseSlot now s t (Just outcome) (TimeMs 10)
            windowOf s (groupOf [1])
    windows <- mapM fetchWith [GoodFetch, GoodFetch, BadFetch, BadFetch, BadFetch, GoodFetch]
    HU.assertEqual "+1, +1, /2, /2, not less than 1, +1" [3, 4, 2, 1, 1, 2] windows

    -- Not bigger than the pool
    forM_ [1 .. 20 :: Int] $ const $ fetchWith GoodFetch
    windowOf s (groupOf [1]) >>= HU.assertEqual "The window of the pool at most" 10


shouldUseWindowsOfOuterGroups :: HU.Assertion
shouldUseWindowsOfOuterGroups = do
    s <- newScheduler 10
    t1 <- atomically $ enqueue now s (groupOf [1, 2]) UntrustedPool
    t2 <- atomically $ enqueue now s (groupOf [1, 3]) UntrustedPool
    t3 <- atomically $ enqueue now s (groupOf [1, 2]) UntrustedPool
    grantedOf [t1, t2, t3] >>= HU.assertEqual
        "The inner groups have room, the outer one doesn't" [True, True, False]

    -- A failure of an inner group halves the window of the outer one as well
    atomically $ releaseSlot now s t2 (Just BadFetch) (TimeMs 10)
    windowOf s (groupOf [1]) >>= HU.assertEqual "Halved" 1
    grantedOf [t3] >>= HU.assertEqual "Still no room" [False]


shouldHaveSlotsPerPool :: HU.Assertion
shouldHaveSlotsPerPool = do
    s <- newScheduler 1
    t1 <- atomically $ enqueue now s (groupOf [1]) UntrustedPool
    t2 <- atomically $ enqueue now s (groupOf [2]) UntrustedPool
    t3 <- atomically $ enqueue now s (groupOf [3]) TrustedPool
    grantedOf [t1, t2, t3] >>= HU.assertEqual
        "Fetches of repositories that worked are not stuck behind the others" [True, False, True]


shouldPreferCheaperGroups :: HU.Assertion
shouldPreferCheaperGroups = do
    s <- newScheduler 1
    -- Group 1 spent 100 seconds fetching
    t0 <- atomically $ enqueue now s (groupOf [1]) UntrustedPool
    atomically $ releaseSlot now s t0 (Just BadFetch) (TimeMs 100_000)

    busy <- atomically $ enqueue now s (groupOf [3]) UntrustedPool
    t1   <- atomically $ enqueue now s (groupOf [1]) UntrustedPool
    t2   <- atomically $ enqueue now s (groupOf [2]) UntrustedPool
    grantedOf [busy, t1, t2] >>= HU.assertEqual "One slot" [True, False, False]

    atomically $ releaseSlot now s busy (Just GoodFetch) (TimeMs 10)
    grantedOf [t1, t2] >>= HU.assertEqual "The one that came later, but spent nothing" [False, True]

    -- The cost is forgotten with time, it halves every half-life
    groups_ <- atomically $ readTVar (s ^. #groups)
    let later = momentAfter now (Seconds 3600)
    HU.assertEqual "Half of it an hour later" 50 $
        maybe 0 (decayedCost (s ^. #config) later) (Map.lookup (groupOf [1]) groups_)


shouldCompareOuterGroupsFirst :: HU.Assertion
shouldCompareOuterGroupsFirst = do
    s <- newScheduler 1
    -- The outer group 1 spent a lot, its new inner group 1/5 nothing yet
    t0 <- atomically $ enqueue now s (groupOf [1, 4]) UntrustedPool
    atomically $ releaseSlot now s t0 (Just BadFetch) (TimeMs 100_000)

    busy <- atomically $ enqueue now s (groupOf [3]) UntrustedPool
    t1   <- atomically $ enqueue now s (groupOf [1, 5]) UntrustedPool
    t2   <- atomically $ enqueue now s (groupOf [2, 6]) UntrustedPool
    atomically $ releaseSlot now s busy (Just GoodFetch) (TimeMs 10)
    grantedOf [t1, t2] >>= HU.assertEqual "A new inner group pays for the outer one" [False, True]


shouldNotLimitRootGroup :: HU.Assertion
shouldNotLimitRootGroup = do
    s <- newScheduler 0
    r <- withFetchSlot s (rootGroup testTa) UntrustedPool (const GoodFetch) (pure (42 :: Int))
    HU.assertEqual "Ran with no slots at all" 42 r


shouldPickCheapestGroup :: HU.Assertion
shouldPickCheapestGroup = do
    s <- newScheduler 10
    -- Group 1 is slow, group 2 spent a little, group 3 nothing yet
    forM_ [(1, 100_000), (2, 1_000)] $ \(n, spent) -> do 
        t <- atomically $ enqueue now s (groupOf [n]) UntrustedPool
        atomically $ releaseSlot now s t (Just BadFetch) (TimeMs spent)
    gs <- readTVarIO (s ^. #groups)
    let pick = pickCheapest (s ^. #config) now gs

    HU.assertEqual "The TA operator if it needs it" 
        (Just $ rootGroup testTa) (pick [groupOf [1], rootGroup testTa])
    HU.assertEqual "Not the expensive one" (Just $ groupOf [2]) (pick [groupOf [1], groupOf [2]])
    HU.assertEqual "One that spent nothing" (Just $ groupOf [3]) (pick [groupOf [1], groupOf [2], groupOf [3]])
    HU.assertEqual "An inner group of an expensive one is expensive" 
        (Just $ groupOf [2]) (pick [groupOf [1, 4], groupOf [2]])
    HU.assertEqual "Equal costs, the one closer to the TA" 
        (Just $ groupOf [5]) (pick [groupOf [3, 6], groupOf [5]])
    HU.assertEqual "Nobody" Nothing (pick [])


shouldCancelTickets :: HU.Assertion
shouldCancelTickets = do
    s <- newScheduler 1
    t1 <- atomically $ enqueue now s (groupOf [1]) UntrustedPool
    t2 <- atomically $ enqueue now s (groupOf [2]) UntrustedPool
    cancelTicket s t2
    atomically (Map.size <$> readTVar (s ^. #waiting)) >>= HU.assertEqual "Not waiting anymore" 0

    cancelTicket s t1
    atomically (Map.lookup UntrustedPool <$> readTVar (s ^. #running)) >>= HU.assertEqual "The slot is free" (Just 0)
    windowOf s (groupOf [1]) >>= HU.assertEqual "Says nothing about the group" 2


shouldTellExceptionsFromCancellation :: HU.Assertion
shouldTellExceptionsFromCancellation = do
    s <- newScheduler 4
    -- Make the window 3
    void $ withFetchSlot s (groupOf [1]) UntrustedPool (const GoodFetch) (pure ())
    windowOf s (groupOf [1]) >>= HU.assertEqual "Grown" 3

    r <- try $ withFetchSlot s (groupOf [1]) UntrustedPool (const GoodFetch) (throwIO $ userError "broken")
    HU.assertBool "The exception gets through" (either (\(_ :: IOException) -> True) (const False) r)
    windowOf s (groupOf [1]) >>= HU.assertEqual "A bad fetch" 1

    started <- newEmptyMVar
    fetch <- async $ withFetchSlot s (groupOf [1]) UntrustedPool (const GoodFetch) $ do
                putMVar started ()
                forever $ threadDelaySeconds 1
    takeMVar started
    cancel fetch
    windowOf s (groupOf [1]) >>= HU.assertEqual "Neither good nor bad" 1
    atomically (Map.lookup UntrustedPool <$> readTVar (s ^. #running)) >>= HU.assertEqual "The slot is free" (Just 0)
    atomically (maybe 0 (^. #inFlight) . Map.lookup (groupOf [1]) <$> readTVar (s ^. #groups))
        >>= HU.assertEqual "Nothing in flight" 0
  where
    threadDelaySeconds n = void $ registerDelay (n * 1_000_000) >>= \v -> atomically (readTVar v >>= check)


newScheduler :: Int -> IO FetchScheduler
newScheduler capacity = atomically $ newFetchScheduler SchedulerConfig {
        poolCapacity  = capacity,
        initialWindow = 2,
        costHalfLife  = Seconds 3600
    }

grantedOf :: [Ticket] -> IO [Bool]
grantedOf = mapM (readTVarIO . (^. #granted))

windowOf :: FetchScheduler -> FetchGroup -> IO Int
windowOf s g = atomically $ maybe 2 (^. #window) . Map.lookup g <$> readTVar (s ^. #groups)

testTa :: TaName
testTa = TaName "ta"

groupOf :: [Int] -> FetchGroup
groupOf ns = FetchGroup testTa [ SKI $ mkKI $ BS.pack [fromIntegral n] | n <- ns ]

now :: Instant
now = Instant 1_700_000_000_000_000_000

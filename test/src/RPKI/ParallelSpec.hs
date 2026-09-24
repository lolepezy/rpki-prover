{-# LANGUAGE OverloadedStrings #-}

module RPKI.ParallelSpec where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (ErrorCall (..), evaluate, throwIO, try)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (liftIO)
import           Data.IORef
import qualified Data.List               as List
import           Data.Maybe              (fromMaybe)
import           Effectful               (Eff, IOE, runEff)
import           Streaming               (Of, Stream)
import qualified Streaming.Prelude       as S

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU
import           Test.Tasty.QuickCheck   as QC

import           GHC.Conc                (getNumCapabilities, getNumProcessors)

import           RPKI.AppMonad
import           RPKI.Cpu                (getPhysicalCpuCount, limitDirs, parseCpuList, parseCpuMax, quotaCpus)
import           RPKI.Parallel
import           RPKI.Reporting


parallelSpec :: TestTree
parallelSpec = testGroup "Parallel" [
        cpuCountSpec,
        workPoolSpec,
        QC.testProperty "txPoolPipeline consumes every item once, in item order" $
            \(items :: [Int]) -> ioProperty $ do
                (r, consumed, _) <- runPool ItemOrder (S.each items) unevenWork (\_ -> pure ())
                pure $ r == Right () && consumed == map (* 2) items,

        QC.testProperty "txPoolPipeline consumes every item once, in completion order" $
            \(items :: [Int]) -> ioProperty $ do
                (r, consumed, _) <- runPool CompletionOrder (S.each items) unevenWork (\_ -> pure ())
                pure $ r == Right () && List.sort consumed == List.sort (map (* 2) items),

        QC.testProperty "txPoolPipeline takes items from a stream doing IO, once each" $
            \(items :: [Int]) inItemOrder -> ioProperty $ do
                pulled <- newIORef (0 :: Int)
                let stream = S.mapM (\x -> atomicModifyIORef' pulled (\n -> (n + 1, x))) (S.each items)
                    (order, arrange) = if inItemOrder 
                                        then (ItemOrder, id) 
                                        else (CompletionOrder, List.sort)
                (r, consumed, _) <- runPool order stream unevenWork (\_ -> pure ())
                pulledCount <- readIORef pulled
                pure $ r == Right () 
                    && pulledCount == length items
                    && arrange consumed == arrange (map (* 2) items),

        HU.testCase "txPoolPipeline runs the transaction once, also with no items" $ do
            (_, _, txCount)  <- runPool ItemOrder (S.each []) pure (\_ -> pure ())
            HU.assertEqual "transactions" 1 txCount
            (_, _, txCount') <- runPool CompletionOrder (S.each [1 .. 1000]) pure (\_ -> pure ())
            HU.assertEqual "transactions" 1 txCount',

        HU.testCase "txPoolPipeline fails, instead of hanging, when a worker throws" $ do
            r <- try $ runPool ItemOrder (S.each [1 .. 1000])
                    (\i -> do
                        when (i == 500) $ throwIO $ ErrorCall "boom"
                        pure i)
                    (\_ -> pure ())
            case r of
                Left (ErrorCall "boom") -> pure ()
                Left e                  -> HU.assertFailure $ "Unexpected exception " <> show e
                Right _                 -> HU.assertFailure "Should have failed",

        HU.testCase "txPoolPipeline returns the consumer's validation error" $ do
            (r, _, _) <- runPool ItemOrder (S.each [1 .. 1000]) pure $ \i ->
                when (i == 500) $ appError $ UnspecifiedE "consumer" "failed"
            HU.assertEqual "result" (Left $ UnspecifiedE "consumer" "failed") r
    ]
  where
    runPool :: ResultOrder -> Stream (Of Int) IO () -> (Int -> IO Int) -> (Int -> Eff AppEffects ())
            -> IO (Either AppError (), [Int], Int)
    runPool order items process onItem = do
        consumed <- newIORef []
        txCount  <- newIORef (0 :: Int)
        (r, _) <- runValidatorIO (newScopes "parallel") $
            txPoolPipeline order items
                (liftIO . (evaluate =<<) . process)
                (\f -> liftIO (modifyIORef' txCount (+ 1)) >> f ())
                (\() p -> do
                    liftIO $ modifyIORef' consumed (p :)
                    onItem p)
        (r, , ) <$> (reverse <$> readIORef consumed) <*> readIORef txCount


workPoolSpec :: TestTree
workPoolSpec = testGroup "Work pool" [
        QC.testProperty "forInPool gives results in the order of the items" $
            \(items :: [Int]) (QC.Positive chunkSize) -> ioProperty $ do
                r <- inPool Nothing $ \pool -> 
                        forInPool pool chunkSize even items $ \x -> 
                            liftIO $ unevenWork x
                pure $ r == map (* 2) items,

        HU.testCase "Runs a tree of tasks waiting for tasks, also with one worker" $ do
            let expected = treeSize 5 4
            r  <- inPool Nothing  $ \pool -> tree pool 5 4
            r1 <- inPool (Just 1) $ \pool -> tree pool 5 4
            HU.assertEqual "all workers" expected r
            HU.assertEqual "one worker"  expected r1,

        HU.testCase "Gives an exception to whoever waits, after all the other tasks are done" $ do
            finished <- newIORef (0 :: Int)
            r <- try $ inPool Nothing $ \pool -> 
                    forInPool pool 1 (const True) [1 .. 100 :: Int] $ \x -> liftIO $ do
                        when (x == 50) $ throwIO $ ErrorCall "boom"
                        threadDelay 1000
                        atomicModifyIORef' finished (\n -> (n + 1, ()))
            doneThen <- readIORef finished
            threadDelay 20_000
            doneLater <- readIORef finished
            case r of
                Left (ErrorCall "boom") -> pure ()
                Left e                  -> HU.assertFailure $ "Unexpected exception " <> show e
                Right _                 -> HU.assertFailure "Should have failed"
            HU.assertEqual "all the others finished" 99 doneThen
            HU.assertEqual "nothing running after" doneThen doneLater
    ]
  where
    -- Each level waits for the next one, every node is a task of its own
    tree :: WorkPool -> Int -> Int -> Eff '[IOE] Int
    tree _ 0 _ = pure 1
    tree pool depth fanOut = do
        children <- forInPool pool 1 (const True) [1 .. fanOut] $ \_ -> 
                        tree pool (depth - 1) fanOut
        pure $ 1 + sum children

    treeSize :: Int -> Int -> Int
    treeSize 0 _ = 1
    treeSize depth fanOut = 1 + fanOut * treeSize (depth - 1) fanOut

    -- Run the action as a task of a pool, as it's meant to be used
    inPool workers f = do
        caps <- getNumCapabilities
        pool <- newWorkPool
        withWorkers pool [0 .. fromMaybe caps workers - 1] $ do
            task <- submitTask pool $ runEff $ f pool
            waitTask task >>= either throwIO pure


-- Uneven, so that the results are ready out of order
unevenWork :: Int -> IO Int
unevenWork x = threadDelay (abs x `mod` 7 * 50) >> pure (x * 2)


cpuCountSpec :: TestTree
cpuCountSpec = testGroup "Physical CPU count" [
        HU.testCase "Parses the kernel's CPU list format" $ do
            HU.assertEqual "range"   (Just [0 .. 15]) (parseCpuList "0-15")
            HU.assertEqual "as in /proc/self/status" (Just [0 .. 15]) (parseCpuList "\t0-15\n")
            HU.assertEqual "ranges"  (Just ([0 .. 3] <> [8 .. 11])) (parseCpuList "0-3,8-11")
            HU.assertEqual "singles" (Just [0, 8]) (parseCpuList "0,8\n")
            HU.assertEqual "empty"   (Just []) (parseCpuList "")
            HU.assertEqual "reversed range" Nothing (parseCpuList "3-1")
            HU.assertEqual "negative" Nothing (parseCpuList "-1")
            HU.assertEqual "garbage" Nothing (parseCpuList "0-3,x"),

        HU.testCase "Reads cgroup v2 cpu.max" $ do
            HU.assertEqual "no quota"    Nothing  (parseCpuMax "max 100000\n")
            HU.assertEqual "two CPUs"    (Just 2) (parseCpuMax "200000 100000\n")
            HU.assertEqual "rounds up"   (Just 3) (parseCpuMax "250000 100000")
            HU.assertEqual "at least one" (Just 1) (parseCpuMax "10000 100000")
            HU.assertEqual "garbage"     Nothing  (parseCpuMax "200000"),

        HU.testCase "Treats a v1 quota of -1 as none" $ do
            HU.assertEqual "none"      Nothing  (quotaCpus (-1) 100000)
            HU.assertEqual "1.5 CPUs"  (Just 2) (quotaCpus 150000 100000),

        HU.testCase "Looks for limits up to the root of the cgroup mount" $ do
            HU.assertEqual "nested" 
                ["/sys/fs/cgroup/user.slice/session.scope", "/sys/fs/cgroup/user.slice", "/sys/fs/cgroup"]
                (limitDirs "/sys/fs/cgroup" "/user.slice/session.scope")
            HU.assertEqual "container root" ["/sys/fs/cgroup"] (limitDirs "/sys/fs/cgroup" "/")
            HU.assertEqual "no escaping the mount" ["/sys/fs/cgroup"] (limitDirs "/sys/fs/cgroup" "/../.."),

        HU.testCase "Never more than the logical processors, never zero" $ do
            processors <- getNumProcessors
            getPhysicalCpuCount >>= \case
                Nothing -> pure ()
                Just n  -> HU.assertBool (show n <> " cores for " <> show processors <> " processors") $
                                n >= 1 && fromIntegral n <= processors
    ]

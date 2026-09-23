{-# LANGUAGE OverloadedStrings #-}

module RPKI.ParallelSpec where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (ErrorCall (..), evaluate, throwIO, try)
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (liftIO)
import           Data.IORef
import qualified Data.List               as List
import           Effectful               (Eff)

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU
import           Test.Tasty.QuickCheck   as QC

import           GHC.Conc                (getNumProcessors)

import           RPKI.AppMonad
import           RPKI.Cpu                (getPhysicalCpuCount, limitDirs, parseCpuList, parseCpuMax, quotaCpus)
import           RPKI.Parallel
import           RPKI.Reporting


parallelSpec :: TestTree
parallelSpec = testGroup "Parallel" [
        cpuCountSpec,
        QC.testProperty "txPoolPipeline consumes every item once, in item order" $
            \(items :: [Int]) -> ioProperty $ do
                (r, consumed, _) <- runPool ItemOrder items unevenWork (\_ -> pure ())
                pure $ r == Right () && consumed == map (* 2) items,

        QC.testProperty "txPoolPipeline consumes every item once, in completion order" $
            \(items :: [Int]) -> ioProperty $ do
                (r, consumed, _) <- runPool CompletionOrder items unevenWork (\_ -> pure ())
                pure $ r == Right () && List.sort consumed == List.sort (map (* 2) items),

        HU.testCase "txPoolPipeline runs the transaction once, also with no items" $ do
            (_, _, txCount)  <- runPool ItemOrder [] pure (\_ -> pure ())
            HU.assertEqual "transactions" 1 txCount
            (_, _, txCount') <- runPool CompletionOrder [1 .. 1000] pure (\_ -> pure ())
            HU.assertEqual "transactions" 1 txCount',

        HU.testCase "txPoolPipeline fails, instead of hanging, when a worker throws" $ do
            r <- try $ runPool ItemOrder [1 .. 1000]
                    (\i -> do
                        when (i == 500) $ throwIO $ ErrorCall "boom"
                        pure i)
                    (\_ -> pure ())
            case r of
                Left (ErrorCall "boom") -> pure ()
                Left e                  -> HU.assertFailure $ "Unexpected exception " <> show e
                Right _                 -> HU.assertFailure "Should have failed",

        HU.testCase "txPoolPipeline returns the consumer's validation error" $ do
            (r, _, _) <- runPool ItemOrder [1 .. 1000] pure $ \i ->
                when (i == 500) $ appError $ UnspecifiedE "consumer" "failed"
            HU.assertEqual "result" (Left $ UnspecifiedE "consumer" "failed") r
    ]
  where
    -- Uneven, so that the results are ready out of order
    unevenWork x = threadDelay (abs x `mod` 7 * 50) >> pure (x * 2)

    runPool :: ResultOrder -> [Int] -> (Int -> IO Int) -> (Int -> Eff AppEffects ())
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

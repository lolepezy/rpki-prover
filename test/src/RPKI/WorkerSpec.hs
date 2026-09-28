module RPKI.WorkerSpec where

import           System.Exit

import           Test.Tasty
import qualified Test.Tasty.HUnit        as HU
import qualified Test.Tasty.QuickCheck   as QC

import           RPKI.Worker

workerSpec :: TestTree
workerSpec = testGroup "Worker" [
        workerExitSpec
    ]

{- | 'WorkerExit' is an agreement between two processes: the worker picks a
     reason, the parent has to read the same reason back out of the exit code.
     Nothing else checks that the two tables stay in step.
-}
workerExitSpec :: TestTree
workerExitSpec = testGroup "Worker exit codes" [
        HU.testCase "Every exit reason survives a round-trip through the exit code" $
            mapM_ (\e -> HU.assertEqual (show e) e (fromExitCode $ toExitCode e)) allReasons,

        HU.testCase "Exit reasons don't share an exit code" $ do
            let codes = map toExitCode allReasons
            HU.assertEqual "Duplicate exit codes" (length codes) (length $ nubOrd codes),

        HU.testCase "The codes the worker and the RTS actually use" $ do
            -- Pinned down: these are what a worker process exits with, and the
            -- RTS's own code for a heap overflow. Changing one silently makes
            -- an older parent misread a newer worker.
            HU.assertEqual "exception"   (ExitFailure 99)  (toExitCode WorkerException)
            HU.assertEqual "parent died" (ExitFailure 111) (toExitCode ParentDied)
            HU.assertEqual "cpu"         (ExitFailure 113) (toExitCode OutOfCpuTime)
            HU.assertEqual "traffic"     (ExitFailure 114) (toExitCode TooMuchTraffic)
            HU.assertEqual "disk io"     (ExitFailure 115) (toExitCode TooMuchDiskIo)
            HU.assertEqual "timeout"     (ExitFailure 122) (toExitCode TimedOut)
            HU.assertEqual "replaced"    (ExitFailure 123) (toExitCode ExecutableReplaced)
            HU.assertEqual "out of memory" (ExitFailure 251) (toExitCode OutOfMemory),

        HU.testCase "A process killed by a signal is reported as such" $ do
            -- `System.Process` reports a signal as a negative exit code, so
            -- SIGKILL (what killWorkers and the OOM killer send) has to come
            -- back as a signal rather than as an unknown code.
            HU.assertEqual "SIGKILL" (KilledBySignal 9)  (fromExitCode $ ExitFailure (-9))
            HU.assertEqual "SIGTERM" (KilledBySignal 15) (fromExitCode $ ExitFailure (-15))
            HU.assertEqual "SIGINT"  (KilledBySignal 2)  (fromExitCode $ ExitFailure (-2)),

        QC.testProperty "Any exit code is understood as something" $ \n ->
            let code = if n == (0 :: Int) then ExitSuccess else ExitFailure n
            in toExitCode (fromExitCode code) == code
    ]
  where
    allReasons = [
            WorkerSucceeded, WorkerException, ParentDied, OutOfCpuTime,
            TooMuchTraffic, TooMuchDiskIo, TimedOut, ExecutableReplaced, OutOfMemory
        ] <> map KilledBySignal [1, 2, 9, 15] <> map UnknownExit [1, 42, 200]

    nubOrd = foldr (\x acc -> x : filter (/= x) acc) []

{-# LANGUAGE OverloadedStrings #-}

module RPKI.SandboxSpec where

import           Control.Lens
import           Control.Monad

import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.List                (isPrefixOf, sort)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as Text

import           System.Directory
import           System.Environment       (getExecutablePath)
import           System.Exit
import           System.FilePath
import           System.IO.Temp           (withSystemTempDirectory)
import           System.Posix.Process     (getProcessID)
import           System.Process.Typed

import           Test.Tasty
import qualified Test.Tasty.HUnit         as HU

import           RPKI.AppTypes
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Meta.UniqueId
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Rsync
import           RPKI.Sandbox
import           RPKI.Time
import           RPKI.Util                (parseRsyncURL)
import           RPKI.Worker


sandboxSpec :: TestTree
sandboxSpec = testGroup "Sandbox" [
        workerSandboxGroup,
        rsyncCommandGroup,
        launcherGroup
    ]

root :: FilePath
root = "/var/lib/rpki"

testConfig :: SandboxMode -> Config
testConfig mode = defaultConfig
    & #rootDirectory .~ Hidden root
    & #programBinaryPath .~ Hidden "/opt/rpki-prover/bin/rpki-prover"
    & #rsyncConf . #rsyncClientPath ?~ Hidden "/usr/bin/rsync"
    & #rsyncConf . #rsyncRoot .~ Hidden (root </> "rsync")
    & #systemConfig . #sandboxMode .~ mode

rsyncUrl :: Text.Text -> RsyncURL
rsyncUrl u = either (error . show) id $ parseRsyncURL u

workerInput :: SandboxMode -> WorkerParams -> IO WorkerInput
workerInput mode params = do
    pid <- getProcessID
    let config = testConfig mode
    pure WorkerInput {
            workerId                = WorkerId "test",
            params                  = params,
            config                  = config,
            initialParentId         = pid,
            workerTimeout           = Timebox 10,
            cpuLimit                = Nothing,
            ioLimits                = config ^. #systemConfig . #cleanupWorker . #ioLimits,
            parentExecutableVersion = thisExecutableVersion
        }

allParams :: [(String, WorkerParams)]
allParams = [
        ("validation", ValidationParams version [] []),
        ("cache-clean-up", CacheCleanupParams version),
        ("rrdp", RrdpFetchParams (newScopes "test")
                    (newRrdpRepository $ RrdpURL $ URI "https://rrdp.example.com/notification.xml")
                    version),
        ("erik", ErikFetchParams (newScopes "test") testFetchConfig [] (FQDN "rpki.example.com") version),
        ("rsync", rsyncParams "rsync://rpki.example.com/repo/")
    ]
  where
    version = WorldVersion 1

testFetchConfig :: FetchConfig
testFetchConfig = newFetchConfig $ testConfig SandboxIfAvailable

rsyncParams :: Text.Text -> WorkerParams
rsyncParams u = RsyncFetchParams (newScopes "test") testFetchConfig
                    (newRsyncRepository $ rsyncUrl u) (WorldVersion 1)

sandboxOf :: SandboxMode -> WorkerParams -> IO WorkerSandbox
sandboxOf mode params = do
    input <- workerInput mode params
    maybe (HU.assertFailure "expected a sandbox") pure $ workerSandbox input ["/custom/ca.pem"]

isBeneath :: FilePath -> FilePath -> Bool
isBeneath parent child = splitDirectories parent `isPrefixOf` splitDirectories child

workerSandboxGroup :: TestTree
workerSandboxGroup = testGroup "Worker sandbox" [
        HU.testCase "No worker is sandboxed with --sandbox off" $
            forM_ allParams $ \(name, params) -> do
                input <- workerInput NoSandbox params
                HU.assertEqual name Nothing $ workerSandbox input []
        ,
        HU.testCase "Every worker is sandboxed otherwise" $
            forM_ [SandboxIfAvailable, SandboxRequired] $ \mode ->
                forM_ allParams $ \(name, params) -> do
                    input <- workerInput mode params
                    HU.assertBool name $ has _Just $ workerSandbox input []
        ,
        HU.testCase "Workers other than rsync write only to the cache and tmp" $
            forM_ (filter ((/= "rsync") . fst) allParams) $ \(name, params) -> do
                sandbox <- sandboxOf SandboxIfAvailable params
                forM_ (sandboxPaths ReadWrite sandbox) $ \p ->
                    HU.assertBool (name <> " can write to " <> p) $
                        p `elem` [root </> "cache", root </> "tmp"]
                HU.assertEqual (name <> " executes nothing") [] $ sandboxPaths ReadExecute sandbox
        ,
        HU.testCase "Validation and cleanup can only write to the cache and have no network" $
            forM_ ["validation", "cache-clean-up"] $ \name -> do
                sandbox <- sandboxOf SandboxIfAvailable (paramsFor name)
                HU.assertEqual name [root </> "cache"] $ sandboxPaths ReadWrite sandbox
                HU.assertEqual name NoNetwork $ sandbox ^. #network
        ,
        HU.testCase "HTTP fetchers can write to the cache and tmp, read CA certificates and use the network" $
            forM_ ["rrdp", "erik"] $ \name -> do
                sandbox <- sandboxOf SandboxIfAvailable (paramsFor name)
                HU.assertEqual name (sort [root </> "cache", root </> "tmp"]) $
                    sort $ sandboxPaths ReadWrite sandbox
                HU.assertEqual name AnyNetwork $ sandbox ^. #network
                HU.assertBool name $ "/etc/ssl" `elem` sandboxPaths ReadOnly sandbox
                HU.assertBool name $ "/custom/ca.pem" `elem` sandboxPaths ReadOnly sandbox
        ,
        HU.testCase "Rsync fetcher can write to the cache and the mirror and run the client" $ do
            sandbox <- sandboxOf SandboxIfAvailable (rsyncParams "rsync://rpki.example.com/repo/")
            HU.assertEqual "writable" (sort [root </> "cache", root </> "rsync"]) $
                sort $ sandboxPaths ReadWrite sandbox
            let executable = sandboxPaths ReadExecute sandbox
            HU.assertBool "launcher" $ "/opt/rpki-prover/bin/rpki-prover" `elem` executable
            HU.assertBool "rsync client" $ "/usr/bin/rsync" `elem` executable
            HU.assertBool "no shell" $ all (\p -> not ("/bin" `isBeneath` p) && not ("/usr/bin" == p)) executable
            HU.assertEqual "network" (TcpConnect [873, 53]) $ sandbox ^. #network
        ,
        HU.testCase "Rsync fetcher can connect to the port from the URL" $ do
            sandbox <- sandboxOf SandboxIfAvailable (rsyncParams "rsync://rpki.example.com:8730/repo/")
            HU.assertEqual "network" (TcpConnect [8730, 53]) $ sandbox ^. #network
        ,
        HU.testCase "Rsync fetcher's sandbox covers everything the rsync client needs" $ do
            -- Landlock layers stack, the client can't get what the worker doesn't have
            let url = "rsync://rpki.example.com:8730/repo/"
            worker <- sandboxOf SandboxIfAvailable (rsyncParams url)
            let client = rsyncClientSandbox (testConfig SandboxIfAvailable) (rsyncUrl url)
                            (root </> "rsync" </> "rpki.example.com_8730" </> "repo")
            HU.assertEqual "network" (client ^. #network) (worker ^. #network)
            forM_ (client ^. #paths) $ \SandboxPath {..} ->
                HU.assertBool ("worker doesn't cover " <> location) $
                    any (\w -> (w ^. #location) `isBeneath` location
                            && covers (w ^. #access) access) (worker ^. #paths)
    ]
  where
    paramsFor name = fromMaybe (error name) $ lookup name allParams

    covers ReadWrite   _           = True
    covers ReadExecute ReadExecute = True
    covers ReadExecute ReadOnly    = True
    covers ReadOnly    ReadOnly    = True
    covers _           _           = False

rsyncCommandGroup :: TestTree
rsyncCommandGroup = testGroup "Rsync client command" [
        HU.testCase "Directory is writable for the whole repository" $ do
            let destination = root </> "rsync/rpki.example.com/repo/"
            let command = rsyncCommand (testConfig SandboxIfAvailable) testFetchConfig
                            (rsyncUrl "rsync://rpki.example.com/repo/") destination RsyncDirectory
            HU.assertEqual "writable" (Just [destination]) $
                sandboxPaths ReadWrite <$> command ^. #rsyncSandbox
        ,
        HU.testCase "Only the directory of a single file is writable" $ do
            let destination = root </> "rsync/rpki.example.com/repo/ta.cer"
            let command = rsyncCommand (testConfig SandboxIfAvailable) testFetchConfig
                            (rsyncUrl "rsync://rpki.example.com/repo/ta.cer") destination RsyncOneFile
            HU.assertEqual "writable" (Just [root </> "rsync/rpki.example.com/repo"]) $
                sandboxPaths ReadWrite <$> command ^. #rsyncSandbox
        ,
        HU.testCase "Rsync runs through the launcher with a clean environment" $ do
            let destination = root </> "rsync/rpki.example.com/repo/"
            let command = rsyncCommand (testConfig SandboxRequired) testFetchConfig
                            (rsyncUrl "rsync://rpki.example.com/repo/") destination RsyncDirectory
            let (program, args, environment) = rsyncLauncherCommand command
            HU.assertEqual "program" "/opt/rpki-prover/bin/rpki-prover" program
            HU.assertEqual "arguments" ["--sandboxed-exec", "/usr/bin/rsync"] $ take 2 args
            HU.assertEqual "rsync arguments" [destination] $ drop (length args - 1) args
            forM_ environment $ \(name, _) ->
                HU.assertBool ("unexpected variable " <> name) $
                    name `elem` ["PATH", "LC_ALL"] || "RPKI_PROVER_" `isPrefixOf` name
            HU.assertEqual "required" (Just "1") $ lookup "RPKI_PROVER_SANDBOX_REQUIRED" environment
            HU.assertEqual "tcp" (Just "873,53") $ lookup "RPKI_PROVER_SANDBOX_TCP" environment
            HU.assertEqual "file size" (Just $ show $ defaultConfig ^. #validationConfig . #maxObjectSize) $
                lookup "RPKI_PROVER_LIMIT_FSIZE" environment
        ,
        HU.testCase "Without sandbox, rsync still gets limits and a clean environment" $ do
            let command = rsyncCommand (testConfig NoSandbox) testFetchConfig
                            (rsyncUrl "rsync://rpki.example.com/repo/")
                            (root </> "rsync/rpki.example.com/repo/") RsyncDirectory
            let (_, _, environment) = rsyncLauncherCommand command
            HU.assertEqual "sandbox" Nothing $ command ^. #rsyncSandbox
            HU.assertBool "no sandbox variables" $
                not $ any (("RPKI_PROVER_SANDBOX_" `isPrefixOf`) . fst) environment
            HU.assertBool "limits" $ has _Just $ lookup "RPKI_PROVER_LIMIT_CPU" environment
        ,
        HU.testCase "Sandbox is encoded into variables" $ do
            let sandbox = WorkerSandbox {
                    paths = requiredPaths ReadWrite ["/a", "/b"]
                        <> requiredPaths ReadOnly ["/c"]
                        <> optionalPaths ReadExecute ["/d", "/d"],
                    network = TcpConnect [873, 53, 873]
                }
            HU.assertEqual "variables" [
                    ("RPKI_PROVER_SANDBOX_RW", "/a:/b"),
                    ("RPKI_PROVER_SANDBOX_RO", "/c"),
                    ("RPKI_PROVER_SANDBOX_EXEC", "/d"),
                    ("RPKI_PROVER_SANDBOX_TCP", "873,53")
                ] $ sandboxEnvironment sandbox
            HU.assertEqual "no network" (Just "none") $
                lookup "RPKI_PROVER_SANDBOX_TCP" $ sandboxEnvironment sandbox { network = NoNetwork }
            HU.assertEqual "any network" (Just "any") $
                lookup "RPKI_PROVER_SANDBOX_TCP" $ sandboxEnvironment sandbox { network = AnyNetwork }
        ,
        HU.testCase "Optional paths that don't exist are dropped" $ do
            resolved <- resolveSandbox WorkerSandbox {
                    paths = requiredPaths ReadOnly ["/does/not/exist/1"]
                        <> optionalPaths ReadOnly ["/does/not/exist/2", "/"],
                    network = NoNetwork
                }
            HU.assertEqual "paths" ["/does/not/exist/1", "/"] $ sandboxPaths ReadOnly resolved
    ]

-- | The test executable links the same C code, so it works as the launcher.
launcherGroup :: TestTree
launcherGroup = testGroup "Launcher" [
        HU.testCase "Runs the program with limits and without our variables" $ do
            (exit, out) <- launch [("RPKI_PROVER_SOMETHING", "x"), ("FOO", "bar"),
                                   ("RPKI_PROVER_LIMIT_CPU", "7"), ("RPKI_PROVER_LIMIT_NOFILE", "64")]
                                  "/bin/sh" ["-c", "echo \"$FOO|$RPKI_PROVER_SOMETHING|$(ulimit -t)|$(ulimit -n)|$(ulimit -c)\""]
            HU.assertEqual "exit code" ExitSuccess exit
            HU.assertEqual "output" "bar||7|64|0\n" out
        ,
        HU.testCase "Passes the exit code through" $ do
            (exit, _) <- launch [] "/bin/sh" ["-c", "exit 23"]
            HU.assertEqual "exit code" (ExitFailure 23) exit
        ,
        HU.testCase "Refuses a relative program path" $ do
            (exit, _) <- launch [] "sh" ["-c", "true"]
            HU.assertEqual "exit code" launcherSetupFailedExitCode exit
        ,
        HU.testCase "Reports a program that can't be executed" $ do
            (exit, _) <- launch [] "/does/not/exist" []
            HU.assertEqual "exit code" launcherExecFailedExitCode exit
        ,
        HU.testCase "Refuses an invalid limit" $ do
            (exit, _) <- launch [("RPKI_PROVER_LIMIT_CPU", "lots")] "/bin/sh" ["-c", "true"]
            HU.assertEqual "exit code" launcherSetupFailedExitCode exit
        ,
        HU.testCase "Required sandbox" $ do
            abi <- getLandlockAbi
            (exit, _) <- launch (requiredEnvironment <> [("RPKI_PROVER_SANDBOX_EXEC", "/")])
                                "/bin/sh" ["-c", "true"]
            case abi of
                Left _  -> HU.assertEqual "refuses to run" launcherSetupFailedExitCode exit
                Right _ -> HU.assertEqual "runs" ExitSuccess exit
        ,
        HU.testCase "Sandboxed program can only write where it is allowed to (Linux)" $ do
            abi <- getLandlockAbi
            case abi of
                Left _  -> pure ()
                Right _ -> withTemp $ \dir -> do
                    let allowed = dir </> "allowed"
                    let denied  = dir </> "denied"
                    mapM_ (createDirectoryIfMissing True) [allowed, denied]
                    -- A different file from the allowed /bin/sh, even where 
                    -- every command is the same busybox binary
                    let shellCopy = denied </> "sh"
                    copyFileWithMetadata "/bin/sh" shellCopy
                    sandbox <- resolveSandbox WorkerSandbox {
                            paths = requiredPaths ReadWrite [allowed, "/dev/null"]
                                <> requiredPaths ReadExecute ["/bin/sh"]
                                <> optionalPaths ReadExecute systemLibraries,
                            network = NoNetwork
                        }
                    let script = "echo ok > " <> allowed </> "file; "
                              <> "(echo no > " <> denied </> "file) 2>/dev/null; "
                              <> "cat " <> allowed </> "file 2>/dev/null; "
                              <> shellCopy <> " -c true 2>/dev/null || echo cannot-run-copy"
                    (exit, out) <- launch (sandboxEnvironment sandbox) "/bin/sh" ["-c", script]
                    HU.assertEqual "exit code" ExitSuccess exit
                    -- cat is a builtin nowhere but busybox, and is not allowed otherwise
                    HU.assertBool ("output: " <> out) $ 
                        out `elem` ["cannot-run-copy\n", "ok\ncannot-run-copy\n"]
                    doesFileExist (allowed </> "file") >>= HU.assertBool "allowed file is written"
                    doesFileExist (denied </> "file") >>= HU.assertBool "denied file is not written" . not
    ]
  where
    withTemp = withSystemTempDirectory "rpki-prover-sandbox"

    launch environment program args = do
        self <- getExecutablePath
        let (launcher, launcherArgs) = sandboxedCommand self program args
        (exit, out, _) <- readProcess $
            setEnv (environment <> [("PATH", "/usr/bin:/bin")]) $
            proc launcher launcherArgs
        pure (exit, LBS8.unpack out)

{-# LANGUAGE OverloadedStrings #-}

{- | Sandboxing worker processes and the programs they run with Landlock
(Linux only).

The parent describes what a process may touch ('WorkerSandbox') and passes it
in environment variables, @cbits/sandbox.c@ applies it before the GHC runtime
starts any threads, since Landlock only restricts the thread that asks for it
and whatever that thread creates later. Once applied, everything not listed
is denied and it can't be lifted.

 * A worker (@rpki-prover --worker@) sandboxes itself and the Haskell side
   only gets to find out how that went ('getSandboxStatus') and to report it.

 * An external program (the rsync client) is started through the launcher,
   @rpki-prover --sandboxed-exec PROGRAM ARGS...@ ('sandboxedCommand'), which
   closes inherited descriptors, sets resource limits ('ProcessLimits'),
   applies the sandbox on top of whatever its parent already has and then
   becomes PROGRAM. The runtime never starts there.
-}
module RPKI.Sandbox (
    PathAccess(..),
    SandboxPath(..),
    NetworkAccess(..),
    WorkerSandbox(..),
    ProcessLimits(..),
    SandboxStatus(..),
    requiredPaths,
    optionalPaths,
    sandboxPaths,
    resolveSandbox,
    sandboxEnvironment,
    limitsEnvironment,
    requiredEnvironment,
    sandboxVariables,
    sandboxedCommand,
    launcherSetupFailedExitCode,
    launcherExecFailedExitCode,
    getSandboxStatus,
    getLandlockAbi,
    systemLibraries,
    resolverFiles,
    certificateFiles,
    certificateOverrides
) where

import           Control.Monad    (filterM)
import           Data.List        (intercalate, nub)
import           Data.Maybe       (catMaybes)
import           Data.String.Interpolate.IsString
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Word        (Word16)
import           Foreign.C.Error  (Errno (..), errnoToIOError)
import           Foreign.C.String (CString, peekCString)
import           Foreign.C.Types  (CInt (..))
import           GHC.Generics     (Generic)
import           System.Directory (doesPathExist, makeAbsolute)
import           System.Environment (lookupEnv)
import           System.Exit      (ExitCode (..))
import           System.FilePath  (splitSearchPath)
import           System.Info      (os)


data PathAccess = ReadOnly | ReadExecute | ReadWrite
    deriving stock (Eq, Ord, Show, Generic)

-- | A directory includes everything beneath it. Paths must not contain ':'.
data SandboxPath = SandboxPath {
        location  :: FilePath,
        access    :: PathAccess,
        -- | A required path that doesn't exist makes the sandbox fail, an
        -- optional one is left out ('resolveSandbox').
        mustExist :: Bool
    }
    deriving stock (Eq, Ord, Show, Generic)

data NetworkAccess
    = NoNetwork
    -- | Only TCP connections to these ports (DNS over UDP is never restricted).
    | TcpConnect [Word16]
    | AnyNetwork
    deriving stock (Eq, Ord, Show, Generic)

-- | What a sandboxed process is still allowed to access. Below Landlock ABI 4
-- the kernel can't restrict network access, whatever 'network' says.
data WorkerSandbox = WorkerSandbox {
        paths   :: [SandboxPath],
        network :: NetworkAccess
    }
    deriving stock (Eq, Ord, Show, Generic)

-- | Resource limits the launcher sets for the program it runs.
data ProcessLimits = ProcessLimits {
        cpuSeconds        :: Maybe Integer,
        addressSpaceBytes :: Maybe Integer,
        fileSizeBytes     :: Maybe Integer,
        openFiles         :: Maybe Integer
    }
    deriving stock (Eq, Ord, Show, Generic)

data SandboxStatus
    = NotSandboxed
    -- | Applied, with the kernel's Landlock ABI version. Below 4 the kernel
    -- can't restrict network access.
    | Sandboxed Int
    -- | Asked for, but the system doesn't support it.
    | SandboxUnsupported Text
    -- | Asked for, but setting it up failed; the worker must not run.
    | SandboxFailed Text
    deriving stock (Eq, Show)

requiredPaths, optionalPaths :: PathAccess -> [FilePath] -> [SandboxPath]
requiredPaths a = map (\p -> SandboxPath p a True)
optionalPaths a = map (\p -> SandboxPath p a False)

sandboxPaths :: PathAccess -> WorkerSandbox -> [FilePath]
sandboxPaths a WorkerSandbox {..} = nub [ location | SandboxPath {..} <- paths, access == a ]

-- | Make paths absolute (the process may resolve relative ones differently)
-- and drop the optional ones that don't exist.
resolveSandbox :: WorkerSandbox -> IO WorkerSandbox
resolveSandbox sandbox@WorkerSandbox {..} = do
    absolute <- mapM (\p -> (\l -> p { location = l }) <$> makeAbsolute (location p)) paths
    existing <- filterM (\p -> if mustExist p then pure True else doesPathExist (location p)) absolute
    pure sandbox { paths = existing }

-- | Names of all the variables @cbits/sandbox.c@ reads.
sandboxVariables :: [String]
sandboxVariables = [
        rwVariable, roVariable, execVariable, tcpVariable, requiredVariable,
        cpuVariable, asVariable, fsizeVariable, nofileVariable
    ]

rwVariable, roVariable, execVariable, tcpVariable, requiredVariable :: String
rwVariable       = "RPKI_PROVER_SANDBOX_RW"
roVariable       = "RPKI_PROVER_SANDBOX_RO"
execVariable     = "RPKI_PROVER_SANDBOX_EXEC"
tcpVariable      = "RPKI_PROVER_SANDBOX_TCP"
requiredVariable = "RPKI_PROVER_SANDBOX_REQUIRED"

cpuVariable, asVariable, fsizeVariable, nofileVariable :: String
cpuVariable    = "RPKI_PROVER_LIMIT_CPU"
asVariable     = "RPKI_PROVER_LIMIT_AS"
fsizeVariable  = "RPKI_PROVER_LIMIT_FSIZE"
nofileVariable = "RPKI_PROVER_LIMIT_NOFILE"

sandboxEnvironment :: WorkerSandbox -> [(String, String)]
sandboxEnvironment sandbox@WorkerSandbox {..} = [
        (rwVariable,   joined ReadWrite),
        (roVariable,   joined ReadOnly),
        (execVariable, joined ReadExecute),
        (tcpVariable,  tcp)
    ]
  where
    joined a = intercalate ":" $ sandboxPaths a sandbox
    tcp = case network of
        NoNetwork        -> "none"
        AnyNetwork       -> "any"
        TcpConnect []    -> "none"
        TcpConnect ports -> intercalate "," $ map show $ nub ports

limitsEnvironment :: ProcessLimits -> [(String, String)]
limitsEnvironment ProcessLimits {..} = catMaybes [
        (cpuVariable, ) . show <$> cpuSeconds,
        -- macOS refuses to set RLIMIT_AS
        if os == "linux" then (asVariable, ) . show <$> addressSpaceBytes else Nothing,
        (fsizeVariable, ) . show <$> fileSizeBytes,
        (nofileVariable, ) . show <$> openFiles
    ]

-- | Tells the launcher not to run anything unsandboxed.
requiredEnvironment :: [(String, String)]
requiredEnvironment = [(requiredVariable, "1")]

-- | Program and arguments to run @program@ through the launcher.
sandboxedCommand :: FilePath -> FilePath -> [String] -> (FilePath, [String])
sandboxedCommand launcher program args = (launcher, "--sandboxed-exec" : program : args)

-- | The launcher couldn't set up limits or the sandbox and didn't run the program.
launcherSetupFailedExitCode :: ExitCode
launcherSetupFailedExitCode = ExitFailure 117

-- | The launcher couldn't execute the program.
launcherExecFailedExitCode :: ExitCode
launcherExecFailedExitCode = ExitFailure 118

-- | Libraries a dynamically linked program needs, execute access is for the
-- dynamic loader.
systemLibraries :: [FilePath]
systemLibraries = [
        "/lib", "/lib64", "/lib32", "/usr/lib", "/usr/lib64", "/usr/lib32",
        "/usr/local/lib", "/etc/ld.so.cache"
    ]

-- | What the C library reads to resolve host names. Library directories
-- are needed as well, glibc loads NSS modules from there.
resolverFiles :: [FilePath]
resolverFiles = [
        "/etc/resolv.conf", "/etc/hosts", "/etc/nsswitch.conf", "/etc/host.conf",
        "/etc/gai.conf", "/etc/services", "/etc/localtime"
    ]

-- | Where TLS clients look for CA certificates: the places crypton-x509-system
-- checks and where their symlinks usually point to.
certificateFiles :: [FilePath]
certificateFiles = [
        "/etc/ssl", "/etc/pki", "/etc/ca-certificates",
        "/usr/share/ca-certificates", "/usr/local/share/ca-certificates",
        "/usr/local/share/certs", "/usr/share/pki"
    ]

-- | What the environment replaces 'certificateFiles' with, if anything.
-- SSL_CERT_DIR can be a list, like PATH.
certificateOverrides :: IO [FilePath]
certificateOverrides = 
    filter (not . null) . concatMap splitSearchPath . catMaybes 
        <$> mapM lookupEnv ["SYSTEM_CERTIFICATE_PATH", "SSL_CERT_FILE", "SSL_CERT_DIR"]

foreign import ccall unsafe "rpki_prover_sandbox_status"
    c_sandboxStatus :: IO CInt

foreign import ccall unsafe "rpki_prover_sandbox_abi"
    c_sandboxAbi :: IO CInt

foreign import ccall unsafe "rpki_prover_sandbox_message"
    c_sandboxMessage :: IO CString

foreign import ccall unsafe "rpki_prover_landlock_abi"
    c_landlockAbi :: IO CInt

getSandboxStatus :: IO SandboxStatus
getSandboxStatus = do
    status  <- c_sandboxStatus
    abi     <- fromIntegral <$> c_sandboxAbi
    message <- Text.pack <$> (peekCString =<< c_sandboxMessage)
    -- Codes are defined in cbits/sandbox.c
    pure $ case status of
        0 -> NotSandboxed
        1 -> Sandboxed abi
        2 -> SandboxUnsupported message
        3 -> SandboxFailed message
        _ -> SandboxFailed [i|unknown sandbox status #{status}|]

-- | Landlock ABI version this system supports, without restricting anything.
getLandlockAbi :: IO (Either Text Int)
getLandlockAbi
    | os /= "linux" = pure $ Left "Landlock is only available on Linux"
    | otherwise = do
        abi <- c_landlockAbi
        pure $ if abi > 0
            then Right $ fromIntegral abi
            else Left $ Text.pack $ show $
                    errnoToIOError "Landlock is not available" (Errno (negate abi)) Nothing Nothing

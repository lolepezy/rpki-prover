{-# LANGUAGE OverloadedStrings #-}

{- | Sandboxing worker processes with Landlock (Linux only).

The parent describes what a worker may touch ('WorkerSandbox') and passes it
to the worker process in environment variables. The worker applies it in
@cbits/sandbox.c@ before the GHC runtime starts any threads, since Landlock
only restricts the thread that asks for it and whatever that thread creates
later. Once applied, everything not listed is denied, including all TCP
connections, and it can't be lifted.

The Haskell side of the worker only gets to find out how that went
('getSandboxStatus') and to report it.
-}
module RPKI.Sandbox (
    WorkerSandbox(..),
    SandboxStatus(..),
    sandboxEnvironment,
    sandboxVariables,
    getSandboxStatus
) where

import           Data.List        (intercalate)
import           Data.String.Interpolate.IsString
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Foreign.C.String (CString, peekCString)
import           Foreign.C.Types  (CInt (..))
import           GHC.Generics     (Generic)


-- | What a sandboxed worker is still allowed to access. Directories include
-- everything beneath them. Paths must not contain ':'.
--
-- With 'writesOnly' only writing is restricted (to 'readWrite'), reading,
-- running programs and the network are not, and 'readOnly' is ignored.
-- Programs the worker runs are restricted in the same way.
data WorkerSandbox = WorkerSandbox {
        readWrite  :: [FilePath],
        readOnly   :: [FilePath],
        writesOnly :: Bool
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

-- | Names of the variables @cbits/sandbox.c@ reads.
sandboxVariables :: [String]
sandboxVariables = [rwVariable, roVariable, writesOnlyVariable]

rwVariable, roVariable, writesOnlyVariable :: String
rwVariable = "RPKI_PROVER_SANDBOX_RW"
roVariable = "RPKI_PROVER_SANDBOX_RO"
writesOnlyVariable = "RPKI_PROVER_SANDBOX_WRITES_ONLY"

sandboxEnvironment :: WorkerSandbox -> [(String, String)]
sandboxEnvironment WorkerSandbox {..} = [
        (rwVariable, intercalate ":" readWrite),
        (roVariable, intercalate ":" readOnly)
    ] <> [ (writesOnlyVariable, "1") | writesOnly ]

foreign import ccall unsafe "rpki_prover_sandbox_status"
    c_sandboxStatus :: IO CInt

foreign import ccall unsafe "rpki_prover_sandbox_abi"
    c_sandboxAbi :: IO CInt

foreign import ccall unsafe "rpki_prover_sandbox_message"
    c_sandboxMessage :: IO CString

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

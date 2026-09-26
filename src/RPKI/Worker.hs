module RPKI.Worker where

import           Effectful
import qualified Control.Exception               as IOExc
import           Effectful.Exception
import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM

import           Control.Lens

import           Conduit
import           Control.Applicative ((<|>))
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Traversable (for)
import           Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as Text

import           Data.Hourglass (Seconds(..))
import           Data.String.Interpolate.IsString
import           Data.Conduit.Process.Typed

import           GHC.Generics

import           System.Directory (makeAbsolute)
import           System.Environment (getEnvironment)
import           System.IO (stdin, stdout)
import           System.Posix.Types
import           System.Posix.Process
import qualified System.Posix.Signals       as Signals

import           RPKI.AppMonad
import           RPKI.AppTypes
import           RPKI.Metrics.Process
import           RPKI.Metrics.System (resourceUsageMetric)
import           RPKI.AppContext
import           RPKI.Config
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.RRDP.Types
import           RPKI.Sandbox
import           RPKI.TAL
import           RPKI.Logging
import           RPKI.Time
import           RPKI.Util (convert, fmtEx)
import           RPKI.Store.Base.Serialisation
import qualified RPKI.Store.Database    as DB
import           RPKI.Meta.UniqueId


{- | This is to run worker processes for some code that is better to be executed in an isolated process.

Every worker 
 - Reads serialised parameters from its stdin (WorkerInput)
 - Writes serialised result to its stdout (WorkerResult)
 - Streams log messages to stderr (see `withWorkerLogger` in RPKI.Logging)

Running a worker is done using `runWorker` which handles all the process machinery, error handling, etc.

Every worker calls `executeWork` that takes care of worker lifecycle.

`WorkerInput` is used by all workers, `WorkerParams` is individual for every worker type.

Some of the machinery is also in Main.

-}
    
newtype WorkerId = WorkerId Text
    deriving stock (Eq, Ord, Generic)
    deriving anyclass (TheBinary)

instance Show WorkerId where
    show (WorkerId w) = unpack w

data WorkerParams = RrdpFetchParams { 
                scopes         :: Scopes, 
                rrdpRepository :: RrdpRepository,
                worldVersion   :: WorldVersion 
            } | 
            RsyncFetchParams { 
                scopes          :: Scopes, 
                fetchConfig     :: FetchConfig, 
                rsyncRepository :: RsyncRepository,
                worldVersion    :: WorldVersion 
            } | 
            ErikFetchParams {
                scopes       :: Scopes,
                fetchConfig  :: FetchConfig,
                relayUris    :: [URI],
                fqdn         :: FQDN,
                worldVersion :: WorldVersion
            } | 
            ValidationParams {                 
                worldVersion   :: WorldVersion,
                allTaNames     :: [TaName],
                talsToValidate :: [TAL]
            } | 
            CacheCleanupParams { 
                worldVersion :: WorldVersion
            }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data WorkerInput = WorkerInput {
        workerId                :: WorkerId,
        params                  :: WorkerParams,
        config                  :: Config,
        initialParentId         :: ProcessID,
        workerTimeout           :: Timebox,
        cpuLimit                :: Maybe CPUTime,
        maxIncomingTrafficMb    :: Maybe Int,
        maxDiskReadMb           :: Maybe Int,
        maxDiskWriteMb          :: Maybe Int,
        parentExecutableVersion :: ExecutableVersion
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data WorkerType = RrdpFetchWorker | RsyncFetchWorker | ErikFetchWorker
                | ValidationWorker | CacheCleanupWorker
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)

workerTypeOf :: WorkerParams -> WorkerType
workerTypeOf = \case
    RrdpFetchParams {}    -> RrdpFetchWorker
    RsyncFetchParams {}   -> RsyncFetchWorker
    ErikFetchParams {}    -> ErikFetchWorker
    ValidationParams {}   -> ValidationWorker
    CacheCleanupParams {} -> CacheCleanupWorker

workerTypeName :: WorkerType -> Text
workerTypeName = \case
    RrdpFetchWorker    -> "rrdp-fetch"
    RsyncFetchWorker   -> "rsync-fetch"
    ErikFetchWorker    -> "erik-fetch"
    ValidationWorker   -> "validation"
    CacheCleanupWorker -> "cache-clean-up"

workerTypeLimits :: Config -> WorkerType -> WorkerLimits
workerTypeLimits config = let SystemConfig {..} = config ^. #systemConfig in \case
    RrdpFetchWorker    -> rrdpWorkerLimits
    RsyncFetchWorker   -> rsyncWorkerLimits
    ErikFetchWorker    -> erikWorkerLimits
    ValidationWorker   -> validationWorkerLimits
    CacheCleanupWorker -> cleanupWorkerLimits

-- | Limits of the worker kind that reports its resource usage under this
-- scope, 'Nothing' for a scope no worker kind owns (e.g. "root").
workerLimitsByName :: Config -> Text -> Maybe WorkerLimits
workerLimitsByName config name =
    workerTypeLimits config <$> List.find ((== name) . workerTypeName) [minBound .. maxBound]

-- | Everything that is fixed about one worker run, worked out from its
-- 'WorkerParams' alone.
data WorkerSpec = WorkerSpec {
        name         :: Text,
        workerId     :: WorkerId,
        workerKind   :: WorkerKind,
        params       :: WorkerParams,
        limits       :: WorkerLimits,
        cliArguments :: [String]
    }
    deriving stock (Show, Generic)

workerSpecFor :: Config -> WorkerParams -> WorkerSpec
workerSpecFor config params = WorkerSpec {..}
  where
    workerType = workerTypeOf params
    name       = workerTypeName workerType
    limits     = workerTypeLimits config workerType

    workerKind = case workerType of
                    RsyncFetchWorker -> RsyncWorker
                    _                -> GenericWorker name

    -- The worker id is for humans to read in `top` or `ps`, the actual
    -- parameters are passed to the worker as serialised 'WorkerParams'.
    workerId = WorkerId $ [i|version:#{worldVersion}:#{name}|] <> maybe "" (":" <>) detail

    -- The world version every worker runs for, plus whatever identifies this
    -- particular run within its kind.
    (worldVersion, detail) = case params of
        RrdpFetchParams { worldVersion = v, rrdpRepository = repo }  ->
            (v, Just $ unURI $ getURL repo)
        RsyncFetchParams { worldVersion = v, rsyncRepository = repo } ->
            (v, Just $ unURI $ getURL repo)
        ErikFetchParams { worldVersion = v, fqdn } ->
            (v, Just $ unFQDN fqdn)
        ValidationParams { worldVersion = v, talsToValidate } ->
            (v, Just $ Text.intercalate "," $ List.sort $ map (unTaName . getTaName) talsToValidate)
        CacheCleanupParams { worldVersion = v } ->
            (v, Nothing)

    cpuCount :: Int = fromIntegral $ config ^. #parallelism . #cpuCount

    -- All three fetchers are tuned the same way and only differ in how many
    -- capabilities they get: a small nursery, and `-Fd1` to give heap back
    -- quickly, since they are short-lived and many of them are alive at once.
    fetcherRts capabilities = [ rtsN capabilities, rtsA "4m", rtsAL "4m", "-Fd1" ]

    -- RRDP and Erik workers start single-threaded to save memory while download is 
    -- happening and set their max capabilities according to the cpuCount when they
    -- get to the stage of parsing-validating-saving objects.
    rtsOptions = case workerType of
        RrdpFetchWorker  -> fetcherRts 1
        ErikFetchWorker  -> fetcherRts 1
        RsyncFetchWorker -> fetcherRts cpuCount
        
        -- TODO make profiling a runtime thing, config? It used to be
        -- [ "-p", "-hT", "-l" ] prepended here.
        ValidationWorker -> [ rtsN cpuCount, rtsA "24m", rtsAL "128m" ]
        CacheCleanupWorker -> [ rtsN 2, rtsA "24m", rtsAL "64m" ]

    cliArguments = [ show workerId ] <> rtsArguments
            (rtsOptions <> [ rtsMaxMemory $ rtsMemValue $ limits ^. #memoryMb ])

-- | 'timeout' rather than the spec's own 'workerTimeout': the caller is allowed
-- to give a worker less wall-clock time than its kind is configured for.
makeWorkerInput :: (MonadIO m) => AppContext s -> WorkerSpec -> Timebox -> m WorkerInput
makeWorkerInput AppContext {..} WorkerSpec { params, workerId, limits } timeout = do
    thisProcessId <- liftIO getProcessID
    let WorkerLimits { cpuLimit, maxIncomingTrafficMb, maxDiskReadMb, maxDiskWriteMb } = limits
    pure $ WorkerInput workerId params config thisProcessId
                        timeout (Just $ asCpuTime cpuLimit)
                        maxIncomingTrafficMb maxDiskReadMb maxDiskWriteMb
                        executableVersion

-- | What a worker is still allowed to access after sandboxing itself (Linux
-- only, see RPKI.Sandbox), 'Nothing' for workers that are not sandboxed.
workerSandbox :: WorkerInput -> Maybe WorkerSandbox
workerSandbox input = case input ^. #params of
    -- Validation only works with the cache: TA certificates are downloaded and
    -- SLURM files are read by the main process. Apart from that:
    --  * /proc/self is for the resource accounting the worker does on itself
    --    (see RPKI.Metrics.Process).
    --  * /dev/null is for SQLite: reading the worker input closes stdin, and when
    --    SQLite gets descriptor 0, 1 or 2 for a database file, it puts /dev/null
    --    there and tries again, so that stray writes to stdout/stderr can't
    --    end up in the database. Without it the database can't be opened.
    ValidationParams {} -> Just WorkerSandbox {
            readWrite          = [cacheDirectory],
            readOnly           = ["/proc/self", "/dev/null"],
            onlyRestrictWrites = False
        }
    -- The rsync fetcher runs the rsync client, which inherits the sandbox, 
    -- so only writing is restricted: to the cache for the worker and to the 
    -- rsync mirror for both. Everything else the client needs (the binary,
    -- libraries, DNS, network) stays available. /dev/null is writable for 
    -- anything that sends its output there.
    RsyncFetchParams {} -> Just WorkerSandbox {
            readWrite          = [cacheDirectory, rsyncDirectory, "/dev/null"],
            readOnly           = [],
            onlyRestrictWrites = True
        }
    _ -> Nothing
  where
    cacheDirectory = configValue $ input ^. #config . #cacheDirectory
    rsyncDirectory = configValue $ input ^. #config . #rsyncConf . #rsyncRoot

-- | The worker gets the parent's environment, minus sandbox settings it may
-- have inherited itself, plus the sandbox settings for this worker, if any.
workerEnvironment :: WorkerInput -> IO [(String, String)]
workerEnvironment input = do
    inherited <- filter ((`notElem` sandboxVariables) . fst) <$> getEnvironment
    sandbox <- for (workerSandbox input) $ \WorkerSandbox {..} -> do
        -- The worker may resolve relative paths differently, don't let it
        rw <- mapM makeAbsolute readWrite
        ro <- mapM makeAbsolute readOnly
        cacheDirectory <- makeAbsolute $ configValue $ input ^. #config . #cacheDirectory
        pure $ sandboxEnvironment (WorkerSandbox rw ro onlyRestrictWrites) <>
            -- SQLite picks a temporary directory by checking which ones exist
            -- and are writable, Landlock doesn't show in that check. Point it
            -- to the cache, the only place it can write to.
            [("SQLITE_TMPDIR", cacheDirectory)]
    pure $ Map.toList $ Map.fromList $ inherited <> fromMaybe [] sandbox

newtype RrdpFetchResult = RrdpFetchResult
                            (Either AppError (RrdpRepository, RrdpFetchStat), ValidationState)    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype RsyncFetchResult = RsyncFetchResult 
                            (Either AppError RsyncRepository, ValidationState)    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype ErikFetchResult = ErikFetchResult 
                            (Either AppError ErikFetchStat, ValidationState)    
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

-- | What the parent process needs to know about a finished Erik fetch beyond
-- the validation state: which relays served it, so the UI can show where the
-- objects actually came from.
newtype ErikFetchStat = ErikFetchStat {
        relayUsage :: [ErikRelayUsage]
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

data ValidationResult = ValidationResult
            ValidationState
            (Map.Map TaName (Fetcheables, EarliestToExpire))
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)

newtype CacheCleanupResult = CacheCleanupResult DB.CleanUpResult
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)              

newtype ErrorResult = ErrorResult Text
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)              

data WorkerResult r = WorkerResult {
        -- | 'Left' carries no @r@, and 'Data.Store' tags the 'Either' before
        -- either branch, so a parent expecting, say, @WorkerResult RrdpFetchResult@
        -- can still decode the @WorkerResult ()@ that Main's @exec@ writes when
        -- the worker fails before it knows what it was going to produce. That is
        -- relied upon; don't give the error branch a payload of its own.
        payload   :: Either ErrorResult r,        
        clockTime :: TimeMs,
        stats     :: ProcessStats
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (TheBinary)    

-- Entry point for a worker. It is supposed to run within a worker process 
-- and do the actual work.
-- 
executeWork :: WorkerInput 
            -> (WorkerExit -> IO ()) -- ^ How to exit the worker process.
            -> (WorkerInput -> (forall a . TheBinary a => a -> IO ()) -> IO ()) -- ^ Actual work to be executed.                            
            -> IO ()
executeWork input exitWith_ actualWork = 
    -- Check if version of the executable has changed compared to the parent.
    -- If that's the case, it usually means we are in the middle of an upgrade. 
    -- In this case bail out, it's likely we can do more harm then good
    if input ^. #parentExecutableVersion /= thisExecutableVersion
        then 
            exitWith_ ExecutableReplaced
        else do 
            Now startedAt <- thisInstant
            workerExit <- newEmptyTMVarIO            
            -- Whoever gets here first decides how the worker ends; the others
            -- are too late and must not report anything.
            firstOut <- newTVarIO True
            let done ec = do 
                    mine <- atomically $ stateTVar firstOut $ \isFirst -> (isFirst, False)
                    when mine $ 
                        reportResourceUsage startedAt ec 
                            `IOExc.finally` atomically (void $ tryPutTMVar workerExit ec)

            mapM_ (\w -> forkFinally w (const $ pure ())) [
                    doTheWork done,
                    dieIfParentDies done,
                    dieAfterTimeout done,
                    dieOfOveruse done
                ]
                
            exitWith_ =<< atomically (takeTMVar workerExit)
  where            
    workerId = input ^. #workerId

    reportResourceUsage startedAt = \case 
        WorkerSucceeded -> pure ()
        ParentDied      -> pure ()
        _ -> do 
            Now now <- thisInstant
            stats   <- processStat
            sendToParent $ SystemMetricsM $ resourceUsageMetric 
                    (workerTypeName $ workerTypeOf $ input ^. #params)
                    (durationMs startedAt now) stats

    -- An exit code on its own says nothing about what went wrong inside, so
    -- send the exception to the parent before giving up on it.
    doTheWork done = 
        (actualWork input writeWorkerOutput >> done WorkerSucceeded)
            `IOExc.catch` \e -> do 
                case IOExc.fromException e of 
                    Just (SomeAsyncException _) -> pure ()
                    Nothing -> sendLogToParent [i|Worker #{workerId} died with an exception: #{fmtEx e}|]
                done WorkerException

    -- Keep track of who's the current process parent: if it is not the same 
    -- as we started with then parent exited/is killed. Exit the worker as well,
    -- there's no point continuing.
    dieIfParentDies done = forever $ do
        threadDelay 500_000
        parentId <- getParentProcessID
        when (parentId /= input ^. #initialParentId) $
            done ParentDied

    -- Time bomb. Wait for the certain timeout and then exit.
    dieAfterTimeout done = do
        let Timebox timebox = input ^. #workerTimeout
        threadDelay $ toMicroseconds timebox
        done TimedOut

    -- Exit if the worker has spent more than it is allowed of any of 
    -- the limits (CPU time, traffic, disk IO).
    dieOfOveruse done = loop
      where
        loop = do 
            -- Stop at the first limit that is exceeded, that's the one the 
            -- exit code is going to be about.
            overuse <- checkCpuTime `orNext` checkTraffic `orNext` checkDiskIo
            case overuse of 
                Nothing -> do 
                    threadDelay 1_000_000
                    loop
                Just (workerExit, reason) -> do 
                    sendLogToParent [i|Worker #{workerId} #{reason}, exiting.|]
                    done workerExit

        orNext thisCheck nextCheck = thisCheck >>= maybe nextCheck (pure . Just)

        checkCpuTime :: IO (Maybe (WorkerExit, Text))
        checkCpuTime = 
            case input ^. #cpuLimit of 
                Nothing       -> pure Nothing
                Just cpuLimit -> do 
                    cpuTime <- getCpuTime
                    pure $ do 
                        guard $ cpuTime > cpuLimit
                        Just (OutOfCpuTime, 
                            [i|used #{cpuTime}ms of CPU time, the limit is #{cpuLimit}ms|])

        checkTraffic :: IO (Maybe (WorkerExit, Text))
        checkTraffic = 
            case input ^. #maxIncomingTrafficMb of 
                Nothing    -> pure Nothing
                Just limit -> do 
                    traffic <- getIncomingTraffic
                    pure $ do 
                        guard $ traffic > mbToSize limit
                        Just (TooMuchTraffic, 
                            [i|downloaded #{sizeMb traffic}mb, the limit is #{limit}mb|])

        checkDiskIo :: IO (Maybe (WorkerExit, Text))
        checkDiskIo = do 
            let readLimit  = input ^. #maxDiskReadMb
                writeLimit = input ^. #maxDiskWriteMb
            if isNothing readLimit && isNothing writeLimit 
                then pure Nothing
                else do 
                    -- One look at /proc for both of them
                    DiskIO {..} <- getProcessDiskIO
                    let tooMuchRead = do 
                            limit <- readLimit
                            guard $ diskRead > mbToSize limit
                            Just (TooMuchDiskIo, 
                                [i|read #{sizeMb diskRead}mb from disk, the limit is #{limit}mb|])
                    let tooMuchWritten = do 
                            limit <- writeLimit
                            guard $ diskWrite > mbToSize limit
                            Just (TooMuchDiskIo, 
                                [i|wrote #{sizeMb diskWrite}mb to disk, the limit is #{limit}mb|])
                    pure $ tooMuchRead <|> tooMuchWritten


readWorkerInput :: (MonadIO m) => m WorkerInput
readWorkerInput = liftIO $ deserialise_ . LBS.toStrict <$> LBS.hGetContents stdin

execWithStats :: MonadIO m => m (Either ErrorResult r) -> m (WorkerResult r)
execWithStats f = do        
    (payload, clockTime) <- timedMS f
    stats <- processStat
    pure WorkerResult {..}


writeWorkerOutput :: TheBinary a => a -> IO ()
writeWorkerOutput = LBS.hPut stdout . LBS.fromStrict . serialise_

rtsArguments :: [String] -> [String]
rtsArguments args = [ "+RTS" ] <> defaultRts <> args <> [ "-RTS" ]

rtsMaxMemory, rtsA, rtsAL :: String -> String
rtsMaxMemory m = "-M" <> m
rtsA m = "-A" <> m
rtsAL m = "-AL" <> m

rtsN :: Int -> String
rtsN n = "-N" <> Prelude.show n

rtsMemValue :: Int -> String
rtsMemValue mb = Prelude.show mb <> "m"

-- Don't do idle GC, it only spins the CPU without any purpose.
--
-- `--disable-delayed-os-memory-return` applies to every worker: without it the
-- RTS releases freed memory with MADV_FREE, which leaves the pages counted in
-- RSS until the kernel actually needs them. Workers are the processes whose RSS
-- is watched and reported ('statProcessRss'), and comparing those numbers only
-- makes sense if they all account for memory the same way.
--
-- -F and -Fd are pinned to the RTS defaults on purpose. The main process bakes
-- in a tighter -F/-Fd to keep its own long-lived heap close to its live data,
-- and since workers are the same executable they would otherwise inherit that
-- and quietly run under a different GC regime. They are short-lived and bounded
-- by -M instead, so trading their throughput for residency makes no sense.
-- Per-worker flags are appended after these and still override them (the rrdp
-- and rsync workers set -Fd1 of their own).
defaultRts :: [String]
defaultRts = [ "-I0", "-F2", "-Fd4", "--disable-delayed-os-memory-return" ]

{- | How a worker process ended.

A worker says why it gave up through its exit code, so both sides have to agree
on the numbers; 'toExitCode' and 'fromExitCode' are that agreement. Every way a
worker can end is a constructor here, including the ways it doesn't choose
itself: the RTS's own code for running out of heap, and being killed by a signal.
-}
data WorkerExit = WorkerSucceeded
                -- | The work threw, see 'executeWork'.
                | WorkerException
                | ParentDied
                | OutOfCpuTime
                | TooMuchTraffic
                | TooMuchDiskIo
                | TimedOut
                | ExecutableReplaced
                -- | Set by the RTS when the heap grows past @-M@, not by us.
                | OutOfMemory
                -- | @System.Process@ reports a process killed by a signal as a
                -- negative exit code.
                | KilledBySignal Int
                | UnknownExit Int
    deriving stock (Eq, Ord, Show, Generic)

toExitCode :: WorkerExit -> ExitCode
toExitCode = \case
    WorkerSucceeded    -> ExitSuccess
    WorkerException    -> ExitFailure 99
    ParentDied         -> ExitFailure 111
    OutOfCpuTime       -> ExitFailure 113
    TooMuchTraffic     -> ExitFailure 114
    TooMuchDiskIo      -> ExitFailure 115
    TimedOut           -> ExitFailure 122
    ExecutableReplaced -> ExitFailure 123
    OutOfMemory        -> ExitFailure 251
    KilledBySignal s   -> ExitFailure (negate s)
    UnknownExit n      -> ExitFailure n

fromExitCode :: ExitCode -> WorkerExit
fromExitCode = \case
    ExitSuccess     -> WorkerSucceeded
    ExitFailure 99  -> WorkerException
    ExitFailure 111 -> ParentDied
    ExitFailure 113 -> OutOfCpuTime
    ExitFailure 114 -> TooMuchTraffic
    ExitFailure 115 -> TooMuchDiskIo
    ExitFailure 122 -> TimedOut
    ExitFailure 123 -> ExecutableReplaced
    ExitFailure 251 -> OutOfMemory
    ExitFailure n
        | n < 0     -> KilledBySignal (negate n)
        | otherwise -> UnknownExit n

-- | What the parent says about a worker that came back without a result: the
-- message to log, the trace to leave behind (if any) and the error to raise
-- with that same message.
workerFailure :: WorkerId -> WorkerExit -> (Text, Maybe Trace, Text -> InternalError)
workerFailure workerId = \case
    TimedOut ->
        ([i|Worker #{workerId} execution timed out.|], Just WorkerTimeoutTrace, WorkerTimeout)
    OutOfCpuTime ->
        ([i|Worker #{workerId} ran out of CPU time.|], Just WorkerCpuOveruseTrace, WorkerOutOfCpuTime)
    TooMuchTraffic ->
        ([i|Worker #{workerId} downloaded too much data.|], Just WorkerIoOveruseTrace, WorkerTooMuchIO)
    TooMuchDiskIo ->
        ([i|Worker #{workerId} did too much disk IO.|], Just WorkerIoOveruseTrace, WorkerTooMuchIO)
    OutOfMemory ->
        ([i|Worker #{workerId} ran out of memory.|], Nothing, WorkerOutOfMemory)
    ExecutableReplaced ->
        ([i|Worker #{workerId} detected that `rpki-prover` binary is different and exited for good.|],
         Nothing, WorkerDetectedDifferentExecutable)
    ParentDied ->
        ([i|Worker #{workerId} exited because its parent process is gone.|], Nothing, InternalError)
    WorkerException ->
        -- The worker sends the exception itself over the log bus before it exits,
        -- so this only has to say that it happened.
        ([i|Worker #{workerId} died with an exception.|], Nothing, InternalError)
    KilledBySignal s ->
        ([i|Worker #{workerId} was killed by #{signalName s}#{killHint s}.|], Nothing, InternalError)
    UnknownExit n ->
        ([i|Worker #{workerId} exited with code = #{n}.|], Nothing, InternalError)
    -- Not reachable from 'runWorkerProcess', which deals with a successful exit
    -- before it gets here; it is here to keep this function total.
    WorkerSucceeded ->
        ([i|Worker #{workerId} exited successfully.|], Nothing, InternalError)
  where
    -- Both of these are routine enough to be worth naming: workers that outlive
    -- their `endOfLife` are SIGKILL-ed by the leftovers cleanup (see
    -- 'RPKI.Workflow.killWorkers'), and so is a worker the kernel decides to
    -- reclaim memory from.
    killHint s | s == sigKILL = ", either by the expired-worker cleanup or by the OOM killer" :: Text
               | otherwise    = ""

signalName :: Int -> Text
signalName s = maybe [i|signal #{s}|] (\n -> [i|#{n :: Text} (#{s})|]) $ lookup s knownSignals
  where
    knownSignals = [
            (fromIntegral Signals.sigHUP,  "SIGHUP"),  (fromIntegral Signals.sigINT,  "SIGINT"),
            (fromIntegral Signals.sigABRT, "SIGABRT"), (fromIntegral Signals.sigKILL, "SIGKILL"),
            (fromIntegral Signals.sigSEGV, "SIGSEGV"), (fromIntegral Signals.sigPIPE, "SIGPIPE"),
            (fromIntegral Signals.sigTERM, "SIGTERM"), (fromIntegral Signals.sigXCPU, "SIGXCPU")
        ]

sigINT, sigKILL :: Int
sigINT  = fromIntegral Signals.sigINT
sigKILL = fromIntegral Signals.sigKILL


-- | How much longer than its own timeout the parent gives a worker before
-- stepping in: enough for the worker to notice and exit by itself, so that its
-- own, more specific, reason for stopping is the one that gets reported.
timeToKillItself :: Seconds
timeToKillItself = Seconds 5

{- | Run a worker for the given parameters and hand back its result.

Everything that is the same for every worker happens here: working out the
worker id, the limits and the RTS options ('workerSpecFor'), registering the
process while it runs, reporting what it used, and turning a failure reported
by the worker itself into an 'AppError'.
-}
runWorker :: (ValidatorIO es, TheBinary r)
            => AppContext s
            -> WorkerParams
            -> Maybe Seconds -- ^ wall-clock timeout, if it is not the configured one
            -> Eff es r
runWorker appContext@AppContext {..} params timeoutOverride = do
    let spec = workerSpecFor config params
    let WorkerSpec { name, workerId, workerKind, limits, cliArguments } = spec
    let timeout = fromMaybe (limits ^. #workerTimeout) timeoutOverride

    workerInput <- makeWorkerInput appContext spec (Timebox timeout)
    workerInfo  <- newWorkerInfo workerKind timeout (convert $ show workerId)

    wr@WorkerResult {..} <- runWorkerProcess logger workerInput cliArguments workerInfo
    case payload of
        Left (ErrorResult e) -> appError $ InternalE $ WorkerError e
        Right r -> do
            logWorkerDone logger workerId wr
            pushSystem logger $ resourceUsageMetric name clockTime stats
            pure r

-- Start the worker process itself, stream its logs to the parent and 
-- make sense of the way it exited.
-- 
runWorkerProcess :: (ValidatorIO es, TheBinary r) => AppLogger 
            -> WorkerInput            
            -> [String] 
            -> WorkerInfo 
            -> Eff es r
runWorkerProcess logger workerInput extraCli workerInfo = do
    let executableToRun = configValue $ workerInput ^. #config . #programBinaryPath
    environment <- liftIO $ workerEnvironment workerInput
    let worker = 
            setStdin (byteStringInput $ LBS.fromStrict $ serialise_ workerInput) $             
            setStderr createSource $
            setStdout byteStringOutput $
            setEnv environment $
                proc executableToRun $ [ "--worker" ] <> extraCli
    
    -- Not `show worker`: with the environment set explicitly it would log all of it
    let commandLine = unwords $ executableToRun : "--worker" : extraCli
    let sandboxed = maybe "" (\sb -> [i|, sandboxed: #{sb}|] :: Text) $ workerSandbox workerInput
    logDebug logger [i|Running worker: #{commandLine} with timeout #{timeout}#{sandboxed}.|]       

    runIt worker `catches` [                    
            Handler $ \e@(SomeAsyncException _) -> throwIO e,
            Handler $ \(_ :: IOException)       -> complain [i|Worker #{workerId} died/killed.|],
            Handler $ \e@(SomeException _)      -> complain [i|Worker #{workerId} died in a strange way: #{fmtEx e}|]       
        ] 
  where    

    timeout = unTimebox $ workerInput ^. #workerTimeout
    workerId = workerInput ^. #workerId

    waitForProcess conf f = IOExc.bracket start stop exec
      where
        start = do 
            p <- startProcess conf
            mpid <- getPid p
            forM_ mpid $ \pid -> 
                registerWorker logger $ workerInfo & #workerPid .~ pid
            pure (mpid, p)           

        stop (mpid, p) = do 
            stopProcess p
            forM_ mpid (deregisterWorker logger)

        exec (_, p) = (,) <$> f p <*> waitExitCode p        

    runIt workerConf = do   
        ((_, workerStdout), exitCode) <- 
            liftIO $ waitForProcess workerConf $ \p ->
                concurrently 
                    (runConduitRes $ getStderr p .| sinkLog logger)
                    (atomically $ getStdout p)

        case fromExitCode exitCode of  
            WorkerSucceeded -> 
                case deserialiseOrFail_ $ LBS.toStrict workerStdout of 
                    Left e -> 
                        complain [i|Failed to deserialise stdout, #{e}, worker #{workerId}, stdout = [#{workerStdout}]|]                             
                    Right r -> 
                        pure r            

            KilledBySignal s | s == sigINT -> do
                -- 
                -- This is a hack to work around a problem in `readProcess`:
                -- it apparently catches an async exception, kills the process (with some signal?)
                -- but doesn't rethrow the exception, so all we have is the worker that exited 
                -- with error code '-2'.
                --
                -- TODO try to find a way to fix with `typed-process` features.
                -- TODO Otherwise make sure it's safe to assume it's always '-2'.
                -- 
                -- This logging message is slightly deceiving: it's not that just the worker 
                -- was killed, but we also know that there was an asynchronous exception, which 
                -- we retrow here to make sure "outer stack" knows about it.
                --
                logError logger [i|Worker #{workerId} died/killed.|]
                throwIO AsyncCancelled                    

            workerExit -> do 
                let (message, workerTrace, toError) = workerFailure workerId workerExit
                logError logger message
                forM_ workerTrace trace
                appError $ InternalE $ toError message

    complain message = do 
        logError logger message
        appError $ InternalE $ InternalError message

logWorkerDone :: (Logger logger, MonadIO m) =>
                logger -> WorkerId -> WorkerResult r -> m ()
logWorkerDone logger workerId WorkerResult { stats = ProcessStats {..}, ..} = do    
    let DiskIO {..} = statDiskIO
    logDebug logger $
        [i|Worker #{workerId} completed, cpuTime: #{statCpuTime}ms, |] <>
        [i|clockTime: #{clockTime}ms, maxRtsHeap: #{statMaxRtsHeap}, maxProcessRss: #{statProcessRss}, |] <>
        [i|incoming traffic: #{sizeMb statIncomingTraffic}mb, |] <>
        [i|disk read: #{sizeMb diskRead}mb, disk write: #{sizeMb diskWrite}mb.|] 

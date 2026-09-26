{- | How many CPUs the process can actually make use of.
All of it is Linux only, read from procfs and sysfs.
-}
module RPKI.Cpu (
    useAvailableCpus,
    getAvailableCpuCount,
    getPhysicalCpuCount,
    getCgroupCpuLimit,
    -- * Exported for tests
    parseCpuList,
    parseCpuMax,
    quotaCpus,
    limitDirs
) where

import           Control.Exception     (IOException, try)
import           Control.Monad         (forM, guard)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.List             as List
import           Data.Maybe            (catMaybes, listToMaybe)
import qualified Data.Set              as Set

import           GHC.Conc              (setNumCapabilities)
import           Numeric.Natural
import           System.FilePath       (dropTrailingPathSeparator, normalise, splitDirectories, 
                                        takeDirectory, (</>))


-- | Set the number of capabilities to the configured CPU count, 
-- but not above `getAvailableCpuCount`
useAvailableCpus :: Natural -> IO Natural
useAvailableCpus configuredCpus = do
    availableCpus <- getAvailableCpuCount
    let cpus = maybe configuredCpus (min configuredCpus) availableCpus
    setNumCapabilities $ fromIntegral cpus
    pure cpus


-- | How many CPUs this process can make use of, when that can be told: the 
-- physical cores it is allowed to run on, limited by its cgroup CPU quota 
-- (a container's `--cpus`, systemd's CPUQuota=). Linux only, `Nothing` 
-- elsewhere.
getAvailableCpuCount :: IO (Maybe Natural)
getAvailableCpuCount = do
    cores <- getPhysicalCpuCount
    quota <- getCgroupCpuLimit
    pure $ case catMaybes [cores, quota] of
        [] -> Nothing
        ns -> Just $ minimum ns

-- | Number of physical cores this process is allowed to run on, i.e. with
-- hyper-threads (SMT siblings) of the same core counted once.
--
-- Linux only: it reads the process' CPU affinity from /proc/self/status and
-- the core topology from sysfs. `Nothing` wherever either can't be read, 
-- including in a worker sandboxed without read access to /proc and /sys.
getPhysicalCpuCount :: IO (Maybe Natural)
getPhysicalCpuCount = do
    status <- readMaybeFile "/proc/self/status"
    case allowedCpus =<< status of
        Nothing   -> pure Nothing
        Just cpus -> do
            cores <- forM cpus $ \cpu -> do
                -- `core_cpus_list` is the current name, older kernels 
                -- only have `thread_siblings_list`
                let topology = "/sys/devices/system/cpu/cpu" <> show cpu <> "/topology/"
                current <- readMaybeFile $ topology <> "core_cpus_list"
                siblings <- case current of
                    Just _  -> pure current
                    Nothing -> readMaybeFile $ topology <> "thread_siblings_list"
                pure $ parseCpuList =<< siblings
            pure $ case Set.size $ Set.fromList $ catMaybes cores of
                0 -> Nothing
                n -> Just $ fromIntegral n
  where
    allowedCpus status = listToMaybe
        [ cpus | line <- C8.lines status,
                 Just value <- [BS.stripPrefix "Cpus_allowed_list:" line],
                 Just cpus <- [parseCpuList value] ]

-- | The CPU quota of the process' cgroup as a number of CPUs, rounded up. 
-- A quota set higher up the hierarchy applies too, so this is the smallest 
-- one on the way from the process' cgroup to the root. Both cgroup v2 
-- (cpu.max) and v1 (cpu.cfs_quota_us); `Nothing` if there is no quota.
--
-- Inside a container the cgroup path in /proc/self/cgroup may not exist 
-- under the container's own view of /sys/fs/cgroup, in which case only the 
-- files at the root of the mount, which are the container's, are found.
getCgroupCpuLimit :: IO (Maybe Natural)
getCgroupCpuLimit = do
    cgroups <- maybe [] (map (C8.split ':') . C8.lines) <$> readMaybeFile "/proc/self/cgroup"
    let v2 = [ C8.unpack path | [_, "", path] <- cgroups ]
        v1 = [ C8.unpack path | [_, controllers, path] <- cgroups,
                                "cpu" `elem` C8.split ',' controllers ]
    v2Limits <- forM (limitDirs "/sys/fs/cgroup" =<< v2) $ \dir ->
        (parseCpuMax =<<) <$> readMaybeFile (dir <> "/cpu.max")
    v1Limits <- forM [ dir | mount <- ["/sys/fs/cgroup/cpu", "/sys/fs/cgroup/cpu,cpuacct"],
                             dir <- limitDirs mount =<< v1 ] $ \dir -> do
        quota  <- readMaybeFile $ dir <> "/cpu.cfs_quota_us"
        period <- readMaybeFile $ dir <> "/cpu.cfs_period_us"
        pure $ do
            q <- readNumber =<< quota
            p <- readNumber =<< period
            quotaCpus q p
    pure $ case catMaybes $ v2Limits <> v1Limits of
        [] -> Nothing
        ns -> Just $ minimum ns
  where
    readNumber = fmap fst . C8.readInt . C8.strip

-- | Directories that may hold CPU limits for a cgroup at `path` under a cgroup
-- filesystem mounted at `mount`: its own, its ancestors', and the mount root's.
-- A path with ".." is outside the process' cgroup namespace (the kernel writes 
-- it that way), so nothing under this mount but its root says anything about it.
limitDirs :: FilePath -> FilePath -> [FilePath]
limitDirs mount path 
    | ".." `elem` splitDirectories path = [mount]
    | otherwise = go $ dropTrailingPathSeparator $ normalise $ mount </> dropWhile (== '/') path
  where
    go dir
        | dir == mount || not (mount `List.isPrefixOf` dir) = [mount]
        | otherwise = dir : go (takeDirectory dir)

-- | Parse cgroup v2's cpu.max: "<quota> <period>", or "max <period>" for none.
parseCpuMax :: BS.ByteString -> Maybe Natural
parseCpuMax s = case C8.words s of
    [quota, period] -> do
        (q, qRest) <- C8.readInt quota
        (p, pRest) <- C8.readInt period
        guard $ BS.null qRest && BS.null pRest
        quotaCpus q p
    _ -> Nothing

-- | Quota and period in microseconds to a number of CPUs, rounded up since 
-- a fraction of a CPU is still worth a thread. Anything not positive means 
-- no quota, as v1's -1 does.
quotaCpus :: Int -> Int -> Maybe Natural
quotaCpus quota period
    | quota > 0 && period > 0 = Just $ fromIntegral $ max 1 $ (quota + period - 1) `div` period
    | otherwise               = Nothing

readMaybeFile :: FilePath -> IO (Maybe BS.ByteString)
readMaybeFile file =
    either (\(_ :: IOException) -> Nothing) Just <$> try (BS.readFile file)

-- | Parse the kernel's list format for sets of CPUs, e.g. "0-3,8-11" or "0,8".
parseCpuList :: BS.ByteString -> Maybe [Int]
parseCpuList s = 
    fmap concat $ traverse range $ filter (not . BS.null) $ C8.split ',' $ C8.strip s
  where
    range r = case C8.split '-' r of
        [a]    -> pure <$> number a
        [a, b] -> do
            lo <- number a
            hi <- number b
            guard $ lo <= hi
            pure [lo .. hi]
        _      -> Nothing

    number t = case C8.readInt t of
        Just (n, rest) | BS.null rest && n >= 0 -> Just n
        _                                       -> Nothing

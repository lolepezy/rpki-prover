import qualified Data.ByteString as BS

bs <- BS.readFile "./test/data/checklist.sig"
let Right rsc = parseRsc bs
let cliOptions = CLIOptions { rpkiRootDirectory = ["/home/puz/tmp/rpki/master/"], resetCache = False, lmdbSize = Nothing, cpuCount = Nothing, localExceptions = [], maxValidationMemory = Nothing, maxRrdpFetchMemory = Nothing, maxRsyncFetchMemory = Nothing, metricsPrefix = Nothing, oldVersionsLifeTimeHours = Nothing, cacheLifetimeHours = Nothing, httpApiPort = Nothing, worker = Nothing, minObjectSize = Nothing, maxObjectSize = Nothing, maxTotalTreeSize = Nothing, maxCertificatePathDepth = Nothing, maxTaRepositories = Nothing, topDownTimeout = Nothing, initialise = False, revalidationInterval = Nothing, rrdpRefreshInterval = Nothing, rsyncRefreshInterval = Nothing, rrdpTimeout = Nothing, rsyncTimeout = Nothing, rsyncClientPath = Nothing, noRsync = False, noRrdp = False, strictManifestValidation = False, withRtr = False }
now <- thisInstant
z <- withLogger MainLogger DebugL  $ \logger -> do { (Right appC, _) <- runValidatorT (newScopes "initialise") $ createAppContext cliOptions  logger DebugL ; runValidatorT (newScopes "validate") $ validateBottomUp appC (RscRO rsc) now} 
z
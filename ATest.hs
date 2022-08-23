import qualified Data.ByteString as BS

bs <- BS.readFile "/home/puz/tmp/rpki/testing/rsync/rpkica.mckay.com/rpki/MCnet/I1lixj77O2aL-RMyQR_z8064QiM.roa"
let Right roa = parseRoa bs
let cliOptions = CLIOptions { rpkiRootDirectory = ["/home/puz/tmp/rpki/testing/"], resetCache = False, lmdbSize = Nothing, cpuCount = Nothing, localExceptions = [], maxValidationMemory = Nothing, maxRrdpFetchMemory = Nothing, maxRsyncFetchMemory = Nothing, metricsPrefix = Nothing, oldVersionsLifeTimeHours = Nothing, cacheLifetimeHours = Nothing, httpApiPort = Nothing, worker = Nothing, minObjectSize = Nothing, maxObjectSize = Nothing, maxTotalTreeSize = Nothing, maxCertificatePathDepth = Nothing, maxTaRepositories = Nothing, topDownTimeout = Nothing, initialise = False, revalidationInterval = Nothing, rrdpRefreshInterval = Nothing, rsyncRefreshInterval = Nothing, rrdpTimeout = Nothing, rsyncTimeout = Nothing, rsyncClientPath = Nothing, noRsync = False, noRrdp = False, strictManifestValidation = False, withRtr = False }
now <- thisInstant
z <- withLogger MainLogger DebugL  $ \logger -> do { (Right appC, _) <- runValidatorT (newScopes "initialise") $ createAppContext cliOptions  logger DebugL ; runValidatorT (newScopes "validate") $ validateBottomUp appC (RoaRO roa) now} 
z
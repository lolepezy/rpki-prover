{-# LANGUAGE FlexibleInstances #-}

module RPKI.Process.CLI where    

import Data.Word (Word16)
import Data.Int (Int16, Int64)
import GHC.TypeLits
import Options.Generic

-- CLI Options-related machinery
data CLIOptions wrapped = CLIOptions {

    -- It is not documented since it's for internal machinery
    worker :: wrapped ::: Maybe String,

    version :: wrapped ::: Bool <?> "Program version.",

    initialise :: wrapped ::: Bool <?>
        "Deprecated, does nothing, used to initialise FS layout before the first launch.",

    once :: wrapped ::: Bool <?>
        ("If set, will run one validation cycle and exit. Http API will not start, " +++ 
         "result will be written to the file set by --vrp-output option (which must also be set)."),

    vrpOutput :: wrapped ::: Maybe FilePath <?> 
        "Path of the file to write VRPs to. Only effectful when --once option is set.",

    noRirTals :: wrapped ::: Bool <?> 
        "If set, RIR TAL files will not be downloaded.",

    refetchRirTals :: wrapped ::: Bool <?> 
        "If set, RIR TAL files will be re-downloaded.",        

    rpkiRootDirectory :: wrapped ::: [FilePath] <?>
        ("Root directory (default is ${HOME}/.rpki/). This option can be passed multiple times and "
         +++ "the last one will be used, it is done for convenience of overriding this option with dockerised version."),

    extraTalsDirectory :: wrapped ::: [FilePath] <?>
        ("And extra directories where to look for TAL files. By default there is none, " +++ 
         "TALs are picked up only from the $rpkiRootDirectory/tals directory"),

    verifySignature :: wrapped ::: Bool <?>
        ("Work as a one-off RSC signature file verifier, not as a server. To work as a verifier it needs the cache " +++
        "of validated RPKI objects and VRPs to exist and be populateds. So verifier can (and should) run next to " +++
        "the running daemon instance of rpki-prover"),

    signatureFile :: wrapped ::: Maybe FilePath <?> ("Path to the RSC signature file."),

    verifyDirectory :: wrapped ::: Maybe FilePath <?>
        ("Path to the directory with the files to be verified using and RSC signaure file."),

    verifyFiles :: wrapped ::: [FilePath] <?>
        ("Files to be verified using and RSC signaure file, may be multiple files."),

    cpuCount :: wrapped ::: Maybe Natural <?>
        ("CPU number available to the program (default is 2). Note that higher CPU counts result in bigger " +++ 
        "memory allocations. It is also recommended to set real CPU core number rather than the (hyper-)thread " +++ 
        "number, since using the latter does not give much benefit and actually may cause performance degradation."),

    fetcherCount :: wrapped ::: Maybe Natural <?>
        ("Maximal number of concurrent fetchers (default is --cpu-count * 3)."),

    resetCache :: wrapped ::: Bool <?>
        "Reset the LMDB cache i.e. remove ~/.rpki/cache/*.mdb files.",

    revalidationInterval :: wrapped ::: Maybe Int64 <?>
        ("Interval between validation cycles, each consisting of traversing RPKI tree for each TA " +++ 
        "and fetching repositories on the way, in seconds. Default is 7 minutes, i.e. 560 seconds."),

    cacheLifetimeHours :: wrapped ::: Maybe Int64 <?>
        "Lifetime of objects in the local cache, in hours (default is 72 hours)",

    versionNumberToKeep :: wrapped ::: Maybe Natural <?>
        ("Number of versions to keep in the local cache (default is 100). " +++
         "Every re-validation creates a new version and associates resulting data " +++
         "(validation results, metrics, VRPs, etc.) with it."),

    rrdpRefreshInterval :: wrapped ::: Maybe Int64 <?>
        ("Period of time after which an RRDP repository must be updated, " +++ 
         "in seconds (default is 120 seconds)"),

    rsyncRefreshInterval :: wrapped ::: Maybe Int64 <?>
        ("Period of time after which an rsync repository must be updated, " +++ 
         "in seconds (default is 11 minutes, i.e. 660 seconds)"),

    rrdpTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for RRDP repositories, in seconds. If fetching of a repository does not " +++ 
         "finish within this timeout, the repository is considered unavailable and fetching process is interrupted"),

    asyncRrdpTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for RRDP repositories when fetched asynchronously, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable and fetching process is interrupted"),

    rsyncTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for rsync repositories, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable and fetching process is interrupted."),

    asyncRsyncTimeout :: wrapped ::: Maybe Int64 <?>
        ("Timebox for rsync repositories when fetched asynchronously, in seconds. If fetching of a repository does not "
       +++ "finish within this timeout, the repository is considered unavailable and fetching process is interrupted"),

    rsyncClientPath :: wrapped ::: Maybe String <?>
        ("Path to rsync client executable. By default rsync client is expected to be in the $PATH."),

    httpApiPort :: wrapped ::: Maybe Word16 <?>
        "Port to listen to for http API (default is 9999)",

    lmdbSize :: wrapped ::: Maybe Int64 <?>
        ("Maximal LMDB cache size in MBs (default is 32768, i.e. 32GB). Note that " +++ 
         "(a) It is the maximal size of LMDB, i.e. it will not claim that much space from the beginning. " +++ 
         "(b) About 1Gb of cache is required for every extra 24 hours of cache life time."),

    withRtr :: wrapped ::: Bool <?>
        "Start RTR server (default is false)",

    rtrAddress :: wrapped ::: Maybe String <?>
        "Address to bind to for the RTR server (default is localhost)",

    rtrPort :: wrapped ::: Maybe Int16 <?>
        "Port to listen to for the RTR server (default is 8283)",

    rtrLogFile :: wrapped ::: Maybe String <?>
        "Path to a file used for RTR log (default is stdout, together with general output).",

    logLevel :: wrapped ::: Maybe String <?>
        "Log level, may be 'error', 'warn', 'info' or 'debug' (case-insensitive). Default is 'info'.",

    strictManifestValidation :: wrapped ::: Bool <?>
        ("Use the strict version of RFC 6486 (https://datatracker.ietf.org/doc/draft-ietf-sidrops-6486bis/02/" +++ 
         " item 6.4) for manifest handling (default is false). More modern version is here " +++
         "https://www.rfc-editor.org/rfc/rfc9286.html#name-relying-party-processing-of and it is the default."),

    allowOverclaiming :: wrapped ::: Bool <?>
        ("Use validation reconsidered algorithm for validating resource sets on certificates " +++ 
         "(https://datatracker.ietf.org/doc/draft-ietf-sidrops-rpki-validation-update/) instead of " +++
         "strict version (https://www.rfc-editor.org/rfc/rfc6487.html#section-7). The default if false, " +++
         "i.e. strict version is used by default."),

    localExceptions :: wrapped ::: [String] <?>
        ("Files with local exceptions in the SLURM format (RFC 8416). It is possible " +++ 
         "to set multiple local exception files (i.e. set this option multiple times " +++ 
         "with different arguments), they all will be united into one internally before applying."),

    maxTaRepositories :: wrapped ::: Maybe Int <?>
        "Maximal number of new repositories that validation run can add per TA (default is 1000).",

    maxCertificatePathDepth :: wrapped ::: Maybe Int <?>
        "Maximal depth of the certificate path from the TA certificate to the RPKI tree (default is 32).",

    maxTotalTreeSize :: wrapped ::: Maybe Int <?>
        ("Maximal total size of the object tree for one TA including all currently supported types of " +++
         "objects (default is 5000000)."),

    maxObjectSize :: wrapped ::: Maybe Integer <?>
        ("Maximal size of an object of any type (certificate, CRL, MFT, GRB, ROA, ASPA) " +++
         "in bytes (default is 32mb, i.e. 33554432 bytes)."),

    minObjectSize :: wrapped ::: Maybe Integer <?>
        "Minimal size of an object of any type in bytes (default is 300).",

    topDownTimeout :: wrapped ::: Maybe Int64 <?>
        "Timebox for one TA validation in seconds (default is 1 hours, i.e. 3600 seconds).",

    noRrdp :: wrapped ::: Bool <?> "Do not fetch RRDP repositories (default is false)",
    noRsync :: wrapped ::: Bool <?> "Do not fetch rsync repositories (default is false)",

    rsyncPrefetchUrl :: wrapped ::: [String] <?>
        ("Rsync repositories that will be fetched instead of their children (defaults are " +++
         "'rsync://rpki.afrinic.net/repository', " +++
         "'rsync://rpki.apnic.net/member_repository', " +++
         "'rsync://rpki-repo.registro.br/repo/', " +++
         "'rsync://repo-rpki.idnic.net/repo/', " +++
         "'rsync://0.sb/repo/', " +++
         "'rsync://rpki.co/repo/', " +++
         "'rsync://rpki-rps.arin.net/repository/', " +++
         "'rsync://rpki-repository.nic.ad.jp/ap/', " +++
         "'rsync://rsync.paas.rpki.ripe.net/repository/', " +++
         "'rsync://rpki.sub.apnic.net/repository/', " +++
         "'rsync://rpki.cnnic.cn/rpki/A9162E3D0000/). " +++
         "It is an optimisation and most of the time this option is of no use."),

    metricsPrefix :: wrapped ::: Maybe String <?>
        "Prefix for Prometheus metrics (default is 'rpki_prover').",

    maxRrdpFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for RRDP fetcher process (default is 1024).",

    maxRsyncFetchMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for rsync fetcher process (default is 1024).",

    maxValidationMemory :: wrapped ::: Maybe Int <?>
        "Maximal allowed memory allocation (in megabytes) for validation process (default is 2048).",

    noIncrementalValidation :: wrapped ::: Bool <?>
        ("Do not use incremental validation algorithm (incremental validation is the default " +++ 
         "so default for this option is false)."),

    noAdaptiveFetchIntervals :: wrapped ::: Bool <?>
        ("Do not use adaptive fetch intervals for repositories (adaptive fetch intervals is the default " +++ 
         "so default for this option is false)."),

    noAdaptiveFetchTimeouts :: wrapped ::: Bool <?>
        ("Do not use adaptive fetch timeouts for repositories (adaptive fetch timeouts is the default " +++ 
         "so default for this option is false)."),

    noAsyncFetch :: wrapped ::: Bool <?>
        ("Do not fetch repositories asynchronously, i.e. only fetch them while validating the RPKI tree " +++ 
        "(default is false, i.e. asynchronous fetches are used by default)."),

    showHiddenConfig :: wrapped ::: Bool <?>
        ("Reveal all config values in the HTTP API call to `/api/system`. " +++ 
         "This is a potential security issue since some of the config values include " +++ 
         "local FS paths (default is false)."),

    noValidityApi :: wrapped ::: Bool <?>
        ("Do not build VRP index for /api/validity calls in the REST API (default is false, i.e. the index " +++ 
         "is built by default). This option is useful for the cases when the VRP index is not needed, " +++ 
         "it will save some memory and CPU spent on the index.") 

} deriving (Generic)

instance ParseRecord (CLIOptions Wrapped) where
    parseRecord = parseRecordWithModifiers lispCaseModifiers

deriving instance Show (CLIOptions Unwrapped)

type (+++) (a :: Symbol) (b :: Symbol) = AppendSymbol a b
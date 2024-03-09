{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module RPKI.Options where

import Numeric.Natural
import Data.Int
import Data.Word ( Word16 )
import GHC.Generics (Generic)

import Options.Applicative

data Options = Options {

    worker     :: Bool,
    initialise :: Bool,

    version :: Bool,

    rpkiRootDirectory :: [FilePath],

    signatureFile :: Maybe FilePath,
    verifySignature :: Bool,    
    verifyDirectory :: Maybe FilePath,
    verifyFiles :: [FilePath],

    cpuCount :: Maybe Natural,

    resetCache :: Bool,
    revalidationInterval :: Maybe Int64,
    cacheLifetimeHours :: Maybe Int64,
    versionNumberToKeep :: Maybe Natural,
    rrdpRefreshInterval :: Maybe Int64,
    rsyncRefreshInterval :: Maybe Int64,
    rrdpTimeout :: Maybe Int64,
    asyncRrdpTimeout :: Maybe Int64,
    rsyncTimeout :: Maybe Int64,
    asyncRsyncTimeout :: Maybe Int64,
    rsyncClientPath :: Maybe String,
    httpApiPort :: Maybe Word16,
    lmdbSize :: Maybe Int64,

    withRtr :: Bool,
    rtrAddress :: Maybe String,
    rtrPort :: Maybe Int16,
    rtrLogFile :: Maybe String,

    logLevel :: Maybe String,
    strictManifestValidation :: Bool,

    localExceptions :: [String],

    maxTaRepositories :: Maybe Int,
    maxCertificatePathDepth :: Maybe Int,
    maxTotalTreeSize :: Maybe Int,
    maxObjectSize :: Maybe Integer,
    minObjectSize :: Maybe Integer,
    topDownTimeout :: Maybe Int64,

    noRrdp :: Bool,
    noRsync :: Bool,

    rsyncPrefetchUrl :: [String],
    metricsPrefix :: Maybe String,

    maxRrdpFetchMemory :: Maybe Int,
    maxRsyncFetchMemory :: Maybe Int,
    maxValidationMemory :: Maybe Int,

    noIncrementalValidation :: Bool,
    noAdaptiveFetchIntervals :: Bool,
    noAdaptiveFetchTimeouts :: Bool 

} deriving (Generic)


parseOptions = Options 
    <$> (switch (long "worker"))
    <*> (switch 
            (long "initialise" <> 
            help "If set, the FS layout will be created and TAL files will be downloaded.")
        ) 
    <*> (switch (long "version")) 
    <*> many (strOption 
            (long "rpki-root-directory" <> 
             help ("Root directory (default is ${HOME}/.rpki/). This option can be passed " <> 
                   "multiple times and the last one will be used, it is done for convenience " <> 
                   "of overriding this option with dockerised version.")))
--     <*> rscVerifyOptions
--   where
--     rscVerifyOptions = 
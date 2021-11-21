{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module RPKI.Config where

import Codec.Serialise

import GHC.Conc
import Numeric.Natural
import Data.Int
import Data.Word

import Data.Hourglass
import Data.Maybe (fromMaybe)
import Data.Monoid

import RPKI.Util (toNatural)
import GHC.Generics (Generic)

data Parallelism = Parallelism {
        cpuCount :: Natural,
        cpuParallelism :: Natural,
        ioParallelism :: Natural
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

data Config = Config {
        programBinaryPath         :: FilePath,
        rootDirectory             :: FilePath,
        talDirectory              :: FilePath,
        tmpDirectory              :: FilePath,
        cacheDirectory            :: FilePath,
        parallelism               :: Parallelism, 
        rsyncConf                 :: RsyncConf,
        rrdpConf                  :: RrdpConf,
        validationConfig          :: ValidationConfig,
        httpApiConf               :: HttpApiConfig,
        rtrConfig                 :: Maybe RtrConfig,
        cacheCleanupInterval      :: Seconds,
        cacheLifeTime             :: Seconds,
        oldVersionsLifetime       :: Seconds,
        storageCompactionInterval :: Seconds,
        lmdbSizeMb                :: Size,
        localExceptions           :: [FilePath]
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

data RsyncConf = RsyncConf {
        rsyncRoot    :: FilePath,
        rsyncTimeout :: Seconds
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

newtype Size = Size Int64
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (Serialise)
    deriving Semigroup via Sum Size
    deriving Monoid via Sum Size

data RrdpConf = RrdpConf {
        tmpRoot     :: FilePath,
        maxSize     :: Size,
        rrdpTimeout :: Seconds
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data ManifestProcessing = RFC6486_Strict | RFC6486
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data ValidationConfig = ValidationConfig {    
        revalidationInterval           :: Seconds,
        rrdpRepositoryRefreshInterval  :: Seconds,
        rsyncRepositoryRefreshInterval :: Seconds,

        -- Used mainly for testing and doesn't really make sense
        -- in a production environment    
        dontFetch                      :: Bool,
        
        manifestProcessing             :: ManifestProcessing,

        -- Maximal object tree depth measured in number of CAs
        maxCertificatePathDepth        :: Int,

        -- How many objects we allow in the tree for one TA.
        -- There needs to be some finite number to limit total
        -- tree validation and prevent DOS attacks.
        maxTotalTreeSize               :: Int,

        -- How many different repositories we allow to add
        -- during one TA top-down validation
        maxTaRepositories              :: Int,

        -- Maximal allowed size of an individual object 
        maxObjectSize                  :: Integer,

        -- Manimal allowed size of an individual object 
        minObjectSize                  :: Integer
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

data HttpApiConfig = HttpApiConfig {
        port :: Word16    
    } 
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Serialise)

data RtrConfig = RtrConfig {
        rtrAddress :: String,
        rtrPort    :: Int16
    } 
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

getRtsCpuCount :: Natural 
getRtsCpuCount = fromMaybe 1 $ toNatural numCapabilities

setCpuCount :: Natural -> IO ()
setCpuCount = setNumCapabilities . fromIntegral


-- Create 2 times more asyncs/tasks than there're capabilities. In most 
-- tested cases it seems to be beneficial for the CPU utilisation ¯\_(ツ)_/¯.    
-- 
-- Hardcoded (not sure it makes sense to make it configurable). Allow for 
-- that many IO operations (http downloads, LMDB reads, etc.) at once.
--
-- TODO There should be distinction between network operations and file/LMDB IO.
makeParallelism :: Natural -> Parallelism
makeParallelism cpus = Parallelism cpus (2 * cpus) 64

defaultConfig :: Config
defaultConfig = Config {
    programBinaryPath = "rpki-prover",
    rootDirectory = "",
    talDirectory = "",
    tmpDirectory = "",
    cacheDirectory = "",
    parallelism = makeParallelism 2,
    rsyncConf = RsyncConf {
        rsyncRoot    = "",
        rsyncTimeout = 7 * 60
    },
    rrdpConf = RrdpConf {
        tmpRoot = "",
        maxSize = Size $ 1024 * 1024 * 1024,
        rrdpTimeout = 7 * 60
    },
    validationConfig = ValidationConfig {
        revalidationInterval           = Seconds $ 13 * 60,
        rrdpRepositoryRefreshInterval  = Seconds 120,
        rsyncRepositoryRefreshInterval = Seconds $ 11 * 60,    
        dontFetch                      = False,
        manifestProcessing             = RFC6486,
        maxCertificatePathDepth        = 32,
        maxTotalTreeSize               = 5_000_000,
        maxObjectSize                  = 32 * 1024 * 1024,
        minObjectSize                  = 32,
        maxTaRepositories              = 1000
    },
    httpApiConf = HttpApiConfig {
        port = 9999
    },
    rtrConfig                 = Nothing,
    cacheCleanupInterval      = Seconds $ 60 * 120,
    cacheLifeTime             = Seconds $ 60 * 60 * 72,
    oldVersionsLifetime       = Seconds $ 60 * 60 * 10,
    storageCompactionInterval = Seconds $ 60 * 60 * 24,
    lmdbSizeMb                = Size $ 32 * 1024,
    localExceptions = []    
}

defaultRtrConfig :: RtrConfig
defaultRtrConfig = RtrConfig { 
        rtrAddress = "localhost",
        rtrPort    = 8283
    }
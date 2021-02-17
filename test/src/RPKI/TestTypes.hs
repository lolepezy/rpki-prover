{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module RPKI.TestTypes where

import Control.Monad
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64               as B64
import qualified Data.ByteString.Short                as BSS
import qualified Data.List                            as List
import qualified Data.Text                            as Text

import           Data.List.NonEmpty                   (NonEmpty)

import           Data.ASN1.BitArray
import           Data.ASN1.Types
import           Data.Bits
import           Data.Word
import           Data.X509                            as X509

import           HaskellWorks.Data.Network.Ip.Ipv4    as V4
import           HaskellWorks.Data.Network.Ip.Ipv6    as V6
import           HaskellWorks.Data.Network.Ip.Range

import           RPKI.Config
import           RPKI.Time
import           RPKI.Repository
import           RPKI.Resources.Resources
import           RPKI.Resources.Types
import           RPKI.RRDP.Types
import           RPKI.Reporting
import           RPKI.RTR.RtrState
import           RPKI.RTR.Types

import           Time.Types

import           Crypto.Error
import           Data.Hourglass

import qualified Crypto.PubKey.Curve25519             as X25519
import qualified Crypto.PubKey.Curve448               as X448
import qualified Crypto.PubKey.DSA                    as DSA
import           Crypto.PubKey.ECC.Types
import qualified Crypto.PubKey.Ed25519                as Ed25519
import qualified Crypto.PubKey.Ed448                  as Ed448
import qualified Crypto.PubKey.RSA                    as RSA
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           RPKI.Util       (convert, mkHash)
import RPKI.CommonTypes


testConfig :: Config
testConfig = Config {
    talDirectory = "",
    tmpDirectory = "",
    cacheDirectory = "",
    parallelism = Parallelism 8 8,
    rsyncConf = RsyncConf {
        rsyncRoot    = "",
        rsyncTimeout = 300
    },
    rrdpConf = RrdpConf {
        tmpRoot = "",
        maxSize = Size 1_000_000_000,
        rrdpTimeout = 300
    },
    validationConfig = ValidationConfig {
        revalidationInterval           = 600,
        rrdpRepositoryRefreshInterval  = 120,
        rsyncRepositoryRefreshInterval = 600,    
        dontFetch                      = False
    },
    httpApiConf = HttpApiConfig {
        port = 9988
    },
    rtrConfig                 = Nothing,
    cacheCleanupInterval      = 60 * 120,
    cacheLifeTime             = 60 * 60 * 12,
    oldVersionsLifetime       = 60 * 60 * 2,
    storageDefragmentInterval = 60 * 60 * 24,
    lmdbSize                  = Size 2_000_000_000
}
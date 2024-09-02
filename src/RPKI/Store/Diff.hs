{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLabels      #-}

module RPKI.Store.Diff where

import           Control.Lens

import           Data.Generics.Product.Typed
import           Data.Generics.Product.Fields

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Short    as BSS
import           Data.Monoid.Generic
import qualified Data.Set                 as Set
import           Data.Tuple.Strict
import           Data.Kind
import           Data.Store               hiding (Size)

import           GHC.Generics
import           RPKI.TAL

import           RPKI.Time                (Instant)

import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.AppTypes
import           RPKI.Domain
import           RPKI.Store.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Serialisation

-- Fuctions for diff-based storage

diff :: FlatPayloads -> FlatPayloads -> PayloadsDiff
diff before after = PayloadsDiff {
        roas  = roasDiff (before ^. typed) (after ^. typed),
        vrps  = vrpsDiff (before ^. typed) (after ^. typed),
        spls  = splDiff (before ^. typed) (after ^. typed),
        aspas = aspaDiff (before ^. typed) (after ^. typed),
        gbrs  = gbrDiff (before ^. typed) (after ^. typed),
        bgpCerts    = bgpCertsDiff (before ^. typed) (after ^. typed),
        validations = validationsDiff (before ^. typed) (after ^. typed),
        metrics     = metricsDiff (before ^. typed) (after ^. typed),
        traces      = tracesDiff (before ^. typed) (after ^. typed)
    }
    

roasDiff :: Roas -> Roas -> GeneralDiff Roas
roasDiff = GeneralDiff

vrpsDiff :: Vrps -> Vrps -> GeneralDiff Vrps
vrpsDiff = GeneralDiff

aspaDiff :: Set.Set Aspa -> Set.Set Aspa -> GeneralDiff (Set.Set Aspa)
aspaDiff = GeneralDiff

splDiff :: Set.Set SplN -> Set.Set SplN -> GeneralDiff (Set.Set SplN)
splDiff = GeneralDiff

gbrDiff :: Set.Set (T2 Hash Gbr) -> Set.Set (T2 Hash Gbr) -> GeneralDiff (Set.Set (T2 Hash Gbr))
gbrDiff = GeneralDiff

bgpCertsDiff :: Set.Set BGPSecPayload -> Set.Set BGPSecPayload -> GeneralDiff (Set.Set BGPSecPayload)
bgpCertsDiff = GeneralDiff

validationsDiff :: Validations -> Validations -> GeneralDiff Validations
validationsDiff = GeneralDiff

metricsDiff :: RawMetric -> RawMetric -> GeneralDiff RawMetric
metricsDiff = GeneralDiff

tracesDiff :: Set.Set Trace -> Set.Set Trace -> GeneralDiff (Set.Set Trace)
tracesDiff = GeneralDiff
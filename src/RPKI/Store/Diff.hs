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

import           Data.Set                 (Set, (\\))
import qualified Data.Set                 as Set

import           RPKI.Reporting
import           RPKI.Repository
import           RPKI.Domain
import           RPKI.Store.Types
import           RPKI.Store.Base.Storable
import           RPKI.Store.Base.Serialisation

-- Fuctions for diff-based storage

diff :: FlatPayloads -> FlatPayloads -> PayloadsDiff
diff before after = PayloadsDiff {
        roas  = roasDiff (before ^. typed) (after ^. typed),
        vrps  = vrpsDiff (before ^. typed) (after ^. typed),
        spls  = makeSetDiff (before ^. typed) (after ^. typed),
        aspas = makeSetDiff (before ^. typed) (after ^. typed),
        gbrs  = makeSetDiff (before ^. typed) (after ^. typed),
        bgpCerts    = makeSetDiff (before ^. typed) (after ^. typed),
        validations = validationsDiff (before ^. typed) (after ^. typed),
        metrics     = metricsDiff (before ^. typed) (after ^. typed),
        traces      = makeSetDiff (before ^. typed) (after ^. typed)
    }
    

roasDiff :: Roas -> Roas -> StoredDiff Roas
roasDiff = StoredDiff

vrpsDiff :: Vrps -> Vrps -> StoredDiff Vrps
vrpsDiff = StoredDiff

validationsDiff :: Validations -> Validations -> StoredDiff Validations
validationsDiff = StoredDiff

metricsDiff :: RawMetric -> RawMetric -> StoredDiff RawMetric
metricsDiff = StoredDiff

makeSetDiff :: Ord a => Set a -> Set a -> StoredDiff (Set a)
makeSetDiff before after = StoredDiff {
        added   = after \\ before,
        deleted = before \\ after
    }     

applySetDiff :: Ord a => Set a -> StoredDiff (Set a) -> Set a 
applySetDiff before diff = before \\ (diff ^. #deleted) <> (diff ^. #added)
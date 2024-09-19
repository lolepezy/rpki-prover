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
import           Control.DeepSeq

import           Data.Generics.Product.Typed

import           Data.Maybe
import           Data.Set                 (Set, (\\))
import qualified Data.Set                 as Set
import           Data.Tuple.Strict
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Map.Monoidal.Strict (MonoidalMap(..))

import           GHC.Generics

import           RPKI.Reporting
import           RPKI.Domain
import           RPKI.Store.Base.Serialisation


data FlatPayloads = FlatPayloads {
        roas        :: Roas,
        vrps        :: Vrps,
        spls        :: Set.Set SplN,
        aspas       :: Set.Set Aspa,
        gbrs        :: Set.Set (T2 Hash Gbr),
        bgpCerts    :: Set.Set BGPSecPayload,
        validations :: Validations,
        metrics     :: RawMetric,
        traces      :: Set.Set Trace
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data PayloadsDiff = PayloadsDiff {
        roas        :: StoredDiff Roas,
        vrps        :: StoredDiff Vrps,
        spls        :: StoredDiff (Set.Set SplN),
        aspas       :: StoredDiff (Set.Set Aspa),
        gbrs        :: StoredDiff (Set.Set (T2 Hash Gbr)),
        bgpCerts    :: StoredDiff (Set.Set BGPSecPayload),
        validations :: StoredDiff Validations,
        metrics     :: StoredDiff RawMetric,
        traces      :: StoredDiff (Set.Set Trace)
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data StoredDiff a = StoredDiff {
        added   :: a,
        deleted :: a
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)        


class Diffable a where
    makeDiff  :: a -> a -> StoredDiff a
    applyDiff :: a -> StoredDiff a -> a

instance Ord a => Diffable (Set.Set a) where
    makeDiff before after = StoredDiff {
            added   = after \\ before,
            deleted = before \\ after
        }     
    applyDiff before diff = 
        before \\ (diff ^. #deleted) <> (diff ^. #added)

instance Diffable Roas where
    makeDiff (Roas (MonoidalMap before)) (Roas (MonoidalMap after)) = let 
        -- zz = [ case Map.lookup ta after of 
        --             Nothing -> Nothing
        --             Just m  -> 
        --         | (ta, x) <- Map.toList before ]

        in StoredDiff (Roas (MonoidalMap before)) (Roas (MonoidalMap after))

    applyDiff before diff = before

instance Diffable Vrps where
    makeDiff = StoredDiff
    applyDiff before diff = before

instance Diffable Validations where
    makeDiff = StoredDiff
    applyDiff before diff = before

instance Diffable RawMetric where
    makeDiff = StoredDiff
    applyDiff before diff = before
        

instance (Eq k, Ord k, Diffable v) => Diffable (Map k v) where
    makeDiff before after = let 
        commonKeys = Set.intersection 
                (Set.fromList $ Map.keys before)
                (Set.fromList $ Map.keys after)

        
        z :: [(k, StoredDiff v)] = [ (k, makeDiff vb va)
                | k  <- Set.toList commonKeys,
                  vb <- maybeToList $ Map.lookup k before,
                  va <- maybeToList $ Map.lookup k after
                ]
        

        qq = foldr (\(k, diff) (added, deleted) -> 
                    (Map.insert k (diff ^. #added) added, 
                     Map.insert k (diff ^. #deleted) deleted)) 
                (mempty, mempty) z

        in StoredDiff before after

    applyDiff before diff = before        


diff :: FlatPayloads -> FlatPayloads -> PayloadsDiff
diff before after = PayloadsDiff {
        roas        = makeDiff (before ^. typed) (after ^. typed),
        vrps        = makeDiff (before ^. typed) (after ^. typed),
        spls        = makeDiff (before ^. typed) (after ^. typed),
        aspas       = makeDiff (before ^. typed) (after ^. typed),
        gbrs        = makeDiff (before ^. typed) (after ^. typed),
        bgpCerts    = makeDiff (before ^. typed) (after ^. typed),
        validations = makeDiff (before ^. typed) (after ^. typed),
        metrics     = makeDiff (before ^. typed) (after ^. typed),
        traces      = makeDiff (before ^. typed) (after ^. typed)
    }
    
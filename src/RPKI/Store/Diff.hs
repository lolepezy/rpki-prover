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
import           Data.Set                 ((\\))
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
        roas        :: ADiff Roas,
        vrps        :: ADiff Vrps,
        spls        :: ADiff (Set.Set SplN),
        aspas       :: ADiff (Set.Set Aspa),
        gbrs        :: ADiff (Set.Set (T2 Hash Gbr)),
        bgpCerts    :: ADiff (Set.Set BGPSecPayload),
        validations :: ADiff Validations,
        metrics     :: ADiff RawMetric,
        traces      :: ADiff (Set.Set Trace)
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)


data ADiff a = NoDiff 
            | AddDiff a 
            | DeleteDiff a 
            | MergeDiff a a
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary, NFData)             

mapDiff :: (a -> b) -> ADiff a -> ADiff b
mapDiff f = \case 
    NoDiff        -> NoDiff
    AddDiff a     -> AddDiff $ f a
    DeleteDiff d  -> DeleteDiff $ f d
    MergeDiff a d -> MergeDiff (f a) (f d)

optimiseDiff :: (v -> Bool) -> ADiff v -> ADiff v
optimiseDiff isEmpty = \case
    NoDiff    -> NoDiff
    AddDiff a 
        | isEmpty a -> NoDiff
        | otherwise -> AddDiff a

    DeleteDiff d  
        | isEmpty d -> NoDiff
        | otherwise -> DeleteDiff d

    MergeDiff a d
        | isEmpty a && isEmpty d -> NoDiff
        | isEmpty d -> AddDiff a
        | isEmpty a -> DeleteDiff d
        | otherwise -> MergeDiff a d

class Diffable a where
    makeDiff  :: a -> a -> ADiff a
    applyDiff :: a -> ADiff a -> a

instance Ord a => Diffable (Set.Set a) where
    makeDiff before after = let         
            added   = after \\ before
            deleted = before \\ after
        in case (Set.null added, Set.null deleted) of 
            (True,  True)  -> NoDiff
            (False, True)  -> AddDiff added
            (True, False)  -> DeleteDiff deleted
            (False, False) -> MergeDiff added deleted

    applyDiff before = \case 
        NoDiff        -> before
        AddDiff a     -> before <> a
        DeleteDiff d  -> before \\ d
        MergeDiff a d -> before \\ d <> a
            

instance (Eq k, Ord k, Eq v, Diffable v) => Diffable (MonoidalMap k v) where
    makeDiff (MonoidalMap before) (MonoidalMap after) = mapDiff MonoidalMap $ makeDiff before after         
    applyDiff (MonoidalMap before) diff = MonoidalMap $ applyDiff before (mapDiff getMonoidalMap diff)        

instance Diffable Roas where
    makeDiff (Roas before) (Roas after) = mapDiff Roas $ makeDiff before after                 
    applyDiff (Roas before) diff = Roas $ applyDiff before (mapDiff unRoas diff)                

instance Diffable Vrps where
    makeDiff (Vrps before) (Vrps after) = mapDiff Vrps $ makeDiff before after                 
    applyDiff (Vrps before) diff = Vrps $ applyDiff before (mapDiff unVrps diff)                

instance Diffable Validations where
    makeDiff (Validations before) (Validations after) = mapDiff Validations $ makeDiff before after                 
    applyDiff (Validations before) diff = Validations $ applyDiff before (mapDiff unValidations diff)                

instance Diffable RawMetric where
    makeDiff _ _ = NoDiff
    applyDiff before diff = before
        

instance (Eq k, Ord k, Eq v, Diffable v) => Diffable (Map k v) where
    makeDiff before after = let 
        beforeKeys = Set.fromList $ Map.keys before
        afterKeys  = Set.fromList $ Map.keys after
        commonKeys = Set.intersection beforeKeys afterKeys
        
        commonKeyDiffs = 
            [ (k, makeDiff vb va)
                | k  <- Set.toList commonKeys,
                  vb <- maybeToList $ Map.lookup k before,
                  va <- maybeToList $ Map.lookup k after
                ]
        
        deleted = [ (k, DeleteDiff v) | (k, v) <- Map.toList before, not (k `Set.member` afterKeys) ]
        added   = [ (k, AddDiff v)    | (k, v) <- Map.toList after, not (k `Set.member` beforeKeys) ]
        
        in optimiseDiff Map.null $
            foldr (\(k, diff) totalDiff@(MergeDiff added deleted) -> let 
                        ins v m = Map.insert k v m 
                    in case diff of 
                        NoDiff        -> totalDiff
                        AddDiff a     -> MergeDiff (ins a added) deleted
                        DeleteDiff d  -> MergeDiff added (ins d deleted)
                        MergeDiff a d -> MergeDiff (ins a added) (ins d deleted)
                ) 
                (MergeDiff mempty mempty)
                (commonKeyDiffs <> deleted <> added)        

    applyDiff before = \case
        NoDiff                  -> before
        AddDiff added           -> add added before
        DeleteDiff deleted      -> delete deleted before
        MergeDiff added deleted -> add added (delete deleted before)
      where
        add added totalMap = 
            foldr (\(k, a) m -> 
                    Map.alter (\v -> Just $ maybe a (`applyDiff` (AddDiff a)) v) k m
                )
                totalMap $ 
                Map.toList added

        delete deleted totalMap = 
            foldr (\(k, d) m -> 
                    Map.alter (\case 
                        Nothing -> Nothing
                        Just v 
                            | v == d    -> Nothing
                            | otherwise -> Just $ applyDiff v (DeleteDiff d)
                    ) k m 
                ) 
                totalMap
                $ Map.toList deleted



payloadDiff :: FlatPayloads -> FlatPayloads -> PayloadsDiff
payloadDiff before after = PayloadsDiff {
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
    
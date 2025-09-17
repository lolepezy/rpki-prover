{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StrictData           #-}

module RPKI.Metrics.Metrics where

import           Control.Lens
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import qualified Data.HashMap.Strict as HashMap
import           GHC.Generics
import           Data.Map.Monoidal.Strict
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Store.Base.Serialisation

data GroupedMetric a = GroupedMetric {
        byTa         :: MonoidalMap TaName a,
        byRepository :: MonoidalMap RpkiURL a,
        total        :: a
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    


groupedValidationMetric :: Metrics -> GroupedMetric ValidationMetric
groupedValidationMetric m@Metrics {..} = GroupedMetric {..}
  where    
    total = mconcat (MonoidalMap.elems byTa) 
                & #uniqueVrpNumber .~ m ^. #vrpCounts . #totalUnique

    byTa = MonoidalMap.mapWithKey calculateUniqueVrps byTa'

    calculateUniqueVrps taName vm = 
        maybe vm (\uniqCount -> vm & #uniqueVrpNumber .~ uniqCount) $
            MonoidalMap.lookup taName (m ^. #vrpCounts . #perTaUnique)

    (byTa', byRepository) = 
        Prelude.foldr combineMetrics mempty $ HashMap.toList $ unMetricMap validationMetrics

    combineMetrics (scope, metric) (pTa, perRepo) = (newPerTa, newPerRepo)
      where
        newPerTa =
            case reverse [ TaName uri | TAFocus uri <- focuses scope ] of
                []      -> pTa
                ta' : _ -> MonoidalMap.singleton ta' metric <> pTa

        newPerRepo =
            -- take the deepest publication point
            case [ pp | PPFocus pp <- focuses scope ] of
                []      -> perRepo
                uri : _ -> MonoidalMap.singleton uri metric <> perRepo        

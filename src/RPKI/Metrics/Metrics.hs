{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StrictData           #-}

module RPKI.Metrics.Metrics where

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import           GHC.Generics
import           Data.Map.Monoidal.Strict
import           RPKI.Domain
import           RPKI.Reporting
import           RPKI.Store.Base.Serialisation


data GroupedValidationMetric a = GroupedValidationMetric {
        byTa         :: MonoidalMap TaName a,
        byRepository :: MonoidalMap RpkiURL a,
        total        :: a
    }
    deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (TheBinary)    

groupedValidationMetric :: RawMetric -> GroupedValidationMetric ValidationMetric
groupedValidationMetric rm@RawMetric {..} = GroupedValidationMetric {..}
  where    
    total = mconcat (MonoidalMap.elems byTa) 
                & #uniqueVrpNumber .~ rm ^. #vrpCounts . #totalUnique

    byTa = MonoidalMap.mapWithKey calculateUniqueVrps byTa'

    calculateUniqueVrps taName vm = 
        maybe vm (\uniqCount -> vm & #uniqueVrpNumber .~ uniqCount) $
            MonoidalMap.lookup taName (rm ^. #vrpCounts . #perTaUnique)

    (byTa', byRepository) = 
        MonoidalMap.foldrWithKey combineMetrics mempty $ unMetricMap validationMetrics

    combineMetrics metricScope metric (pTa, perRepo) = (newPerTa, newPerRepo)
      where
        newPerTa =
            case Prelude.take 1 $ reverse [ TaName uri | TAFocus uri <- scopeList metricScope ] of
                []      -> pTa
                ta' : _ -> MonoidalMap.singleton ta' metric <> pTa

        newPerRepo =
            -- take the deepest PP
            case Prelude.take 1 [ pp | PPFocus pp <- scopeList metricScope ] of
                []      -> perRepo
                uri : _ -> MonoidalMap.singleton uri metric <> perRepo        
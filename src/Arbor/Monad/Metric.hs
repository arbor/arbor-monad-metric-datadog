{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Arbor.Monad.Metric
  ( MonadMetrics
  , Z.getMetrics

  , newMetricsIO
  , extractValues
  ) where

import Arbor.Monad.Metric.Type     (MetricFamily (..), MetricMap, Metrics (Metrics), MonadMetrics)
import Control.Concurrent.STM.TVar
import Control.Monad.STM           (STM)
import Data.Proxy

import qualified Arbor.Monad.Metric.Type as Z
import qualified Control.Concurrent.STM  as STM
import qualified Data.Map.Strict         as M

newMetricsIO :: IO Metrics
newMetricsIO = Metrics
  <$> STM.newTVarIO M.empty
  <*> STM.newTVarIO M.empty

extractValues :: forall k. ()
  => MetricFamily k
  => Proxy k
  -> MetricMap k (MetricState k)
  -> STM ([(k, MetricValue k)], [TVar (MetricState k)])
extractValues pk m = do
  let names = M.keys m
  let tvars = M.elems m
  nums <- fmap (metricStateToValue pk <$>) . sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

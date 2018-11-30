{-# LANGUAGE DataKinds        #-}

module Arbor.Monad.Metric
  ( MonadMetrics
  , Z.getMetrics

  , newMetricsIO
  , valuesByKeys
  , extractValues
  ) where

import Arbor.Monad.Metric.Type     (MetricFamily (..), MetricMap, Metrics (Metrics), MonadMetrics, getMetricMap)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Control.Monad.STM           (STM, atomically)

import qualified Arbor.Monad.Metric.Type as Z
import qualified Control.Concurrent.STM  as STM
import qualified Data.Map.Strict         as M

newMetricsIO :: IO Metrics
newMetricsIO = Metrics
  <$> STM.newTVarIO M.empty
  <*> STM.newTVarIO M.empty

valuesByKeys :: ()
  => Ord k
  => MetricFamily k
  => MonadMetrics m
  => [k]
  -> m [MetricValue k]
valuesByKeys ks = do
  metricMap <- getMetricMap
  liftIO $ atomically $ sequence $ readTVar <$> ((metricMap M.!) <$> ks)

extractValues :: ()
  => MetricMap k (MetricValue k)
  -> STM ([(k, MetricValue k)], [TVar (MetricValue k)])
extractValues m = do
  let names = M.keys m
  let tvars = M.elems m
  nums <- sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Metric
  ( MonadMetrics
  , Z.getMetrics

  , newMetricsIO
  , resetStats
  , valuesByKeys
  , extractValues
  ) where

import Arbor.Monad.Metric.Type     (MetricFamily (..), MetricMap, Metrics (Metrics), MonadMetrics, getMetricMap)
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM           (STM, atomically)
import Data.Foldable
import Data.Generics.Product.Any

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

resetStats :: ()
  => MonadMetrics m
  => m ()
resetStats = do
  metrics   <- Z.getMetrics
  counters  <- liftIO $ STM.readTVarIO $ metrics ^. the @"counters"
  gauges    <- liftIO $ STM.readTVarIO $ metrics ^. the @"gauges"
  setZeroes counters
  setZeroes gauges

setZeroes :: ()
  => MonadIO m
  => Num (MetricValue k)
  => MetricMap k (MetricValue k)
  -> m ()
setZeroes cs = liftIO $ atomically $ do
  (_, tvars) <- extractValues cs
  traverse_ (`modifyTVar` const 0) tvars

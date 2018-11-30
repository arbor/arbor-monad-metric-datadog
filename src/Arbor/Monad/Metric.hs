{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Metric
  ( MonadMetrics
  , Z.getMetrics

  , incByKey
  , incByKey'
  , addByKey
  , addByKey'
  , setByKey
  , setByKey'

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

-- Increase the current value by 1
incByKey :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => MonadMetrics m
  => k
  -> m ()
incByKey = modifyByKey (+1)

-- Increase the current value by 1
incByKey' :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => k
  -> Metrics
  -> IO ()
incByKey' = modifyByKey' (+1)

-- Increase the current value by n
addByKey :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => MonadMetrics m
  => MetricValue k
  -> k
  -> m ()
addByKey n = modifyByKey (+n)

-- Increase the current value by n
addByKey' :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => MetricValue k
  -> k
  -> Metrics
  -> IO ()
addByKey' n = modifyByKey' (+n)

-- Set the current value
setByKey :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => MonadMetrics m
  => MetricValue k -> k -> m ()
setByKey value = modifyByKey (const value)

-- Set the current value
setByKey' :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => MetricValue k
  -> k
  -> Metrics
  -> IO ()
setByKey' value = modifyByKey' (const value)

-- Modify the current value with the supplied function
modifyByKey :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => MonadMetrics m
  => (MetricValue k -> MetricValue k)
  -> k
  -> m ()
modifyByKey f key = do
  metrics <- Z.getMetrics
  liftIO $ modifyByKey' f key metrics

-- Modify the current value with the supplied function
modifyByKey' :: ()
  => Ord k
  => Num (MetricValue k)
  => MetricFamily k
  => (MetricValue k -> MetricValue k)
  -> k
  -> Metrics
  -> IO ()
modifyByKey' f key metrics = do
  let tCounters = metricMapTVarOf metrics
  STM.atomically $ do
    counters <- STM.readTVar tCounters
    case counters M.!? key of
      Just tv -> modifyTVar tv f
      Nothing -> do
        tv <- STM.newTVar (f 0)
        let counters' = M.insert key tv counters
        STM.writeTVar tCounters counters'

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

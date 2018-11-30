module Arbor.Monad.Metric.Generic
  ( incByKey
  , incByKey'
  , addByKey
  , addByKey'
  , setByKey
  , setByKey'
  ) where

import Arbor.Monad.Metric.Type     (MetricFamily (..), Metrics, MonadMetrics)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class

import qualified Arbor.Monad.Metric.Type as Z
import qualified Control.Concurrent.STM  as STM
import qualified Data.Map.Strict         as M
    
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

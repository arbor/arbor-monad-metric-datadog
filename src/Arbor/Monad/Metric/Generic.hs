{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Arbor.Monad.Metric.Generic
  ( metric
  ) where

import Arbor.Monad.Metric.Type     (MetricFamily (..), Metrics, MonadMetrics)
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Semigroup (Semigroup, (<>))
import Data.Proxy

import qualified Arbor.Monad.Metric.Type as Z
import qualified Control.Concurrent.STM  as STM
import qualified Data.Map.Strict         as M

-- Modify the current value with the supplied function
metric :: ()
  => Ord k
  => Semigroup (MetricState k)
  => MetricFamily k
  => MonadMetrics m
  => k
  -> MetricValue k
  -> m ()
metric key v = do
  metrics <- Z.getMetrics
  liftIO $ metric' key v metrics

-- Modify the current value with the supplied function
metric' :: forall k . ()
  => Ord k
  => MetricFamily k
  => Semigroup (MetricState k)
  => k
  -> MetricValue k
  -> Metrics
  -> IO ()
metric' key value metrics = do
  let tCounters = metricMapTVarOf metrics :: STM.TVar (Z.MetricMap k (MetricState k))
  STM.atomically $ do
    counters <- STM.readTVar tCounters
    case counters M.!? key of
      Just tv -> modifyTVar tv (<> metricValueToState (Proxy @k) value)
      Nothing -> do
        tv <- STM.newTVar (metricValueToState (Proxy @k) value)
        let counters' = M.insert key tv counters
        STM.writeTVar tCounters counters'

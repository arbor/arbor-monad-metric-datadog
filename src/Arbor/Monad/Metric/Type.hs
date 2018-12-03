{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Arbor.Monad.Metric.Type where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.Proxy
import Data.Semigroup
import GHC.Generics

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict        as M

newtype Counter = Counter
  { name :: String
  } deriving (Eq, Ord, Show)

newtype Gauge = Gauge
  { name :: String
  } deriving (Eq, Ord, Show)

type MetricMap k v = M.Map k (STM.TVar v)

data Metrics = Metrics
  { counters :: STM.TVar (MetricMap Counter (MetricState Counter))
  , gauges   :: STM.TVar (MetricMap Gauge   (MetricState Gauge  ))
  } deriving (Generic)

class (Monad m, MonadIO m) => MonadMetrics m where
  getMetrics :: m Metrics

instance MonadMetrics m => MonadMetrics (ExceptT e m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (IdentityT m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (MaybeT m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (ReaderT e m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (ResourceT m) where
  getMetrics = lift getMetrics

instance MonadMetrics m => MonadMetrics (StateT s m) where
  getMetrics = lift getMetrics

class MetricFamily k where
  type MetricValue k
  type MetricState k
  metricMapTVarOf :: Metrics -> STM.TVar (MetricMap k (MetricState k))
  metricValueToState :: Proxy k -> MetricValue k -> MetricState k
  metricStateToValue :: Proxy k -> MetricState k -> MetricValue k

getMetricMapTVar :: (MetricFamily k, MonadMetrics m) => m (STM.TVar (MetricMap k (MetricState k)))
getMetricMapTVar = getMetrics <&> metricMapTVarOf

getMetricMap :: (MetricFamily k, MonadMetrics m) => m (MetricMap k (MetricState k))
getMetricMap = liftIO . STM.readTVarIO =<< getMetricMapTVar

instance MetricFamily Counter where
  type MetricValue Counter = Int
  type MetricState Counter = Sum Int
  metricMapTVarOf = (^. the @"counters")
  metricValueToState _ = Sum
  metricStateToValue _ = getSum

instance MetricFamily Gauge where
  type MetricValue Gauge = Double
  type MetricState Gauge = Last Double
  metricMapTVarOf = (^. the @"gauges")
  metricValueToState _ = Last
  metricStateToValue _ = getLast

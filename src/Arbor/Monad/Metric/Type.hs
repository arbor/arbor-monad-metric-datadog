{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arbor.Monad.Metric.Type where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import GHC.Generics

import qualified Control.Concurrent.STM as STM
import qualified Data.Map.Strict        as M

type CounterKey = String

type MetricMap v = M.Map CounterKey (STM.TVar v)

data Metrics = Metrics
  { counters :: STM.TVar (MetricMap Int)
  , gauges   :: STM.TVar (MetricMap Int)
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

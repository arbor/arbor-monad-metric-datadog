module Arbor.Monad.Counter.Type where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Map.Strict

type CounterKey = String

newtype CounterValue = CounterValue
  { _counterValueVar   :: TVar Int
  }

type CountersMap = Map CounterKey CounterValue

data Counters = Counters
  { _countersCur   :: CountersMap
  , _countersPre   :: CountersMap
  , _countersTotal :: CountersMap
  }

class (Monad m, MonadIO m) => MonadCounters m where
  getCounters :: m Counters

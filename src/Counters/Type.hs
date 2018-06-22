module Counters.Type where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Map.Strict

type CounterKey = String

data CounterValue = CounterValue
  { _counterValueVar :: TVar Int
  }

data Counters = Counters
  { _countersMap :: Map CounterKey CounterValue
  }

countersEmpty :: Counters
countersEmpty = Counters empty

class (Functor m, Applicative m, MonadIO m) => MonadCounters m where
  getCounters :: m Counters
  getTotals :: m Counters

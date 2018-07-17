{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Arbor.Monad.Counter.Type where

import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class
import Data.Map.Strict
import GHC.Generics

type CounterKey = String

newtype CounterValue = CounterValue
  { var   :: TVar Int
  } deriving (Generic)

type CountersMap = Map CounterKey CounterValue

data Counters = Counters
  { current  :: CountersMap
  , previous :: CountersMap
  , total    :: CountersMap
  } deriving (Generic)

class (Monad m, MonadIO m) => MonadCounters m where
  getCounters :: m Counters

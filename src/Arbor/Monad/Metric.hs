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
  , currentStats
  ) where

import Arbor.Monad.Metric.Type     (CounterKey, MetricMap, Metrics (Metrics), MonadMetrics)
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
incByKey :: MonadMetrics m => CounterKey -> m ()
incByKey = modifyByKey (+1)

-- Increase the current value by 1
incByKey' :: Metrics -> CounterKey -> IO ()
incByKey' = modifyByKey' (+1)

-- Increase the current value by n
addByKey :: MonadMetrics m => Int -> CounterKey -> m ()
addByKey n = modifyByKey (+n)

-- Increase the current value by n
addByKey' :: Int -> Metrics -> CounterKey -> IO ()
addByKey' n = modifyByKey' (+n)

-- Set the current value
setByKey :: MonadMetrics m => Int -> CounterKey -> m ()
setByKey value = modifyByKey (const value)

-- Set the current value
setByKey' :: Int -> Metrics -> CounterKey -> IO ()
setByKey' value = modifyByKey' (const value)

-- Modify the current value with the supplied function
modifyByKey :: MonadMetrics m => (Int -> Int) -> CounterKey -> m ()
modifyByKey f key = do
  metrics <- Z.getMetrics
  liftIO $ modifyByKey' f metrics key

-- Modify the current value with the supplied function
modifyByKey' :: (Int -> Int) -> Metrics -> CounterKey -> IO ()
modifyByKey' f metrics key = do
  let tCounters = metrics ^. the @"counters"
  STM.atomically $ do
    counters <- STM.readTVar tCounters
    case counters M.!? key of
      Just tv -> modifyTVar tv f
      Nothing -> do
        tv <- STM.newTVar (f 0)
        let counters' = M.insert key tv counters
        STM.writeTVar tCounters counters'

valuesByKeys :: MonadMetrics m => [CounterKey] -> m [Int]
valuesByKeys ks = do
  (Metrics tCounters _) <- Z.getMetrics
  counters <- liftIO $ STM.readTVarIO tCounters
  liftIO $ atomically $ sequence $ readTVar <$> ((counters M.!) <$> ks)

extractValues :: MetricMap Int -> STM ([(CounterKey, Int)], [TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = M.elems m
  nums <- sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

currentStats :: MonadMetrics m => m (MetricMap Int)
currentStats = Z.getMetrics <&> (^. the @"counters") >>= liftIO . STM.readTVarIO

resetStats :: MonadMetrics m => m ()
resetStats = do
  metrics   <- Z.getMetrics
  counters  <- liftIO $ STM.readTVarIO $ metrics ^. the @"counters"
  gauges    <- liftIO $ STM.readTVarIO $ metrics ^. the @"gauges"
  sequence_ $ setZeroes <$> [counters, gauges]

setZeroes :: MonadIO m => MetricMap Int -> m ()
setZeroes cs = liftIO $ atomically $ do
  (_, tvars) <- extractValues cs
  traverse_ (`modifyTVar` const 0) tvars

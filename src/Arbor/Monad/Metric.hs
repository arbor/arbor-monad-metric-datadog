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
  , newMetricsMap
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
import qualified Data.List               as DL
import qualified Data.Map.Strict         as M

newMetricsIO :: IO Metrics
newMetricsIO = Metrics
  <$> STM.newTVarIO M.empty
  <*> STM.newTVarIO M.empty

newMetricsMap :: [CounterKey] -> IO (M.Map CounterKey (STM.TVar Int))
newMetricsMap (k:ks) = do
  m <- newMetricsMap ks
  v <- STM.newTVarIO 0
  return $ M.insert k v m
newMetricsMap [] = return M.empty

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

-- Modify the current value with the supplied function
modifyByKey :: MonadMetrics m => (Int -> Int) -> CounterKey -> m ()
modifyByKey f key = do
  metrics <- Z.getMetrics
  liftIO $ modifyByKey' f metrics key

-- Modify the current value with the supplied function
modifyByKey' :: (Int -> Int) -> Metrics -> CounterKey -> IO ()
modifyByKey' f (Metrics tCounters _) key = do
  counters <- STM.readTVarIO tCounters
  let tv = counters M.! key
  atomically $ modifyTVar tv f

-- Set the current value
setByKey :: MonadMetrics m => Int -> CounterKey -> m ()
setByKey value key = do
  metrics <- Z.getMetrics
  liftIO $ setByKey' value metrics key

-- Set the current value
setByKey' :: Int -> Metrics -> CounterKey -> IO ()
setByKey' value (Metrics tCounters _) key = do
  counters <- STM.readTVarIO tCounters
  let tv = counters M.! key
  atomically $ writeTVar tv value

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

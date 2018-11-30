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
  , counterMetrics
  ) where

import Arbor.Monad.Metric.Type     (Counter, Gauge, MetricMap, Metrics (Metrics), MonadMetrics)
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
incByKey :: MonadMetrics m => Counter -> m ()
incByKey = modifyByKey (+1)

-- Increase the current value by 1
incByKey' :: Metrics -> Counter -> IO ()
incByKey' = modifyByKey' (+1)

-- Increase the current value by n
addByKey :: MonadMetrics m => Int -> Counter -> m ()
addByKey n = modifyByKey (+n)

-- Increase the current value by n
addByKey' :: Int -> Metrics -> Counter -> IO ()
addByKey' n = modifyByKey' (+n)

-- Set the current value
setByKey :: MonadMetrics m => Int -> Counter -> m ()
setByKey value = modifyByKey (const value)

-- Set the current value
setByKey' :: Int -> Metrics -> Counter -> IO ()
setByKey' value = modifyByKey' (const value)

-- Modify the current value with the supplied function
modifyByKey :: MonadMetrics m => (Int -> Int) -> Counter -> m ()
modifyByKey f key = do
  metrics <- Z.getMetrics
  liftIO $ modifyByKey' f metrics key

-- Modify the current value with the supplied function
modifyByKey' :: (Int -> Int) -> Metrics -> Counter -> IO ()
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

valuesByKeys :: MonadMetrics m => [Counter] -> m [Int]
valuesByKeys ks = do
  (Metrics tCounters _) <- Z.getMetrics
  counters <- liftIO $ STM.readTVarIO tCounters
  liftIO $ atomically $ sequence $ readTVar <$> ((counters M.!) <$> ks)

extractValues :: MetricMap k Int -> STM ([(k, Int)], [TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = M.elems m
  nums <- sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

counterMetrics :: MonadMetrics m => m (MetricMap Counter Int)
counterMetrics = Z.getMetrics <&> (^. the @"counters") >>= liftIO . STM.readTVarIO

resetStats :: MonadMetrics m => m ()
resetStats = do
  metrics   <- Z.getMetrics
  counters  <- liftIO $ STM.readTVarIO $ metrics ^. the @"counters"
  gauges    <- liftIO $ STM.readTVarIO $ metrics ^. the @"gauges"
  setZeroes counters
  setZeroes gauges

setZeroes :: MonadIO m => MetricMap k Int -> m ()
setZeroes cs = liftIO $ atomically $ do
  (_, tvars) <- extractValues cs
  traverse_ (`modifyTVar` const 0) tvars

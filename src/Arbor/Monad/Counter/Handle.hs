{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Counter.Handle
  ( MonadCounters
  , Z.getCounters
  , incByKey
  , incByKey'
  , newCounters
  , resetStats
  , valuesByKeys
  , extractValues
  , newCountersMap
  , deltaStats
  ) where

import Arbor.Monad.Counter.Type    (CounterKey, CounterValue (CounterValue), Counters (Counters), CountersMap, MonadCounters)
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM           (STM, atomically)
import Data.Foldable
import Data.Generics.Product.Any

import qualified Arbor.Monad.Counter.Type as Z
import qualified Data.List                as DL
import qualified Data.Map.Strict          as M

newCounters :: [CounterKey] -> IO Counters
newCounters ks = Counters <$> newCountersMap ks <*> newCountersMap ks <*> newCountersMap ks

newCountersMap :: [CounterKey] -> IO CountersMap
newCountersMap (k:ks) = do
  m <- newCountersMap ks
  v <- CounterValue <$> newTVarIO 0
  return $ M.insert k v m
newCountersMap [] = return M.empty

-- Increase the current value by 1
incByKey :: MonadCounters m => CounterKey -> m ()
incByKey key = do
  (Counters cur _ _) <- Z.getCounters
  let (CounterValue v) = cur M.! key
  liftIO $ atomically $ modifyTVar v (+1)

-- Increase the current value by 1
incByKey' :: Counters -> CounterKey -> IO ()
incByKey' (Counters cur _ _) key = do
  let (CounterValue v) = cur M.! key
  atomically $ modifyTVar v (+1)

valuesByKeys :: MonadCounters m => [CounterKey] -> m [Int]
valuesByKeys ks = do
  (Counters cur _ _) <- Z.getCounters
  liftIO $ atomically $ sequence $ readTVar <$> ((\k -> cur M.! k ^. the @"var") <$> ks)

extractValues :: CountersMap -> STM ([(CounterKey, Int)], [TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = (^. the @"var") <$> M.elems m
  nums <- sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

-- store the current stats into previous;
-- accumulate stats in total
-- calculate the delta
deltaStats :: MonadCounters m => m CountersMap
deltaStats = do
  counters <- Z.getCounters
  deltas <- liftIO $ newCountersMap $ M.keys $ counters ^. the @"current"
  -- deltaCounters is accumulated into based on the diff between last and current counter values.
  liftIO $ atomically $ do
    (_, oldTvars)   <- extractValues $ counters ^. the @"previous"
    (_, newTvars)   <- extractValues $ counters ^. the @"current"
    (_, totalTvars) <- extractValues $ counters ^. the @"total"
    (_, deltaTvars) <- extractValues deltas
    for_ (DL.zip4 oldTvars newTvars totalTvars deltaTvars) $ \(old, new, total, delta) -> do
      new' <- readTVar new
      old' <- readTVar old
      total' <- readTVar total
      writeTVar old new'
      writeTVar delta (new' - old')
      writeTVar total (total' + (new' - old'))
    return deltas

resetStats :: MonadCounters m => m ()
resetStats = do
  counters <- Z.getCounters
  sequence_ $ setZeroes <$> [counters ^. the @"current", counters ^. the @"previous", counters ^. the @"total"]

setZeroes :: MonadIO m => CountersMap -> m ()
setZeroes cs = liftIO $ atomically $ do
    (_, tvars) <- extractValues cs
    traverse_ (`modifyTVar` const 0) tvars

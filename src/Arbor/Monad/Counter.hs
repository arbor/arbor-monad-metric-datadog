{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Counter
  ( MonadCounters
  , Z.getCounters

  , incByKey
  , incByKey'
  , addByKey
  , addByKey'
  , setByKey
  , setByKey'

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
newCounters ks = Counters <$> newCountersMap ks <*> newCountersMap ks

newCountersMap :: [CounterKey] -> IO CountersMap
newCountersMap (k:ks) = do
  m <- newCountersMap ks
  v <- CounterValue <$> newTVarIO 0
  return $ M.insert k v m
newCountersMap [] = return M.empty

-- Increase the current value by 1
incByKey :: MonadCounters m => CounterKey -> m ()
incByKey = modifyByKey (+1)

-- Increase the current value by 1
incByKey' :: Counters -> CounterKey -> IO ()
incByKey' = modifyByKey' (+1)

-- Increase the current value by n
addByKey :: MonadCounters m => Int -> CounterKey -> m ()
addByKey n = modifyByKey (+n)

-- Increase the current value by n
addByKey' :: Int -> Counters -> CounterKey -> IO ()
addByKey' n = modifyByKey' (+n)

-- Modify the current value with the supplied function
modifyByKey :: MonadCounters m => (Int -> Int) -> CounterKey -> m ()
modifyByKey f key = do
  counters <- Z.getCounters
  liftIO $ modifyByKey' f counters key

-- Modify the current value with the supplied function
modifyByKey' :: (Int -> Int) -> Counters -> CounterKey -> IO ()
modifyByKey' f (Counters cur _) key = do
  let (CounterValue tv) = cur M.! key
  atomically $ modifyTVar tv f

-- Set the current value
setByKey :: MonadCounters m => Int -> CounterKey -> m ()
setByKey value key = do
  counters <- Z.getCounters
  liftIO $ setByKey' value counters key

-- Set the current value
setByKey' :: Int -> Counters -> CounterKey -> IO ()
setByKey' value (Counters cur _) key = do
  let (CounterValue tv) = cur M.! key
  atomically $ writeTVar tv value

valuesByKeys :: MonadCounters m => [CounterKey] -> m [Int]
valuesByKeys ks = do
  (Counters cur _) <- Z.getCounters
  liftIO $ atomically $ sequence $ readTVar <$> ((\k -> cur M.! k ^. the @"var") <$> ks)

extractValues :: CountersMap -> STM ([(CounterKey, Int)], [TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = (^. the @"var") <$> M.elems m
  nums <- sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

-- store the current stats into previous;
-- calculate the delta
deltaStats :: MonadCounters m => m CountersMap
deltaStats = do
  counters <- Z.getCounters
  deltas <- liftIO $ newCountersMap $ M.keys $ counters ^. the @"current"
  -- deltaCounters is accumulated into based on the diff between last and current counter values.
  liftIO $ atomically $ do
    (_, oldTvars)   <- extractValues $ counters ^. the @"previous"
    (_, newTvars)   <- extractValues $ counters ^. the @"current"
    (_, deltaTvars) <- extractValues deltas
    for_ (DL.zip3 oldTvars newTvars deltaTvars) $ \(old, new, delta) -> do
      new' <- readTVar new
      old' <- readTVar old
      writeTVar old new'
      writeTVar delta (new' - old')
    return deltas

resetStats :: MonadCounters m => m ()
resetStats = do
  counters <- Z.getCounters
  sequence_ $ setZeroes <$> [counters ^. the @"current", counters ^. the @"previous"]

setZeroes :: MonadIO m => CountersMap -> m ()
setZeroes cs = liftIO $ atomically $ do
  (_, tvars) <- extractValues cs
  traverse_ (`modifyTVar` const 0) tvars

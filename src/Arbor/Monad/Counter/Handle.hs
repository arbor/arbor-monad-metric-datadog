{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Counter.Handle
  ( MonadCounters
  , Z.getCounters
  , incByKey
  , incByKey'
  , logStats
  , newCounters
  , resetStats
  , sendSummary
  , valuesByKeys
  ) where

import Arbor.Monad.Counter.Type     (CounterKey, CounterValue (CounterValue), Counters (Counters), CountersMap, MonadCounters)
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.STM            (STM, atomically)
import Data.Foldable
import Data.Generics.Product.Fields
import Data.Semigroup               ((<>))
import Network.StatsD

import qualified Arbor.Monad.Counter.Type as Z
import qualified Data.List                as DL
import qualified Data.Map.Strict          as M
import qualified Data.Text                as T

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
  liftIO $ atomically $ sequence $ readTVar <$> ((\k -> cur M.! k ^. field @"var") <$> ks)

extractValues :: CountersMap -> STM ([(CounterKey, Int)], [TVar Int])
extractValues m = do
  let names = M.keys m
  let tvars = (^. field @"var") <$> M.elems m
  nums <- sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

logStats :: (MonadStats m, MonadCounters m, MonadLogger m) => m ()
logStats = do
  deltas <- deltaStats
  (stats, _) <- liftIO $ atomically $ extractValues deltas
  let nonzero = DL.filter (\e -> snd e /= 0) stats

  unless (DL.null nonzero) $
    logInfoN $ T.pack $ DL.intercalate ", " $ (\(n, i) -> n <> ": " <> show i) <$> nonzero

  traverse_ sendMetric (mkTaggedMetrics "counters" stats)
  traverse_ sendMetric (mkNonTaggedMetrics stats)

sendSummary :: (MonadIO m, MonadCounters m, MonadStats m) => String -> Tag -> String -> m ()
sendSummary etitle etag fn = do
  counters    <- Z.getCounters
  (stats, _)  <- liftIO $ atomically $ extractValues $ counters ^. field @"previous"
  sendEvent (mkEvent stats etitle etag fn)

metricName :: String -> T.Text
metricName n = T.replace " " "_" (T.pack n)

-- create metric m, but tag with stat:[actual stat name]
mkTaggedMetrics :: String -> [(String, Int)] -> [Metric]
mkTaggedMetrics m =
  fmap (\(n, i) -> gauge (MetricName (metricName m)) id i & tags %~ ([tag "stat" (T.pack n)] ++))

-- create metrics for each counter
mkNonTaggedMetrics :: [(String, Int)] -> [Metric]
mkNonTaggedMetrics =
  fmap (\(n, i) -> gauge (MetricName (metricName n)) id i)

mkEvent :: [(String, Int)] -> String -> Tag -> String -> Event
mkEvent stats etitle etag fn = event (T.pack etitle) desc & tags %~ ([etag] ++)
  where desc = T.intercalate "\n" $ T.pack <$> ("File processed: " <> fn) : info
        info = (\(n, i) -> n <> ": " <> show i) <$> stats

-- store the current stats into previous;
-- accumulate stats in total
-- calculate the delta
deltaStats :: MonadCounters m => m CountersMap
deltaStats = do
  counters <- Z.getCounters
  deltas <- liftIO $ newCountersMap $ M.keys $ counters ^. field @"current"

  liftIO $ do
    -- deltaCounters is accumulated into based on the diff between last and current counter values.

    atomically $ do
      (_, oldTvars) <- extractValues $ counters ^. field @"previous"
      (_, newTvars) <- extractValues $ counters ^. field @"current"
      (_, totalTvars) <- extractValues $ counters ^. field @"total"
      (_, deltaTvars) <- extractValues deltas

      traverse_ (\(old, new, total, delta) -> do
          new' <- readTVar new
          old' <- readTVar old
          total' <- readTVar total
          writeTVar old new'
          writeTVar delta (new' - old')
          writeTVar total (total' + (new' - old'))
        ) (DL.zip4 oldTvars newTvars totalTvars deltaTvars)

    return deltas

resetStats :: MonadCounters m => m ()
resetStats = do
  counters <- Z.getCounters
  sequence_ $ setZeroes <$> [counters ^. field @"current", counters ^. field @"previous", counters ^. field @"total"]

setZeroes :: MonadIO m => CountersMap -> m ()
setZeroes cs = liftIO $ atomically $ do
    (_, tvars) <- extractValues cs
    traverse_ (`modifyTVar` const 0) tvars

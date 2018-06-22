module Counters.Handle
  ( MonadCounters
  , getCounters
  , getTotals
  , keys
  , logStats
  , newCounters
  , sendSummary
  , accumulateStats
  , handleErr
  , resetStats
  )
where

import Arbor.Logger
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM           (STM, atomically)
import Counters.Type
import Data.Foldable
import Data.Semigroup              ((<>))
import Data.String.Utils           (replace)
import Network.StatsD

import qualified Counters.Lens   as L
import qualified Data.List       as DL
import qualified Data.Map.Strict as M
import qualified Data.Text       as T

newCounters :: [CounterKey] -> IO Counters
newCounters (k:ks) = do
  (Counters m) <- newCounters ks
  v <- CounterValue <$> newTVarIO 0
  return $ Counters $ M.insert k v m
newCounters [] = return countersEmpty

keys :: Counters -> [CounterKey]
keys (Counters m) = M.keys m

handleErr :: Counters -> CounterKey -> IO ()
handleErr (Counters m) key = do
  let (CounterValue v) = m M.! key
  atomically $ modifyTVar v (+1)

extractValues :: Counters -> STM ([(CounterKey, Int)], [TVar Int])
extractValues (Counters m) = do
  let names = M.keys m
  let tvars = (^. L.var) <$> M.elems m
  nums <- sequence $ readTVar <$> tvars
  return (zip names nums, tvars)

logStats :: (MonadStats m, MonadCounters m, MonadLogger m) => m ()
logStats = do
  deltas <- accumulateStats
  (stats, _) <- liftIO $ atomically $ extractValues deltas
  let nonzero = DL.filter (\e -> snd e /= 0) stats

  unless (DL.null nonzero) $
    logInfo $ DL.intercalate ", " $ (\(n, i) -> n ++ ": " ++ show i) <$> nonzero

  traverse_ sendMetric (mkMetrics "counters" stats)

sendSummary :: (MonadIO m, MonadCounters m, MonadStats m) => String -> m ()
sendSummary fn = do
  totals <- getTotals
  (stats, _) <- liftIO $ atomically $ extractValues totals
  sendEvent (mkEvent stats fn)

metricName :: String -> T.Text
metricName n = T.pack $ replace " " "_" n ++ ".count"

-- create metric m, but tag with stat:[actual stat name]
mkMetrics :: String -> [(String, Int)] -> [Metric]
mkMetrics m =
  fmap (\(n, i) -> gauge (MetricName (metricName m)) id i & tags %~ ([tag "stat" (T.pack n)] ++))

mkEvent :: [(String, Int)] -> String -> Event
mkEvent stats fn = event (T.pack "Repfeed enriched") desc & tags %~ ([tag "repfeed" "enriched"] ++)
  where desc = T.intercalate "\n" $ T.pack <$> ("File processed: " <> fn) : info
        info = (\(n, i) -> n <> ": " <> show i) <$> stats

accumulateStats :: MonadCounters m => m Counters
accumulateStats = do
  oldCounters' <- getTotals
  newCounters' <- getCounters

  liftIO $ do
    -- deltaCounters is accumulated into based on the diff between last and current counter values.
    let deltaCounters = countersEmpty

    atomically $ do
      (_, oldTvars) <- extractValues oldCounters'
      (_, newTvars) <- extractValues newCounters'
      (_, deltaTvars) <- extractValues deltaCounters

      traverse_ (\(old, new, delta) -> do
          new' <- readTVar new
          old' <- readTVar old
          writeTVar old new'
          writeTVar delta (new' - old')
        ) (zip3 oldTvars newTvars deltaTvars)

    return deltaCounters

resetStats :: MonadCounters m => m ()
resetStats = do
  counters <- getCounters
  totals <- getTotals
  setZeroes counters
  setZeroes totals

setZeroes :: MonadIO m => Counters -> m ()
setZeroes cs = liftIO $ atomically $ do
    (_, tvars) <- extractValues cs
    traverse_ (`modifyTVar` const 0) tvars

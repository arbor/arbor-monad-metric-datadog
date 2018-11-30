{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Arbor.Monad.Metric.Datadog
  ( logStats
  ) where

import Arbor.Monad.Metric.Type   (CounterKey, MonadCounters)
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.STM         (atomically)
import Data.Foldable
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))

import qualified Arbor.Monad.Metric        as C
import qualified Arbor.Network.StatsD      as S
import qualified Arbor.Network.StatsD.Type as Z
import qualified Data.Text                 as T

logStats :: (S.MonadStats m, MonadCounters m) => m ()
logStats = do
  (currents , _)  <- C.currentStats >>= liftIO . atomically . C.extractValues

  -- case DL.filter (\e -> snd e /= 0) deltas of
  --   nonzero -> unless (DL.null nonzero) $
  --     logInfoN $ T.pack $ DL.intercalate ", " $ (\(n, i) -> n <> ": " <> show i) <$> nonzero

  traverse_ S.sendMetric $ currents & mkMetricsGaugeTagged "counters"
  traverse_ S.sendMetric $ currents & mkMetricsGaugeNonTagged

-- sendSummary :: (MonadIO m, MonadCounters m, S.MonadStats m) => String -> Z.Tag -> String -> m ()
-- sendSummary etitle etag fn = do
--   counters    <- Z.getCounters
--   (stats, _)  <- liftIO $ atomically $ C.extractValues $ counters ^. the @"previous"
--   S.sendEvent (mkEvent stats etitle etag fn)

metricName :: String -> T.Text
metricName n = T.replace " " "_" (T.pack n)

-- create metric m, but tag with stat:[actual stat name]
mkMetricsGaugeTagged :: String -> [(String, Int)] -> [Z.Metric]
mkMetricsGaugeTagged m =
  fmap (\(n, i) -> S.gauge (Z.MetricName (metricName m)) id i & the @"tags" %~ ([S.tag "stat" (T.pack n)] ++))

-- create metrics for each counter
mkMetricsGaugeNonTagged :: [(String, Int)] -> [S.Metric]
mkMetricsGaugeNonTagged =
  fmap (\(n, i) -> S.gauge (Z.MetricName (metricName n)) id i)

-- create metric m, but tag with stat:[actual stat name]
mkMetricsCounterTagged :: CounterKey -> [(String, Int)] -> [Z.Metric]
mkMetricsCounterTagged m =
  fmap (\(n, i) -> S.addCounter (Z.MetricName (metricName m)) id i & the @"tags" %~ ([S.tag "stat" (T.pack n)] ++))

-- create metrics for each counter
mkMetricsCounterNonTagged :: [(String, Int)] -> [S.Metric]
mkMetricsCounterNonTagged =
  fmap (\(n, i) -> S.addCounter (Z.MetricName (metricName n)) id i)

mkEvent :: [(String, Int)] -> String -> Z.Tag -> String -> Z.Event
mkEvent stats etitle etag fn = S.event (T.pack etitle) desc & the @"tags" %~ ([etag] ++)
  where desc = T.intercalate "\n" $ T.pack <$> ("File processed: " <> fn) : info
        info = (\(n, i) -> n <> ": " <> show i) <$> stats

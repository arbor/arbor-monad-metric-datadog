{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Arbor.Monad.Metric.Datadog
  ( logStats
  , mkEvent
  ) where

import Arbor.Monad.Metric.Type   (Counter (..), Gauge (..), MonadMetrics, getMetricMapTVar)
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Generics.Product.Any
import Data.Proxy
import Data.Semigroup            ((<>))

import qualified Arbor.Monad.Metric        as C
import qualified Arbor.Network.StatsD      as S
import qualified Arbor.Network.StatsD.Type as Z
import qualified Control.Concurrent.STM    as STM
import qualified Data.Map.Strict           as M
import qualified Data.Text                 as T

logStats :: (S.MonadStats m, MonadMetrics m) => m ()
logStats = do
  tCounterMap <- getMetricMapTVar
  (counters, _)  <- liftIO . STM.atomically $ STM.swapTVar tCounterMap M.empty >>= C.extractValues (Proxy @Counter)
  traverse_ S.sendMetric $ mkMetricsCounterTagged "counters" counters
  traverse_ S.sendMetric $ mkMetricsCounterNonTagged counters

  tGaugeMap <- getMetricMapTVar
  (gauge, _)  <- liftIO . STM.atomically $ STM.swapTVar tGaugeMap M.empty >>= C.extractValues (Proxy @Gauge)
  traverse_ S.sendMetric $ mkMetricsGaugeTagged "gauge" gauge
  traverse_ S.sendMetric $ mkMetricsGaugeNonTagged gauge

metricName :: String -> T.Text
metricName n = T.replace " " "_" (T.pack n)

-- create metric m, but tag with stat:[actual stat name]
mkMetricsGaugeTagged :: String -> [(Gauge, Double)] -> [Z.Metric]
mkMetricsGaugeTagged m =
  fmap (\(Gauge n, i) -> S.gauge (Z.MetricName (metricName m)) id i & the @"tags" %~ ([S.tag "stat" (T.pack n)] ++))

-- create metrics for each counter
mkMetricsGaugeNonTagged :: [(Gauge, Double)] -> [S.Metric]
mkMetricsGaugeNonTagged =
  fmap (\(Gauge n, i) -> S.gauge (Z.MetricName (metricName n)) id i)

-- create metric m, but tag with stat:[actual stat name]
mkMetricsCounterTagged :: String -> [(Counter, Int)] -> [Z.Metric]
mkMetricsCounterTagged m =
  fmap (\(Counter n, i) -> S.addCounter (Z.MetricName (metricName m)) id i & the @"tags" %~ ([S.tag "stat" (T.pack n)] ++))

-- create metrics for each counter
mkMetricsCounterNonTagged :: [(Counter, Int)] -> [S.Metric]
mkMetricsCounterNonTagged =
  fmap (\(Counter n, i) -> S.addCounter (Z.MetricName (metricName n)) id i)

mkEvent :: [(String, Int)] -> String -> Z.Tag -> String -> Z.Event
mkEvent stats etitle etag fn = S.event (T.pack etitle) desc & the @"tags" %~ ([etag] ++)
  where desc = T.intercalate "\n" $ T.pack <$> ("File processed: " <> fn) : info
        info = (\(n, i) -> n <> ": " <> show i) <$> stats

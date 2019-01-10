{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Arbor.Monad.Metric.Datadog
  ( logStats
  , mkEvent
  ) where

import Arbor.Monad.Metric.Datadog.Internal
import Arbor.Monad.Metric.Datadog.Internal.Show
import Arbor.Monad.Metric.Type                  (Counter, Gauge, GaugeCounter, MonadMetrics, getMetricMapTVar, Tag)
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Generics.Product.Any
import Data.Proxy
import Data.Semigroup                           ((<>))
import Data.Generics.Product.Fields


import qualified Arbor.Monad.Metric        as C
import qualified Arbor.Network.StatsD      as S
import qualified Arbor.Network.StatsD.Type as Z
import qualified Control.Concurrent.STM    as STM
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import qualified Data.Text                 as T

logStats :: (S.MonadStats m, MonadMetrics m) => m ()
logStats = do
  tCounterMap <- getMetricMapTVar
  (counters, _) <- liftIO . STM.atomically $ STM.swapTVar tCounterMap M.empty >>= C.extractValues (Proxy @Counter)
  traverse_ S.sendMetric $ mkMetrics S.addCounter counters

  tGaugeMap <- getMetricMapTVar
  (gauges, _)   <- liftIO . STM.atomically $ STM.swapTVar tGaugeMap M.empty >>= C.extractValues (Proxy @Gauge)
  traverse_ S.sendMetric $ mkMetrics S.gauge gauges

  tGaugeCounterMap <- getMetricMapTVar
  (gaugeCounters, _) <- liftIO . STM.atomically $ STM.swapTVar tGaugeCounterMap  M.empty >>= C.extractValues (Proxy @GaugeCounter)
  traverse_ S.sendMetric $ mkMetrics S.gauge gaugeCounters

metricName :: String -> T.Text
metricName n = T.replace " " "_" (T.pack n)

mkMetrics :: forall g v. ()
  => HasField "name" g g T.Text T.Text
  => HasField "tags" g g (S.Set Tag) (S.Set Tag)
  => (Z.MetricName -> (v -> v) -> v -> Z.Metric)
  -> [(g, v)]
  -> [S.Metric]
mkMetrics m = fmap (uncurry mk)
  where mk :: g -> v -> S.Metric
        mk g val = m (Z.MetricName (metricName (T.unpack name))) id val & the @"tags" .~ (toStat <$> tags)
          where name = g ^. the @"name"
                tags = g ^. the @"tags" & S.toList

mkEvent :: [(String, Int)] -> String -> Z.Tag -> String -> Z.Event
mkEvent stats etitle etag fn = S.event (T.pack etitle) desc & the @"tags" %~ ([etag] ++)
  where desc = T.intercalate "\n" $ T.pack <$> ("File processed: " <> fn) : info
        info = (\(n, i) -> n <> ": " <> showInt i) <$> stats

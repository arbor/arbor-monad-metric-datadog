{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Arbor.Monad.Metric.Datadog
  ( logStats
  , mkEvent
  ) where

import Arbor.Monad.Metric.Datadog.Internal
import Arbor.Monad.Metric.Datadog.Internal.Show
import Arbor.Monad.Metric.Type                  (Counter, Gauge, MonadMetrics, getMetricMapTVar)
import Control.Lens
import Control.Monad.IO.Class
import Data.Foldable
import Data.Generics.Product.Any
import Data.Proxy
import Data.Semigroup                           ((<>))

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
  (counters, _)  <- liftIO . STM.atomically $ STM.swapTVar tCounterMap M.empty >>= C.extractValues (Proxy @Counter)
  traverse_ S.sendMetric $ mkMetricsCounter counters

  tGaugeMap <- getMetricMapTVar
  (gauge, _)  <- liftIO . STM.atomically $ STM.swapTVar tGaugeMap M.empty >>= C.extractValues (Proxy @Gauge)
  traverse_ S.sendMetric $ mkMetricsGauge gauge

metricName :: String -> T.Text
metricName n = T.replace " " "_" (T.pack n)

-- create metrics for each counter
mkMetricsGauge :: [(Gauge, Double)] -> [S.Metric]
mkMetricsGauge = fmap (uncurry mkGauge)
  where mkGauge :: Gauge -> Double -> S.Metric
        mkGauge g v = S.gauge (Z.MetricName (metricName (T.unpack name))) id v & the @"tags" .~ (toStat <$> tags)
          where name = g ^. the @"name"
                tags = g ^. the @"tags" & S.toList

-- create metrics for each counter
mkMetricsCounter :: [(Counter, Int)] -> [S.Metric]
mkMetricsCounter = fmap (uncurry mkCounter)
  where mkCounter :: Counter -> Int -> S.Metric
        mkCounter g v = S.addCounter (Z.MetricName (metricName (T.unpack name))) id v & the @"tags" .~ (toStat <$> tags)
          where name = g ^. the @"name"
                tags = g ^. the @"tags" & S.toList

mkEvent :: [(String, Int)] -> String -> Z.Tag -> String -> Z.Event
mkEvent stats etitle etag fn = S.event (T.pack etitle) desc & the @"tags" %~ ([etag] ++)
  where desc = T.intercalate "\n" $ T.pack <$> ("File processed: " <> fn) : info
        info = (\(n, i) -> n <> ": " <> showInt i) <$> stats

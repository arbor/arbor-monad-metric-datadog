{-# LANGUAGE TypeFamilies #-}

module Arbor.Monad.Metric.Datadog.Internal where

import qualified Arbor.Monad.Metric.Type as M
import qualified Arbor.Network.StatsD    as S

class ToStat a where
  type StatType a
  toStat :: a -> StatType a

instance ToStat M.Tag where
  type StatType M.Tag = S.Tag
  toStat (M.Tag n v) = S.tag n v

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Arbor.Monad.Datadog.MetricApp
  ( runMetricApp
  ) where

import Arbor.Monad.Metric
import Control.Monad.Catch
import Control.Monad.Logger      (LoggingT, MonadLogger, runLoggingT)
import Control.Monad.Reader
import Data.Generics.Product.Any
import GHC.Generics
import System.Log.FastLogger

import qualified Arbor.Network.StatsD      as S
import qualified Arbor.Network.StatsD.Type as Z

data MiniConfig = MiniConfig
  { metrics     :: Metrics
  , statsClient :: Z.StatsClient
  } deriving (Generic)

instance MonadMetrics MetricApp where
  getMetrics = reader metrics

instance S.MonadStats MetricApp where
  getStatsClient = reader statsClient

newtype MetricApp a = MetricApp
  { unMetricApp :: ReaderT MiniConfig (LoggingT IO) a
  }
  deriving ( Functor
            , Applicative
            , Monad
            , MonadIO
            , MonadThrow
            , MonadCatch
            , MonadLogger
            , MonadReader MiniConfig)

runMetricApp :: MetricApp () -> IO ()
runMetricApp f = do
  let statsOpts = Z.DogStatsSettings "localhost" 5555
  statsClient <- S.createStatsClient statsOpts (Z.MetricName "MetricApp") []
  metrics <- newMetricsIO
  let config = MiniConfig metrics statsClient
  runLoggingT (runReaderT (unMetricApp f) config) $ \_ _ _ _ -> return ()

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Arbor.Monad.Datadog.MetricSpec
  ( spec
  ) where

import Control.Concurrent
import Control.Exception      (bracket)
import Control.Monad.IO.Class
import Data.Proxy
import Data.Semigroup         ((<>))

import qualified Arbor.Monad.Datadog.MetricApp as A
import qualified Arbor.Monad.Datadog.UdpServer as UDP
import qualified Arbor.Monad.Metric            as M
import qualified Arbor.Monad.Metric.Datadog    as M
import qualified Arbor.Monad.Metric.Type       as M
import qualified Control.Concurrent.STM        as STM
import qualified Data.ByteString.Char8         as BS
import qualified Data.Map.Strict               as MAP
import qualified Network.Socket                as S hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString     as S
import qualified System.IO                     as IO

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

handler :: STM.TVar [BS.ByteString] -> UDP.UdpHandler
handler tMsgs addr msg = do
  STM.atomically $ STM.modifyTVar tMsgs (msg:)
  putStrLn $ "From " ++ show addr ++ ": " ++ show msg

spec :: Spec
spec = describe "Arbor.Monad.MetricSpec" $ do
  it "Metrics library actually sends statsd messages over UDP" $ requireTest $ do
    tMessages <- liftIO $ STM.newTVarIO []
    sock <- liftIO $ UDP.createUdpServer "5555"
    threadId <- liftIO $ forkIO $ UDP.runUdpServer sock (handler tMessages)
    liftIO $ threadDelay 1000000
    let counterExpected = "MetricApp.counters:10|c|#stat:test.counter\nMetricApp.test.counter:10|c\n" :: BS.ByteString
    let gaugeExpected = "MetricApp.gauge:20.000000|g|#stat:test.gauge\nMetricApp.test.gauge:20.000000|g\n" :: BS.ByteString
    liftIO $ A.runMetricApp $ do
      M.metric (M.Counter "test.counter") 10
      M.metric (M.Gauge   "test.gauge"  ) 20
      M.logStats
    liftIO $ threadDelay 3000000
    liftIO $ killThread threadId
    messages <- liftIO $ STM.readTVarIO tMessages
    messages === [counterExpected <> gaugeExpected]

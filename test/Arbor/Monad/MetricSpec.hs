module Arbor.Monad.MetricSpec
  ( spec
  ) where

import Control.Exception      (bracket)
import Control.Monad.IO.Class

import qualified Arbor.Monad.UdpServer     as UDP
import qualified Data.ByteString.Char8     as BS
import qualified Network.Socket            as S hiding (recv, recvFrom, send, sendTo)
import qualified Network.Socket.ByteString as S

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

plainHandler :: UDP.UdpHandler
plainHandler addr msg = putStrLn $ "From " ++ show addr ++ ": " ++ show msg

spec :: Spec
spec = describe "Arbor.Monad.MetricSpec" $ do
  it "" $ do
    -- liftIO $ UDP.runUdpServer "5555" plainHandler
    True `shouldBe` True

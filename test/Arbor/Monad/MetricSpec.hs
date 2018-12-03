module Arbor.Monad.MetricSpec
  ( spec
  ) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "Arbor.Monad.MetricSpec" $ do
  it "" $ do
    True `shouldBe` True

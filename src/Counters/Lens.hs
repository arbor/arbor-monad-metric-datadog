{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Counters.Lens where

import Control.Lens
import Counters.Type

makeFields ''CounterValue
makeFields ''Counters

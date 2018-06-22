{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Arbor.Monad.Counter.Lens where

import Arbor.Monad.Counter.Type
import Control.Lens

makeFields ''CounterValue
makeFields ''Counters

{-# LANGUAGE DeriveGeneric #-}
module FMU where

import qualified HFMU as HFMU
import Vars
import GHC.Generics

foreign export ccall setup :: IO ()
setup :: IO ()
setup = HFMU.setup doStep inputs outputs

data Inputs = Inputs {pValve :: Port} deriving (Generic)
inputs :: Inputs
inputs = Inputs {pValve = Port {vRef = 1, causality = Input, type' = Boolean, val = BooleanVal False}}

data Outputs = Outputs {pLevel :: Port} deriving (Generic)
outputs :: Outputs
outputs = Outputs {pLevel = Port {vRef = 2, causality = Output, type' = Real, val = RealVal 1.5}}

doStep :: Int
doStep = 1

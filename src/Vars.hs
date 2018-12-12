-- To allow name clashes in record fields
{-# LANGUAGE DuplicateRecordFields #-}

module Vars where

import Data.HashMap.Strict
import HSFMIInterface
import Foreign.C.Types (CInt, CBool, CDouble, CSize)

data SVType = Real | Integer | String | Boolean
data SVTypeVal = RealVal Double | IntegerVal Int | StringVal String | BooleanVal Bool
data SVCausality = Input | Output | Parameter
data SVVariability = Fixed | Continuous | Discrete
data SVInitial = Exact | Calculated
data Port = Port {vRef :: Int,
                  causality :: SVCausality,
                  variability :: SVVariability,
                  initial :: Maybe SVInitial,
                  type' :: SVType,
                  val :: SVTypeVal}

type SV = HashMap String Port
type Inputs = SV
type Outputs = SV
type Parameters = SV
type DoStepType = Inputs -> Outputs -> Parameters -> (Status, Outputs)
data Setup = Setup {ins :: Inputs,
                   outs :: Outputs,
                   pars :: Parameters,
                   doStepFunc :: DoStepType,
                   period :: Double}


data FMIComponent = FMIComponent {inputs :: Inputs,
                                  outputs :: Outputs,
                                  parameters :: Parameters,
                                  doStep :: DoStepType,
                                  stopTime :: Maybe CDouble,
                                  state :: FMUState,
                                  period_ :: Double}

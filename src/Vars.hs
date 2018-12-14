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

type SVs = HashMap String Port
type DoStepType = SVs -> (Status, SVs)
data Setup = Setup {variables :: SVs,
                   doStepFunc :: DoStepType,
                   period :: Double}


data FMIComponent = FMIComponent {vars :: SVs,
                                  doStep :: DoStepType,
                                  stopTime :: Maybe CDouble,
                                  state :: FMUState,
                                  period_ :: Double,
                                  remTime :: Double,
                                  endTime :: Double}

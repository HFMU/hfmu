-- To allow name clashes in record fields
{-# LANGUAGE DuplicateRecordFields #-}

module Vars where

import Data.HashMap.Strict
import HSFMIInterface
import Foreign.C.Types (CInt, CBool, CDouble, CSize)

data SVType = Real | Integer | String | Boolean deriving (Show)
data SVTypeVal = RealVal Double | IntegerVal Int | StringVal String | BooleanVal Bool deriving (Show)
data SVCausality = Input | Output | Parameter deriving (Show)
data SVVariability = Fixed | Continuous | Discrete deriving (Show)
data SVInitial = Exact | Calculated deriving (Show)
data Port = Port {vRef :: Int,
                  causality :: SVCausality,
                  variability :: SVVariability,
                  initial :: Maybe SVInitial,
                  type' :: SVType,
                  val :: SVTypeVal} deriving (Show)

type SVs = HashMap String Port
type DoStepType = SVs -> (Status, SVs)
data Setup = Setup {variables :: SVs,
                   doStepFunc :: DoStepType,
                   period :: Double}


data FMIComponent = FMIComponent {vars :: SVs,
                                  doStep :: DoStepType,
                                  endTime :: Maybe Double,
                                  state :: FMUState,
                                  period_ :: Double,
                                  remTime :: Double} 

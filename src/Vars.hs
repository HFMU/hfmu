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

data Setup a = Setup {variables :: SVs,
                   doStepFunc :: DoStepType a,
                   period :: Double,
                   userState :: UserState a}



newtype UserState x = UserState x

data DoStepResult x = DoStepResult {dsrStatus :: Status, dsrSvs :: SVs, dsrState :: UserState x }

type DoStepType a = SVs -> UserState a -> DoStepResult a

data FMIComponent x = FMIComponent {vars :: SVs,
                                  doStep :: DoStepType x,
                                  endTime :: Maybe Double,
                                  state :: FMUState,
                                  period_ :: Double,
                                  remTime :: Double,
                                  userState_ :: UserState x }

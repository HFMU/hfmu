-- To allow name clashes in record fields
{-# LANGUAGE DuplicateRecordFields #-}

module Vars where

import Data.HashMap.Strict

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
module SVs where

import Vars
import Data.HashMap.Strict as HM

inputs :: HashMap String Port
inputs =
  insert "level" Port {vRef = 1, causality = Input, variability = Continuous, initial=Nothing, type' = Real, val = RealVal 1.5}
  empty

outputs :: HashMap String Port
outputs =
  insert "valve" Port {vRef = 3, causality = Output, variability = Discrete, initial=Just Calculated, type' = Boolean, val = BooleanVal False}
  empty

parameters :: HashMap String Port
parameters =
  insert "minLevel" Port {vRef = 0, causality = Parameter, variability = Fixed, initial=Just Exact, type' = Real, val = RealVal 1} $
  insert "maxLevel" Port {vRef = 2, causality = Parameter, variability = Fixed, initial=Just Exact, type' = Real, val = RealVal 2}
  empty

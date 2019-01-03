module SVs where

import Data.HFMU.Types as T
import Data.HashMap.Strict as HM

inputs :: HashMap String SV
inputs =
  insert "level" SV {svRef = 1, svCausality = Input, svVariability = Continuous, svInitial=Nothing, svType = Real, svVal = RealVal 1.5}
  empty

outputs :: HashMap String SV
outputs =
  insert "valve" SV {svRef = 3, svCausality = Output, svVariability = Discrete, svInitial=Just Calculated, svType = Boolean, svVal = BooleanVal False}
  empty

parameters :: HashMap String SV
parameters =
  insert "minLevel" SV {svRef = 0, svCausality = Parameter, svVariability = Fixed, svInitial=Just Exact, svType = Real, svVal = RealVal 1} $
  insert "maxLevel" SV {svRef = 2, svCausality = Parameter, svVariability = Fixed, svInitial=Just Exact, svType = Real, svVal = RealVal 2}
  empty

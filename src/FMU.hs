{-# LANGUAGE ScopedTypeVariables #-}
module FMU where

import Vars
import HSFMIInterface
import Data.Tuple.Sequence (sequenceT)
import HMOperations
import qualified SVs
import qualified HFMU as H
import qualified Data.HashMap.Strict as HM

foreign export ccall setup :: IO ()
setup :: IO ()
--setup = HFMU.setup doStep inputs outputs
setup = H.setup Setup {variables = HM.union SVs.inputs (HM.union SVs.outputs SVs.parameters), doStepFunc = FMU.doStep}

doStep :: SVs -> (Status, SVs)
doStep svs =
  let valve :: Maybe SVTypeVal =
        do
          mM <- retrieveMinAndMaxLevel svs
          level <- getPortVal svs "level"
          calcValve level mM $ getPortVal svs "valve"
  in
    case valve of
      Just v -> (OK, adjustPortVal svs "valve" v)
      Nothing -> (Fatal, svs)

retrieveMinAndMaxLevel :: SVs -> Maybe (SVTypeVal, SVTypeVal)
retrieveMinAndMaxLevel par =
    sequenceT (getPortVal par "minLevel",  getPortVal par "maxLevel")

calcValve :: SVTypeVal -> (SVTypeVal,SVTypeVal) -> Maybe SVTypeVal -> Maybe SVTypeVal
calcValve level@(RealVal lvl) minMaxLevel@(RealVal min, RealVal max) prevValve
  | lvl <= min = Just $ BooleanVal False
  | lvl >= max = Just $ BooleanVal True
  | otherwise = prevValve
calcValve _ _ _ = Nothing

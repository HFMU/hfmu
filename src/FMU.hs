{-# LANGUAGE ScopedTypeVariables #-}
module FMU where

import Vars
import HSFMIInterface
import Data.Tuple.Sequence (sequenceT)
import HMOperations
import qualified SVs
import qualified HFMU as H

foreign export ccall setup :: IO ()
setup :: IO ()
--setup = HFMU.setup doStep inputs outputs
setup = H.setup Setup {ins = SVs.inputs, outs = SVs.outputs, pars = SVs.parameters, doStepFunc = FMU.doStep}

doStep :: Inputs -> Outputs -> Parameters -> (Status, Outputs)
doStep inp out par =
  let valve :: Maybe SVTypeVal =
        do
          mM <- retrieveMinAndMaxLevel par
          level <- getPortVal inp "level"
          calcValve level mM $ getPortVal out "valve"
  in
    case valve of
      Just v -> (OK, adjustPortVal out "valve" v)
      Nothing -> (Fatal, out)

retrieveMinAndMaxLevel :: Parameters -> Maybe (SVTypeVal, SVTypeVal)
retrieveMinAndMaxLevel par =
    sequenceT (getPortVal par "minLevel",  getPortVal par "maxLevel")

calcValve :: SVTypeVal -> (SVTypeVal,SVTypeVal) -> Maybe SVTypeVal -> Maybe SVTypeVal
calcValve (RealVal lvl) (RealVal min, RealVal max) prevValve
  | lvl <= min = Just $ BooleanVal False
  | lvl >= max = Just $ BooleanVal True
  | otherwise = prevValve
calcValve _ _ _ = Nothing

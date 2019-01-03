{-# LANGUAGE ScopedTypeVariables #-}
module FMU where

import qualified Vars as V
import HSFMIInterface
import Data.Tuple.Sequence (sequenceT)
import HMOperations
import qualified SVs
import qualified HFMU as H
import qualified Data.HashMap.Strict as HM
import Debug.Trace

foreign export ccall setup :: IO ()
setup :: IO ()
--setup = HFMU.setup doStep inputs outputs
setup = H.setup V.Setup {V.variables = HM.union SVs.inputs (HM.union SVs.outputs SVs.parameters),
                       V.doStepFunc = FMU.doStep,
                       V.period=0.1,
                       V.userState = V.UserState mFMUState}

newtype MFMUState = MFMUState {mmax :: Int}

mFMUState :: MFMUState
mFMUState = MFMUState {mmax = 1}

doStep :: V.DoStepType MFMUState
doStep svs us@(V.UserState (MFMUState {mmax=m}))  =
  let valve :: Maybe V.SVTypeVal =
        do
          trace (show m) (Just ())
          mM <- retrieveMinAndMaxLevel svs
          level <- getPortVal svs "level"
          calcValve level mM $ getPortVal svs "valve"
  in
    case valve of
      Just v -> V.DoStepResult {V.dsrStatus = OK,
                               V.dsrSvs = adjustPortVal svs "valve" v,
                               V.dsrState = V.UserState (MFMUState {mmax = 2})}
      Nothing -> V.DoStepResult {V.dsrStatus = Fatal,
                                V.dsrSvs = svs,
                                V.dsrState = us}

retrieveMinAndMaxLevel :: V.SVs -> Maybe (V.SVTypeVal, V.SVTypeVal)
retrieveMinAndMaxLevel par =
    sequenceT (getPortVal par "minLevel",  getPortVal par "maxLevel")

calcValve :: V.SVTypeVal -> (V.SVTypeVal,V.SVTypeVal) -> Maybe V.SVTypeVal -> Maybe V.SVTypeVal
calcValve level@(V.RealVal lvl) minMaxLevel@(V.RealVal min, V.RealVal max) prevValve
  | lvl <= min = Just $ V.BooleanVal False
  | lvl >= max = Just $ V.BooleanVal True
  | otherwise = prevValve
calcValve _ _ _ = Nothing

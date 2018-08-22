{-# LANGUAGE DuplicateRecordFields #-}

module FMU where

import HFMU
import UPorts
import System.Environment

foreign export ccall initDoStepFunction :: IO ()
initDoStepFunction :: IO ()
initDoStepFunction = HFMU.storeDoStepFunction doStep

doStep :: DoStepData -> IO (DoStepReturn)
doStep x =
  let ctxt = context (x :: DoStepData)
      ctxt' = UPorts.setOutputValve ctxt (not $ UPorts.getOutputValve ctxt)
  in
    pure DoStepReturn {context = ctxt', status=OK}

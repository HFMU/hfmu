{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HFMU where

import qualified HSFMIInterface as FMII
import qualified FMIFunctions as FF
import Foreign.C.String
import Foreign.C.Types
import Foreign (Ptr, nullPtr, FunPtr, StablePtr)
import Foreign
import Data.IORef
import Control.Monad
import qualified Vars as V
import System.IO.Unsafe
import qualified Data.HashMap.Strict as HM


foreign import ccall "dynamic" mkFunPtrLogger :: FMII.CallbackLogger -> FMII.CompEnvT -> CString -> FMII.FMIStatus -> CString -> CString -> IO ()

-- First function call to the FMU. Should move Setup to FMIComponent
--fmi2Instantiate :: instanceName -> fmuType -> fmuGUID -> fmuResourceLocation -> functions -> visible -> loggingOn
foreign export ccall fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr FMII.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef V.FMIComponent))
fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr FMII.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef V.FMIComponent))
fmi2Instantiate _ _ _ _ ptrCbFuncs _ _ = do
  (cbFuncs :: FMII.CallbackFunctions) <- peek ptrCbFuncs
  -- logger fmi2ComponentEnv instanceName::String Status::fmi2Status Category::String Message::String
  instanceName :: CString <- newCString "instanceName";
  category :: CString <- newCString "logError";
  msg :: CString <- newCString "HS-Message: Error";
  -- Invoke the function pointer
  (mkFunPtrLogger . FMII.logger $ cbFuncs) nullPtr instanceName (CInt 3) category msg
  -- Move Setup to FMIComponent
  state <- getSetup setupVar
  case state of
    Nothing -> newStablePtr =<< newIORef V.FMIComponent {} -- ERROR SHOULD BE THROWN
    Just (s :: V.Setup) -> do
      ioref <- newIORef  V.FMIComponent {inputs = V.ins s, outputs = V.outs s, parameters = V.pars s, doStep = V.doStepFunc s, stopTime = Nothing, state = FMII.SlaveUnderEvaluationA,
                                        period_ = V.period s}
      newStablePtr ioref

-- StablePtr IORef FMIComponent -> CBool -> CDouble -> CDouble -> CBool -> CDouble -> IO (Status)
-- fmi2Status fmi2SetupExperiment(fmi2Component c,
-- fmi2Boolean toleranceDefined,
-- fmi2Real tolerance,
-- fmi2Real startTime,
-- fmi2Boolean stopTimeDefined,
-- fmi2Real stopTime);
-- Everything besides stopTime is ignored and stopTime is expected to be defined.
foreign export ccall fmi2SetupExperiment :: FF.FMISetupExperimentType
fmi2SetupExperiment :: FF.FMISetupExperimentType
fmi2SetupExperiment comp _ _ _ _ stopTime = do
  state' <- getState comp
  writeState comp $ state' {V.stopTime = Just stopTime}
  (pure . FMII.statusToCInt) FMII.OK

foreign export ccall fmi2EnterInitializationMode :: FF.FMIEnterInitializationModeType
fmi2EnterInitializationMode :: FF.FMIEnterInitializationModeType
fmi2EnterInitializationMode comp = do
  state <- getState comp
  case V.stopTime state of
    Just _ -> (writeState comp $ state {V.state = FMII.SlaveUnderEvaluationB}) >> pure (FMII.statusToCInt FMII.OK)
    Nothing -> pure $ FMII.statusToCInt FMII.Fatal

foreign export ccall fmi2ExitInitializationMode :: FF.FMIExitInitializationModeType
fmi2ExitInitializationMode :: FF.FMIExitInitializationModeType
fmi2ExitInitializationMode comp = do
  state <- getState comp
  writeState comp $ updateState state FMII.SlaveInitialized
  pure . FMII.statusToCInt $ FMII.OK

foreign export ccall fmi2SetInteger :: FF.FMISetIntegerType
fmi2SetInteger :: FF.FMISetIntegerType
fmi2SetInteger comp varRefs size varVals = do
  varRefs' <- peekArray (fromIntegral size) varRefs
  varVals' <- peekArray (fromIntegral size) varVals
  let refVal = zip varRefs' varVals' in
    case head refVal of
      (1,1) -> pure $ FMII.statusToCInt FMII.OK
      otherwise ->  pure $ FMII.statusToCInt FMII.Fatal
      
foreign export ccall fmi2SetReal :: FF.FMISetRealType
fmi2SetReal :: FF.FMISetRealType
fmi2SetReal comp varRefs size varVals = do
  state <- getState comp
  varRefs' :: [CInt] <- peekArray (fromIntegral size) varRefs
  varVals' :: [CDouble] <- peekArray (fromIntegral size) varVals
  let varVals'' = map (\x -> (V.RealVal . realToFrac) x) varVals'
      refVals :: [(CInt, V.SVTypeVal)] = zip varRefs' varVals''
      inputs :: V.Inputs = V.inputs state
      inputs' = foldr updateVal inputs refVals 
    in
    do
      writeState comp $ updateInputs state inputs'
      pure . FMII.statusToCInt $ FMII.OK
      
-- fmi2Component comp, fmi2Real currentCommunicationPoint, fmi2Real communicationStepSize, fmi2Real noSetFMUStatePriorToCurrentPoint
foreign export ccall fmi2DoStep :: FF.FMIDoStepType
fmi2DoStep :: FF.FMIDoStepType
fmi2DoStep = undefined

updateVal :: (CInt, V.SVTypeVal) -> V.SV -> V.SV
updateVal (valRef, valVal) hm = HM.map updateValWithValRef hm
  where
    updateValWithValRef :: V.Port -> V.Port
    updateValWithValRef x = if V.vRef x == fromIntegral valRef then x {V.val = valVal} else x
  

-- Retrieves state
getState :: StablePtr (IORef a) -> IO a
getState = deRefStablePtr >=> readIORef

-- Stores state
writeState :: StablePtr (IORef a) -> a -> IO ()
writeState ptr state = do
  ioref <- deRefStablePtr ptr
  writeIORef ioref state

-- Invoked from FMU
setup :: V.Setup -> IO ()
setup = storeSetup

-- Functions related to the user defined doStep function
{-# NOINLINE setupVar #-}
setupVar :: IORef (Maybe (V.Setup))
setupVar = unsafePerformIO $ newIORef Nothing

storeSetup :: V.Setup-> IO ()
storeSetup = writeIORef setupVar . Just

getSetup :: IORef (Maybe (V.Setup)) -> IO (Maybe V.Setup)
getSetup = readIORef

updateState :: V.FMIComponent -> FMII.FMUState -> V.FMIComponent
updateState c s = c {V.state = s}

updateInputs :: V.FMIComponent -> V.Inputs -> V.FMIComponent
updateInputs c i = c {V.inputs = i}
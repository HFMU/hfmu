{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module HFMU where

import Foreign.C.String
import Foreign.C.Types
import Foreign (Ptr, nullPtr, FunPtr, StablePtr)
import Foreign
import Data.IORef
import Control.Monad
import System.IO.Unsafe
import qualified Data.HashMap.Strict as HM
import Data.List
import Debug.Trace
import qualified Data.HFMU.Types as T
import qualified Data.HFMU.Internal.FMITypes as FMIT
import qualified Data.HFMU.Internal.FMIFunctionTypes as FMIFT


foreign import ccall "dynamic" mkFunPtrLogger :: FMII.CallbackLogger -> FMII.CompEnvT -> CString -> FMII.FMIStatus -> CString -> CString -> IO ()


-- ==============================================================
-- =================== STATE CHANGE FUNCTIONS ===================
-- ==============================================================

-- First function call to the FMU. Should move Setup to FMIComponent
--fmi2Instantiate :: instanceName -> fmuType -> fmuGUID -> fmuResourceLocation -> functions -> visible -> loggingOn
foreign export ccall fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr FMII.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef (V.FMIComponent a)))
fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr FMII.CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef (V.FMIComponent a)))
fmi2Instantiate _ _ _ _ ptrCbFuncs _ _ = do
  (cbFuncs :: FMII.CallbackFunctions) <- peek ptrCbFuncs
  -- logger fmi2ComponentEnv instanceName::String Status::fmi2Status Category::String Message::String
  instanceName :: CString <- newCString "instanceName";
  category :: CString <- newCString "logError";
  msg :: CString <- newCString "HS-Message: Error";
  -- Invoke the function pointer
  (mkFunPtrLogger . FMII.logger $ cbFuncs) nullPtr instanceName (CInt 3) category msg
  -- Move Setup to FMIComponent
  state <- getSetupImpure setupVar
  case state of
    Nothing -> putStrLn "NothingCase" >> (newStablePtr =<< newIORef V.FMIComponent {}) -- ERROR SHOULD BE THROWN
    Just s -> do
      putStrLn "JustCase"
      ioref <- newIORef  V.FMIComponent {vars = V.variables s, doStep = V.doStepFunc s,
                                         endTime = Nothing, state = FMII.Instantiated, period_ = V.period s, remTime = V.period s, userState_ = V.userState s}
      newStablePtr ioref

-- StablePtr IORef FMIComponent -> CBool -> CDouble -> CDouble -> CBool -> CDouble -> IO (Status)
-- fmi2Status fmi2SetupExperiment(fmi2Component c,
-- fmi2Boolean toleranceDefined,
-- fmi2Real tolerance,
-- fmi2Real startTime,
-- fmi2Boolean stopTimeDefined,
-- fmi2Real stopTime);
-- Everything besides stopTime is ignored and stopTime is expected to be defined.
foreign export ccall fmi2SetupExperiment :: FF.FMISetupExperimentType a
fmi2SetupExperiment :: FF.FMISetupExperimentType a
fmi2SetupExperiment comp _ _ _ _ stopTime = do
  state' <- getStateImpure comp
  writeStateImpure comp $ state' {V.endTime = Just $ realToFrac stopTime}
  (pure . FMII.statusToCInt) FMII.OK

foreign export ccall fmi2EnterInitializationMode :: FF.FMIEnterInitializationModeType a
fmi2EnterInitializationMode :: FF.FMIEnterInitializationModeType a
fmi2EnterInitializationMode comp = do
  state <- getStateImpure comp
  case V.endTime state of
    Just _ -> (writeStateImpure comp $ state {V.state = FMII.InitializationMode}) >> pure (FMII.statusToCInt FMII.OK)
    Nothing -> pure $ FMII.statusToCInt FMII.Fatal

foreign export ccall fmi2ExitInitializationMode :: FF.FMIExitInitializationModeType a
fmi2ExitInitializationMode :: FF.FMIExitInitializationModeType a
fmi2ExitInitializationMode comp = do
  state <- getStateImpure comp
  writeStateImpure comp $ updateState state FMII.SlaveInitialized
  pure . FMII.statusToCInt $ FMII.OK

foreign export ccall fmi2Terminate :: FF.FMITerminateType a
fmi2Terminate :: FF.FMITerminateType a
fmi2Terminate comp = do
  state <- getStateImpure comp
  updStateCalcStatusImpure comp (state {V.state = FMII.Terminated},FMII.OK)  

foreign export ccall fmi2FreeInstance :: FF.FMIFreeInstanceType a
fmi2FreeInstance :: FF.FMIFreeInstanceType a
fmi2FreeInstance comp = do
  freeStablePtr comp
  pure . FMII.statusToCInt $ FMII.OK 

-- ==============================================================
-- =================== SET FUNCTIONS ============================
-- ==============================================================

foreign export ccall fmi2SetInteger :: FF.FMISetIntegerType a
fmi2SetInteger :: FF.FMISetIntegerType a
fmi2SetInteger comp varRefs size varVals =
  setLogicImpure comp varRefs size varVals (V.IntegerVal . fromIntegral)

foreign export ccall fmi2SetReal :: FF.FMISetRealType a
fmi2SetReal :: FF.FMISetRealType a
fmi2SetReal comp varRefs size varVals =
  setLogicImpure comp varRefs size varVals (V.RealVal . realToFrac)

setLogicImpure :: Storable b => FF.FMUStateType a -> Ptr CUInt -> CSize -> Ptr b -> (b -> V.SVTypeVal) -> IO CInt
setLogicImpure comp varRefs size varVals varValConvF =
  do
    state <- getStateImpure comp
    varRefs' :: [CUInt] <- peekArray (fromIntegral size) varRefs
    varVals' <- peekArray (fromIntegral size) varVals
    let
      varVals'' :: [V.SVTypeVal] = map varValConvF varVals'
      varRefs'' = map fromIntegral varRefs'
      stateStatus = setLogic state varRefs'' varVals''
      in
      updStateCalcStatusImpure comp stateStatus


setLogic :: V.FMIComponent a -> [Int] -> [V.SVTypeVal] -> (V.FMIComponent a, FMII.Status)
setLogic state@V.FMIComponent {vars = vs} vRefs vVals =
  let
    refVals = zip vRefs vVals
    ys' = foldr updateVal vs refVals in
    (state {V.vars = ys'}, FMII.OK)
  where
    updateVal :: (Int, V.SVTypeVal) -> V.SVs -> V.SVs
    updateVal (valRef, valVal) hm = HM.map updateValWithValRef hm
      where
        updateValWithValRef :: V.Port -> V.Port
        updateValWithValRef x = if V.vRef x == valRef then x {V.val = valVal} else x


-- ==============================================================
-- =================== GET FUNCTIONS ============================
-- ==============================================================

foreign export ccall fmi2GetBoolean :: FF.FMIGetBooleanType a
fmi2GetBoolean :: FF.FMIGetBooleanType a
fmi2GetBoolean comp varRefs size varVals =
  let valToCBool (x :: V.SVTypeVal) =
        case x of
          V.BooleanVal b -> Just . fromBool $ b
          _ -> Nothing in
    getLogicImpure comp varRefs size varVals valToCBool

foreign export ccall fmi2GetReal :: FF.FMIGetRealType a
fmi2GetReal :: FF.FMIGetRealType a
fmi2GetReal comp varRefs size varVals =
  let valToCDouble (x :: V.SVTypeVal) =
        case x of
          V.RealVal b -> Just . realToFrac $ b
          _ -> Nothing in
    getLogicImpure comp varRefs size varVals valToCDouble



getLogicImpure :: Storable a => FF.FMUStateType b -> Ptr CUInt -> CSize -> Ptr a -> (V.SVTypeVal -> Maybe a) -> IO CInt
getLogicImpure comp varRefs size varVals toVarValF = do
  state <- getStateImpure comp
  varRefs' :: [CUInt] <- peekArray (fromIntegral size) varRefs
  let (values,status) = getLogic state (map fromIntegral varRefs') toVarValF in
    case values of
      Nothing -> pure . FMII.statusToCInt $ status
      Just values' -> pokeArray varVals values' >> (pure . FMII.statusToCInt) status

getLogic :: V.FMIComponent b -> [Int] -> (V.SVTypeVal -> Maybe a) -> (Maybe [a], FMII.Status )
getLogic state vRefs valConvF =
  let outputPorts = HM.elems . V.vars $ state
      values = getVsFromVRefs outputPorts vRefs valConvF in
    case values of
      Nothing -> (Nothing, FMII.Fatal)
      x -> (x, FMII.OK)

getVsFromVRefs :: [V.Port] -> [Int] -> (V.SVTypeVal -> Maybe a) -> Maybe [a]
getVsFromVRefs ps refs f = traverse (\x -> f =<< findValWithRef x ps) refs
  where
    findValWithRef :: Int -> [V.Port] -> Maybe V.SVTypeVal
    findValWithRef ref ps' = V.val <$> find (\(x :: V.Port) -> V.vRef x == ref) ps'


-- ==============================================================
-- =================== DoStep FUNCTION ==========================
-- ==============================================================


-- fmi2Component comp,
-- fmi2Real currentCommunicationPoint,
-- fmi2Real communicationStepSize,
-- fmi2Real noSetFMUStatePriorToCurrentPoint
-- 
foreign export ccall fmi2DoStep :: FF.FMIDoStepType a
fmi2DoStep :: FF.FMIDoStepType a
fmi2DoStep comp ccp css ns =
  do
    state <- getStateImpure comp
    let
      css' = realToFrac css
      ccp' = realToFrac ccp
      (state', status) = doStepLogic state ccp' css' (toBool ns)
      in
      case status of
        FMII.OK -> writeStateImpure comp state' >> (pure . FMII.statusToCInt) FMII.OK
        _ -> (pure . FMII.statusToCInt) status


doStepLogic :: V.FMIComponent a -> Double -> Double -> Bool -> (V.FMIComponent a, FMII.Status)
doStepLogic state ccp css ns =
  case V.endTime state of
    Nothing -> (state, FMII.Fatal)
    Just endTime ->
      if ccp + css > endTime
      then (state, FMII.Fatal)
      else
        let RDS {remTime = rt, vars = vs, status = st, rdsState = rdsS } = calcDoStep (V.doStep state) (V.vars state) (V.period_ state) (V.remTime state) endTime css (V.userState_ state)
        in (state {V.remTime = rt, V.vars = vs, V.userState_ = rdsS}, st)

data RDS a = RDS {remTime :: Double, vars :: V.SVs, status :: FMII.Status, rdsState :: V.UserState a}

calcDoStep :: V.DoStepType a -> V.SVs -> Double -> Double -> Double -> Double -> V.UserState a -> RDS a
calcDoStep doStepF svs peri remTime endTime css us =
  if css < remTime
  then RDS {remTime = remTime - css, vars = svs, status = FMII.OK, rdsState = us}
  else execDoStep
  where
    execDoStep =
      let
        V.DoStepResult {V.dsrStatus = st, dsrSvs = svs', dsrState = sta} = doStepF svs us 
        css' = css - remTime -- ccs' is the remaining communication step size
      in
        if st == FMII.OK
        then calcDoStep doStepF svs' peri peri endTime css' sta
        else RDS {remTime = remTime, vars = svs', status = st, rdsState = sta}



-- ==============================================================
-- =================== SETUP FUNCTIONs ==========================
-- ==============================================================

-- Invoked from FMU
setup :: V.Setup a -> IO ()
setup = storeSetupImpure

-- Functions related to the user defined doStep function
{-# NOINLINE setupVar #-}
setupVar :: IORef (Maybe (V.Setup a))
setupVar = unsafePerformIO $ newIORef Nothing

storeSetupImpure :: V.Setup a -> IO ()
storeSetupImpure = writeIORef setupVar . Just

getSetupImpure :: IORef (Maybe (V.Setup a)) -> IO (Maybe (V.Setup a))
getSetupImpure = readIORef


-- ==============================================================
-- =================== STATE FUNCTIONs ==========================
-- ==============================================================

-- Retrieves state
getStateImpure :: StablePtr (IORef a) -> IO a
getStateImpure = deRefStablePtr >=> readIORef

-- Stores state
writeStateImpure :: StablePtr (IORef a) -> a -> IO ()
writeStateImpure ptr state = do
  ioref <- deRefStablePtr ptr
  writeIORef ioref state



updStateCalcStatusImpure :: FF.FMUStateType a -> (V.FMIComponent a, FMII.Status) -> IO CInt
updStateCalcStatusImpure comp (state,status) = writeStateImpure comp state >> (pure . FMII.statusToCInt) status

-- ==============================================================
-- =================== UTIL FUNCTIONs ===========================
-- ==============================================================

updateState :: V.FMIComponent a -> FMII.FMUState -> V.FMIComponent a
updateState c s = c {V.state = s}

updateInputs :: V.FMIComponent a -> V.SVs -> V.FMIComponent a
updateInputs c i = c {V.vars = i}


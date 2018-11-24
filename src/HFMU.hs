{-# LANGUAGE ScopedTypeVariables #-}

module HFMU where

import HSFMIInterface
import Foreign.C.String
import Foreign.C.Types
import Foreign (Ptr, nullPtr, FunPtr, StablePtr)
import Foreign
import Data.IORef
import Control.Monad


foreign import ccall "dynamic" mkFunPtrLogger :: CallbackLogger -> CompEnvT -> CString -> FMIStatus -> CString -> CString -> IO ()

--fmi2Instantiate :: instanceName -> fmuType -> fmuGUID -> fmuResourceLocation -> functions -> visible -> loggingOn
foreign export ccall fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef FMIComponent))
fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr CallbackFunctions -> CBool -> CBool -> IO (StablePtr (IORef FMIComponent))
fmi2Instantiate _ _ _ _ ptrCbFuncs _ _ = do
  (cbFuncs :: CallbackFunctions) <- peek ptrCbFuncs
  -- logger fmi2ComponentEnv instanceName::String Status::fmi2Status Category::String Message::String
  instanceName :: CString <- newCString "instanceName";
  category :: CString <- newCString "logError";
  msg :: CString <- newCString "HS-Message: Error";
  (mkFunPtrLogger . logger $ cbFuncs) nullPtr instanceName (CInt 3) category msg
  ioref <- newIORef  FMIComponent {testInput = 0, testOutput = 0, stopTime = Nothing, state = ModelUnderEvaluation}
  newStablePtr ioref

-- StablePtr IORef FMIComponent -> CBool -> CDouble -> CDouble -> CBool -> CDouble -> IO (Status)
-- fmi2Status fmi2SetupExperiment(fmi2Component c,
-- fmi2Boolean toleranceDefined,
-- fmi2Real tolerance,
-- fmi2Real startTime,
-- fmi2Boolean stopTimeDefined,
-- fmi2Real stopTime);
-- We are ignoring anything but stopTime currently, and we expect it to be defined.
foreign export ccall fmi2SetupExperiment :: FMISetupExperimentType
fmi2SetupExperiment :: FMISetupExperimentType
fmi2SetupExperiment comp _ _ _ _ stopTime = do
  state' <- getState comp
  writeState comp $ FMIComponent {testInput = testInput state', testOutput = testOutput state', stopTime = Just stopTime, state = state state'}
  (pure . statusToCInt) OK

foreign export ccall fmi2EnterInitializationMode :: FMIEnterInitializationModeType
fmi2EnterInitializationMode :: FMIEnterInitializationModeType
fmi2EnterInitializationMode comp = do
  state <- getState comp
  pure $ case stopTime state of
    Just(_) -> statusToCInt OK
    Nothing -> statusToCInt Fatal

foreign export ccall fmi2ExitInitializationMode :: FMIExitInitializationModeType
fmi2ExitInitializationMode :: FMIExitInitializationModeType
fmi2ExitInitializationMode _ = pure . statusToCInt $ OK

foreign export ccall fmi2SetInteger :: FMISetIntegerType
fmi2SetInteger :: FMISetIntegerType
fmi2SetInteger comp varRefs size varVals = do
  varRefs' <- peekArray (fromIntegral size) varRefs
  varVals' <- peekArray (fromIntegral size) varVals
  let refVal = zip varRefs' varVals' in
    case hd refVal of
      (1,1) -> _
      otherwise ->  pure $ statusToCInt Fatal

setCorrespondingValue :: FMIComponent -> CInt -> FMIComponent
setCorrespondingValue st val = do
  case state st of
    ModelUnderEvaluation -> FMIComponent {testInput = (fromIntegral val), testOutput = (fromIntegral val)+10 , stopTime = stopTime st, state = state st}
    otherwise  -> FMIComponent {testInput = (fromIntegral val), testOutput = (fromIntegral val), stopTime = stopTime st, state = state st}

-- Retrieves state
getState :: StablePtr (IORef a) -> IO a
getState = deRefStablePtr >=> readIORef

-- Stores state
writeState :: StablePtr (IORef a) -> a -> IO ()
writeState ptr state = deRefStablePtr ptr >>= \ioref -> writeIORef ioref state

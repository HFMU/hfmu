module Data.HFMU.Internal.FMITypes where

import qualified Data.HFMU.Types as T
import Foreign.C.Types
import Foreign.C.String
import Foreign (StablePtr, FunPtr, Ptr, nullPtr, Int32)
import Data.IORef
import Foreign.Storable
import Control.Monad (ap)

data LogCategory2 = Ok | Warning | Error | Fatal

showLogCategory :: LogCategory2 -> String
showLogCategory Ok = "OK"
showLogCategory Warning = "Warning"
showLogCategory Error = "Error"
showLogCategory Fatal = "Fatal"

logCategoryToInt :: LogCategory2 -> Int32
logCategoryToInt Ok = 0
logCategoryToInt Warning = 1
logCategoryToInt Error = 3 -- Skipping fmi2Discard
logCategoryToInt Fatal = 4

type LoggingFunctionType = LogCategory2 -> String -> IO ()

foreign import ccall "dynamic" mkFunPtrLogger :: CallbackLogger -> CompEnvT -> CString -> FMIStatus -> CString -> CString -> IO ()

-- | The 'createLoggingFunction' creates a logging function that accepts a LogCategory2 and a message and sends it to the C callback.
createLoggingFunction :: CallbackFunctions -> String -> LoggingFunctionType
createLoggingFunction cbFuncs instanceName logCat msg =
  let loggerFunction = logger cbFuncs
      loggerFunctionPtr = mkFunPtrLogger loggerFunction
   in do
        instanceName_ <- newCString instanceName
        logCategory <- newCString (showLogCategory logCat)
        msg_ <- newCString msg
        loggerFunctionPtr nullPtr instanceName_ (CInt (logCategoryToInt logCat)) logCategory msg_
statusToCInt :: T.Status -> CInt
statusToCInt = fromIntegral . fromEnum

type FMIStatus = CInt
type CompEnvT = Ptr ()
data FMUState = Instantiated
  | InitializationMode
  | SlaveInitialized
  | Terminated
  deriving (Enum, Show)

type FMUStateType a = StablePtr (IORef (FMIComponent a))

type FMIFuncReturn = IO CInt

data FMIComponent x = FMIComponent {fcVars :: T.SVs,
                                  fcDoStep :: T.DoStepFunType x,
                                  fcEndTime :: Maybe Double,
                                  fcState :: FMUState,
                                  fcPeriod :: Double,
                                  fcRemTime :: Double,
                                  fcUserState :: T.UserState x,
                                  fcLoggingFunction :: LoggingFunctionType}

type CallbackLogger =
  FunPtr(CompEnvT -> CString ->  FMIStatus -> CString -> CString -> IO ())

type StepFinished = FunPtr(CompEnvT -> FMIStatus -> IO ())

data CallbackFunctions = CallbackFunctions {logger :: CallbackLogger,
                                            allocMem :: Ptr(), -- Ignoring
                                            freeMem :: Ptr (), -- Ignoring
                                            stepFinished :: StepFinished,
                                            compEnv :: CompEnvT}

#include "fmi2Functions.h"


instance Storable CallbackFunctions where
  sizeOf _ = (#size fmi2CallbackFunctions)
  alignment _ = (#alignment fmi2CallbackFunctions)
  peek p = return CallbackFunctions
    `ap` (#{peek fmi2CallbackFunctions, logger} p)
    `ap` return nullPtr
    `ap` return nullPtr
    `ap` (#{peek fmi2CallbackFunctions, stepFinished} p)
    `ap`  (#{peek fmi2CallbackFunctions, componentEnvironment} p)

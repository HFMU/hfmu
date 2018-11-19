{-# LINE 1 "HSFMIInterface.hsc" #-}
module HSFMIInterface where
import Foreign.Storable
import Foreign (Ptr, nullPtr, FunPtr, StablePtr)
import Foreign.C.Types (CInt)
import Foreign.C.String
import Control.Monad (ap)



type CompEnvT = Ptr ()
type FMIStatus = CInt
type FMIType = CInt

data Status = OK | Warning | Discard | Error | Fatal | Pending deriving (Enum, Show)
type CallbackLogger = FunPtr(CompEnvT -> CString -> FMIStatus -> CString -> CString -> IO ())
type StepFinished = FunPtr(CompEnvT -> FMIStatus -> IO ())
data CallbackFunctions = CallbackFunctions {
  logger :: CallbackLogger,
  allocMem :: Ptr(), -- Ignoring
  freeMem :: Ptr (), -- Ignoring
  stepFinished :: StepFinished,
  compEnv :: CompEnvT
  }

instance Storable CallbackFunctions where
  sizeOf _ = ((40))
{-# LINE 27 "HSFMIInterface.hsc" #-}
  alignment _ = (8)
{-# LINE 28 "HSFMIInterface.hsc" #-}
  peek p = return CallbackFunctions
    `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
{-# LINE 30 "HSFMIInterface.hsc" #-}
    `ap` return nullPtr
    `ap` return nullPtr
    `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 24) p)
{-# LINE 33 "HSFMIInterface.hsc" #-}
    `ap`  ((\hsc_ptr -> peekByteOff hsc_ptr 32) p)
{-# LINE 34 "HSFMIInterface.hsc" #-}


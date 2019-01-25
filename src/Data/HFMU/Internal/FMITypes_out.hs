{-# LINE 1 "src/Data/HFMU/Internal/FMITypes.hs" #-}
{-# LINE 1 "FMITypes.hsc" #-}
module Data.HFMU.Internal.FMITypes where
import qualified Data.HFMU.Types as T
import Foreign.C.Types (CInt)
import Foreign.C.String
import Foreign (StablePtr, FunPtr, Ptr)
import Data.IORef
import Foreign.Storable
import Control.Monad (ap)


type FMIStatus = CInt
type CompEnvT = Ptr ()
data FMUState = Instantiated
  | InitializationMode
  | SlaveInitialized
  | Terminated
  deriving (Enum, Show)

type FMUStateType a = StablePtr (IORef (FMIComponent a))

type FMIFuncReturn = IO CInt

data FMIComponent x = FMIComponent {vars :: T.SVs,
                                  doStep :: T.DoStepFunType x,
                                  endTime :: Maybe Double,
                                  state :: FMUState,
                                  period_ :: Double,
                                  remTime :: Double,
                                  userState_ :: T.UserState x }

type CallbackLogger =
  FunPtr(CompEnvT -> CString ->  FMIStatus -> CString -> CString -> IO ())

type StepFinished = FunPtr(CompEnvT -> FMIStatus -> IO ())

data CallbackFunctions = CallbackFunctions {logger :: CallbackLogger,
                                            allocMem :: Ptr(), -- Ignoring
                                            freeMem :: Ptr (), -- Ignoring
                                            stepFinished :: StepFinished,
                                            compEnv :: CompEnvT}




instance Storable CallbackFunctions where
  sizeOf _ = ((40))
{-# LINE 47 "FMITypes.hsc" #-}
  alignment _ = (8)
{-# LINE 48 "FMITypes.hsc" #-}
  peek p = return CallbackFunctions
    `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 0) p)
{-# LINE 50 "FMITypes.hsc" #-}
    `ap` return nullPtr
    `ap` return nullPtr
    `ap` ((\hsc_ptr -> peekByteOff hsc_ptr 24) p)
{-# LINE 53 "FMITypes.hsc" #-}
    `ap`  ((\hsc_ptr -> peekByteOff hsc_ptr 32) p)
{-# LINE 54 "FMITypes.hsc" #-}

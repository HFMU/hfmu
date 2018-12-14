module FMIFunctions where

import Data.IORef
import HSFMIInterface
import Foreign (Ptr, nullPtr, FunPtr, StablePtr)
import Foreign.C.Types (CInt, CBool, CDouble, CSize)
import Vars

type FMUStateType = StablePtr (IORef FMIComponent)

type FMIFuncReturn = IO (CInt)

type FMISetupExperimentType = FMUStateType -> CBool -> CDouble -> CDouble -> CBool -> CDouble -> FMIFuncReturn

type FMIEnterInitializationModeType = FMUStateType -> FMIFuncReturn

type FMIExitInitializationModeType = FMUStateType -> FMIFuncReturn

type FMITerminateType = FMUStateType -> FMIFuncReturn

type FMIFreeInstanceType = FMUStateType -> FMIFuncReturn

type FMISetIntegerType = FMUStateType -> Ptr CInt -> CSize -> Ptr CInt -> FMIFuncReturn

type FMISetRealType = FMUStateType -> Ptr CInt -> CSize -> Ptr CDouble -> FMIFuncReturn

type FMIGetBooleanType = FMUStateType -> Ptr CInt -> CSize -> Ptr CBool -> FMIFuncReturn

type FMIGetRealType = FMUStateType -> Ptr CInt -> CSize -> Ptr CDouble -> FMIFuncReturn

type FMIDoStepType = FMUStateType -> CDouble -> CDouble -> CBool -> FMIFuncReturn


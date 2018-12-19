module FMIFunctions where

import Data.IORef
import HSFMIInterface
import Foreign (Ptr, nullPtr, FunPtr, StablePtr)
import Foreign.C.Types (CInt, CUInt, CBool, CDouble, CSize)
import Vars

type FMUStateType = StablePtr (IORef FMIComponent)

type FMIFuncReturn = IO (CInt)

type FMISetupExperimentType = FMUStateType -> CBool -> CDouble -> CDouble -> CBool -> CDouble -> FMIFuncReturn

type FMIEnterInitializationModeType = FMUStateType -> FMIFuncReturn

type FMIExitInitializationModeType = FMUStateType -> FMIFuncReturn

type FMITerminateType = FMUStateType -> FMIFuncReturn

type FMIFreeInstanceType = FMUStateType -> FMIFuncReturn

type FMISetIntegerType = FMUStateType -> Ptr CUInt -> CSize -> Ptr CInt -> FMIFuncReturn

type FMISetRealType = FMUStateType -> Ptr CUInt -> CSize -> Ptr CDouble -> FMIFuncReturn

type FMIGetBooleanType = FMUStateType -> Ptr CUInt -> CSize -> Ptr CInt -> FMIFuncReturn

type FMIGetRealType = FMUStateType -> Ptr CUInt -> CSize -> Ptr CDouble -> FMIFuncReturn

type FMIDoStepType = FMUStateType -> CDouble -> CDouble -> CBool -> FMIFuncReturn


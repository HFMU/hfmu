{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-} -- To allow for same field name in different records

module HFMU where

import Foreign.C.String
import Foreign.StablePtr
import Data.IORef
import System.IO.Unsafe
import Control.Monad

-- FMI Ports state
newtype PortState = PortState Int

-- User defined state
newtype UState = UState Int 

-- User Accesible State
data UAccState = UAccState {pState :: PortState, uState :: UState}

-- State of the entire FMU
data HState = HState {fmuTRemain :: FmuTRemain,
                      fmuSS :: FmuSS,
                      uAccState :: UAccState}

-- Remaining time of step
newtype FmuTRemain = FmuTRemain Double 

-- Step size of FMU
newtype FmuSS = FmuSS Double

-- Communication Step Size (size of the step the FMU should progress)
newtype FmiSS = FmiSS Double

-- Current time of master
newtype FmiT = FmiT Double

data Fmi2Status = Fmi2OK | Fmi2Warning | Fmi2Discard | Fmi2Error | Fmi2Fatal deriving Enum

foreign export ccall fmi2DoStep :: StablePtr (IORef HState) -> FmiT -> FmiSS -> Bool -> IO Int
fmi2DoStep :: StablePtr (IORef HState) -> FmiT -> FmiSS -> Bool -> IO Int
fmi2DoStep stPtr fmiMasterT fmiSS delPrevState =
  do
    state <- getState stPtr
    doStepFunction <- getDoStepFunction
    case doStepFunction of
      Nothing -> pure $ fromEnum Fmi2Fatal
      Just doStepF -> do
        pSRes <- performStep doStepF (uAccState (state :: HState)) fmiSS (fmuSS state) (fmuTRemain state)
        case pSRes of
          Left fmi2Status -> pure $ fromEnum fmi2Status
          Right pStepResult -> writeState stPtr (updateHStateFromPStepResult state pStepResult) >> pure (fromEnum Fmi2OK)

updateHStateFromPStepResult :: HState -> PStepResult -> HState
updateHStateFromPStepResult state pStepResult = HState {fmuTRemain = tRemain pStepResult, fmuSS=fmuSS state, uAccState = uAccState (pStepResult :: PStepResult)}


-- Result from performStep function
data PStepResult = PStepResult {tRemain :: FmuTRemain, uAccState :: UAccState}

-- Performs X dosteps.
-- For example - if FmuTRemain is 2, FmuSS is 3, and FmiSS is 10 then it performs
-- 1 step and then FmiSS is 8 and FmuTRemain is 3
-- 1 step and then FmiSS is 5 and FMUTRemain is 3
-- 1 step and then FmiSS is 2 and FMUTRemain is 3
-- 0 step and then FMUTRemain is 1
-- In total: 3 steps
performStep :: (UAccState -> IO (Either Fmi2Status UAccState)) -> UAccState -> FmiSS -> FmuSS -> FmuTRemain -> IO (Either Fmi2Status PStepResult)
performStep userDoStep state (FmiSS fmiSS) fmuSS'@(FmuSS fmuSS) (FmuTRemain fmuTRemain) =
  if fmiSS >= fmuTRemain
  then
    do
      r <- userDoStep state
      case r of
        (Left err) -> pure $ Left err
        (Right uAccState) -> performStep userDoStep uAccState (FmiSS (fmiSS - fmuTRemain)) fmuSS' (FmuTRemain fmuSS)
  else pure $ Right (PStepResult {tRemain = FmuTRemain (fmuTRemain - fmiSS), uAccState = state})

getState :: StablePtr (IORef a) -> IO a
getState = deRefStablePtr >=> readIORef

writeState :: StablePtr(IORef a) -> a -> IO ()
writeState ptr state = deRefStablePtr ptr >>= \ioref -> writeIORef ioref state

{-# NOINLINE stDoStepFunction #-}
stDoStepFunction :: IORef (Maybe (UAccState -> IO (Either Fmi2Status UAccState)))
stDoStepFunction = unsafePerformIO $ newIORef Nothing

storeDoStepFunction :: (UAccState -> IO (Either Fmi2Status UAccState)) -> IO ()
storeDoStepFunction  = ((writeIORef stDoStepFunction) . Just)

getDoStepFunction :: IO (Maybe (UAccState -> IO (Either Fmi2Status UAccState)))
getDoStepFunction = readIORef stDoStepFunction

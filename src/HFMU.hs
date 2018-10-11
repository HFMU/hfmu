{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-} -- To allow for same field name in different records

module HFMU where

import Foreign.C.Types (CDouble (CDouble), CInt (CInt), CSize (CSize))
import Foreign (Ptr)
import Foreign.StablePtr
import Data.IORef
import System.IO.Unsafe
import Control.Monad
import StateTypes

{-
-}
foreign export ccall fmi2DoStep :: CString -> CInt -> CString -> CString -> 

{-
stPtr is state pointer
cfmiMasterT is the current time of the master
fmiSS is the step size
delPrevState is whether to delete the previous state or not
-}
foreign export ccall fmi2DoStep :: StablePtr (IORef HState) -> CDouble -> CDouble -> Bool -> IO Int
fmi2DoStep :: StablePtr (IORef HState) -> CDouble -> CDouble -> Bool -> IO Int
fmi2DoStep stPtr cfmiMasterT (CDouble fmiSS) delPrevState =
  do
    state <- getState stPtr
    doStepFunction <- getDoStepFunction
    case doStepFunction of
      Nothing -> pure $ fromEnum Fmi2Fatal
      Just doStepF -> do
        pSRes <- performStep doStepF (uAccState (state :: HState)) (FmiSS fmiSS) (fmuSS state) (fmuTRemain state)
        case pSRes of
          Left fmi2Status -> pure $ fromEnum fmi2Status
          Right pStepResult -> let newState = updateHStateFromPStepResult state pStepResult in
            writeState stPtr newState >> pure (fromEnum Fmi2OK)

updateHStateFromPStepResult :: HState -> PStepResult -> HState
updateHStateFromPStepResult state pStepResult = HState {fmuTRemain = tRemain pStepResult,
                                                        fmuSS=fmuSS state,
                                                        uAccState = uAccState (pStepResult :: PStepResult)}


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

-- Retrieves state
getState :: StablePtr (IORef a) -> IO a
getState = deRefStablePtr >=> readIORef

-- Stores state
writeState :: StablePtr (IORef a) -> a -> IO ()
writeState ptr state = deRefStablePtr ptr >>= \ioref -> writeIORef ioref state

-- Functions related to the user defined doStep function
{-# NOINLINE stDoStepFunction #-}
stDoStepFunction :: IORef (Maybe (UAccState -> IO (Either Fmi2Status UAccState)))
stDoStepFunction = unsafePerformIO $ newIORef Nothing

storeDoStepFunction :: (UAccState -> IO (Either Fmi2Status UAccState)) -> IO ()
storeDoStepFunction = ((writeIORef stDoStepFunction) . Just)

getDoStepFunction :: IO (Maybe (UAccState -> IO (Either Fmi2Status UAccState)))
getDoStepFunction = readIORef stDoStepFunction

foreign export ccall fmi2GetReal :: StablePtr (IORef HState) -> Ptr CInt -> CSize -> Ptr CDouble -> IO CInt
fmi2GetReal :: StablePtr (IORef HState) -> Ptr CInt -> CSize -> Ptr CDouble -> IO CInt
fmi2GetReal = undefined

module StateTypes where

-- FMI Ports state
data PortState = PortState {outputValve :: Bool, inputLevel :: Int, parameterMinLevel :: Int, parameterMaxLevel :: Int }

setOutputValve pState val = setValRefBool pState 1 val

getInputLevel pState = getValRefInt pState 2

getParameterMinLevel pState = getValRefInt pState 3

getParameterMaxLevel pState = getValRefInt pState 4


getValRefBool pState x = case x of
  1 -> Just $ outputValve pState
  _ -> Nothing
setValRefBool pState x val = case x of
  1 -> Just $ pState { outputValve = val }
  _ -> Nothing

setValRefInt pState x val = case x of
  2 -> Just $ pState {inputLevel = val}
  3 -> Just $ pState {parameterMinLevel = val}
  4 -> Just $ pState {parameterMaxLevel = val}
  _ -> Nothing

getValRefInt pState x = case x of
  2 -> Just $ inputLevel pState
  3 -> Just $ parameterMinLevel pState
  4 -> Just $ parameterMaxLevel pState
  _ -> Nothing

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

data Fmi2States = Nothing | Instantiated | Initialization | SlaveInitialized

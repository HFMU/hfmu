module Data.HFMU.Types where
import qualified Data.HashMap.Strict as HM

data SVType = Real | Integer | String | Boolean deriving (Show)
data SVTypeVal = RealVal Double | IntegerVal Int | StringVal String | BooleanVal Bool deriving (Show)
data SVCausality = Input | Output | Parameter deriving (Show)
data SVVariability = Fixed | Continuous | Discrete deriving (Show)
data SVInitial = Exact | Calculated deriving (Show)

data SV = SV {svRef :: Int,
              svCausality :: SVCausality,
              svVariability :: SVVariability,
              svInitial :: Maybe SVInitial,
              svType :: SVType,
              svVal :: SVTypeVal} deriving (Show)

type SVs = HM.HashMap String SV


data FMIStatus = OK | Warning | Discard | Error | Fatal | Pending deriving (Enum, Show, Eq)

newtype UserState x = UserState x

data DoStepResult x = DoStepResult {dsrStatus :: FMIStatus, dsrSvs :: SVs, dsrState :: UserState x }

type DoStepFunType a = SVs -> UserState a -> DoStepResult a

data Setup a = Setup {sSvs :: SVs,
                   sDoStepFunc :: DoStepFunType a,
                   sPeriod :: Double,
                   sUserState :: UserState a}

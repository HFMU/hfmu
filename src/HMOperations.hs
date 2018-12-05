module HMOperations where

import Vars
import qualified Data.HashMap.Strict as HM

adjustPortVal :: SV -> String -> SVTypeVal -> SV
adjustPortVal sv name v = HM.adjust (\p -> p {val = v}) name sv


getPortVal :: SV -> String -> Maybe SVTypeVal
getPortVal sv name = val <$> HM.lookup name sv

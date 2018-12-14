module HMOperations where

import Vars
import qualified Data.HashMap.Strict as HM

adjustPortVal :: SVs -> String -> SVTypeVal -> SVs
adjustPortVal sv name v = HM.adjust (\p -> p {val = v}) name sv


getPortVal :: SVs -> String -> Maybe SVTypeVal
getPortVal sv name = val <$> HM.lookup name sv



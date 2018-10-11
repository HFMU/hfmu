-- To allow name clashes in record fields
{-# LANGUAGE DuplicateRecordFields #-}

module Vars where

data SVType = Real | Integer | String | Boolean
data SVTypeVal = RealVal Double | IntegerVal Int | StringVal String | BooleanVal Bool

data Port = Port {name :: String, valueReference :: Int, fmi2Type :: SVType, value :: Maybe SVTypeVal}

data InputPort = InputPort {port :: Port, outputUpdates :: Maybe (SVTypeVal -> [(String, SVTypeVal)])}

data OutputPort = OutputPort {port :: Port, dependsOn :: [String]}
data ParameterPort = ParameterPort {port :: Port}

data Ports = Ports {inputPorts :: [InputPort], outputPorts :: [OutputPort], parameterPorts :: [ParameterPort]}

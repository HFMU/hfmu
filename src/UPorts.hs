{-# LANGUAGE DuplicateRecordFields #-}
module UPorts where
import Vars


outputValve :: OutputPort
outputValve = OutputPort {port = Port {name = "valve", valueReference = 1, fmi2Type = Boolean, value = Just $ BooleanVal False}, dependsOn=[]}

inputLevel :: InputPort
inputLevel = InputPort {port = Port {name = "level", valueReference = 2, fmi2Type = Real, value = Just $ RealVal 2}, outputUpdates=Nothing}



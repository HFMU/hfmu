module UPorts where
import HFMU

initialInt :: Int -> Ctxt -> Ctxt
initialInt = undefined

-- start=xx;
setOutputValve :: Ctxt -> Bool -> Ctxt
setOutputValve = undefined

getOutputValve :: Ctxt -> Bool
getOutputValve = undefined

-- start=xx; name=xx;
getInputLevel :: Ctxt -> Int
getInputLevel = undefined

-- start=xx; name=xx;
getParameterMinLevel :: Ctxt -> Int
getParameterMinLevel = undefined

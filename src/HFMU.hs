{-# LANGUAGE ScopedTypeVariables #-}

module HFMU where

import HSFMIInterface
import Foreign.C.String
import Foreign.C.Types
import Foreign

data FMIComponent = FMIComponent {test :: Int}

foreign import ccall "dynamic" mkFunPtrLogger :: CallbackLogger -> CompEnvT -> CString -> FMIStatus -> CString -> CString -> IO ()

--fmi2Instantiate :: instanceName -> fmuType -> fmuGUID -> fmuResourceLocation -> functions -> visible -> loggingOn
foreign export ccall fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr CallbackFunctions -> CBool -> CBool -> IO (StablePtr FMIComponent)
fmi2Instantiate :: CString -> CInt -> CString -> CString -> Ptr CallbackFunctions -> CBool -> CBool -> IO (StablePtr FMIComponent)
fmi2Instantiate _ _ _ _ ptrCbFuncs _ _ = do
  (cbFuncs :: CallbackFunctions) <- peek ptrCbFuncs
  -- logger fmi2ComponentEnv instanceName::String Status::fmi2Status Category::String Message::String
  instanceName :: CString <- newCString "instanceName";
  category :: CString <- newCString "logError";
  msg :: CString <- newCString "HS-Message: Error";
  (mkFunPtrLogger . logger $ cbFuncs) nullPtr instanceName (CInt 3) category msg
  newStablePtr $ FMIComponent {test = 2}

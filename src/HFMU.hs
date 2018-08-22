{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-} -- To allow for same field name in different records

module HFMU where

import Foreign.C.String
import Foreign.StablePtr
import Data.IORef
import System.IO.Unsafe
import Control.Monad

newtype Ctxt = Ctxt { lst :: [Int] } deriving Show

-- https://www.reddit.com/r/haskell/comments/8693a3/usefulness_of_duplicaterecordfields/

data DoStepReturn = DoStepReturn {context :: Ctxt,
                                status :: Fmi2Status}

data DoStepData = DoStepData {context :: Ctxt,
                             stepSize :: Double}


foreign export ccall foo :: Int -> IO Int
foo :: Int -> IO Int
foo n = do
  putStrLn "HS: Foo invoked"
  return 5

foreign export ccall instantiateLEG :: CString -> IO Bool
instantiateLEG :: CString -> IO Bool
instantiateLEG x = do
  putStrLn "HS: instantiateLEG invoked"
  name <- peekCString x
  putStrLn $ mappend "HS: name parameter " name
  return True

foreign export ccall instantiate :: CString -> IO (StablePtr (IORef Ctxt))
instantiate :: CString -> IO (StablePtr (IORef Ctxt))
instantiate x = do
  putStrLn "HS: instantiate invoked"
  name <- peekCString x
  putStrLn $ mappend "HS: name parameter " name
  ref <- newIORef (Ctxt [1])
  ptr <- newStablePtr ref
  pure ptr

foreign export ccall printEnv :: StablePtr (IORef Ctxt) -> IO Int
printEnv :: StablePtr (IORef Ctxt) -> IO Int
printEnv x = do
  putStrLn "HS: printEnv invoked"
  (ref :: IORef Ctxt) <- deRefStablePtr x
  (d :: Ctxt) <- readIORef ref
  putStrLn $ mappend "HS: " (show d)
  return 2

foreign export ccall updateEnv :: StablePtr (IORef Ctxt) -> IO Bool
updateEnv :: StablePtr (IORef Ctxt) -> IO Bool
updateEnv x = do
  putStrLn "HS updateEnv: invoked"
  (ref :: IORef Ctxt) <- deRefStablePtr x
  modifyIORef' ref updCtxt
  ctxt <- readIORef ref
  putStrLn $ mappend "HS updateEnv: IORef " (show ctxt)
  return True
  where
    updCtxt :: Ctxt -> Ctxt
    updCtxt ctxt = let x = lst ctxt in
      Ctxt (x ++ [2])

newtype StepSize = StepSize Double;

data Fmi2Status = Fmi2OK | Fmi2Warning | Fmi2Discard | Fmi2Error | Fmi2Fatal
fmi2StatusToInt :: Fmi2Status -> Int
fmi2StatusToInt Fmi2OK = 0;
fmi2StatusToInt Fmi2Warning = 1;
fmi2StatusToInt Fmi2Discard = 2;
fmi2StatusToInt Fmi2Error = 3;
fmi2StatusToInt Fmi2Fatal = 4;


foreign export ccall fmi2DoStep :: StablePtr (IORef Ctxt) -> Double -> Double -> Bool -> IO Int
fmi2DoStep :: StablePtr (IORef Ctxt) -> Double -> Double -> Bool -> IO Int
fmi2DoStep state t h delPrevState = do
  ctxt <- getCtxt state
  (uDoStep :: Maybe (DoStepData -> IO DoStepReturn)) <- readIORef stDoStepFunction
  case uDoStep of
    Nothing -> pure $ fmi2StatusToInt Fmi2Fatal
    Just uDoStep' ->
      do
        (dsr :: DoStepReturn) <- uDoStep' DoStepData {context = ctxt, stepSize = h}
        writeCtxt state (context (dsr :: DoStepReturn))
        pure $ (fmi2StatusToInt . status) dsr

getCtxt :: StablePtr (IORef Ctxt) -> IO Ctxt
getCtxt = deRefStablePtr >=> readIORef

writeCtxt :: StablePtr(IORef Ctxt) -> Ctxt -> IO ()
writeCtxt ptr ctxt = deRefStablePtr ptr >>= \ioref -> writeIORef ioref ctxt

stDoStepFunction :: IORef (Maybe (DoStepData -> IO DoStepReturn))
stDoStepFunction = unsafePerformIO (newIORef Nothing)

storeDoStepFunction :: (DoStepData -> IO DoStepReturn) -> IO ()
storeDoStepFunction f = do
  writeIORef stDoStepFunction $ Just f


{-# LANGUAGE ForeignFunctionInterface #-}

module HFMU where

foreign export ccall foo :: Int -> IO Int

foo :: Int -> IO Int
foo n = do
  putStrLn "Foo invoked"
  return 5

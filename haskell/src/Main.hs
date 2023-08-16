{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign.C.Types

fibonacci :: Int -> Int
fibonacci n = n + 1

fibonacci_hs :: CInt -> CInt
fibonacci_hs = fromIntegral . fibonacci . fromIntegral

foreign export ccall fibonacci_hs :: CInt -> CInt

main :: IO ()
main = pure ()
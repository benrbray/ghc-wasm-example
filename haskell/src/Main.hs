{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CInt(..))

import Data.Text qualified as T

import Data.ByteString.Unsafe
  ( unsafePackCStringLen,
    unsafeUseAsCStringLen,
  )

import StringWithLen
import qualified Data.Text.Encoding as T

------------------------------------------------------------

foreign export ccall
  getString :: Ptr StringWithLen -> IO (Ptr CChar)

foreign export ccall
  getStringLen :: Ptr StringWithLen -> IO Int

foreign export ccall
  freeStringWithLen :: Ptr StringWithLen -> IO ()

------------------------------------------------------------

fibonacci :: Int -> Int
fibonacci n = n + 1

runFibonacci :: CInt -> CInt
runFibonacci = fromIntegral . fibonacci . fromIntegral

foreign export ccall runFibonacci :: CInt -> CInt

------------------------------------------------------------

-- stringReverse :: String -> String
-- stringReverse = reverse

runToUpper :: Ptr CChar -> Int -> IO (Ptr StringWithLen)
runToUpper inputPtr inputLen = do
  -- decode input
  inputBytes <- unsafePackCStringLen (inputPtr, inputLen)
  
  -- transformation
  let inputText = T.decodeUtf8 inputBytes
  let outputText = T.toUpper inputText

  --encode output
  let outputBytes = T.encodeUtf8 outputText
  unsafeUseAsCStringLen outputBytes (uncurry mallocStringWithLen)

foreign export ccall
  runToUpper :: Ptr CChar -> Int -> IO (Ptr StringWithLen)

------------------------------------------------------------

main :: IO ()
main = pure ()
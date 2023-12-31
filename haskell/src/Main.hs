{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Actions.Parse qualified as Parse

import Foreign (Ptr)
import Foreign.C.Types (CChar(..), CInt(..))

import Control.Exception (catch, SomeException)

import Data.Text (Text)
import Data.Text qualified as T
import Data.Aeson qualified as Aeson

import Data.ByteString qualified as BS
import Data.ByteString.Unsafe
  ( unsafePackCStringLen,
    unsafeUseAsCStringLen,
  )

import StringWithLen
import qualified Data.Text.Encoding as T

type ForeignStringTransform = Ptr CChar -> Int -> IO (Ptr StringWithLen)

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

runTextTransform :: (Text -> Text) -> ForeignStringTransform
runTextTransform trans inputPtr inputLen = do
  -- decode input
  inputBytes <- unsafePackCStringLen (inputPtr, inputLen)
  
  -- transformation
  let inputText = T.decodeUtf8 inputBytes
  let outputText = trans inputText

  --encode output
  let outputBytes = T.encodeUtf8 outputText
  unsafeUseAsCStringLen outputBytes (uncurry mallocStringWithLen)

runJsonTransform :: (Aeson.FromJSON a, Aeson.ToJSON b)
  => (a -> IO b)  -- transform
  -> (Text -> b)  -- display error
  -> ForeignStringTransform
runJsonTransform trans dispError inputPtr inputLen = do

  -- decode input JSON and run transform
  result <- handleError $ do
    inputBytes <- unsafePackCStringLen (inputPtr, inputLen)
    case Aeson.eitherDecodeStrict' inputBytes of
      Right input -> trans input
      Left e      -> error $ "error while decoding input: " ++ show e
  
  -- encode result JSON
  let outputBytes = BS.toStrict $ Aeson.encode result
  unsafeUseAsCStringLen outputBytes (uncurry mallocStringWithLen)

  where
    -- Uncaught errors aren't propagated to the browser, so
    -- this is a stop-gap to capture absolutely every error and
    -- send back the error without any other information
    formatError e = T.pack . show $ (e :: SomeException)
    handleError = (`catch` (pure . dispError . formatError))

------------------------------------------------------------

runToUpper :: ForeignStringTransform
runToUpper = runTextTransform T.toUpper

foreign export ccall runToUpper :: ForeignStringTransform

------------------------------------------------------------

runParse :: ForeignStringTransform
runParse = runJsonTransform Parse.runParse Parse.dispError

foreign export ccall runParse :: ForeignStringTransform

------------------------------------------------------------

main :: IO ()
main = pure ()
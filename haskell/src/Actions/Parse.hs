{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

module Actions.Parse where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Text qualified as T

-- megaparsec, parser-combinators
import Parse.Expr (parse, ParseResult (..))
import Syntax.Expr as Stx

--------------------------------------------------------------------------------

newtype Input = Input {
    inputText :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass FromJSON

data Output = Output {
    outputExpr :: Maybe Stx.Expr,
    outputError :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass ToJSON

--------------------------------------------------------------------------------

runParse :: Input -> IO Output
runParse Input{..} = pure $
  case parse inputText of
    Left err ->
      Output {
        outputExpr = Nothing,
        outputError = Just err
      }
    Right ParseResult{ parsedExpr } ->
      Output {
        outputExpr = Just parsedExpr,
        outputError = Nothing
      }

dispError :: Text -> Output
dispError t =
  Output {
    outputExpr = Nothing,
    outputError = Just t
  }
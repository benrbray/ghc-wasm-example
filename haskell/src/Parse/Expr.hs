module Parse.Expr where

import Data.Text (Text)
import Data.Text qualified as T
import Parse.Parser
import Parse.Keyword

import Text.Megaparsec ((<?>))
import Text.Megaparsec      qualified as MP
import Text.Megaparsec.Char qualified as MP

import Syntax.Expr qualified as Stx

------------------------------------------------------------

nameP :: Parser Stx.Name
nameP = nameP0 >>= check
  where
    nameP0 = T.pack <$> lexeme ((:) <$> MP.letterChar <*> MP.many MP.alphaNumChar <?> "variable")
    reserved = ["let", "in"]
    check :: Text -> Parser Stx.Name
    check s =
      if s `elem` reserved
      then fail $ "keyword " ++ show s ++ " cannot be used as a variable name"
      else return s

variableP :: Parser Stx.Expr
variableP = Stx.Var <$> nameP

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

letExprP :: Parser Stx.Expr
letExprP = Stx.Let <$> (_let *> nameP) <*> (_equal *> simpleExprP) <*> (_in *> exprP)

spineP :: Parser Stx.Expr
spineP = foldl1 Stx.App <$> MP.some simpleExprP

lamP :: Parser Stx.Expr
lamP = Stx.Lam <$> (_backslash *> nameP) <*> (_arrow *> exprP)

exprP :: Parser Stx.Expr
exprP =
  MP.choice [
    MP.try spineP,
    letExprP,
    lamP,
    variableP
  ]

simpleExprP :: Parser Stx.Expr
simpleExprP = MP.choice [ variableP, parens exprP ]

------------------------------------------------------------

newtype ParseResult = ParseResult
  { parsedExpr  :: Stx.Expr
  } deriving (Show, Eq)

parse :: Text -> Either Text ParseResult
parse t =
  case MP.runParser (exprP <* MP.eof) "[demo]" t of
    Left peb -> Left . T.pack . show $ MP.errorBundlePretty peb
    Right x -> Right $ ParseResult x
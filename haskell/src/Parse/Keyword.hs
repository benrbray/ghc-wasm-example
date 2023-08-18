module Parse.Keyword where

import Data.Text (Text)
import Parse.Parser

import qualified Text.Megaparsec.Char.Lexer as L

------------------------------------------------------------

_backslash :: Parser Text
_backslash = symbol "\\"

_arrow :: Parser Text
_arrow = symbol "->"

_let :: Parser Text
_let = symbol "let"

_in :: Parser Text
_in = symbol "in"

_equal :: Parser Text
_equal = symbol "="

_leftparen :: Parser Text
_leftparen = symbol "("

_rightparen :: Parser Text
_rightparen = symbol ")"

------------------------------------------------------------

integer :: Parser Integer
integer = lexeme L.decimal
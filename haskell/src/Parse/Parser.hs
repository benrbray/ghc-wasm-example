module Parse.Parser where

-- megaparsec, parser-combinators
import Text.Megaparsec      qualified as MP
import Text.Megaparsec.Char qualified as MP
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Void
import Data.Text (Text)

--------------------------------------------------------------------------------

type Parser = MP.Parsec Void Text

sc :: Parser ()
sc = L.space MP.space1 MP.empty MP.empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
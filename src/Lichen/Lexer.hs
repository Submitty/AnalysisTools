module Lichen.Lexer where

import qualified Data.ByteString as BS

import Text.Megaparsec
import Text.Megaparsec.ByteString
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Lexer as L

import Lichen.Error

type Lexer a = FilePath -> BS.ByteString -> Erring [a]

-- Parse a C-style character literal. Ex: 'a', '@'.
charLit :: Parser Char
charLit = char '\'' *> L.charLiteral <* char '\''

-- Parse a C-style string literal. Ex: "a", "hello, world".
strLit :: Parser String
strLit = char '\"' *> manyTill L.charLiteral (char '\"')

-- Parse a C-style identifier (letter or underscore followed by any number
-- of letters, digits, and underscores).
ident :: Parser String
ident = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')

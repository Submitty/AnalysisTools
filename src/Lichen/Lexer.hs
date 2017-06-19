module Lichen.Lexer where

import qualified Data.ByteString as BS

import Text.Megaparsec
import Text.Megaparsec.ByteString
import Text.Megaparsec.Error
import qualified Text.Megaparsec.Lexer as L

type LexError = ParseError (Token BS.ByteString) Dec
type Lexer a = FilePath -> BS.ByteString -> Either LexError [a]

charLit :: Parser Char
charLit = char '\'' *> L.charLiteral <* char '\''

strLit :: Parser String
strLit = char '\"' *> manyTill L.charLiteral (char '\"')

ident :: Parser String
ident = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

printLexError :: LexError -> IO ()
printLexError = putStrLn . parseErrorPretty

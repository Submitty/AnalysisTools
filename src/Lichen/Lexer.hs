module Lichen.Lexer where

import Data.Foldable()
import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS

import Text.Megaparsec
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L

import Lichen.Error

type Lexer a = FilePath -> BS.ByteString -> Erring [Tagged a]

data TokPos = TokPos
            { startLine :: !Pos
            , endLine :: !Pos
            , startCol :: !Pos
            , endCol :: !Pos
            } deriving (Show, Eq, Ord)
data Tagged a = Tagged { tdata :: a, tpos :: TokPos } deriving (Show, Eq)
instance Ord a => Ord (Tagged a) where
        compare (Tagged x _) (Tagged y _) = compare x y

wrap :: Foldable t => Parser (t a) -> b -> Parser (Tagged b)
wrap p x = do
        pos <- NE.head . statePos <$> getParserState
        s <- p
        --_ <- p
        return . Tagged x $ TokPos (sourceLine pos) (sourceLine pos) (sourceColumn pos) (sourceColumn pos <> unsafePos (fromIntegral $ length s))
        --return (x, TokPos (sourceLine pos) (sourceLine pos) (sourceColumn pos) (sourceColumn pos <> unsafePos 1))

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

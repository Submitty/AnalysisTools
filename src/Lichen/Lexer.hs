{-# LANGUAGE OverloadedStrings #-}

module Lichen.Lexer where

import Data.Aeson
import Data.Foldable()
import Data.Functor (($>))
import Data.Semigroup ((<>))
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as BS

import Text.Megaparsec
import Text.Megaparsec.ByteString

import Lichen.Error

type Lexer a = FilePath -> BS.ByteString -> Erring [Tagged a]

data TokPos = TokPos { startLine :: !Pos
                     , endLine :: !Pos
                     , startCol :: !Pos
                     , endCol :: !Pos
                     } deriving (Show, Eq, Ord)
data Tagged a = Tagged { tdata :: a, tpos :: TokPos } deriving (Show, Eq)
instance Ord a => Ord (Tagged a) where
    compare (Tagged x _) (Tagged y _) = compare x y
instance Functor Tagged where
    fmap f (Tagged x p) = Tagged (f x) p
instance Show a => ToJSON (Tagged a) where
    toJSON (Tagged x p) = object [ "token" .= show x
                                 , "start_line" .= unPos (startLine p)
                                 , "end_line" .= unPos (endLine p)
                                 , "start_col" .= unPos (startCol p)
                                 , "end_col" .= unPos (endCol p)
                                 ]

wrap :: Foldable t => Parser (t a) -> b -> Parser (Tagged b)
wrap p x = do
        pos <- NE.head . statePos <$> getParserState
        s <- p
        return . Tagged x $ TokPos (sourceLine pos) (sourceLine pos) (sourceColumn pos) (sourceColumn pos <> unsafePos (fromIntegral $ length s))

wrapid :: Foldable t => Parser (t a) -> Parser (Tagged (t a))
wrapid p = do
        pos <- NE.head . statePos <$> getParserState
        s <- p
        return . Tagged s $ TokPos (sourceLine pos) (sourceLine pos) (sourceColumn pos) (sourceColumn pos <> unsafePos (fromIntegral $ length s))

reserved :: String -> Parser String
reserved s = try (string s >> notFollowedBy (alphaNumChar <|> char '_') >> pure s)

operator :: String -> Parser String
operator = try . string

term :: Parser Char
term = char '\r' <|> (head <$> eol) <|> (eof $> '\n') 

-- Parse a C-style character literal. Ex: 'a', '@'.
charLit :: Parser String
charLit = char '\'' *> manyTill (noneOf ['\'']) (char '\'' <|> (eof >> pure ' '))

-- Parse a C-style string literal. Ex: "a", "hello, world".
strLit :: Parser String
strLit = char '\"' *> manyTill (noneOf ['"']) (char '\"' <|> (eof >> pure ' '))

quote :: String -> String
quote s = ('"':s) ++ ['"']

-- Parse a C-style identifier (letter or underscore followed by any number
-- of letters, digits, and underscores).
ident :: Parser String
ident = (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_')

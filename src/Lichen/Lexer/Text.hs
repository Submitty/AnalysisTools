{-# LANGUAGE OverloadedStrings #-}

module Lichen.Lexer.Text where

import Control.Applicative
import Control.Monad.Except

import Data.Char
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.ByteString

import Lichen.Error
import Lichen.Lexer

sc :: Parser ()
sc = void $ many spaceChar

lex :: Lexer T.Text
lex p d = case runParser (many (sc *> wrapid (some (satisfy (not . isSpace))) <* sc)) p d of
              Left e -> throwError $ LexError e
              Right t -> return (fmap T.pack <$> t)
